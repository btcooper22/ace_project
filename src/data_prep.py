import pandas as pd
import numpy as np
from imblearn.over_sampling import SMOTENC
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import OneHotEncoder, MinMaxScaler
from category_encoders.leave_one_out import LeaveOneOutEncoder
import re

import nltk
nltk.download("wordnet")
nltk.download("stopwords")


def start_pipeline(dataf):
    return dataf.copy()


def skip_missing_features(features, dataf):
    return [feature for feature in features if feature in dataf.columns]


def clean_data(dataf):

    # replace blank text fields with no_information
    text_features = ["medical_history", "examination_summary", "recommendation"]
    text_features = skip_missing_features(text_features, dataf)
    for feature in text_features:
            dataf[feature].fillna("no_information", inplace=True)

    # replace "None" values with nan or "Not Stated" category for ethnicity
    if "ethnicity" in dataf.columns:
        dataf.ethnicity.replace("None", "Not Stated", inplace=True)
    dataf.replace("None", np.nan, inplace=True)

    # set numeric features to float after removing "None" values
    num_features = ["age", "ox_sat", "resp_rate", "heart_rate", "temp"]
    num_features = skip_missing_features(num_features, dataf)
    for feature in num_features:
        dataf[feature] = dataf[feature].astype("float")

    if "ox_sat" in dataf.columns:
        dataf["ox_sat"] = dataf.ox_sat.apply(
            lambda ox_sat: 100 if ox_sat > 100 else ox_sat
        )

    # clean and convert cat features to categorical datatype
    for feature in dataf.columns:
        if feature in text_features + num_features:
            continue
        dataf[feature] = (dataf[feature]
                          .str.strip() # remove all punctuation
                          .str.lower() # set to all lower case
                          .astype("category"))
        if len(dataf[feature].cat.categories) > 50:
            print("=" * 80)
            print(f"Warning: {feature} has 50 or more categories")
            print("=" * 80)

    # set hospital_reqd to 1/0 for data analysis
    dataf["hospital_reqd"] = (dataf.hospital_reqd == "y").astype("int")

    return dataf


def fill_nas(dataf):

    # divide data into examples containing na values and complete examples
    na_mask = dataf.isna().any(axis=1)
    na_dataf = dataf[na_mask]
    complete_dataf = dataf[~na_mask]


    # produce list counting no of nas per example
    na_counts = na_dataf.isna().sum(axis=1)
    # remove examples with 2 or more nas (few in number and likely to be more noisy)
    na_dataf = na_dataf[na_counts == 1]

    # produce a list of feature names that contain na_values
    na_features = na_dataf.isna().any()
    na_feature_names = na_features[na_features].index

    for feature in na_feature_names:
        feature_dtype = na_dataf[feature].dtype
        if feature_dtype == "float":
            na_dataf[feature].fillna(
                complete_dataf[feature].mean(), inplace=True
            )
        elif feature_dtype.name == "category":
            na_dataf[feature].fillna(
                complete_dataf[feature].mode()[0], inplace=True
            )
        else:
            print(f"Warning: {feature} is dtype {feature_dtype}\n "
                  f"cannont process {feature_dtype} features - nas will "
                  f"persist")

    return pd.concat([complete_dataf, na_dataf])


def update_referral_from(dataf):

    # set referral from to 3 simple locations -> gp / ed / ccda
    referral_from_translations = {
        "gp": "gp",
        "a&e": "ed",
        "ed": "ed",
        "ccda": "ccda"
    }

    def translate_referral_from(text):
        for referral_string in referral_from_translations.keys():
            if referral_string in text[:4]:
                return referral_from_translations[referral_string]
        return "none"
    dataf["referral_from"] = dataf.referral_from.apply(
        lambda text: translate_referral_from(text)
    )

    return dataf


def add_allergy_features(dataf):
    # refactor allergy feature to one-hot-style Y / N features
    for allergy in ["food", "drug", "other"]:
        allergy_name = allergy + "_allergy"
        dataf[allergy_name] = dataf.allergies.apply(
            lambda x: 'y' if allergy in x.lower() else 'n'
        ).astype("category")

    dataf.drop("allergies", axis=1, inplace=True)

    return dataf


def add_ethnicity_features(dataf):
    # new ethnicity features
    # set reported ethnicity to other if not British / Pakistani
    dataf["simple_ethnicity"] = dataf.ethnicity.apply(
        lambda x: x if x in ["pakistani", "british"] else "other"
    ).replace(np.nan, "other")

    dataf["simple_ethnicity"] = dataf.simple_ethnicity.astype("category")

    # Group ethnicities into European / Asian / Other
    asian = ["indian", "mixed Asian", "pakistani", "white asain", "british asian",
             "asian", "sri Lankan", "other asian background", "bangladeshi"]

    european = ["slovak", "british", "other white background", "czech republic",
                "white europeon", "white british", "commonwealth russian",
                "other european", "mixed white"]

    dataf["group_ethnicity"] = "other"
    dataf.loc[dataf.ethnicity.isin(european), "group_ethnicity"] = "european"
    dataf.loc[dataf.ethnicity.isin(asian), "group_ethnicity"] = "asian"

    dataf["group_ethnicity"] = dataf.group_ethnicity.astype("category")
    dataf.drop("ethnicity", axis=1, inplace=True)

    return dataf


def add_ace_apls_features(dataf):
    # add features from ace referral sheets
    # ox_sat_low feature: < 94 = low
    dataf["ox_sat_low"] = dataf.ox_sat.apply(
        lambda x: "y" if x < 94 else "n"
    ).astype("category")

    # age_range feature: "pre_school" = (2-5), "primary" = (5-12),
    # "secondary" (12+)
    dataf["age_range"] = "pre_school"

    primary = (dataf.age >= 5) & (dataf.age < 12)
    dataf.loc[primary, "age_range"] = "primary"

    secondary = dataf.age >= 12
    dataf.loc[secondary, "age_range"] = "secondary"

    dataf["age_range"] = dataf.age_range.astype("category")

    # set low / normal / high ranges for ace / apls heart / resp rate
    # see ace & apls referral criteria for more info

    # generic function to set low / normal / high ranges for a feature within
    # age ranges
    def set_low_norm_high(feature, feature_range, age_range, new_feature_name):

        if new_feature_name not in dataf.columns:
            dataf[new_feature_name] = "normal"

        age_mask = ((dataf.age >= age_range[0]) &
                    (dataf.age < age_range[1]))
        low_mask = age_mask & (dataf[feature] < feature_range[0])
        high_mask = age_mask & (dataf[feature] > feature_range[1])

        dataf.loc[low_mask, new_feature_name] = "low"
        dataf.loc[high_mask, new_feature_name] = "high"

    # parameters for different apls / ace criteria
    ace_resp_rate_feature = {
        "feature": "resp_rate",
        "age_ranges": [(0, 5), (5, 12), (12, 18)],
        "feature_ranges": [(25, 30), (20, 25), (15, 20)],
        "new_feature_name": "ace_resp_rate_cat"
    }

    ace_heart_rate_feature = {
        "feature": "heart_rate",
        "age_ranges": [(0, 5), (5, 12), (12, 18)],
        "feature_ranges": [(95, 140), (80, 120), (60, 100)],
        "new_feature_name": "ace_heart_rate_cat"
    }

    apls_resp_rate_feature = {
        "feature": "resp_rate",
        "age_ranges": [(0, 2), (2, 8), (8, 12), (12, 18)],
        "feature_ranges": [(20, 40), (20, 30), (15, 25), (12, 24)],
        "new_feature_name": "apls_resp_rate_cat"
    }

    apls_heart_rate_feature = {
        "feature": "heart_rate",
        "age_ranges": [(0, 2), (2, 3), (3, 4), (4, 6), (6, 8),
                       (8, 12), (12, 14), (14, 18)],
        "feature_ranges": [(100, 160), (100, 150), (90, 140), (80, 135), (80, 130),
                           (70, 120), (65, 115), (60, 110)],
        "new_feature_name": "apls_heart_rate_cat"
    }

    high_low_features = [
        ace_resp_rate_feature,
        ace_heart_rate_feature,
        apls_resp_rate_feature,
        apls_heart_rate_feature
    ]

    for hl_feature in high_low_features:

        feature = hl_feature["feature"]
        age_ranges = hl_feature["age_ranges"]
        feature_ranges = hl_feature["feature_ranges"]
        new_feature_name = hl_feature["new_feature_name"]

        for age_range, feature_range in zip(age_ranges, feature_ranges):
            set_low_norm_high(feature=feature,
                              feature_range=feature_range,
                              age_range=age_range,
                              new_feature_name=new_feature_name)

        dataf[new_feature_name] = dataf[new_feature_name].astype("category")

    # overall feature for meeting ace referral criteria
    dataf["meets_ace_criteria"] = "n"

    meets_ace_criteria = ((dataf.ox_sat_low == "n") &
                          (dataf.ace_heart_rate_cat == "normal") &
                          (dataf.ace_resp_rate_cat == "normal") &
                          (dataf.gut_feeling != "unwell") &
                          (dataf.illness_severity != "Moderate"))

    dataf.loc[meets_ace_criteria, "meets_ace_criteria"] = "y"
    dataf["meets_ace_criteria"] = dataf.meets_ace_criteria.astype("category")

    return dataf


def add_free_text_features(dataf):

    stopwords = set(nltk.corpus.stopwords.words("english"))
    custom_stopwords = ["july", "and"]
    for stopword in custom_stopwords:
        stopwords.add(stopword)

    lemmatizer = nltk.WordNetLemmatizer()
    stemmer = nltk.SnowballStemmer("english")
    tokenizer = nltk.tokenize.RegexpTokenizer(r"(\d+|[a-zA-Z]+)")

    replace_words = {
        "asthma": r"asthm\w+",
        "wheeze": r"wheez\w+"
    }

    def ace_text_preprocessor(text):
        text = text.lower()
        for re_word in replace_words.keys():
            text = re.sub(replace_words[re_word], re_word, text)
        tokens = []
        for token in tokenizer.tokenize(text):
            if token.lower() not in stopwords:
                lem_token = lemmatizer.lemmatize(token, pos='v')
                stem_lem_token = stemmer.stem(lem_token)
                tokens.append(stem_lem_token)
        return " ".join(tokens)

    simple_text_features = {
        "hist_montel": ("montel", "medical_history"),
        "hist_wheez_episod": ("wheez episod", "medical_history"),
        "hist_know_asthma": ("know asthma", "medical_history"),
        "hist_asthma": ("asthma", "medical_history"),
        "summary_salbutamol": ("salbutamol", "examination_summary"),
        "recommend_prednisolon": ("prednisolon", "recommendation"),
        "recommend_inhal": ("inhal", "recommendation"),
        "recommend_spacer": ("spacer", "recommendation"),
    }

    def find_token(token, text):
        preprocessed_text = ace_text_preprocessor(text)
        return "y" if token in preprocessed_text else "n"

    text_features = ["medical_history", "examination_summary", "recommendation"]
    if all([text_feature in dataf.columns for text_feature in text_features]):
        for new_feature, text_feature_tuple, in simple_text_features.items():
            token, text_feature = text_feature_tuple
            dataf[new_feature] = dataf[text_feature].apply(
                lambda text: find_token(token, text)
            ).astype("category")
        dataf.drop(["medical_history",
                    "examination_summary",
                    "recommendation"],
                   axis=1, inplace=True)

    return dataf


def run_default_pipeline(dataf):

    dataf = (dataf.pipe(start_pipeline)
             .pipe(clean_data)
             .pipe(fill_nas))

    if "referral_from" in dataf.columns:
        dataf = dataf.pipe(update_referral_from)

    if "allergies" in dataf.columns:
        dataf = dataf.pipe(add_allergy_features)
    else:
        print("Warning: Allergies Missing - skipping")

    if "ethnicity" in dataf.columns:
        dataf = dataf.pipe(add_ethnicity_features)
    else:
        print("Warning: Ethnicity Missing - skipping")

    required_ace_apls_features = ["age", "resp_rate", "heart_rate", "ox_sat",
                                  "gut_feeling", "illness_severity"]
    missing_ace_apls_features = [
        feature for feature in required_ace_apls_features
        if feature not in dataf.columns
    ]
    if missing_ace_apls_features:
        print(f"Warning: {missing_ace_apls_features} missing - skipping")
    else:
        dataf = dataf.pipe(add_ace_apls_features)

    text_features = ["medical_history", "examination_summary",
                     "recommendation"]
    missing_text_features = [feature for feature in text_features
                             if feature not in dataf.columns]
    if missing_text_features:
        print(f"Warning: {missing_text_features} missing - skipping")
    else:
        dataf = dataf.pipe(add_free_text_features)

    return dataf


def return_train_test(dataf, run_pipeline=True):

    if run_pipeline:
        dataf = dataf.pipe(run_default_pipeline)

    # split train / test from complete examples only
    # (avoid introducing noise into test set from inferring nas)
    X_train, X_test, y_train, y_test = train_test_split(
        dataf.drop("hospital_reqd", axis=1),
        dataf.hospital_reqd,
        test_size=0.33,
        stratify=dataf.hospital_reqd,
        random_state=1)

    return X_train, y_train, X_test, y_test


def add_synthetic_examples(X_train, y_train):

    n_orig_examples = len(X_train)
    cat_feature_idxs = []
    for i, col in enumerate(X_train.columns):
        if not X_train[col].dtype in ["int", "float"]:
            cat_feature_idxs.append(i)

    smote = SMOTENC(random_state=1,
                    categorical_features=cat_feature_idxs)

    X_train, y_train = smote.fit_resample(X_train, y_train)

    # create is synthetic feature
    n_synthetic_examples = len(X_train) - n_orig_examples
    is_synthetic = np.concatenate([np.zeros(n_orig_examples),
                                  np.ones(n_synthetic_examples)])
    X_train["is_synthetic"] = is_synthetic

    return X_train, y_train


def encode_and_scale(X_train, y_train, X_test, cat_encoder, scaled=False):

    if cat_encoder not in ["one_hot", "target"]:
        raise ValueError('Encoder must be one of "one_hot" or "target"')

    for df in [X_train, y_train, X_test]:
        df.reset_index(drop=True, inplace=True)

    if "is_synthetic" in X_train.columns:
        contains_synthetic_examples = True
        synthetic = X_train.is_synthetic == 1
        X_train.drop("is_synthetic", axis=1, inplace=True)
    else:
        contains_synthetic_examples = False

    cat_features = [feature for feature in X_train.columns
                    if X_train[feature].dtype.name == "category"]

    num_features = [feature for feature in X_train.columns
                    if feature not in cat_features]

    if cat_encoder == "one_hot":

        full_df = pd.concat([X_train, X_test])
        encoder = OneHotEncoder(
            sparse=False,
            drop="if_binary"
        ).fit(full_df[cat_features])

        encoded_feature_names = []
        for feature, categories, drop_idx in zip(cat_features,
                                                 encoder.categories_,
                                                 encoder.drop_idx_):
            for idx, category in enumerate(categories):
                if idx == drop_idx:
                    continue
                name = feature + '_' + category
                encoded_feature_names.append(name)

    elif cat_encoder == "target":

        encoder = LeaveOneOutEncoder()
        if contains_synthetic_examples:
            encoder.fit(X_train[~synthetic][cat_features],
                        y_train[~synthetic])
        else:
            encoder.fit(X_train[cat_features], y_train)
        encoded_feature_names = cat_features

    if scaled:
        mm_scaler = MinMaxScaler().fit(X_train[num_features])

    encoded_scaled_train_test = []
    for df in [X_train, X_test]:
        enc_cat_data = pd.DataFrame(encoder.transform(df[cat_features]),
                                    columns=encoded_feature_names)

        num_data = df[num_features]
        if scaled:
            num_data = pd.DataFrame(mm_scaler.transform(num_data),
                                    columns=num_features)

        encoded_df = pd.concat([enc_cat_data, num_data], axis=1)

        encoded_scaled_train_test.append(encoded_df)

    X_train, X_test, = encoded_scaled_train_test

    return X_train, X_test
