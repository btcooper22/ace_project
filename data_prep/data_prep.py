import pandas as pd
import numpy as np
from imblearn.over_sampling import SMOTENC
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import OneHotEncoder, MinMaxScaler
from category_encoders.leave_one_out import LeaveOneOutEncoder


def start_pipeline(dataf):
    return dataf.copy()


def clean_data(dataf):

    # check columns are present and named correctly
    reqd_features = ['hospital_reqd', 'referral_from', 'age', 'address',
                     'ethnicity', 'gender', 'allergies', 'referral_date',
                     'referral_time', 'illness_severity', 'activity_level',
                     'gut_feeling', 'ox_sat', 'resp_rate', 'heart_rate',
                     'temp', 'sepsis', 'safeguarding']
    if sorted(dataf.columns) != sorted(reqd_features):
        missing_features = [feature for feature in reqd_features
                            if feature not in dataf.columns]
        extra_features = [feature for feature in dataf.columns
                          if feature not in reqd_features]
        raise ValueError(
            f"Input data should contain the folowing columns:\n{missing_features}"
            + f"\nand shoudn't contain the following columns:\n{extra_features}"
        )

    # replace "None" values with nan or "Not Stated" category for ethnicity
    dataf.ethnicity.replace("None", "Not Stated", inplace=True)
    dataf.replace("None", np.nan, inplace=True)

    # set numeric features to float after removing "None" values
    float_features = ["ox_sat", "resp_rate", "heart_rate", "temp"]
    for feature in float_features:
        dataf[feature] = dataf[feature].astype("float")

    # set all referral from A&E to ED (same meaning)
    ane_mask = dataf.referral_from == "A&E"
    dataf.loc[ane_mask, "referral_from"] = "ED"

    # clean and convert cat features to categorical datatype
    # (other than allergies - treated separately)
    cat_features = ['referral_from', 'address', 'ethnicity', 'gender',
                    'referral_date', 'referral_time', 'illness_severity',
                    'activity_level', 'gut_feeling', 'sepsis', 'safeguarding']

    for feature in cat_features:
        dataf[feature] = (dataf[feature].str.strip()
                          .astype("category"))

    return dataf


def fill_nas(dataf):

    na_mask = dataf.isna().any(axis=1)
    na_dataf = dataf[na_mask]
    complete_dataf = dataf[~na_mask]

    # split train / test from complete examples only
    # (avoid introducing noise into test set from inferring nas)

    # infer missing na values

    # produce list counting no of nas per example
    na_counts = na_dataf.isna().sum(axis=1)
    # remove examples with 2 or more nas (few in number and likely to be more noisy)
    na_dataf = na_dataf[na_counts == 1]

    infer_na_values = {
        "activity_level": "mode",
        "gut_feeling": "mode",
        "ox_sat": "mean",
        "resp_rate": "mean",
        "heart_rate": "mean",
        "temp": "mean"
    }

    for feature, agg_method in infer_na_values.items():
        if agg_method == "mean":
            na_dataf[feature].fillna(complete_dataf[feature].mean(), inplace=True)
        elif agg_method == "mode":
            na_dataf[feature].fillna(complete_dataf[feature].mode()[0], inplace=True)

    return pd.concat([complete_dataf, na_dataf])


def add_features(dataf):
    # refactor allergy feature
    for allergy in ["Food", "Drug", "Other"]:
        allergy_name = allergy.lower() + "_allergy"
        dataf[allergy_name] = dataf.allergies.apply(
            lambda x: 'Y' if allergy in x else 'N'
        ).astype("category")
    dataf.drop("allergies", axis=1, inplace=True)

    # set hospital_reqd to 1/0 for data analysis
    dataf["hospital_reqd"] = (dataf.hospital_reqd == "Y").astype("int")

    # new ethnicity features
    # set reported ethnicity to other if not British / Pakistani
    dataf["simple_ethnicity"] = dataf.ethnicity.apply(
        lambda x: x if x in ["Pakistani", "British"] else "other"
    ).replace(np.nan, "other")

    dataf["simple_ethnicity"] = dataf.simple_ethnicity.astype("category")

    # Group ethnicities into European / Asian / Other
    asian = ["Indian", "Mixed Asian", "Pakistani", "white Asain", "British Asian",
             "Asian", "Sri Lankan", "Other Asian background", "Bangladeshi"]

    european = ["Slovak", "British", "Other white background", "Czech Republic",
                "White Europeon", "White British", "CommonWealth Russian",
                "Other European", "Mixed White"]

    dataf["group_ethnicity"] = "other"
    dataf.loc[dataf.ethnicity.isin(european), "group_ethnicity"] = "european"
    dataf.loc[dataf.ethnicity.isin(asian), "group_ethnicity"] = "asian"

    dataf["group_ethnicity"] = dataf.group_ethnicity.astype("category")
    dataf.drop("ethnicity", axis=1, inplace=True)

    # add features from ace referral sheets
    # ox_sat_low feature: < 94 = low
    dataf["ox_sat_low"] = dataf.ox_sat.apply(
        lambda x: "Y" if x < 94 else "N"
    ).astype("category")

    # age_range feature: "pre_school" = (2-5), "primary" = (5-12), "secondary" (12+)
    dataf["age_range"] = "pre_school"

    primary = (dataf.age >= 5) & (dataf.age < 12)
    dataf.loc[primary, "age_range"] = "primary"

    secondary = dataf.age >= 12
    dataf.loc[secondary, "age_range"] = "secondary"

    dataf["age_range"] = dataf.age_range.astype("category")

    # set low / normal / high ranges for ace / apls heart / resp rate
    # see ace & apls referral criteria for more info

    # generic function to set low / normal / high ranges for a feature within age ranges
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
    dataf["meets_ace_criteria"] = "N"

    meets_ace_criteria = ((dataf.ox_sat_low == "N") &
                          (dataf.ace_heart_rate_cat == "normal") &
                          (dataf.ace_resp_rate_cat == "normal") &
                          (dataf.gut_feeling != "unwell") &
                          (dataf.illness_severity != "Moderate"))

    dataf.loc[meets_ace_criteria, "meets_ace_criteria"] = "Y"
    dataf["meets_ace_criteria"] = dataf.meets_ace_criteria.astype("category")

    return dataf


def return_train_test(dataf):

    dataf = (dataf
             .pipe(start_pipeline)
             .pipe(clean_data)
             .pipe(add_features)
             .pipe(fill_nas))

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
        encoder = OneHotEncoder(sparse=False).fit(full_df[cat_features])

        encoded_feature_names = []
        for feature, categories in zip(cat_features, encoder.categories_):
            for category in categories:
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
