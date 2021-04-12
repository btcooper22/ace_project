import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
import re

#import nltk
## UNCOMMENT AND RUN WHEN FIRST USING NLTK - REQUIRES PACKAGE DOWNLOADS ##
#nltk.download("wordnet")
#nltk.download("stopwords")
#####


def start_pipeline(dataf):
    return dataf.copy()


def skip_missing_features(features, dataf):
    return [feature for feature in features if feature in dataf.columns]


def clean_data(dataf):

    # replace blank text fields with no_information
    text_features = ["medical_history", "examination_summary", "recommendation"]
    text_features = skip_missing_features(text_features, dataf)
    for feature in text_features:
            dataf[feature].fillna("noinfo", inplace=True)

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
    ).astype("category")

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


def update_ethnicity_feature(dataf):
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

    def positive_mention_in_phrase(word, text):
        hit = False
        if word in text:
            match_not = re.search(r"\bnot?\b", text)
            if match_not:
                not_end_pos = match_not.span()[1]
                word_start_pos = re.search(word, text).span()[0]
                gap = word_start_pos - not_end_pos
                if gap < 1 or gap > 50:
                    hit = True
            else:
                hit = True
        return hit

    def mentions_asthma(note):
        for phrase in re.split(r"[\n.,-]", note.lower()):
            if positive_mention_in_phrase("asthm", phrase):
                return True
        return False

    def mentions_salbutamol(note):
        for phrase in re.split(r"[\n.,-]", note.lower()):
            if positive_mention_in_phrase("salbut", phrase):
                return True
            elif "post" in phrase and "salbut" in phrase:
                return True
        return False

    dataf["mentions_asthma"] = dataf.medical_history.apply(
        lambda note: "y" if mentions_asthma(note) else "n"
    ).astype("category")

    dataf["mentions_salbutamol"] = dataf.examination_summary.apply(
        lambda note: "y" if mentions_salbutamol(note) else "n"
    ).astype("category")

    dataf.drop(["medical_history", "examination_summary", "recommendation"],
               axis=1,
               inplace=True)

    return dataf


def run_default_pipeline(dataf):

    required_features_for_functions = {
        update_referral_from: ["referral_from"],
        add_allergy_features: ["allergies"],
        update_ethnicity_feature: ["ethnicity"],
        add_ace_apls_features: ["age", "resp_rate", "heart_rate", "ox_sat",
                                "gut_feeling", "illness_severity"],
        add_free_text_features: ["medical_history", "examination_summary",
                                 "recommendation"]
    }

    dataf = (dataf.pipe(start_pipeline)
             .pipe(clean_data)
             .pipe(fill_nas))

    for function, required_features in required_features_for_functions.items():
        contains_required_features = np.all(
            [feature in dataf.columns
             for feature in required_features]
        )
        if contains_required_features:
            dataf = dataf.pipe(function)
        else:
            print(f"Required features missing to run {function}")

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
    
    for df in [X_train, y_train, X_test, y_test]:
        df.reset_index(drop=True, inplace=True)

    return X_train, y_train, X_test, y_test


