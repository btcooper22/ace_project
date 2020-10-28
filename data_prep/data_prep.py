import pandas as pd
import numpy as np


def prep_ace_data(loc):
    ace_data = pd.read_csv(loc)

    # check columns are present and named correctly
    reqd_features = ['hospital_reqd', 'referral_from', 'age', 'address',
                     'ethnicity', 'gender', 'allergies', 'referral_date',
                     'referral_time', 'illness_severity', 'activity_level',
                     'gut_feeling', 'ox_sat', 'resp_rate', 'heart_rate',
                     'temp', 'sepsis', 'safeguarding']

    if sorted(ace_data.columns) != sorted(reqd_features):
        missing_features = [feature for feature in reqd_features
                            if feature not in ace_data.columns]
        extra_features = [feature for feature in ace_data.columns
                          if feature not in reqd_features]
        raise ValueError(
            f"Input data should contain the folowing columns:\n{missing_features}"
            + f"\nand shoudn't contain the following columns:\n{extra_features}"
        )

    # replace "None" values with nan
    ace_data.replace("None", np.nan, inplace=True)

    # set numeric features to float after removing "None" values
    float_features = ["ox_sat", "resp_rate", "heart_rate", "temp"]
    for feature in float_features:
        ace_data[feature] = ace_data[feature].astype("float")

    # clean and convert cat features to categorical datatype
    # (other than allergies - treated separately)
    cat_features = ['referral_from', 'address', 'ethnicity', 'gender',
                    'referral_date', 'referral_time', 'illness_severity',
                    'activity_level', 'gut_feeling', 'sepsis', 'safeguarding']
    for feature in cat_features:
        ace_data[feature] = (ace_data[feature].str.strip()
                             .astype("category"))

    # refactor allergy feature
    for allergy in ["Food", "Drug", "Other"]:
        allergy_name = allergy.lower() + "_allergy"
        ace_data[allergy_name] = ace_data.allergies.apply(
            lambda x: 'Y' if allergy in x else 'N'
        ).astype("category")
    ace_data.drop("allergies", axis=1, inplace=True)

    # set hospital_reqd to 1/0 for data analysis
    ace_data["hospital_reqd"] = (ace_data.hospital_reqd == "Y").astype("int")

    # new ethnicity features
    # set reported ethnicity to other if not British / Pakistani
    ace_data["simple_ethnicity"] = ace_data.ethnicity.apply(
        lambda x: x if x in ["Pakistani", "British"] else "other"
    ).replace(np.nan, "other")

    ace_data["simple_ethnicity"] = ace_data.simple_ethnicity.astype("category")

    # Group ethnicities into European / Asian / Other
    asian = ["Indian", "Mixed Asian", "Pakistani", "white Asain", "British Asian",
             "Asian", "Sri Lankan", "Other Asian background", "Bangladeshi"]

    european = ["Slovak", "British", "Other white background", "Czech Republic",
                "White Europeon", "White British", "CommonWealth Russian",
                "Other European", "Mixed White"]

    ace_data["group_ethnicity"] = "other"
    ace_data.loc[ace_data.ethnicity.isin(european), "group_ethnicity"] = "european"
    ace_data.loc[ace_data.ethnicity.isin(asian), "group_ethnicity"] = "asian"

    ace_data["group_ethnicity"] = ace_data.group_ethnicity.astype("category")
    ace_data.drop("ethnicity", axis=1, inplace=True)

    # add features from ace referral sheets
    # ox_sat_low feature: < 94 = low
    ace_data["ox_sat_low"] = ace_data.ox_sat.apply(
        lambda x: "Y" if x < 94 else "N"
    ).astype("category")

    # age_range feature: "pre_school" = (2-5), "primary" = (5-12), "secondary" (12+)
    ace_data["age_range"] = "pre_school"

    primary = (ace_data.age >= 5) & (ace_data.age < 12)
    ace_data.loc[primary, "age_range"] = "primary"

    secondary = ace_data.age >= 12
    ace_data.loc[secondary, "age_range"] = "secondary"

    ace_data["age_range"] = ace_data.age_range.astype("category")

    # set low / normal / high ranges for ace / apls heart / resp rate
    # see ace & apls referral criteria for more info

    # generic function to set low / normal / high ranges for a feature within age ranges
    def set_low_norm_high(feature, feature_range, age_range, new_feature_name):

        if new_feature_name not in ace_data.columns:
            ace_data[new_feature_name] = "normal"

        age_mask = ((ace_data.age >= age_range[0]) &
                    (ace_data.age < age_range[1]))
        low_mask = age_mask & (ace_data[feature] < feature_range[0])
        high_mask = age_mask & (ace_data[feature] > feature_range[1])

        ace_data.loc[low_mask, new_feature_name] = "low"
        ace_data.loc[high_mask, new_feature_name] = "high"

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

        ace_data[new_feature_name] = ace_data[new_feature_name].astype("category")

    # overall feature for meeting ace referral criteria
    ace_data["meets_ace_criteria"] = "N"

    meets_ace_criteria = ((ace_data.ox_sat_low == "N") &
                          (ace_data.ace_heart_rate_cat == "normal") &
                          (ace_data.ace_resp_rate_cat == "normal") &
                          (ace_data.gut_feeling != "unwell") &
                          (ace_data.illness_severity != "Moderate"))

    ace_data.loc[meets_ace_criteria, "meets_ace_criteria"] = "Y"
    ace_data["meets_ace_criteria"] = ace_data.meets_ace_criteria.astype("category")

    return ace_data
