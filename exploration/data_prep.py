import pandas as pd
import numpy as np

ace_data = pd.read_csv("data/ace_referral_data.csv")

ace_data.replace("None", np.nan, inplace=True)

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
        lambda x: allergy in x
    ).astype("int")

# fix numerical features with 'None'

float_features = ["ox_sat", "resp_rate", "heart_rate", "temp"]
for feature in float_features:
    ace_data[feature] = ace_data[feature].astype("float")

ace_data["hospital_reqd"] = (ace_data.hospital_reqd == "Y").astype("int")

# set simple_ethnicity to other if not British / Pakistani

ace_data["simple_ethnicity"] = ace_data.ethnicity.apply(
    lambda x: x if x in ["Pakistani", "British"] else "other"
).replace(np.nan, "other")

ace_data["simple_ethnicity"] = ace_data.simple_ethnicity.astype("category")

# group_ethnicity: European / Asian / Other feature

asian = ["Indian", "Mixed Asian", "Pakistani", "white Asain", "British Asian",
         "Asian", "Sri Lankan", "Other Asian background", "Bangladeshi"]

european = ["Slovak", "British", "Other white background", "Czech Republic",
            "White Europeon", "White British", "CommonWealth Russian",
            "Other European", "Mixed White"]

ace_data["group_ethnicity"] = "other"
ace_data.loc[ace_data.ethnicity.isin(european), "group_ethnicity"] = "european"
ace_data.loc[ace_data.ethnicity.isin(asian), "group_ethnicity"] = "asian"
ace_data["group_ethnicity"] = ace_data.group_ethnicity.astype("category")

# ox_sat_low feature: > 94 = low

ace_data["ox_sat_low"] = (ace_data.ox_sat < 94).astype("int")

# age_range feature: "pre_school" = (2-5), "primary" = (5-12), "secondary" (12+)

ace_data["age_range"] = "pre_school"

primary = (ace_data.age >= 5) & (ace_data.age < 12)
ace_data.loc[primary, "age_range"] = "primary"

secondary = ace_data.age >= 12
ace_data.loc[secondary, "age_range"] = "secondary"

ace_data["age_range"] = ace_data.age_range.astype("category")

# heart_rate_cat feature: as per referral sheets

ace_data["heart_rate_cat"] = "normal"

pre_school = ace_data.age_range == "pre_school"
pre_school_high = pre_school & (ace_data.heart_rate > 140)
ace_data.loc[pre_school_high, "heart_rate_cat"] = "high"
pre_school_low = pre_school & (ace_data.heart_rate < 95)
ace_data.loc[pre_school_low, "heart_rate_cat"] = "low"

primary_high = primary & (ace_data.heart_rate > 120)
ace_data.loc[primary_high, "heart_rate_cat"] = "high"
primary_low = primary & (ace_data.heart_rate < 80)
ace_data.loc[primary_low, "heart_rate_cat"] = "low"

secondary_high = secondary & (ace_data.heart_rate > 100)
ace_data.loc[secondary_high, "heart_rate_cat"] = "high"
secondary_low = secondary & (ace_data.heart_rate < 60)
ace_data.loc[secondary_low, "heart_rate_cat"] = "low"

ace_data["heart_rate_cat"] = ace_data.heart_rate_cat.astype("category")

# resp_rate_cat feature: as per referral sheets

ace_data["resp_rate_cat"] = "normal"

pre_school_high = pre_school & (ace_data.resp_rate > 30)
ace_data.loc[pre_school_high, "resp_rate_cat"] = "high"
pre_school_low = pre_school & (ace_data.resp_rate < 25)
ace_data.loc[pre_school_low, "resp_rate_cat"] = "low"

primary_high = primary & (ace_data.resp_rate > 25)
ace_data.loc[primary_high, "resp_rate_cat"] = "high"
primary_low = primary & (ace_data.resp_rate < 20)
ace_data.loc[primary_low, "resp_rate_cat"] = "low"

secondary_high = secondary & (ace_data.resp_rate > 20)
ace_data.loc[secondary_high, "resp_rate_cat"] = "high"
secondary_low = secondary & (ace_data.resp_rate < 15)
ace_data.loc[secondary_low, "resp_rate_cat"] = "low"

ace_data["resp_rate_cat"] = ace_data.resp_rate_cat.astype("category")

# meets_ace_criteria feature: as per referral sheets

ace_data["meets_ace_criteria"] = 0

meets_ace_criteria = ((ace_data.ox_sat_low == 0) &
                      (ace_data.heart_rate_cat == "normal") &
                      (ace_data.resp_rate_cat == "normal") &
                      (ace_data.gut_feeling != "unwell") &
                      (ace_data.illness_severity != "Moderate"))

ace_data.loc[meets_ace_criteria, "meets_ace_criteria"] = 1

# apls_heart_rate_cat feature: as per physiological parameters guide

ace_data["apls_heart_rate_cat"] = "normal"


def set_apls_heart_rate(age_range, hr_range):
    age_mask = ((ace_data.age >= age_range[0]) &
                (ace_data.age < age_range[1]))
    low_mask = age_mask & (ace_data.heart_rate < hr_range[0])
    high_mask = age_mask & (ace_data.heart_rate > hr_range[1])
    ace_data.loc[low_mask, "apls_heart_rate_cat"] = "low"
    ace_data.loc[high_mask, "apls_heart_rate_cat"] = "high"


set_apls_heart_rate(age_range=(0, 2), hr_range=(100, 160))
set_apls_heart_rate(age_range=(2, 3), hr_range=(100, 150))
set_apls_heart_rate(age_range=(3, 4), hr_range=(90, 140))
set_apls_heart_rate(age_range=(4, 6), hr_range=(80, 135))
set_apls_heart_rate(age_range=(6, 8), hr_range=(80, 130))
set_apls_heart_rate(age_range=(8, 12), hr_range=(70, 120))
set_apls_heart_rate(age_range=(12, 14), hr_range=(65, 115))
set_apls_heart_rate(age_range=(14, 18), hr_range=(60, 110))

# apls_resp_rate_cat feature: as per physiological parameters guide

ace_data["apls_resp_rate_cat"] = "normal"


def set_apls_resp_rate(age_range, rr_range):
    age_mask = ((ace_data.age >= age_range[0]) &
                (ace_data.age < age_range[1]))
    low_mask = age_mask & (ace_data.resp_rate < rr_range[0])
    high_mask = age_mask & (ace_data.resp_rate > rr_range[1])
    ace_data.loc[low_mask, "apls_resp_rate_cat"] = "low"
    ace_data.loc[high_mask, "apls_resp_rate_cat"] = "high"


set_apls_resp_rate(age_range=(0, 2), rr_range=(20, 40))
set_apls_resp_rate(age_range=(2, 8), rr_range=(20, 30))
set_apls_resp_rate(age_range=(8, 12), rr_range=(15, 25))
set_apls_resp_rate(age_range=(12, 18), rr_range=(12, 24))

# save .pkl and .csv copies of prepped data
ace_data.to_pickle("data/ace_data_prepped.pkl")
ace_data.to_csv("data/ace_data_prepped.csv")
