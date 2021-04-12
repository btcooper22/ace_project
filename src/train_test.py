import warnings
warnings.filterwarnings("ignore")
from category_encoders.leave_one_out import LeaveOneOutEncoder
from imblearn.over_sampling import SMOTENC
from imblearn.under_sampling import RandomUnderSampler
import numpy as np
import pandas as pd
from sklearn.metrics import (make_scorer, confusion_matrix, precision_score,
                             f1_score, roc_auc_score, accuracy_score,
                             recall_score)
from sklearn.model_selection import StratifiedKFold, ParameterGrid
from sklearn.utils.class_weight import compute_class_weight
from sklearn.preprocessing import OneHotEncoder, MinMaxScaler

import sys
sys.path.append('//')
from tqdm import tqdm

# custom scoring functions for cv loop
true_neg = make_scorer(lambda y, y_pred: confusion_matrix(y, y_pred)[0][0])
false_neg = make_scorer(lambda y, y_pred: confusion_matrix(y, y_pred)[1][0])
true_pos = make_scorer(lambda y, y_pred: confusion_matrix(y, y_pred)[1][1])
false_pos = make_scorer(lambda y, y_pred: confusion_matrix(y, y_pred)[0][1])
precision = make_scorer(precision_score, zero_division=0)


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


# dict of scoring functions
scoring = {
    "f1": make_scorer(f1_score),
    "roc_auc": make_scorer(roc_auc_score),
    "accuracy": make_scorer(accuracy_score),
    "recall": make_scorer(recall_score),
    "precision": make_scorer(precision_score),
    "true_pos": true_pos,
    "true_neg": true_neg,
    "false_pos": false_pos,
    "false_neg": false_neg
}


def score_classifier(clf, X, y):
    """
    scores a classifier against metrics in scoring dict

    :param clf: (object: sklearn classifier) classifier to be scored
    :param X: (object: pandas dataframe) matrix of training vectors
    :param y: (object: pandas series) vector of target labels
    :return: (dict) group of {score function name: score} pairs
    """

    scores = {}
    for name, scorer in scoring.items():
        scores[name] = scorer(clf, X, y)
    return scores


def cv_score_classifier(params, clf, X_train, y_train,
                        cat_encoder="one_hot",
                        resample=None,
                        n_random_iterations=1,
                        scaled=False,
                        n_splits=3,
                        weight_y=False):
    """
    custom cv loop to score classifier functions

    implemented to account for smote example generation and target encoding -
    both should only be performed on training data and not validation data -
    not possible to achieve this separation using the sklearn pipeline and
    gridsearchcv.

    :param clf: (object: sklearn classifier) classifier to train and score
    :param X_train: (object: pandas dataframe) explanatory training data
    :param y_train: (object: pandas series) training data labels
    :param params: (dict) parameters for classifier
    :param cat_encoder: (str: "one_hot") categorical encoder for data
    either "one_hot" / "target"
    :param resample: (str: None) set "SMOTE" to add SMOTE synthetic examples,
    set "undersample" to randomly under sample majority label
    training
    :param scaled: (bool: False) set True to scale numeric features
    :param n_splits: (int: 3) number of splits for cv loop
    :param weight_y: (bool: False) set True if clf requires sample_weights
    :return:
    """

    # dict to store cumulative cv scores
    total_cv_scores = {}
    for score_type in scoring.keys():
        total_cv_scores[score_type] = []

    for random_state in range(n_random_iterations):
        # create splits for cv loop
        splitter = StratifiedKFold(n_splits=n_splits,
                                   shuffle=True,
                                   random_state=random_state)
        splits = list(splitter.split(X_train, y_train))

        for train_idxs, val_idxs in splits:
            # divide data into train and validation sets for this cv loop
            cv_X_train, cv_y_train, X_val, y_val = (X_train.iloc[train_idxs],
                                                    y_train.iloc[train_idxs],
                                                    X_train.iloc[val_idxs],
                                                    y_train.iloc[val_idxs])

            if resample is not None:
                if resample == "smote":
                    cv_X_train, cv_y_train = add_synthetic_examples(cv_X_train,
                                                                    cv_y_train)
                elif resample == "undersample":
                    rus = RandomUnderSampler(random_state=random_state)
                    cv_X_train, cv_y_train = rus.fit_resample(cv_X_train,
                                                              cv_y_train)
                else:
                    raise ValueError(
                        'resample must be one of "smote" / "undersample" / '
                        'None'
                    )

            # encode categorical features and scale numeric if required
            cv_X_train, X_val, = encode_and_scale(
                cv_X_train, cv_y_train, X_val,
                cat_encoder=cat_encoder,
                scaled=scaled)

            if weight_y:
                # calculate array of weights for y labels
                pos_weight, neg_weight = compute_class_weight(
                    class_weight="balanced",
                    classes=[1,0],
                    y=cv_y_train)
                y_weights = cv_y_train.apply(lambda y: pos_weight if y else neg_weight)
                # train model using parameters, weights and cv loop data
                cv_clf = (clf
                          .set_params(**params)
                          .fit(cv_X_train, cv_y_train, sample_weight=y_weights))
            else:
                # train model using parameters and cv loop data
                cv_clf = (clf
                          .set_params(**params)
                          .fit(cv_X_train, cv_y_train))

            # score classifier on cv validation set and add scores to total
            iter_scores = score_classifier(cv_clf, X_val, y_val)
            for score_type, score in iter_scores.items():
                total_cv_scores[score_type].append(score)

    output_scores = {}
    for score_type, scores in total_cv_scores.items():
        output_scores["mean_" + score_type] = np.mean(scores)
        output_scores["std_" + score_type] = np.std(scores)

    return output_scores


def param_search_classifier(param_grid, verbose=True, **kwargs):
    """
    custom param grid search to compliment cv_score_classifier function

    :param param_grid: (dict) parameters on which to perform grid search
    :param kwargs: arguments for cv_score_classifier function
    :return: (dict: best_scores, dict: best_params) scores and parameters for
    highest scoring model
    """

    if verbose:
        print("=" * 50)
    if "cat_encoder" in kwargs.keys():
        cat_encoder = kwargs["cat_encoder"]
    else:
        cat_encoder = "one_hot"
    if "add_synthetic" in kwargs.keys():
        add_synthetic = kwargs["add_synthetic"]
    else:
        add_synthetic = False
    if verbose:
        print(f"Testing {kwargs['clf']} classifier with {cat_encoder} "
              "encoded features"
              f"{' and SMOTE examples.' if add_synthetic else '.'}")
        print("=" * 50, flush=True)

    param_grid = ParameterGrid(param_grid)
    # variable to store best param combo and relevant scores
    best_scores = {}
    best_params = {}

    for params in tqdm(param_grid, disable=(not verbose)):
        cv_scores = cv_score_classifier(params=params, **kwargs)
        better_than_random = cv_scores["mean_accuracy"] > 0.5
        if not best_scores:
            best_scores = cv_scores
            best_params = params
        elif cv_scores["mean_f1"] > best_scores["mean_f1"] and \
                better_than_random:
            best_scores = cv_scores
            best_params = params

    return best_scores, best_params

