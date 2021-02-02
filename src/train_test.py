import warnings
warnings.filterwarnings("ignore")
from sklearn.metrics import (make_scorer, confusion_matrix, precision_score,
                             f1_score, roc_auc_score, accuracy_score,
                             recall_score)
from sklearn.model_selection import StratifiedKFold, ParameterGrid
from sklearn.utils.class_weight import compute_class_weight

import sys
sys.path.append('//')
from src.data_prep import *
from tqdm import tqdm

# custom scoring functions for cv loop
true_neg = make_scorer(lambda y, y_pred: confusion_matrix(y, y_pred)[0][0])
false_neg = make_scorer(lambda y, y_pred: confusion_matrix(y, y_pred)[1][0])
true_pos = make_scorer(lambda y, y_pred: confusion_matrix(y, y_pred)[1][1])
false_pos = make_scorer(lambda y, y_pred: confusion_matrix(y, y_pred)[0][1])
precision = make_scorer(precision_score, zero_division=0)

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


def cv_score_classifier(clf, X_train, y_train, params,
                        cat_encoder="one_hot",
                        add_synthetic=False,
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
    :param add_synthetic: (bool: False) set True to add smote examples before
    training
    :param scaled: (bool: False) set True to scale numeric features
    :param n_splits: (int: 3) number of splits for cv loop
    :param weight_y: (bool: False) set True if clf requires sample_weights
    :return:
    """

    # create splits for cv loop
    splitter = StratifiedKFold(n_splits=n_splits, random_state=1)
    splits = list(splitter.split(X_train, y_train))

    total_cv_scores = {} # dict to store cumulative cv scores
    for train_idxs, val_idxs  in splits:
        # divide data into train and validation sets for this cv loop
        cv_X_train, cv_y_train, X_val, y_val = (X_train.iloc[train_idxs],
                                                y_train.iloc[train_idxs],
                                                X_train.iloc[val_idxs],
                                                y_train.iloc[val_idxs])

        if add_synthetic: # add smote examples to balance data if required
            cv_X_train, cv_y_train = add_synthetic_examples(cv_X_train, cv_y_train)

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
        scores = score_classifier(cv_clf, X_val, y_val)
        if total_cv_scores:
            for key, value in scores.items():
                total_cv_scores[key] += value
        else:
            total_cv_scores = scores

    mean_cv_scores = {}
    for key, value in total_cv_scores.items():
        mean_cv_scores[key] = value / n_splits

    return mean_cv_scores


def param_search_classifier(param_grid, **kwargs):
    """
    custom param grid search to compliment cv_score_classifier function

    :param param_grid: (dict) parameters on which to perform grid search
    :param kwargs: arguments for cv_score_classifier function
    :return: (dict: best_scores, dict: best_params) scores and parameters for
    highest scoring model
    """

    print("=" * 50)
    if "cat_encoder" in kwargs.keys():
        cat_encoder = kwargs["cat_encoder"]
    else:
        cat_encoder = "one_hot"
    if "add_synthetic" in kwargs.keys():
        add_synthetic = kwargs["add_synthetic"]
    else:
        add_synthetic = False
    print(f"Testing {kwargs['clf']} classifier with {cat_encoder} "
          "encoded features"
          f"{' and SMOTE examples.' if add_synthetic else '.'}")
    print("=" * 50, flush=True)

    param_grid = ParameterGrid(param_grid)
    # variable to store best param combo and relevant scores
    best_scores = {}
    best_params = {}

    for params in tqdm(param_grid):
        mean_cv_scores = cv_score_classifier(params=params,
                                             **kwargs)
        better_than_random = mean_cv_scores["accuracy"] > 0.5
        if not best_scores:
            best_scores = mean_cv_scores
            best_params = params
        elif mean_cv_scores["f1"] > best_scores["f1"] and better_than_random:
            best_scores = mean_cv_scores
            best_params = params

    return best_scores, best_params


def howmany_within_range(row, minimum, maximum):
    """Returns how many numbers lie within `maximum` and `minimum` in a given `row`"""
    count = 0
    for n in row:
        if minimum <= n <= maximum:
            count = count + 1
    return count
