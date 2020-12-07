import warnings
warnings.filterwarnings("ignore")
from sklearn.metrics import (make_scorer, confusion_matrix, precision_score,
                             f1_score, roc_auc_score, accuracy_score,
                             recall_score)
from sklearn.model_selection import StratifiedKFold, ParameterGrid

import sys
sys.path.append('/Users/samrelins/Documents/LIDA/ace_project/')
from data_prep.data_prep import *

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


def score_classifier(clf, x, y):
    """
    scores a classifier against metrics in scoring dict

    :param clf: (object: sklearn classifier) classifier to be scored
    :param x: (object: pandas dataframe) matrix of training vectors
    :param y: (object: pandas series) vector of target labels
    :return: (dict) group of {score function name: score} pairs
    """

    scores = {}
    for name, scorer in scoring.items():
        scores[name] = scorer(clf, x, y)
    return scores


def cv_score_classifier(clf, x_train, y_train, params,
                        cat_encoder="one_hot",
                        add_synthetic=false,
                        scaled=false,
                        n_splits=3,
                        weight_y=false):
    """
    custom cv loop to score classifier functions

    implemented to account for smote example generation and target encoding -
    both should only be performed on training data and not validation data -
    not possible to achieve this separation using the sklearn pipeline and
    gridsearchcv.

    :param clf: (object: sklearn classifier) classifier to train and score
    :param x_train: (object: pandas dataframe) explanatory training data
    :param y_train: (object: pandas series) training data labels
    :param params: (dict) parameters for classifier
    :param cat_encoder: (str: "one_hot") categorical encoder for data
    either "one_hot" / "target"
    :param add_synthetic: (bool: false) set true to add smote examples before
    training
    :param scaled: (bool: false) set true to scale numeric features
    :param n_splits: (int: 3) number of splilts for cv loop
    :param weight_y: (bool: false) set true if clf requires sample_weights
    :return:
    """

    # create splits for cv loop
    splitter = stratifiedkfold(n_splits=n_splits, random_state=1)
    splits = list(splitter.split(x_train, y_train))

    total_cv_scores = {} # dict to store cumulative cv scores
    for train_idxs, val_idxs  in splits:
        # divide data into train and validation sets for this cv loop
        cv_x_train, cv_y_train, x_val, y_val = (x_train.iloc[train_idxs],
                                                y_train.iloc[train_idxs],
                                                x_train.iloc[val_idxs],
                                                y_train.iloc[val_idxs])

        if add_synthetic: # add smote examples to balance data if required
            cv_x_train, cv_y_train = add_synthetic_examples(cv_x_train, cv_y_train)

        # encode categorical features and scale numeric if required
        cv_x_train, x_val, = encode_and_scale(
            cv_x_train, cv_y_train, x_val,
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
                      .fit(cv_x_train, cv_y_train, sample_weight=y_weights))
        else:
            # train model using parameters and cv loop data
            cv_clf = (clf
                      .set_params(**params)
                      .fit(cv_x_train, cv_y_train))

        # score classifier on cv validation set and add scores to total
        scores = score_classifier(cv_clf, x_val, y_val)
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
    param_grid = parametergrid(param_grid)
    # variable to store best param combo and relevant scores
    best_scores = {}
    best_params = {}
    for params in param_grid:
        mean_cv_scores = cv_score_classifier(params=params,
                                             **kwargs)
        if not best_scores:
            best_scores = mean_cv_scores
            best_params = params
        elif mean_cv_scores["f1"] > best_scores["f1"]:
            best_scores = mean_cv_scores
            best_params = params

    return best_scores, best_params

