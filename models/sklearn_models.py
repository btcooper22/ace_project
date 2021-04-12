from functools import partial
from multiprocessing import Pool, cpu_count
import os
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.svm import SVC
from sklearn.discriminant_analysis import QuadraticDiscriminantAnalysis
from sklearn.ensemble import (RandomForestClassifier,
                              GradientBoostingClassifier,
                              AdaBoostClassifier)
from sklearn.linear_model import LogisticRegression
from sklearn.naive_bayes import GaussianNB
from sklearn.neighbors import KNeighborsClassifier
import time

import sys
sys.path.append('/Users/samrelins/Documents/LIDA/ace_project/')
from src.train_test import *
from src.data_prep import *


def return_knn_params(balanced=False):
    clf = KNeighborsClassifier()
    param_grid = {'n_neighbors': np.arange(1,10),
                  'weights': ['uniform','distance'],
                  'p': [1,2],
                  'metric':['minkowski','euclidean','manhattan'],
                  'n_jobs':[-2]}
    return {"clf": clf, "param_grid": param_grid, "scaled": True,
            "weight_y": False}


def return_svm_params(balanced=False):
    clf = SVC()
    param_grid = {'kernel': ['linear','rbf'],
                  'C': np.linspace(0.01, 1.5, 10), # np.logspace(2,5,6)
                  'gamma': ["scale", "auto"]} # np.logspace(-4,0.5,10)}
    if balanced:
        param_grid["class_weight"] = "balanced",
    return {"clf": clf, "param_grid": param_grid, "scaled": True,
            "weight_y": False}


def return_random_forest_params(balanced=False):
    clf = RandomForestClassifier(n_estimators=100)
    param_grid = {'max_depth': [4, 6, 10, 14, 20],
                  'n_estimators': [30, 100, 130, 300],
                  'min_samples_split': [2, 3, 10, 13, 30],
                  'max_features': [0.3, 0.4, 0.5, "auto"],
                  'n_jobs': [-2]}
    if balanced:
        param_grid["class_weight"] = "balanced",
    return {"clf": clf, "param_grid": param_grid, "scaled": False,
            "weight_y": False}


def return_grad_boost_params(balanced=False):
    clf = GradientBoostingClassifier(n_estimators=100,random_state=0)
    param_grid = {'learning_rate': [0.1, 0.05, 0.02, 0.01],
                  'n_estimators': [30, 100, 130, 300],
                  'max_depth': [4, 6, 10, 14, 20],
                  'min_samples_split': [3, 10, 13, 30],
                  'max_features': [x for x in np.linspace(0.2,0.4,4)]}
    return {"clf": clf, "param_grid": param_grid, "scaled": False,
            "weight_y": balanced}


def return_ada_boost_params(balanced=False):
    clf = AdaBoostClassifier(random_state=0)
    param_grid = {'n_estimators': [30, 100, 130, 300],
                  'learning_rate': [0.001,0.01,0.1,0.2,0.5]}
    return {"clf": clf, "param_grid": param_grid, "scaled": False,
            "weight_y": balanced}


def return_naive_bayes_params(balanced=False):
    clf = GaussianNB()
    param_grid = {'var_smoothing':  np.logspace(-11,-3,9,base=10)}
    return {"clf": clf, "param_grid": param_grid, "scaled": False,
            "weight_y": balanced}


def return_lr_params(balanced=False):
    clf = LogisticRegression(random_state=0, max_iter=10000)
    param_grid = {'penalty' : ['l2'],
                  'solver': ["liblinear"],
                  'C' : np.linspace(0.01, 1.5, 10)}
    if balanced:
        param_grid["class_weight"] = "balanced",
    return {"clf": clf, "param_grid": param_grid, "scaled": True,
            "weight_y": False}


def return_qda_params(balanced=False):
    clf = QuadraticDiscriminantAnalysis()
    param_grid = {'reg_param':  [0.0, 0.01, 0.03, 0.1, 0.3]}
    if balanced:
        print("no available balancing technique for QDA")
    return {"clf": clf, "param_grid": param_grid, "scaled": True,
            "weight_y": False}


def return_lda_params(balanced=False):
    clf = LinearDiscriminantAnalysis()
    param_grid = {'solver':  ["svd", "lsqr", "eigen"],
                  "shrinkage": [None, "auto", 0.1, 0.3, 0.8, 1]}
    if balanced:
        print("no available balancing technique for LDA")
    return {"clf": clf, "param_grid": param_grid, "scaled": True,
            "weight_y": False}


if __name__ == "__main__":

    data_loc = "/Users/samrelins/Documents/LIDA/ace_project/data/"
    outputs_dir = os.path.join(data_loc, "sklearn_models_outputs/")
    if not os.path.isdir(outputs_dir):
        os.mkdir(outputs_dir)
    
    ace_data_loc = os.path.join(data_loc, "ace_data_extra.csv")
    ace_data= pd.read_csv(ace_data_loc)
    text_features = ["medical_history", "examination_summary", "recommendation"]
    X_train, y_train, X_test, y_test = return_train_test(
        ace_data.drop(text_features, axis=1)
    )


    techniques_dict = {'K Nearest Neighbours': return_knn_params,
                       'Support Vector Machines': return_svm_params,
                       'Random Forest Classifier': return_random_forest_params,
                       'Gradient Boosting Classifier': return_grad_boost_params,
                       'Ada Boost classifier': return_ada_boost_params,
                       'Gaussian Naieve Bayes': return_naive_bayes_params,
                       'Logistic Regression': return_lr_params,
                       'Quadratic Discriminant Analysis': return_qda_params}

    data_prep_types = [
        "one_hot_balanced",
        "target_balanced",
        "one_hot_smote",
        "target_smote",
        "one_hot_undersample",
        "target_undersample"
    ]

    best_params = {}
    best_model_scores = {}
    for data_prep_type in data_prep_types:
        loop_begin = time.time()
        print(50 * '=')
        print(f"Data Prep: {data_prep_type}")
        print(50 * '=')

        cat_encoder = "one_hot" if "one_hot" in data_prep_type else "target"
        balanced = True if "balanced" in data_prep_type else False
        resample = "undersample" if "undersample" in data_prep_type else None
        resample = "smote" if "smote" in data_prep_type else resample

        scores_dict = {}
        best_loop_params = {}
        for model_name, model_params_f in techniques_dict.items():
            print(f"fitting {model_name}......")
            ## param search goes in here

            model_params_dict = model_params_f(balanced=balanced)
            clf = model_params_dict["clf"]
            param_grid = model_params_dict["param_grid"]
            scaled = model_params_dict["scaled"]
            weight_y = model_params_dict["weight_y"]

            params_list = [params for params in ParameterGrid(param_grid)]
            # variable to store best param combo and relevant scores

            with Pool(processes=cpu_count() - 2) as pool:
                results = pool.map(partial(cv_score_classifier,
                                           clf=clf,
                                           X_train=X_train,
                                           y_train=y_train,
                                           cat_encoder=cat_encoder,
                                           resample=resample,
                                           n_random_iterations=10,
                                           scaled=scaled),
                                   params_list)

            results = pd.DataFrame(results)
            non_random_results = results[results.mean_accuracy > 0.5]
            if len(non_random_results):
                highest_f1 = (non_random_results.mean_f1 ==
                              non_random_results.mean_f1.max())
                best_result = non_random_results[highest_f1].iloc[0]
                best_result_idx = best_result.name
                best_params = params_list[best_result_idx]
                scores_dict[model_name] = best_result
                best_loop_params[model_name] = best_params
            else:
                print(f"No {model_name} better than random selection")
            ##############
            print("done.")

        best_prep_scores = pd.DataFrame(
            scores_dict.values(),
            index=scores_dict.keys())
        best_model_scores[data_prep_type] = best_prep_scores
        best_prep_scores.to_csv(outputs_dir +
                                data_prep_type +
                                "_scores.csv")
        pd.DataFrame(best_loop_params).to_csv(outputs_dir +
                                              data_prep_type +
                                              "_params.csv")
        best_params[data_prep_type] = best_loop_params
        loop_end = time.time()
        print(f"Loop Execution Time: {(loop_end - loop_begin) / 60} mins")


    all_results = pd.DataFrame([])
    for data_prep_method, scores_df in best_model_scores.items():

        results_idx_tuples = [(data_prep_method, classifier)
                              for classifier in scores_df.index]
        new_results_idx = pd.MultiIndex.from_tuples(
            results_idx_tuples,
            names=["data prep method", "classifier"]
        )
        new_idx_scores_df = scores_df.set_index(new_results_idx)
        all_results = pd.concat([all_results, new_idx_scores_df])
    all_results.to_csv(outputs_dir + "scores.csv")
    pd.DataFrame(best_params).to_csv(outputs_dir + "params.csv")








