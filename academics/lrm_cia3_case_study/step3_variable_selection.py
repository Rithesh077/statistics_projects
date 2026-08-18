"""
step 3: variable selection (best subset)

- enumerate all possible subsets of candidate predictors
- fit ols for each subset and compute r-squared, adj r-squared, aic, bic
- rank models and select the best by adjusted r-squared
- generate comparison chart
"""

import itertools
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.api as sm
from config import save_fig


def variable_selection(df_analysis):
    print("\nstep 3: variable selection (best subset)\n")

    target = "life_expectancy"
    candidate_predictors = ["expected_schooling", "mean_schooling", "log_gni"]
    y = df_analysis[target]

    results = []

    for k in range(1, len(candidate_predictors) + 1):
        for subset in itertools.combinations(candidate_predictors, k):
            subset_list = list(subset)
            X = sm.add_constant(df_analysis[subset_list])
            model = sm.OLS(y, X).fit()
            results.append({
                "predictors": " + ".join(subset_list),
                "n_predictors": len(subset_list),
                "r_squared": model.rsquared,
                "adj_r_squared": model.rsquared_adj,
                "aic": model.aic,
                "bic": model.bic,
            })

    results_df = pd.DataFrame(results)
    results_df = results_df.sort_values("adj_r_squared", ascending=False)
    results_df = results_df.reset_index(drop=True)

    print("all possible models (ranked by adjusted r-squared):\n")
    print(results_df.to_string(index=False))

    best_adj = results_df.iloc[0]
    best_aic_row = results_df.loc[results_df["aic"].idxmin()]
    best_bic_row = results_df.loc[results_df["bic"].idxmin()]

    print(f"\nbest by adjusted r-squared: {best_adj['predictors']}")
    print(f"  adj r2 = {best_adj['adj_r_squared']:.4f}")
    print(f"best by aic: {best_aic_row['predictors']}")
    print(f"  aic = {best_aic_row['aic']:.2f}")
    print(f"best by bic: {best_bic_row['predictors']}")
    print(f"  bic = {best_bic_row['bic']:.2f}")

    best_predictors = best_adj["predictors"].split(" + ")
    print(f"\nselected model: life_expectancy ~ {' + '.join(best_predictors)}")

    # comparison chart
    fig, axes = plt.subplots(1, 3, figsize=(16, 5))

    model_labels = results_df["predictors"].values
    short_labels = [p.replace("expected_schooling", "exp_sch")
                     .replace("mean_schooling", "mean_sch")
                    for p in model_labels]
    x_pos = range(len(results_df))

    axes[0].barh(x_pos, results_df["adj_r_squared"], color="#3498db")
    axes[0].set_yticks(list(x_pos))
    axes[0].set_yticklabels(short_labels, fontsize=7)
    axes[0].set_xlabel("adjusted r-squared")
    axes[0].set_title("adjusted r-squared (higher = better)", weight="bold")

    axes[1].barh(x_pos, results_df["aic"], color="#e74c3c")
    axes[1].set_yticks(list(x_pos))
    axes[1].set_yticklabels(short_labels, fontsize=7)
    axes[1].set_xlabel("aic")
    axes[1].set_title("aic (lower = better)", weight="bold")

    axes[2].barh(x_pos, results_df["bic"], color="#2ecc71")
    axes[2].set_yticks(list(x_pos))
    axes[2].set_yticklabels(short_labels, fontsize=7)
    axes[2].set_xlabel("bic")
    axes[2].set_title("bic (lower = better)", weight="bold")

    fig.suptitle("best subset variable selection: model comparison",
                 fontsize=13, weight="bold")
    plt.tight_layout()
    save_fig(fig, "02_subset_selection")

    return best_predictors, results_df


if __name__ == "__main__":
    from step1_data_understanding import load_and_clean
    from step2_variable_identification import identify_variables
    df = load_and_clean()
    df_analysis = identify_variables(df)
    best_predictors, results_df = variable_selection(df_analysis)
