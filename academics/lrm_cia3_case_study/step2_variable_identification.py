"""
step 2: identification of variables

- compute correlation matrix to support variable selection
- identify life expectancy as the dependent variable
- select expected schooling, mean schooling, log(gni) as candidate predictors
- provide justification based on domain knowledge and correlation analysis
- generate correlation heatmap
"""

import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from config import save_fig


def identify_variables(df):
    print("\nstep 2: identification of variables\n")

    cols_of_interest = ["life_expectancy", "expected_schooling",
                        "mean_schooling", "gni_per_capita"]
    df_analysis = df[cols_of_interest].dropna().copy()

    # log transformation of gni (diminishing returns of income on health)
    df_analysis["log_gni"] = np.log(df_analysis["gni_per_capita"])

    # correlation matrix using analysis variables
    corr_cols = ["life_expectancy", "expected_schooling",
                 "mean_schooling", "log_gni"]
    corr_matrix = df_analysis[corr_cols].corr().round(3)
    print("correlation matrix:")
    print(corr_matrix.to_string())

    # variable selection and justification
    print("""
dependent variable (y): life_expectancy
  life expectancy at birth (years) measures population health outcomes.
  it is a meaningful dependent variable because we want to understand
  what socioeconomic factors drive longevity across countries.

  note: hdi_value is excluded as a predictor because the hdi formula
  already incorporates life expectancy as one of its three dimensions,
  which would create a circular (tautological) regression.

independent variables (x):
  expected_schooling  - education access, years a child is expected to attend school
  mean_schooling      - educational attainment, average years for adults (25+)
  log_gni             - log of gross national income per capita (2021 ppp $)

reasoning:
  education and income are well-established social determinants of health
  in public health literature. higher education leads to better health
  literacy, healthier behaviours, and better access to healthcare.
  higher income allows investment in healthcare infrastructure, nutrition,
  sanitation, and clean water. we use log(gni) because the relationship
  between income and health shows diminishing marginal returns -- an
  extra dollar of income matters far more for a poor country than a
  rich one.
""")

    print(
        f"analysis dataset: {df_analysis.shape[0]} countries for life expectancy modelling")

    # correlation heatmap
    labels = ["life exp", "exp schooling", "mean schooling", "log(gni)"]
    fig, ax = plt.subplots(figsize=(9, 7))
    sns.heatmap(corr_matrix, annot=True, fmt=".3f", cmap="RdBu_r", center=0,
                square=True, linewidths=0.5, vmin=-1, vmax=1,
                xticklabels=labels, yticklabels=labels,
                annot_kws={"size": 12}, ax=ax)
    ax.set_title("correlation heatmap: life expectancy and candidate predictors",
                 weight="bold")
    plt.tight_layout()
    save_fig(fig, "01_correlation_heatmap")

    return df_analysis


if __name__ == "__main__":
    from step1_data_understanding import load_and_clean
    df = load_and_clean()
    df_analysis = identify_variables(df)
