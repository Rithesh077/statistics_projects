"""
step 5: model specification and estimation

- specify the ols regression model for life expectancy
- estimate the model using statsmodels with selected predictors
- interpret each coefficient in context (rigorous log interpretation)
- report vif for multicollinearity
- report goodness-of-fit (r2, adjusted r2, f-statistic, aic, bic)
- generate coefficient plot, vif chart, actual vs predicted plot
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.api as sm
from statsmodels.stats.outliers_influence import variance_inflation_factor
from config import save_fig


def fit_model(df_analysis, best_predictors):
    print("\nstep 5: model specification and estimation\n")

    df_analysis = df_analysis.copy()

    # ensure log_gni exists
    if "log_gni" not in df_analysis.columns:
        df_analysis["log_gni"] = np.log(df_analysis["gni_per_capita"])

    # model specification (dynamic based on selected predictors)
    formula_rhs = " + ".join([f"b{i+1}({p})" for i,
                             p in enumerate(best_predictors)])
    print(f"""model specification:
  life_expectancy = b0 + {formula_rhs} + e

  the dependent variable is life expectancy at birth (years).
  predictors were selected via best subset selection in step 3.
  log transformation is applied to gni_per_capita because the
  relationship between income and health shows diminishing
  marginal returns -- an extra dollar of income matters far
  more for a poor country than a rich one.
""")

    X = df_analysis[best_predictors]
    y = df_analysis["life_expectancy"]
    X_const = sm.add_constant(X)

    model = sm.OLS(y, X_const).fit()
    print(model.summary())

    # coefficient interpretation
    print("\ncoefficient interpretation:\n")
    coeffs = model.params
    pvals = model.pvalues

    for var in best_predictors:
        sig = "significant" if pvals[var] < 0.05 else "not significant"
        direction = "increase" if coeffs[var] > 0 else "decrease"
        print(f"  {var}:")
        print(
            f"    coefficient = {coeffs[var]:.6f}, p-value = {pvals[var]:.4f} ({sig})")
        if var == "log_gni":
            # rigorous log-level interpretation: dy = b * ln(1.01) for a 1% increase
            pct_effect = coeffs[var] * np.log(1.01)
            print(f"    a 1% increase in gni per capita is associated with a")
            print(
                f"    {abs(pct_effect):.4f}-year {direction} in life expectancy")
            print(
                f"    (exact: b * ln(1.01) = {coeffs[var]:.4f} * {np.log(1.01):.6f} = {pct_effect:.4f})")
        else:
            print(
                f"    a one-unit increase in {var.replace('_', ' ')} is associated")
            print(
                f"    with a {abs(coeffs[var]):.4f}-year {direction} in life expectancy,")
            print(f"    holding other variables constant")
        print()

    # vif - multicollinearity check
    print("multicollinearity check (vif):\n")
    vif_data = pd.DataFrame()
    vif_data["variable"] = X.columns
    vif_data["vif"] = [variance_inflation_factor(
        X_const.values, i + 1) for i in range(len(X.columns))]
    vif_data = vif_data.sort_values(
        "vif", ascending=True).reset_index(drop=True)

    for _, row in vif_data.iterrows():
        flag = "high" if row["vif"] > 10 else (
            "moderate" if row["vif"] > 5 else "ok")
        print(f"  {row['variable']:22s}  vif = {row['vif']:8.3f}  ({flag})")

    print("""
  vif interpretation:
    vif < 5  : acceptable
    5-10     : high, consider removing or combining variables
    vif > 10 : severe multicollinearity

  note: expected_schooling and mean_schooling both measure education,
  so some correlation between them is expected. this does not invalidate
  the model but may make individual coefficient estimates less stable.
""")

    # goodness of fit
    dw = sm.stats.durbin_watson(model.resid)
    print(f"goodness of fit:")
    print(
        f"  r-squared          = {model.rsquared:.4f}  ({model.rsquared*100:.1f}% variance explained)")
    print(f"  adjusted r-squared = {model.rsquared_adj:.4f}")
    print(
        f"  f-statistic        = {model.fvalue:.2f}  (p = {model.f_pvalue:.2e})")
    print(f"  aic                = {model.aic:.2f}")
    print(f"  bic                = {model.bic:.2f}")
    print(f"  durbin-watson      = {dw:.4f}")

    print(f"""
  the model explains {model.rsquared*100:.1f}% of the variation in life expectancy.
  the f-test is highly significant, confirming overall model validity.
  durbin-watson = {dw:.4f}. note: for cross-sectional data, the ordering
  of observations is arbitrary (here, by hdi rank), so the durbin-watson
  statistic should be interpreted cautiously.
""")

    # coefficient plot
    names = [n for n in model.params.index if n != "const"]
    coefs = [model.params[n] for n in names]
    ci = model.conf_int()
    lo = [ci.loc[n, 0] for n in names]
    hi = [ci.loc[n, 1] for n in names]
    colors = ["#3498db" if model.pvalues[n] < 0.05 else "#aaa" for n in names]

    fig, ax = plt.subplots(figsize=(9, 5))
    ax.barh(names, coefs, xerr=[[c - l for c, l in zip(coefs, lo)],
                                [h - c for c, h in zip(coefs, hi)]],
            color=colors, edgecolor="white", capsize=4)
    ax.axvline(0, color="grey", ls="--", lw=1)
    ax.set_xlabel("coefficient")
    ax.set_title(
        "regression coefficients with 95% ci (blue = significant)", weight="bold")
    plt.tight_layout()
    save_fig(fig, "05_coefficient_plot")

    # vif chart
    fig, ax = plt.subplots(figsize=(9, 4))
    bar_colors = ["#2ecc71" if v < 5 else "#f39c12" if v <
                  10 else "#e74c3c" for v in vif_data["vif"]]
    bars = ax.barh(vif_data["variable"], vif_data["vif"],
                   color=bar_colors, edgecolor="white")
    ax.axvline(5, ls="--", color="#f39c12", lw=1.5, label="moderate (5)")
    ax.axvline(10, ls="--", color="#e74c3c", lw=1.5, label="severe (10)")
    ax.set_xlabel("vif value")
    ax.set_title("variance inflation factor", weight="bold")
    ax.legend()
    for bar, val in zip(bars, vif_data["vif"]):
        ax.text(bar.get_width() + 0.1, bar.get_y() + bar.get_height() / 2,
                f"{val:.2f}", va="center", fontsize=9)
    plt.tight_layout()
    save_fig(fig, "06_vif")

    # actual vs predicted
    df_analysis["predicted"] = model.fittedvalues
    df_analysis["residuals"] = model.resid

    fig, ax = plt.subplots(figsize=(8, 6))
    sc = ax.scatter(df_analysis["life_expectancy"], df_analysis["predicted"],
                    c=df_analysis["residuals"], cmap="RdBu_r", s=30, alpha=0.7,
                    edgecolors="white", linewidths=0.3)
    plt.colorbar(sc, ax=ax, label="residual (years)")
    rng = [df_analysis["life_expectancy"].min(
    ), df_analysis["life_expectancy"].max()]
    ax.plot(rng, rng, "r--", lw=2, label="perfect prediction")
    ax.set_xlabel("actual life expectancy (years)")
    ax.set_ylabel("predicted life expectancy (years)")
    ax.set_title(
        f"actual vs predicted (r2 = {model.rsquared:.4f})", weight="bold")
    ax.legend()
    ax.grid(alpha=0.3)
    plt.tight_layout()
    save_fig(fig, "07_actual_vs_predicted")

    return model, df_analysis, X_const


if __name__ == "__main__":
    from step1_data_understanding import load_and_clean
    from step2_variable_identification import identify_variables
    from step3_variable_selection import variable_selection
    df = load_and_clean()
    df_analysis = identify_variables(df)
    best_predictors, _ = variable_selection(df_analysis)
    model, df_analysis, X_const = fit_model(df_analysis, best_predictors)
