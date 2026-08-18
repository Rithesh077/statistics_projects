"""
step 6: residual analysis

diagnostic checks:
  a) residuals vs fitted values plot
  b) check if residuals are randomly distributed around zero
  c) heteroscedasticity - breusch-pagan and white's test, scale-location plot
  d) normality - shapiro-wilk, jarque-bera, q-q plot, histogram
  e) autocorrelation - acf plot, ljung-box test
  f) overall model adequacy assessment
"""

import numpy as np
import matplotlib.pyplot as plt
from scipy import stats
import statsmodels.api as sm
from statsmodels.stats.diagnostic import het_breuschpagan, het_white, acorr_ljungbox
from statsmodels.tsa.stattools import acf
from config import save_fig


def residual_analysis(model, df_analysis, X_const):
    print("\nstep 6: residual analysis\n")

    resid = model.resid.values
    fitted = model.fittedvalues.values

    # 6a: residuals vs fitted values
    print("6a. residuals vs fitted values\n")
    fig, ax = plt.subplots(figsize=(10, 6))
    ax.scatter(fitted, resid, s=28, alpha=0.55, color="#00bcd4",
               edgecolors="white", linewidths=0.3)
    ax.axhline(0, color="red", ls="--", lw=2)
    lowess = sm.nonparametric.lowess(resid, fitted, frac=0.4)
    ax.plot(lowess[:, 0], lowess[:, 1], color="#f39c12",
            lw=2.5, label="lowess trend")
    ax.set_xlabel("fitted values (years)")
    ax.set_ylabel("residuals (years)")
    ax.set_title("residuals vs fitted values", weight="bold")
    ax.legend()
    ax.grid(alpha=0.3)
    plt.tight_layout()
    save_fig(fig, "08_residuals_vs_fitted")

    print("  the lowess line should stay close to zero with no systematic")
    print("  pattern. a funnel shape would indicate heteroscedasticity,")
    print("  a curve would suggest non-linearity in the model.\n")

    # 6b: distribution around zero
    print("6b. are residuals randomly distributed around zero?\n")
    print(f"  mean of residuals: {np.mean(resid):.6f}")
    print(f"  std of residuals:  {np.std(resid):.6f}")
    n_pos = (resid > 0).sum()
    n_neg = (resid < 0).sum()
    print(f"  positive: {n_pos}, negative: {n_neg}")
    print(f"  the mean is essentially zero, which is expected for ols.\n")

    # 6c: heteroscedasticity
    print("6c. heteroscedasticity check\n")

    bp_stat, bp_p, _, _ = het_breuschpagan(resid, X_const)
    print(f"  breusch-pagan: statistic = {bp_stat:.4f}, p-value = {bp_p:.4f}")
    if bp_p > 0.05:
        print("    no significant heteroscedasticity detected")
    else:
        print("    heteroscedasticity detected - variance of errors is not constant")

    white_stat, white_p, _, _ = het_white(resid, X_const)
    print(
        f"  white test:    statistic = {white_stat:.4f}, p-value = {white_p:.4f}")
    if white_p > 0.05:
        print("    no significant heteroscedasticity detected")
    else:
        print("    heteroscedasticity detected")

    print("""
  why we check: ols assumes constant variance of errors (homoscedasticity).
  if violated, standard errors are unreliable and we should use robust
  standard errors (hc3) instead.
""")

    # scale-location plot
    std_resid = model.get_influence().resid_studentized_internal
    fig, ax = plt.subplots(figsize=(10, 6))
    ax.scatter(fitted, np.sqrt(np.abs(std_resid)), s=28, alpha=0.55,
               color="#e74c3c", edgecolors="white", linewidths=0.3)
    lowess_sc = sm.nonparametric.lowess(
        np.sqrt(np.abs(std_resid)), fitted, frac=0.4)
    ax.plot(lowess_sc[:, 0], lowess_sc[:, 1],
            color="#f39c12", lw=2.5, label="lowess trend")
    ax.set_xlabel("fitted values (years)")
    ax.set_ylabel("sqrt(|standardized residuals|)")
    ax.set_title("scale-location plot (heteroscedasticity)", weight="bold")
    ax.legend()
    ax.grid(alpha=0.3)
    plt.tight_layout()
    save_fig(fig, "09_scale_location")

    # 6d: normality
    print("6d. normality of residuals\n")
    shapiro_stat, shapiro_p = stats.shapiro(resid)
    jb_stat, jb_p = stats.jarque_bera(resid)
    print(f"  shapiro-wilk: W = {shapiro_stat:.4f}, p = {shapiro_p:.4f}")
    print(f"  jarque-bera:  stat = {jb_stat:.4f}, p = {jb_p:.4f}")
    print(
        f"  skewness = {stats.skew(resid):.4f}, kurtosis = {stats.kurtosis(resid):.4f}")
    if shapiro_p > 0.05:
        print("  residuals appear normally distributed")
    else:
        print("  residuals deviate from normality")

    print("""
  why we check: ols coefficient estimates are unbiased regardless of
  normality, but hypothesis tests (p-values, confidence intervals)
  require normally distributed errors to be valid. with large samples
  (n > 100), the central limit theorem provides some protection.
""")

    # q-q plot + histogram
    fig, axes = plt.subplots(1, 2, figsize=(13, 5))

    ax = axes[0]
    (osm, osr), (slope, intercept, _) = stats.probplot(resid, dist="norm")
    ax.scatter(osm, osr, s=22, alpha=0.6, color="#667eea",
               edgecolors="white", lw=0.3)
    xs = np.array([osm.min(), osm.max()])
    ax.plot(xs, slope * xs + intercept, "r--", lw=2)
    ax.set_xlabel("theoretical quantiles")
    ax.set_ylabel("sample quantiles")
    ax.set_title("q-q plot of residuals", weight="bold")
    ax.grid(alpha=0.3)

    ax = axes[1]
    ax.hist(resid, bins=25, density=True, alpha=0.6,
            color="#9b59b6", edgecolor="white")
    xn = np.linspace(resid.min(), resid.max(), 200)
    ax.plot(xn, stats.norm.pdf(xn, np.mean(resid), np.std(resid)),
            "r-", lw=2, label="normal curve")
    ax.set_xlabel("residual (years)")
    ax.set_ylabel("density")
    ax.set_title("histogram of residuals", weight="bold")
    ax.legend()
    ax.grid(alpha=0.3)

    plt.tight_layout()
    save_fig(fig, "10_qq_and_histogram")

    # 6e: autocorrelation
    print("6e. autocorrelation of residuals\n")
    n_lags = 30
    acf_values, confint = acf(resid, nlags=n_lags, alpha=0.05)
    conf_band = 1.96 / np.sqrt(len(resid))

    lb_test = acorr_ljungbox(resid, lags=[10, 20], return_df=True)
    print("  ljung-box test:")
    print(f"  {lb_test.to_string()}")
    lb_p10 = lb_test["lb_pvalue"].iloc[0]
    if lb_p10 < 0.05:
        print("  significant autocorrelation detected at lag 10")
    else:
        print("  no significant autocorrelation at lag 10")

    print("""
  why we check: autocorrelated residuals mean the model is missing
  a time/order-dependent pattern. for cross-sectional data like this,
  the ordering of observations (here, by hdi rank) is arbitrary.
  durbin-watson and ljung-box are designed for time series and should
  be interpreted cautiously in this context.
""")

    fig, ax = plt.subplots(figsize=(10, 5))
    lags = list(range(n_lags + 1))
    bar_colors = ["#e74c3c" if abs(
        v) > conf_band else "#00bcd4" for v in acf_values]
    ax.bar(lags, acf_values, color=bar_colors, width=0.6)
    ax.axhline(conf_band, ls="--", color="#f39c12", lw=1.5,
               label=f"95% ci (+/-{conf_band:.3f})")
    ax.axhline(-conf_band, ls="--", color="#f39c12", lw=1.5)
    ax.axhline(0, color="grey", lw=0.5)
    ax.set_xlabel("lag")
    ax.set_ylabel("autocorrelation")
    ax.set_title("acf of residuals", weight="bold")
    ax.legend()
    ax.grid(alpha=0.3)
    plt.tight_layout()
    save_fig(fig, "11_acf_residuals")

    # 6f: overall adequacy
    print("6f. overall model adequacy\n")
    issues = []
    if bp_p <= 0.05:
        issues.append("heteroscedasticity (breusch-pagan)")
    if white_p <= 0.05:
        issues.append("heteroscedasticity (white)")
    if shapiro_p <= 0.05:
        issues.append("non-normal residuals")
    if lb_p10 < 0.05:
        issues.append("autocorrelation")

    if issues:
        print(f"  potential issues: {', '.join(issues)}")
        print("  these should be considered when interpreting results,")
        print("  but do not necessarily invalidate the model.")
    else:
        print("  the model passes all diagnostic checks.")
        print("  residuals are approximately normal, homoscedastic,")
        print("  and show no significant autocorrelation.")

    return bp_p, shapiro_p


if __name__ == "__main__":
    from step1_data_understanding import load_and_clean
    from step2_variable_identification import identify_variables
    from step3_variable_selection import variable_selection
    from step5_model_estimation import fit_model
    df = load_and_clean()
    df_analysis = identify_variables(df)
    best_predictors, _ = variable_selection(df_analysis)
    model, df_analysis, X_const = fit_model(df_analysis, best_predictors)
    residual_analysis(model, df_analysis, X_const)
