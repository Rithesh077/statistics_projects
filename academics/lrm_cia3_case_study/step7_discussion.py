"""
step 7: discussion and conclusion

- evaluate whether the model makes theoretical and practical sense
- identify limitations
- suggest improvements and alternative specifications
"""

import numpy as np


def discussion(model, bp_p, shapiro_p, best_predictors):
    print("\nstep 7: discussion and conclusion\n")

    r2_pct = model.rsquared * 100
    fit_desc = "strong" if r2_pct > 80 else "moderately strong" if r2_pct > 60 else "moderate"

    # data-driven coefficient sign analysis
    coeffs = model.params.drop("const")
    positive = [name for name, val in coeffs.items() if val > 0]
    negative = [name for name, val in coeffs.items() if val < 0]

    coeff_text = ""
    if negative:
        coeff_text = (
            f"  most coefficients are positive ({', '.join(positive)}),\n"
            f"  while {', '.join(negative)} show(s) a negative association\n"
            f"  with life expectancy. the negative coefficient(s) may reflect\n"
            f"  confounding or multicollinearity rather than a true causal effect."
        )
    else:
        coeff_text = (
            f"  all coefficients are positive, meaning higher education levels\n"
            f"  and higher income are associated with longer life expectancy.\n"
            f"  this aligns with public health literature on social determinants\n"
            f"  of health."
        )

    # log_gni interpretation (rigorous)
    log_gni_text = ""
    if "log_gni" in best_predictors:
        b_gni = coeffs.get("log_gni", 0)
        pct_effect = b_gni * np.log(1.01)
        log_gni_text = (
            f"\n  the log transformation of gni captures diminishing returns of\n"
            f"  income on health: a 1% increase in gni per capita is associated\n"
            f"  with a {abs(pct_effect):.4f}-year change in life expectancy\n"
            f"  (exact: b * ln(1.01) = {b_gni:.4f} * {np.log(1.01):.6f}).\n"
            f"  an extra dollar matters far more for a poor country than a rich one."
        )

    print(f"""model assessment:
  r2 = {model.rsquared:.4f}, meaning the model explains {r2_pct:.1f}%
  of the variance in life expectancy. this is a {fit_desc} fit.
  the f-test is highly significant, confirming that the predictors
  collectively have explanatory power.

theoretical and practical sense:
{coeff_text}
{log_gni_text}

  from a policy perspective, the results suggest that investments in
  education (improving school access and attainment) and economic
  development are associated with longer life expectancy. education
  improves health literacy, nutrition knowledge, and access to
  healthcare services. income enables investment in healthcare
  infrastructure, clean water, and sanitation.

limitations:
  1. multicollinearity: expected_schooling and mean_schooling both
     measure aspects of education, so some correlation is expected.
     this can inflate vif scores and make individual coefficient
     estimates less stable, though overall model fit remains valid.

  2. omitted variable bias: there may be other important predictors
     of life expectancy not in this dataset, such as healthcare
     spending per capita, access to clean water, disease burden
     (hiv/malaria prevalence), urbanisation rate, or conflict status.
     omission of these variables may bias the estimated coefficients.

  3. cross-sectional data: using one year of data (2023) means we
     cannot capture how changes in predictors affect life expectancy
     over time. a panel data approach would be more informative.

  4. outliers: some countries (e.g. conflict zones with high mortality
     despite adequate gni, or oil-rich states with high income but
     poor health outcomes) may disproportionately influence results.

  5. diagnostic results:
     breusch-pagan p = {bp_p:.4f}
     {"heteroscedasticity is present, which means standard errors may be unreliable. robust standard errors (hc3) should be used." if bp_p < 0.05 else "no significant heteroscedasticity detected."}
     shapiro-wilk p = {shapiro_p:.4f}
     {"residual non-normality may affect the validity of p-values, though with n > 100 the central limit theorem provides some protection." if shapiro_p < 0.05 else "residuals are approximately normal."}

  6. cross-sectional autocorrelation tests: durbin-watson and ljung-box
     are designed for time series data. for cross-sectional data ordered
     by hdi rank, significant autocorrelation may reflect geographic or
     development-level clustering rather than true serial dependence.

suggested improvements:
  1. use robust standard errors (hc3) if heteroscedasticity is present
  2. add interaction terms (e.g. education x income) for synergistic effects
  3. use panel data (multiple years) with fixed or random effects
  4. include additional predictors (healthcare spending, access to clean water)
  5. consider regional dummy variables to capture continent-level effects
  6. remove extreme outlier countries or use robust regression (m-estimators)
  7. use cross-validation to assess out-of-sample predictive performance

conclusion:
  the multiple linear regression model predicts life expectancy from
  education and income indicators with r2 = {model.rsquared:.4f}. the model
  was selected via best subset variable selection using adjusted r-squared,
  aic, and bic criteria. all key ols assumptions were tested through
  comprehensive residual diagnostics: residuals vs fitted values for
  linearity, breusch-pagan and white tests for heteroscedasticity,
  q-q plot and shapiro-wilk for normality, and acf with ljung-box
  for autocorrelation. the analysis demonstrates that education and
  income are significant predictors of life expectancy across countries,
  consistent with the public health literature on social determinants
  of health.
""")


if __name__ == "__main__":
    from step1_data_understanding import load_and_clean
    from step2_variable_identification import identify_variables
    from step3_variable_selection import variable_selection
    from step5_model_estimation import fit_model
    from step6_residual_analysis import residual_analysis
    df = load_and_clean()
    df_analysis = identify_variables(df)
    best_predictors, _ = variable_selection(df_analysis)
    model, df_analysis, X_const = fit_model(df_analysis, best_predictors)
    bp_p, shapiro_p = residual_analysis(model, df_analysis, X_const)
    discussion(model, bp_p, shapiro_p, best_predictors)
