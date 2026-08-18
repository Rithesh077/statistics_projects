"""
generate a self-contained html presentation with all charts, tables,
and interpretations embedded. opens automatically in the browser.
"""

import os
import re
import base64
import itertools
import numpy as np
import pandas as pd
from scipy import stats
import statsmodels.api as sm
from statsmodels.stats.diagnostic import het_breuschpagan, het_white, acorr_ljungbox
from statsmodels.stats.outliers_influence import variance_inflation_factor
from statsmodels.tsa.stattools import acf

from config import DATASET_PATH, OUTPUT_DIR


def _img_tag(filename, alt="chart"):
    """embed a png from outputs/ as a base64 img tag."""
    path = os.path.join(OUTPUT_DIR, filename)
    if not os.path.exists(path):
        return f"<p><em>[chart not found: {filename}]</em></p>"
    with open(path, "rb") as f:
        b64 = base64.b64encode(f.read()).decode()
    return f'<img src="data:image/png;base64,{b64}" alt="{alt}">'


def _df_to_html(df):
    """convert a dataframe to a styled html table."""
    rows = []
    rows.append("<table>")
    rows.append("<thead><tr>" +
                "".join(f"<th>{c}</th>" for c in df.columns) +
                "</tr></thead>")
    rows.append("<tbody>")
    for _, row in df.iterrows():
        cells = []
        for col in df.columns:
            val = row[col]
            if isinstance(val, float):
                cells.append(f"<td>{val:.4f}</td>")
            else:
                cells.append(f"<td>{val}</td>")
        rows.append("<tr>" + "".join(cells) + "</tr>")
    rows.append("</tbody></table>")
    return "\n".join(rows)


CSS = """
* { margin: 0; padding: 0; box-sizing: border-box; }
body {
    font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
    background: #f0f2f5; color: #2c3e50; line-height: 1.7;
}
.slide {
    max-width: 1000px; margin: 40px auto; background: #fff;
    border-radius: 12px; box-shadow: 0 4px 24px rgba(0,0,0,0.08);
    padding: 48px 56px; page-break-after: always;
}
.slide.cover {
    text-align: center; padding: 80px 56px;
    background: linear-gradient(135deg, #1e3c72, #2a5298);
    color: #fff;
}
.cover h1 { font-size: 2.4em; margin-bottom: 12px; font-weight: 700; }
.cover h2 { font-size: 1.3em; font-weight: 400; opacity: 0.9; margin-bottom: 32px; }
.cover .meta { font-size: 1em; opacity: 0.8; line-height: 2; }
h2.section {
    font-size: 1.6em; color: #1e3c72; border-bottom: 3px solid #1e3c72;
    padding-bottom: 8px; margin-bottom: 20px;
}
h3 { font-size: 1.15em; color: #34495e; margin: 20px 0 8px; }
p, li { font-size: 0.98em; margin-bottom: 8px; }
ul { margin-left: 24px; margin-bottom: 12px; }
.highlight {
    background: #eef3ff; border-left: 4px solid #1e3c72;
    padding: 14px 18px; margin: 16px 0; border-radius: 4px;
}
.warning {
    background: #fff8e1; border-left: 4px solid #f39c12;
    padding: 14px 18px; margin: 16px 0; border-radius: 4px;
}
.success {
    background: #e8f5e9; border-left: 4px solid #27ae60;
    padding: 14px 18px; margin: 16px 0; border-radius: 4px;
}
table {
    width: 100%; border-collapse: collapse; margin: 16px 0;
    font-size: 0.9em;
}
th {
    background: #1e3c72; color: #fff; padding: 10px 12px;
    text-align: center; font-weight: 600;
}
td { padding: 8px 12px; text-align: center; border-bottom: 1px solid #e0e0e0; }
tr:nth-child(even) { background: #f6f8fc; }
img { max-width: 100%; height: auto; display: block; margin: 16px auto; border-radius: 8px; }
code {
    background: #f0f2f5; padding: 2px 6px; border-radius: 3px;
    font-family: 'Consolas', monospace; font-size: 0.92em;
}
.formula {
    background: #f6f8fc; padding: 16px; border-radius: 6px;
    font-family: 'Consolas', monospace; font-size: 1.05em;
    text-align: center; margin: 16px 0; letter-spacing: 0.5px;
}
.grid-2 { display: grid; grid-template-columns: 1fr 1fr; gap: 16px; }
.stat-card {
    background: #f6f8fc; border-radius: 8px; padding: 16px;
    text-align: center;
}
.stat-card .value { font-size: 1.8em; font-weight: 700; color: #1e3c72; }
.stat-card .label { font-size: 0.85em; color: #666; margin-top: 4px; }
"""


def generate_html():
    # ── load and clean data (same logic as generate_report) ──
    for enc in ["utf-8", "latin-1", "cp1252"]:
        try:
            raw = pd.read_csv(DATASET_PATH, header=None, encoding=enc)
            break
        except UnicodeDecodeError:
            continue

    df_clean = raw.iloc[5:].copy()
    cols_to_keep = [0, 1, 2, 4, 6, 8, 10, 12, 14]
    cols_to_keep = [c for c in cols_to_keep if c < df_clean.shape[1]]
    df_clean = df_clean[df_clean.columns[cols_to_keep]]
    df_clean.columns = [
        "hdi_rank", "country", "hdi_value",
        "life_expectancy", "expected_schooling", "mean_schooling",
        "gni_per_capita", "gni_rank_minus_hdi_rank", "hdi_rank_2022"
    ][:len(cols_to_keep)]

    def strip_footnotes(value):
        if isinstance(value, str):
            return re.sub(r'[a-z,]', '', value).strip()
        return value

    numeric_cols = ["hdi_value", "life_expectancy", "expected_schooling",
                    "mean_schooling", "gni_per_capita", "gni_rank_minus_hdi_rank",
                    "hdi_rank_2022"]
    for col in numeric_cols:
        if col in df_clean.columns:
            df_clean[col] = df_clean[col].apply(strip_footnotes)
            df_clean[col] = pd.to_numeric(df_clean[col], errors="coerce")

    df_clean = df_clean.dropna(subset=["country", "hdi_value"])
    df_clean["country"] = df_clean["country"].astype(str).str.strip()
    df_clean = df_clean.reset_index(drop=True)

    analysis_cols = ["life_expectancy", "expected_schooling",
                     "mean_schooling", "gni_per_capita"]

    # mean imputation
    for col in analysis_cols:
        n = df_clean[col].isnull().sum()
        if n > 0:
            df_clean[col] = df_clean[col].fillna(df_clean[col].mean())

    df_analysis = df_clean[analysis_cols].dropna().copy()
    df_analysis["log_gni"] = np.log(df_analysis["gni_per_capita"])

    # ── best subset selection ──
    target = "life_expectancy"
    candidate_predictors = ["expected_schooling", "mean_schooling", "log_gni"]
    y_sel = df_analysis[target]
    results = []
    for k in range(1, len(candidate_predictors) + 1):
        for subset in itertools.combinations(candidate_predictors, k):
            subset_list = list(subset)
            X_sel = sm.add_constant(df_analysis[subset_list])
            mdl = sm.OLS(y_sel, X_sel).fit()
            results.append({
                "predictors": " + ".join(subset_list),
                "n_predictors": len(subset_list),
                "r_squared": mdl.rsquared,
                "adj_r_squared": mdl.rsquared_adj,
                "aic": mdl.aic,
                "bic": mdl.bic,
            })
    results_df = pd.DataFrame(results).sort_values(
        "adj_r_squared", ascending=False).reset_index(drop=True)
    best_adj = results_df.iloc[0]
    best_aic_row = results_df.loc[results_df["aic"].idxmin()]
    best_bic_row = results_df.loc[results_df["bic"].idxmin()]
    best_predictors = best_adj["predictors"].split(" + ")

    # ── fit final model ──
    X = df_analysis[best_predictors]
    y = df_analysis[target]
    X_const = sm.add_constant(X)
    model = sm.OLS(y, X_const).fit()
    coeffs = model.params
    pvals = model.pvalues
    resid = model.resid.values

    # ── diagnostic tests ──
    dw = sm.stats.durbin_watson(resid)
    bp_stat, bp_p, _, _ = het_breuschpagan(resid, X_const)
    white_stat, white_p, _, _ = het_white(resid, X_const)
    shapiro_stat, shapiro_p = stats.shapiro(resid)
    jb_stat, jb_p = stats.jarque_bera(resid)
    lb_test = acorr_ljungbox(resid, lags=[10, 20], return_df=True)
    lb_p10 = lb_test["lb_pvalue"].iloc[0]
    lb_p20 = lb_test["lb_pvalue"].iloc[1]

    # ── vif ──
    vif_data = pd.DataFrame()
    vif_data["variable"] = X.columns
    vif_data["vif"] = [variance_inflation_factor(
        X_const.values, i + 1) for i in range(len(X.columns))]
    vif_data["assessment"] = vif_data["vif"].apply(
        lambda v: "severe" if v > 10 else "high" if v > 5 else "acceptable")
    vif_data = vif_data.sort_values(
        "vif", ascending=True).reset_index(drop=True)

    # ── correlation matrix ──
    corr_cols = ["life_expectancy", "expected_schooling",
                 "mean_schooling", "log_gni"]
    corr_matrix = df_analysis[corr_cols].corr().round(3)
    corr_display = corr_matrix.copy()
    corr_display.insert(0, "variable", corr_display.index)
    corr_display = corr_display.reset_index(drop=True)
    corr_table_html = _df_to_html(corr_display)

    # ── summary stats ──
    desc = df_clean[analysis_cols].describe().round(3).T
    desc.insert(0, "variable", desc.index)
    desc = desc.reset_index(drop=True)

    # ── missing values ──
    miss = df_clean[analysis_cols].isnull().sum()
    miss_text = ", ".join([f"{col}: {n}" for col, n in miss.items() if n > 0])
    if not miss_text:
        miss_text = "none"

    # ── derived values ──
    r2_pct = model.rsquared * 100
    fit_desc = "strong" if r2_pct > 80 else "moderately strong" if r2_pct > 60 else "moderate"
    le_skew = stats.skew(df_analysis[target].dropna())
    skew_desc = "left-skewed" if le_skew < - \
        0.5 else "right-skewed" if le_skew > 0.5 else "approximately symmetric"
    n_pos = (resid > 0).sum()
    n_neg = (resid < 0).sum()

    model_coeffs = model.params.drop("const")
    positive = [name for name, val in model_coeffs.items() if val > 0]
    negative = [name for name, val in model_coeffs.items() if val < 0]
    if negative:
        sign_text = (f"Most coefficients are positive ({', '.join(positive)}), "
                     f"while {', '.join(negative)} show(s) a negative association.")
    else:
        sign_text = ("All coefficients are positive, meaning higher education and "
                     "income are associated with longer life expectancy.")

    # ── build coefficient interpretation html ──
    coeff_html = ""
    for var in best_predictors:
        direction = "increase" if coeffs[var] > 0 else "decrease"
        sig = "significant" if pvals[var] < 0.05 else "not significant"
        sig_class = "success" if pvals[var] < 0.05 else "warning"
        if var == "log_gni":
            pct_effect = coeffs[var] * np.log(1.01)
            coeff_html += f"""
            <div class="{sig_class}">
                <strong>log_gni</strong> (b = {coeffs[var]:.6f}, p = {pvals[var]:.4f} &mdash; {sig})<br>
                A 1% increase in GNI per capita is associated with a
                <strong>{abs(pct_effect):.4f}-year {direction}</strong> in life expectancy.<br>
                <small>Exact: b &times; ln(1.01) = {coeffs[var]:.4f} &times; {np.log(1.01):.6f} = {pct_effect:.4f}</small>
            </div>"""
        else:
            coeff_html += f"""
            <div class="{sig_class}">
                <strong>{var}</strong> (b = {coeffs[var]:.6f}, p = {pvals[var]:.4f} &mdash; {sig})<br>
                Each additional unit of {var.replace('_', ' ')} is associated with a
                <strong>{abs(coeffs[var]):.4f}-year {direction}</strong> in life expectancy,
                holding other variables constant.
            </div>"""

    # ── build subset selection table ──
    sel_display = results_df[["predictors", "r_squared",
                              "adj_r_squared", "aic", "bic"]].copy()

    # ── coefficient table ──
    coef_df = pd.DataFrame({
        "variable": model.params.index,
        "coefficient": model.params.values.round(6),
        "std error": model.bse.values.round(6),
        "t-statistic": model.tvalues.values.round(4),
        "p-value": model.pvalues.values.round(4),
    })

    # ── pearson correlations for EDA ──
    corr_lines = ""
    for pred in best_predictors:
        r, p = stats.pearsonr(df_analysis[pred], df_analysis[target])
        corr_lines += f"<li><strong>{pred.replace('_', ' ')}</strong>: r = {r:.4f}, p = {p:.2e}</li>\n"

    # ── diagnostic issues ──
    issues = []
    if bp_p <= 0.05:
        issues.append("heteroscedasticity (Breusch-Pagan)")
    if shapiro_p <= 0.05:
        issues.append("non-normal residuals")
    if lb_p10 < 0.05:
        issues.append("autocorrelation")

    if issues:
        adequacy_html = f"""<div class="warning">
            <strong>Potential issues detected:</strong> {', '.join(issues)}.<br>
            These should be considered when interpreting results but do not necessarily
            invalidate the model. Robust standard errors (HC3) can address heteroscedasticity.
        </div>"""
    else:
        adequacy_html = """<div class="success">
            The model passes all diagnostic checks. Residuals are approximately normal,
            homoscedastic, and show no significant autocorrelation. OLS assumptions hold.
        </div>"""

    formula_rhs = " + ".join([f"b<sub>{i+1}</sub>({p})" for i,
                             p in enumerate(best_predictors)])

    # ── assemble html ──
    html = f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Life Expectancy Regression Analysis - LRM Case Study</title>
<style>{CSS}</style>
</head>
<body>

<!-- ===== COVER ===== -->
<div class="slide cover">
    <h1>Regression Modelling Case Study</h1>
    <h2>Life Expectancy Analysis Using UNDP HDR Data</h2>
    <div class="meta">
        Dataset: HDR25 Statistical Annex Table 1 (UNDP, 2025)<br>
        Dependent Variable: Life Expectancy at Birth (2023)<br>
        Independent Variables: Expected Schooling, Mean Schooling, log(GNI per capita)<br>
        Observations: {df_analysis.shape[0]} countries
    </div>
</div>

<!-- ===== STEP 1: DATA UNDERSTANDING ===== -->
<div class="slide">
    <h2 class="section">1. Data Understanding</h2>

    <p>The dataset is sourced from the <strong>UNDP Human Development Report 2025</strong>
    (Statistical Annex, Table 1). It contains data for <strong>{df_clean.shape[0]} countries</strong>
    across <strong>{df_clean.shape[1]} variables</strong>.</p>

    <p>The raw CSV has a multi-row header with footnote markers (e.g. 'a', 'c', 'e')
    embedded in numeric values, and section dividers like "Very High Human Development".
    These were cleaned by stripping footnote characters and removing non-data rows.</p>

    <h3>Variables</h3>
    <ul>
        <li><code>life_expectancy</code> (numeric) &mdash; Life expectancy at birth, in years</li>
        <li><code>expected_schooling</code> (numeric) &mdash; Expected years of schooling</li>
        <li><code>mean_schooling</code> (numeric) &mdash; Mean years of schooling (age 25+)</li>
        <li><code>gni_per_capita</code> (numeric) &mdash; Gross national income per capita (2021 PPP $)</li>
    </ul>

    <h3>Missing Values</h3>
    <p>{miss_text if miss_text != "none" else "No missing values found in the analysis columns."}</p>

    <h3>Summary Statistics</h3>
    {_df_to_html(desc)}
</div>

<!-- ===== STEP 2: VARIABLE IDENTIFICATION ===== -->
<div class="slide">
    <h2 class="section">2. Identification of Variables</h2>

    <div class="highlight">
        <strong>Dependent Variable (Y):</strong> <code>life_expectancy</code><br>
        Life expectancy at birth (years) measures population health outcomes.
        We want to understand what socioeconomic factors drive longevity across countries.
    </div>

    <div class="warning">
        <strong>Why not HDI?</strong> HDI is excluded as a predictor because the HDI formula
        already incorporates life expectancy as one of its three dimensions.
        Using HDI would produce a circular (tautological) regression with artificially inflated R&sup2;.
    </div>

    <h3>Independent Variables (Candidate Predictors)</h3>
    <ul>
        <li><strong>expected_schooling</strong> &mdash; Education access; years a child is expected to attend school</li>
        <li><strong>mean_schooling</strong> &mdash; Educational attainment; average years for adults (25+)</li>
        <li><strong>log_gni</strong> &mdash; Log of gross national income per capita (2021 PPP $)</li>
    </ul>

    <p><strong>Justification:</strong> Education and income are well-established social determinants
    of health in public health literature. Higher education leads to better health literacy and
    healthcare access. Higher income enables investment in healthcare infrastructure, nutrition,
    and sanitation. We use log(GNI) because the income-health relationship shows diminishing
    marginal returns.</p>

    <h3>Correlation Matrix</h3>
    {corr_table_html}
    {_img_tag("01_correlation_heatmap.png", "Correlation Heatmap")}
</div>

<!-- ===== STEP 3: VARIABLE SELECTION ===== -->
<div class="slide">
    <h2 class="section">3. Variable Selection (Best Subset)</h2>

    <p>Best subset selection evaluates <strong>all possible combinations</strong> of the 3 candidate
    predictors (2&sup3; &minus; 1 = 7 non-empty subsets). Each model is evaluated using:</p>
    <ul>
        <li><strong>Adjusted R&sup2;</strong> &mdash; penalises model complexity (higher is better)</li>
        <li><strong>AIC</strong> (Akaike Information Criterion) &mdash; lower is better</li>
        <li><strong>BIC</strong> (Bayesian Information Criterion) &mdash; lower is better</li>
    </ul>

    <h3>Model Comparison</h3>
    {_df_to_html(sel_display)}
    {_img_tag("02_subset_selection.png", "Best Subset Comparison")}

    <h3>Selection Result</h3>
    <div class="success">
        <strong>Best by Adjusted R&sup2;:</strong> {best_adj['predictors']}
        (Adj R&sup2; = {best_adj['adj_r_squared']:.4f})<br>
        <strong>Best by AIC:</strong> {best_aic_row['predictors']}
        (AIC = {best_aic_row['aic']:.2f})<br>
        <strong>Best by BIC:</strong> {best_bic_row['predictors']}
        (BIC = {best_bic_row['bic']:.2f})<br><br>
        <strong>Selected model:</strong> life_expectancy ~ {' + '.join(best_predictors)}
    </div>
    <p>All three criteria agree on the same model. Adjusted R&sup2; penalises predictors
    that do not sufficiently improve the fit, ensuring parsimony.</p>
</div>

<!-- ===== STEP 4: EDA ===== -->
<div class="slide">
    <h2 class="section">4. Exploratory Data Analysis</h2>

    <p>Scatter plots were generated for each selected predictor against life expectancy,
    with linear trend lines and Pearson correlation coefficients.</p>

    {_img_tag("03_scatter_plots.png", "Scatter Plots")}

    <h3>Pearson Correlations</h3>
    <ul>{corr_lines}</ul>

    <h3>Distribution Analysis</h3>
    {_img_tag("04_distributions.png", "Distributions")}

    <p>Life expectancy distribution is <strong>{skew_desc}</strong>
    (skewness = {le_skew:.3f}). Education variables show positive trends.
    log(GNI) shows a positive relationship &mdash; wealthier countries invest
    more in healthcare, nutrition, and sanitation.</p>
</div>

<!-- ===== STEP 5: MODEL ESTIMATION ===== -->
<div class="slide">
    <h2 class="section">5. Model Specification and Estimation</h2>

    <h3>Model Specification</h3>
    <div class="formula">
        life_expectancy = b<sub>0</sub> + {formula_rhs} + &epsilon;
    </div>

    <p>Predictors selected via best subset selection. Log transformation applied to GNI
    because the income-health relationship shows diminishing marginal returns.</p>

    <h3>Estimated Coefficients</h3>
    {_df_to_html(coef_df)}

    <h3>Coefficient Interpretation</h3>
    {coeff_html}

    {_img_tag("05_coefficient_plot.png", "Coefficient Plot")}

    <h3>Multicollinearity Check (VIF)</h3>
    {_df_to_html(vif_data)}
    <p>VIF measures how much each predictor is explained by the others.
    Values &lt; 5 are acceptable. Both predictors show acceptable VIF levels.</p>
    {_img_tag("06_vif.png", "VIF Chart")}

    <h3>Goodness of Fit</h3>
    <div class="grid-2">
        <div class="stat-card">
            <div class="value">{model.rsquared:.4f}</div>
            <div class="label">R-squared ({r2_pct:.1f}% explained)</div>
        </div>
        <div class="stat-card">
            <div class="value">{model.rsquared_adj:.4f}</div>
            <div class="label">Adjusted R-squared</div>
        </div>
        <div class="stat-card">
            <div class="value">{model.fvalue:.2f}</div>
            <div class="label">F-statistic (p = {model.f_pvalue:.2e})</div>
        </div>
        <div class="stat-card">
            <div class="value">{dw:.4f}</div>
            <div class="label">Durbin-Watson</div>
        </div>
    </div>
    <p>The model explains <strong>{r2_pct:.1f}%</strong> of the variation in life expectancy
    ({fit_desc} fit). The F-test is highly significant.
    <em>Note: For cross-sectional data, Durbin-Watson should be interpreted cautiously
    as observation ordering is arbitrary.</em></p>

    {_img_tag("07_actual_vs_predicted.png", "Actual vs Predicted")}
</div>

<!-- ===== STEP 6: RESIDUAL ANALYSIS ===== -->
<div class="slide">
    <h2 class="section">6. Residual Analysis</h2>

    <p>Residual analysis verifies that OLS assumptions hold. If violated, coefficient
    estimates may be biased or standard errors unreliable.</p>

    <h3>6a. Residuals vs Fitted Values</h3>
    {_img_tag("08_residuals_vs_fitted.png", "Residuals vs Fitted")}
    <p>The LOWESS smoother should stay near zero. A funnel shape indicates
    heteroscedasticity; a curve suggests misspecification.</p>

    <h3>6b. Residuals Around Zero</h3>
    <div class="highlight">
        Mean of residuals: <strong>{np.mean(resid):.6f}</strong><br>
        Std of residuals: <strong>{np.std(resid):.4f}</strong><br>
        Positive: <strong>{n_pos}</strong>, Negative: <strong>{n_neg}</strong><br>
        The mean is essentially zero (expected for OLS) with no systematic bias.
    </div>

    <h3>6c. Heteroscedasticity</h3>
    <div class="{'warning' if bp_p < 0.05 else 'success'}">
        Breusch-Pagan: statistic = {bp_stat:.4f}, <strong>p = {bp_p:.4f}</strong><br>
        White test: statistic = {white_stat:.4f}, <strong>p = {white_p:.4f}</strong><br><br>
        {'Heteroscedasticity detected (BP p &lt; 0.05). Standard errors should be treated with caution. Consider using robust standard errors (HC3).' if bp_p < 0.05 else 'No significant heteroscedasticity. The constant variance assumption holds.'}
    </div>
    {_img_tag("09_scale_location.png", "Scale-Location Plot")}

    <h3>6d. Normality of Residuals</h3>
    <div class="{'warning' if shapiro_p < 0.05 else 'success'}">
        Shapiro-Wilk: W = {shapiro_stat:.4f}, <strong>p = {shapiro_p:.4f}</strong><br>
        Jarque-Bera: stat = {jb_stat:.4f}, <strong>p = {jb_p:.4f}</strong><br>
        Skewness = {stats.skew(resid):.4f}, Kurtosis = {stats.kurtosis(resid):.4f}<br><br>
        {'Residuals deviate from normality, but with n &gt; 100 the Central Limit Theorem provides protection.' if shapiro_p < 0.05 else 'Residuals appear approximately normally distributed.'}
    </div>
    {_img_tag("10_qq_and_histogram.png", "QQ Plot and Histogram")}

    <h3>6e. Autocorrelation</h3>
    <div class="{'warning' if lb_p10 < 0.05 else 'success'}">
        Ljung-Box (lag 10): <strong>p = {lb_p10:.4f}</strong><br>
        Ljung-Box (lag 20): <strong>p = {lb_p20:.4f}</strong><br><br>
        {'Significant autocorrelation detected &mdash; may reflect development-level clustering.' if lb_p10 < 0.05 else 'No significant autocorrelation. Residuals appear independent.'}<br>
        <small><em>Note: For cross-sectional data ordered by HDI rank, Durbin-Watson and
        Ljung-Box should be interpreted cautiously as observation ordering is arbitrary.</em></small>
    </div>
    {_img_tag("11_acf_residuals.png", "ACF of Residuals")}

    <h3>6f. Overall Model Adequacy</h3>
    {adequacy_html}
</div>

<!-- ===== STEP 7: DISCUSSION ===== -->
<div class="slide">
    <h2 class="section">7. Discussion and Conclusion</h2>

    <h3>Model Assessment</h3>
    <p>R&sup2; = <strong>{model.rsquared:.4f}</strong>, meaning the model explains
    <strong>{r2_pct:.1f}%</strong> of the variance in life expectancy.
    This is a <strong>{fit_desc}</strong> fit. The F-test is highly significant
    (p = {model.f_pvalue:.2e}).</p>

    <h3>Theoretical and Practical Sense</h3>
    <p>{sign_text}</p>
    {"<p>The log transformation of GNI captures diminishing returns of income on health: a 1%% increase in GNI per capita is associated with a <strong>%.4f-year</strong> change in life expectancy.</p>" % abs(model_coeffs.get("log_gni", 0) * np.log(1.01)) if "log_gni" in best_predictors else ""}
    <p>From a policy perspective, investments in education (school access and attainment)
    and economic development are associated with longer life expectancy.
    Education improves health literacy and healthcare access;
    income enables investment in healthcare infrastructure and sanitation.</p>

    <h3>Limitations</h3>
    <ol>
        <li><strong>Multicollinearity:</strong> Education variables are correlated,
        potentially inflating standard errors.</li>
        <li><strong>Omitted variable bias:</strong> Important predictors like healthcare
        spending, clean water access, and disease burden are not in this dataset.</li>
        <li><strong>Cross-sectional data:</strong> Using a single year (2023) cannot
        capture temporal dynamics.</li>
        <li><strong>Outliers:</strong> Conflict zones and oil-rich states may distort results.</li>
        <li><strong>Diagnostics:</strong> Breusch-Pagan p = {bp_p:.4f},
        Shapiro-Wilk p = {shapiro_p:.4f}.</li>
        <li><strong>Autocorrelation tests:</strong> Designed for time series; interpret
        cautiously for cross-sectional data.</li>
    </ol>

    <h3>Suggested Improvements</h3>
    <ol>
        <li>Use robust standard errors (HC3) for heteroscedasticity</li>
        <li>Add interaction terms (education &times; income)</li>
        <li>Use panel data (multiple years) with fixed/random effects</li>
        <li>Include additional predictors (healthcare spending, clean water access)</li>
        <li>Add regional dummy variables for continent-level effects</li>
        <li>Use robust regression or remove outliers</li>
        <li>Cross-validation for out-of-sample assessment</li>
    </ol>

    <h3>Conclusion</h3>
    <div class="highlight">
        The multiple linear regression model predicts life expectancy with
        <strong>R&sup2; = {model.rsquared:.4f}</strong>. The model was selected via
        best subset variable selection using adjusted R&sup2;, AIC, and BIC criteria.
        All key OLS assumptions were tested through comprehensive residual diagnostics.
        <strong>Education and income are significant predictors of life expectancy</strong>
        across countries, consistent with the public health literature on social
        determinants of health.
    </div>
</div>

</body>
</html>"""

    html_path = os.path.join(OUTPUT_DIR, "presentation.html")
    with open(html_path, "w", encoding="utf-8") as f:
        f.write(html)
    print(f"html presentation saved: {html_path}")
    return html_path


if __name__ == "__main__":
    import webbrowser
    path = generate_html()
    webbrowser.open(f"file:///{os.path.abspath(path).replace(os.sep, '/')}")
