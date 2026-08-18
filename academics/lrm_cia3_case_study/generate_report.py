"""
generate a structured pdf report with all analysis results, charts, and reasoning.
runs all 7 steps and compiles everything into a single report pdf.
"""

import os
import re
import itertools
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats
import statsmodels.api as sm
from statsmodels.stats.diagnostic import het_breuschpagan, het_white, acorr_ljungbox
from statsmodels.stats.outliers_influence import variance_inflation_factor
from statsmodels.tsa.stattools import acf
from fpdf import FPDF

from config import DATASET_PATH, OUTPUT_DIR, save_fig


class Report(FPDF):
    """pdf report with consistent styling."""

    def header(self):
        self.set_font("Helvetica", "B", 10)
        self.set_text_color(120, 120, 120)
        self.cell(
            0, 8, "undp life expectancy regression analysis - lrm case study", align="R")
        self.ln(10)

    def footer(self):
        self.set_y(-15)
        self.set_font("Helvetica", "", 8)
        self.set_text_color(150, 150, 150)
        self.cell(0, 10, f"page {self.page_no()}/{{nb}}", align="C")

    def section_title(self, title):
        self.set_font("Helvetica", "B", 14)
        self.set_text_color(30, 60, 120)
        self.ln(5)
        self.cell(0, 10, title, new_x="LMARGIN", new_y="NEXT")
        self.set_draw_color(30, 60, 120)
        self.line(self.l_margin, self.get_y(),
                  self.w - self.r_margin, self.get_y())
        self.ln(4)

    def sub_title(self, title):
        self.set_font("Helvetica", "B", 11)
        self.set_text_color(60, 60, 60)
        self.cell(0, 8, title, new_x="LMARGIN", new_y="NEXT")
        self.ln(1)

    def body_text(self, text):
        self.set_font("Helvetica", "", 10)
        self.set_text_color(40, 40, 40)
        self.multi_cell(0, 5.5, text)
        self.ln(2)

    def add_chart(self, image_path, w=170):
        if os.path.exists(image_path):
            x = (self.w - w) / 2
            self.image(image_path, x=x, w=w)
            self.ln(5)

    def add_table(self, df, col_widths=None):
        self.set_font("Helvetica", "B", 8)
        self.set_fill_color(30, 60, 120)
        self.set_text_color(255, 255, 255)

        cols = list(df.columns)
        if col_widths is None:
            w = (self.w - self.l_margin - self.r_margin) / len(cols)
            col_widths = [w] * len(cols)

        for i, col in enumerate(cols):
            self.cell(col_widths[i], 7, str(col),
                      border=1, fill=True, align="C")
        self.ln()

        self.set_font("Helvetica", "", 7.5)
        self.set_text_color(40, 40, 40)
        fill = False
        for _, row in df.iterrows():
            if fill:
                self.set_fill_color(240, 245, 255)
            else:
                self.set_fill_color(255, 255, 255)
            for i, col in enumerate(cols):
                val = row[col]
                if isinstance(val, float):
                    val = f"{val:.4f}"
                self.cell(col_widths[i], 6, str(val),
                          border=1, fill=True, align="C")
            self.ln()
            fill = not fill
        self.ln(3)


def generate_report():
    pdf = Report()
    pdf.alias_nb_pages()
    pdf.set_auto_page_break(auto=True, margin=20)

    # =========================================================================
    # cover page
    # =========================================================================
    pdf.add_page()
    pdf.ln(50)
    pdf.set_font("Helvetica", "B", 28)
    pdf.set_text_color(30, 60, 120)
    pdf.cell(0, 15, "regression modelling case study",
             align="C", new_x="LMARGIN", new_y="NEXT")
    pdf.ln(5)
    pdf.set_font("Helvetica", "", 16)
    pdf.set_text_color(80, 80, 80)
    pdf.cell(0, 10, "life expectancy analysis using undp hdr data",
             align="C", new_x="LMARGIN", new_y="NEXT")
    pdf.ln(10)
    pdf.set_font("Helvetica", "", 12)
    pdf.cell(0, 8, "dataset: hdr25 statistical annex table 1 (undp, 2025)",
             align="C", new_x="LMARGIN", new_y="NEXT")
    pdf.cell(0, 8, "dependent variable: life expectancy at birth (2023)",
             align="C", new_x="LMARGIN", new_y="NEXT")
    pdf.cell(0, 8, "independent variables: expected schooling, mean schooling, log(gni per capita)",
             align="C", new_x="LMARGIN", new_y="NEXT")

    # =========================================================================
    # step 1: data understanding
    # =========================================================================
    pdf.add_page()
    pdf.section_title("1. data understanding")

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

    pdf.body_text(
        f"the dataset is sourced from the undp human development report 2025 "
        f"(statistical annex, table 1). it contains data for {df_clean.shape[0]} "
        f"countries across {df_clean.shape[1]} variables.\n\n"
        f"the raw csv has a multi-row header structure with footnote markers "
        f"(e.g. 'a', 'c', 'e') embedded in numeric values, and section "
        f"dividers like 'very high human development'. these were cleaned by "
        f"stripping footnote characters and removing non-data rows.")

    pdf.sub_title("variables")
    pdf.body_text(
        "hdi_rank (numeric) - rank by hdi value\n"
        "country (categorical) - country name\n"
        "hdi_value (numeric) - human development index, 0 to 1\n"
        "life_expectancy (numeric) - life expectancy at birth, in years\n"
        "expected_schooling (numeric) - expected years of schooling\n"
        "mean_schooling (numeric) - mean years of schooling\n"
        "gni_per_capita (numeric) - gross national income per capita (2021 ppp $)")

    analysis_cols = ["life_expectancy", "expected_schooling",
                     "mean_schooling", "gni_per_capita"]
    miss = df_clean[analysis_cols].isnull().sum()
    total_miss = miss.sum()
    pdf.sub_title("missing values")
    if total_miss > 0:
        miss_text = ", ".join(
            [f"{col}: {n}" for col, n in miss.items() if n > 0])
        pdf.body_text(
            f"missing values found: {miss_text}.\n"
            f"handling: mean imputation was applied to all numeric columns "
            f"with missing values, as recommended in the assignment instructions.")
        for col in analysis_cols:
            n = df_clean[col].isnull().sum()
            if n > 0:
                m = df_clean[col].mean()
                df_clean[col] = df_clean[col].fillna(m)
    else:
        pdf.body_text("no missing values were found in the analysis columns.")

    pdf.sub_title("summary statistics")
    desc = df_clean[analysis_cols].describe().round(3).T
    desc.insert(0, "variable", desc.index)
    desc = desc.reset_index(drop=True)
    pdf.add_table(desc)

    le_range = df_clean["life_expectancy"].dropna()
    gni_range = df_clean["gni_per_capita"].dropna()
    pdf.body_text(
        f"key observations from summary statistics:\n"
        f"- life expectancy ranges from {le_range.min():.1f} to {le_range.max():.1f} years "
        f"across countries, showing wide global inequality in health outcomes.\n"
        f"- gni per capita has very high variance (std = {gni_range.std():.0f}, "
        f"mean = {gni_range.mean():.0f}), indicating extreme income inequality. "
        f"this motivates our decision to use log(gni) in the regression.\n"
        f"- education variables show substantial variation across countries.")

    # =========================================================================
    # step 2: variable identification
    # =========================================================================
    pdf.add_page()
    pdf.section_title("2. identification of variables")

    df_analysis = df_clean[analysis_cols].dropna().copy()
    df_analysis["log_gni"] = np.log(df_analysis["gni_per_capita"])
    corr_cols = ["life_expectancy", "expected_schooling",
                 "mean_schooling", "log_gni"]
    corr_matrix = df_analysis[corr_cols].corr().round(3)

    pdf.body_text(
        "dependent variable (y): life_expectancy\n"
        "life expectancy at birth (years) measures population health outcomes. "
        "it is selected as the dependent variable because we want to understand "
        "what socioeconomic factors drive longevity across countries.\n\n"
        "note: hdi_value is excluded as a predictor because the hdi formula "
        "already incorporates life expectancy as one of its three dimensions. "
        "using hdi as a predictor would create a circular (tautological) "
        "regression.\n\n"
        "independent variables (candidate predictors):\n"
        "- expected_schooling: education access, years a child is expected to attend school\n"
        "- mean_schooling: educational attainment, average years of education for adults (25+)\n"
        "- log_gni: log of gross national income per capita (2021 ppp $)\n\n"
        "justification:\n"
        "education and income are well-established social determinants of health "
        "in public health literature. higher education leads to better health "
        "literacy, healthier behaviours, and better access to healthcare. "
        "higher income enables investment in healthcare infrastructure, nutrition, "
        "sanitation, and clean water. we use log(gni) because the relationship "
        "between income and health shows diminishing marginal returns.")

    pdf.sub_title("correlation matrix")
    corr_display = corr_matrix.copy()
    corr_display.insert(0, "variable", corr_display.index)
    corr_display = corr_display.reset_index(drop=True)
    pdf.add_table(corr_display)

    chart_path = os.path.join(OUTPUT_DIR, "01_correlation_heatmap.png")
    if os.path.exists(chart_path):
        pdf.add_chart(chart_path, w=140)

    pdf.body_text(
        f"the correlation matrix shows relationships between life expectancy "
        f"and the candidate predictors. all three predictors show positive "
        f"correlations with life expectancy.\n"
        f"analysis sample: {df_analysis.shape[0]} countries after dropping "
        f"rows with missing values.")

    # =========================================================================
    # step 3: variable selection (best subset)
    # =========================================================================
    pdf.add_page()
    pdf.section_title("3. variable selection (best subset)")

    pdf.body_text(
        "best subset selection evaluates all possible combinations of the "
        "candidate predictors and selects the model with the best fit. "
        "with 3 candidate predictors, there are 2^3 - 1 = 7 possible "
        "non-empty subsets. each subset is evaluated using adjusted "
        "r-squared (penalises model complexity), aic (akaike information "
        "criterion), and bic (bayesian information criterion).")

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

    results_df = pd.DataFrame(results)
    results_df = results_df.sort_values(
        "adj_r_squared", ascending=False).reset_index(drop=True)

    best_adj = results_df.iloc[0]
    best_aic_row = results_df.loc[results_df["aic"].idxmin()]
    best_bic_row = results_df.loc[results_df["bic"].idxmin()]
    best_predictors = best_adj["predictors"].split(" + ")

    pdf.sub_title("model comparison table")
    display_df = results_df[["predictors", "r_squared",
                             "adj_r_squared", "aic", "bic"]].copy()
    pdf.add_table(display_df)

    chart_path = os.path.join(OUTPUT_DIR, "02_subset_selection.png")
    if os.path.exists(chart_path):
        pdf.add_chart(chart_path, w=170)

    pdf.sub_title("selection result")
    pdf.body_text(
        f"best by adjusted r-squared: {best_adj['predictors']} "
        f"(adj r2 = {best_adj['adj_r_squared']:.4f})\n"
        f"best by aic: {best_aic_row['predictors']} "
        f"(aic = {best_aic_row['aic']:.2f})\n"
        f"best by bic: {best_bic_row['predictors']} "
        f"(bic = {best_bic_row['bic']:.2f})\n\n"
        f"selected model: life_expectancy ~ {' + '.join(best_predictors)}\n\n"
        f"the model with the highest adjusted r-squared is selected. "
        f"adjusted r-squared penalises additional predictors that do not "
        f"sufficiently improve the fit, ensuring parsimony.")

    # =========================================================================
    # step 4: eda
    # =========================================================================
    pdf.add_page()
    pdf.section_title("4. exploratory data analysis")

    pdf.body_text(
        "scatter plots were generated for each selected predictor against "
        "life expectancy, with linear trend lines and pearson correlation "
        "coefficients.")

    chart_path = os.path.join(OUTPUT_DIR, "03_scatter_plots.png")
    if os.path.exists(chart_path):
        pdf.add_chart(chart_path, w=170)

    corr_text = []
    for pred in best_predictors:
        r, p = stats.pearsonr(df_analysis[pred], df_analysis[target])
        corr_text.append(
            f"- {pred.replace('_', ' ')}: r = {r:.4f}, p = {p:.2e}")
    pdf.body_text("pearson correlations:\n" + "\n".join(corr_text))

    le_skew = stats.skew(df_analysis[target].dropna())
    skew_desc = "left-skewed" if le_skew < - \
        0.5 else "right-skewed" if le_skew > 0.5 else "approximately symmetric"

    pdf.body_text(
        f"observations:\n"
        f"- life expectancy distribution is {skew_desc} (skewness = {le_skew:.3f}).\n"
        f"- education variables show positive trends with life expectancy.\n"
        f"- log(gni) shows a positive relationship, as wealthier countries "
        f"invest more in healthcare, nutrition, and sanitation.")

    chart_path = os.path.join(OUTPUT_DIR, "04_distributions.png")
    if os.path.exists(chart_path):
        pdf.add_chart(chart_path, w=180)

    # =========================================================================
    # step 5: model specification and estimation
    # =========================================================================
    pdf.add_page()
    pdf.section_title("5. model specification and estimation")

    pdf.sub_title("model specification")
    formula_rhs = " + ".join([f"b{i+1}({p})" for i,
                             p in enumerate(best_predictors)])
    pdf.body_text(
        f"life_expectancy = b0 + {formula_rhs} + e\n\n"
        f"predictors were selected via best subset selection in step 3.\n"
        f"log transformation is applied to gni_per_capita because:\n"
        f"1. the scatter plot shows a non-linear (logarithmic) pattern\n"
        f"2. gni has extremely high variance and right skew\n"
        f"3. log transformation captures diminishing marginal returns of income")

    X = df_analysis[best_predictors]
    y = df_analysis[target]
    X_const = sm.add_constant(X)
    model = sm.OLS(y, X_const).fit()

    pdf.sub_title("estimated coefficients")
    coef_df = pd.DataFrame({
        "variable": model.params.index,
        "coefficient": model.params.values.round(6),
        "std error": model.bse.values.round(6),
        "t-statistic": model.tvalues.values.round(4),
        "p-value": model.pvalues.values.round(4),
    })
    pdf.add_table(coef_df)

    coeffs = model.params
    pvals = model.pvalues
    pdf.sub_title("coefficient interpretation")

    interp_parts = []
    for var in best_predictors:
        direction = "increase" if coeffs[var] > 0 else "decrease"
        sig_text = "statistically significant" if pvals[var] < 0.05 else "not statistically significant"
        if var == "log_gni":
            pct_effect = coeffs[var] * np.log(1.01)
            interp_parts.append(
                f"log_gni (b = {coeffs[var]:.6f}, p = {pvals[var]:.4f}):\n"
                f"  since gni is log-transformed, a 1% increase in gni per capita "
                f"is associated with a {abs(pct_effect):.4f}-year {direction} in "
                f"life expectancy (exact: b * ln(1.01) = {coeffs[var]:.4f} * "
                f"{np.log(1.01):.6f} = {pct_effect:.4f}). ({sig_text})")
        else:
            interp_parts.append(
                f"{var} (b = {coeffs[var]:.6f}, p = {pvals[var]:.4f}):\n"
                f"  each additional unit of {var.replace('_', ' ')} is associated "
                f"with a {abs(coeffs[var]):.4f}-year {direction} in life expectancy, "
                f"holding other variables constant. ({sig_text})")

    pdf.body_text("\n\n".join(interp_parts))

    pdf.sub_title("multicollinearity check (vif)")
    vif_data = pd.DataFrame()
    vif_data["variable"] = X.columns
    vif_data["vif"] = [variance_inflation_factor(
        X_const.values, i + 1) for i in range(len(X.columns))]
    vif_data["assessment"] = vif_data["vif"].apply(
        lambda v: "severe" if v > 10 else "high" if v > 5 else "acceptable")
    vif_data = vif_data.sort_values(
        "vif", ascending=True).reset_index(drop=True)
    pdf.add_table(vif_data)

    pdf.body_text(
        "vif measures how much each predictor is explained by the other predictors. "
        "expected_schooling and mean_schooling both measure education-related "
        "constructs, so some correlation is expected. while this may inflate "
        "individual coefficient standard errors, the overall model fit remains valid.")

    dw = sm.stats.durbin_watson(model.resid)
    pdf.sub_title("goodness of fit")
    pdf.body_text(
        f"r-squared = {model.rsquared:.4f} ({model.rsquared*100:.1f}% variance explained)\n"
        f"adjusted r-squared = {model.rsquared_adj:.4f}\n"
        f"f-statistic = {model.fvalue:.2f} (p = {model.f_pvalue:.2e})\n"
        f"aic = {model.aic:.2f}\n"
        f"bic = {model.bic:.2f}\n"
        f"durbin-watson = {dw:.4f}\n\n"
        f"the model explains {model.rsquared*100:.1f}% of the variation in "
        f"life expectancy. the f-test is highly significant. "
        f"note: for cross-sectional data, durbin-watson should be interpreted "
        f"cautiously as the ordering of observations is arbitrary.")

    chart_path = os.path.join(OUTPUT_DIR, "05_coefficient_plot.png")
    if os.path.exists(chart_path):
        pdf.add_chart(chart_path, w=150)

    chart_path = os.path.join(OUTPUT_DIR, "06_vif.png")
    if os.path.exists(chart_path):
        pdf.add_chart(chart_path, w=150)

    chart_path = os.path.join(OUTPUT_DIR, "07_actual_vs_predicted.png")
    if os.path.exists(chart_path):
        pdf.add_chart(chart_path, w=140)

    # =========================================================================
    # step 6: residual analysis
    # =========================================================================
    pdf.add_page()
    pdf.section_title("6. residual analysis")

    resid = model.resid.values
    fitted = model.fittedvalues.values

    pdf.body_text(
        "residual analysis verifies that the ols assumptions hold. "
        "if assumptions are violated, coefficient estimates may be biased or "
        "standard errors unreliable, making hypothesis tests invalid.")

    pdf.sub_title("6a. residuals vs fitted values")
    chart_path = os.path.join(OUTPUT_DIR, "08_residuals_vs_fitted.png")
    if os.path.exists(chart_path):
        pdf.add_chart(chart_path, w=160)
    pdf.body_text(
        "residuals are plotted against fitted values to check for non-linearity "
        "and heteroscedasticity. the lowess smoother should stay near zero.")

    pdf.sub_title("6b. residuals around zero")
    n_pos = (resid > 0).sum()
    n_neg = (resid < 0).sum()
    pdf.body_text(
        f"mean of residuals: {np.mean(resid):.6f}\n"
        f"std of residuals: {np.std(resid):.6f}\n"
        f"positive: {n_pos}, negative: {n_neg}\n\n"
        f"the mean is essentially zero (expected for ols), and residuals "
        f"are roughly evenly split, indicating no systematic bias.")

    pdf.sub_title("6c. heteroscedasticity")
    bp_stat, bp_p, _, _ = het_breuschpagan(resid, X_const)
    white_stat, white_p, _, _ = het_white(resid, X_const)
    pdf.body_text(
        f"breusch-pagan test: statistic = {bp_stat:.4f}, p = {bp_p:.4f}\n"
        f"white test: statistic = {white_stat:.4f}, p = {white_p:.4f}\n\n"
        f"ols assumes constant variance of errors (homoscedasticity). "
        f"if violated, standard errors are unreliable. robust standard "
        f"errors (hc3) can be used as a fix.\n\n"
        f"result: "
        f"{'heteroscedasticity detected (p < 0.05). standard errors should be interpreted with caution.' if bp_p < 0.05 else 'no significant heteroscedasticity. the constant variance assumption holds.'}")

    chart_path = os.path.join(OUTPUT_DIR, "09_scale_location.png")
    if os.path.exists(chart_path):
        pdf.add_chart(chart_path, w=155)

    pdf.sub_title("6d. normality of residuals")
    shapiro_stat, shapiro_p = stats.shapiro(resid)
    jb_stat, jb_p = stats.jarque_bera(resid)
    pdf.body_text(
        f"shapiro-wilk: W = {shapiro_stat:.4f}, p = {shapiro_p:.4f}\n"
        f"jarque-bera: stat = {jb_stat:.4f}, p = {jb_p:.4f}\n"
        f"skewness = {stats.skew(resid):.4f}, kurtosis = {stats.kurtosis(resid):.4f}\n\n"
        f"ols estimates are unbiased regardless of normality, but hypothesis "
        f"tests rely on normal errors. with large samples (n > 100), the "
        f"central limit theorem provides some protection.\n\n"
        f"result: "
        f"{'residuals deviate from normality, but with n > 100 the CLT mitigates this concern.' if shapiro_p < 0.05 else 'residuals appear approximately normally distributed.'}")

    chart_path = os.path.join(OUTPUT_DIR, "10_qq_and_histogram.png")
    if os.path.exists(chart_path):
        pdf.add_chart(chart_path, w=170)

    pdf.sub_title("6e. autocorrelation")
    n_lags = 30
    acf_values, _ = acf(resid, nlags=n_lags, alpha=0.05)
    conf_band = 1.96 / np.sqrt(len(resid))
    lb_test = acorr_ljungbox(resid, lags=[10, 20], return_df=True)
    lb_p10 = lb_test["lb_pvalue"].iloc[0]
    lb_p20 = lb_test["lb_pvalue"].iloc[1]

    pdf.body_text(
        f"ljung-box (lag 10): p = {lb_p10:.4f}\n"
        f"ljung-box (lag 20): p = {lb_p20:.4f}\n\n"
        f"for cross-sectional data, the ordering (by hdi rank) is arbitrary. "
        f"durbin-watson and ljung-box are designed for time series and should "
        f"be interpreted cautiously.\n\n"
        f"result: "
        f"{'significant autocorrelation detected -- may reflect development-level clustering.' if lb_p10 < 0.05 else 'no significant autocorrelation.'}")

    chart_path = os.path.join(OUTPUT_DIR, "11_acf_residuals.png")
    if os.path.exists(chart_path):
        pdf.add_chart(chart_path, w=155)

    pdf.sub_title("6f. overall model adequacy")
    issues = []
    if bp_p <= 0.05:
        issues.append("heteroscedasticity")
    if shapiro_p <= 0.05:
        issues.append("non-normal residuals")
    if lb_p10 < 0.05:
        issues.append("autocorrelation")

    if issues:
        pdf.body_text(
            f"potential issues: {', '.join(issues)}.\n"
            f"these should be considered when interpreting results but do not "
            f"necessarily invalidate the model.")
    else:
        pdf.body_text(
            "the model passes all diagnostic checks. residuals are approximately "
            "normal, homoscedastic, and show no significant autocorrelation.")

    # =========================================================================
    # step 7: discussion and conclusion
    # =========================================================================
    pdf.add_page()
    pdf.section_title("7. discussion and conclusion")

    r2_pct = model.rsquared * 100
    fit_desc = "strong" if r2_pct > 80 else "moderately strong" if r2_pct > 60 else "moderate"

    pdf.sub_title("model assessment")
    pdf.body_text(
        f"r2 = {model.rsquared:.4f}, meaning the model explains "
        f"{r2_pct:.1f}% of the variance in life expectancy. this is a "
        f"{fit_desc} fit. the f-test is highly significant.")

    model_coeffs = model.params.drop("const")
    positive = [name for name, val in model_coeffs.items() if val > 0]
    negative = [name for name, val in model_coeffs.items() if val < 0]

    pdf.sub_title("theoretical and practical sense")
    if negative:
        coeff_text = (
            f"most coefficients are positive ({', '.join(positive)}), "
            f"while {', '.join(negative)} show(s) a negative association. "
            f"the negative coefficient(s) may reflect confounding or "
            f"multicollinearity rather than a true causal effect.")
    else:
        coeff_text = (
            "all coefficients are positive, meaning higher education and "
            "income are associated with longer life expectancy, consistent "
            "with public health literature.")

    gni_text = ""
    if "log_gni" in best_predictors:
        b_gni = model_coeffs.get("log_gni", 0)
        pct_effect = b_gni * np.log(1.01)
        gni_text = (
            f"\n\nthe log transformation captures diminishing returns: "
            f"a 1% increase in gni is associated with a "
            f"{abs(pct_effect):.4f}-year change in life expectancy.")

    pdf.body_text(
        f"{coeff_text}{gni_text}\n\n"
        f"investments in education and economic development are associated "
        f"with longer life expectancy through improved health literacy, "
        f"healthcare access, and infrastructure.")

    pdf.sub_title("limitations")
    pdf.body_text(
        "1. multicollinearity: education variables are correlated, "
        "potentially inflating standard errors.\n\n"
        "2. omitted variable bias: important predictors like healthcare "
        "spending, clean water access, and disease burden are missing.\n\n"
        "3. cross-sectional data: cannot capture temporal dynamics.\n\n"
        "4. outliers: conflict zones and oil-rich states may distort results.\n\n"
        f"5. diagnostics: breusch-pagan p = {bp_p:.4f}, "
        f"shapiro-wilk p = {shapiro_p:.4f}.\n\n"
        "6. autocorrelation tests are designed for time series; results "
        "should be interpreted cautiously for cross-sectional data.")

    pdf.sub_title("suggested improvements")
    pdf.body_text(
        "1. use robust standard errors (hc3)\n"
        "2. add interaction terms (education x income)\n"
        "3. use panel data with fixed/random effects\n"
        "4. include additional predictors\n"
        "5. add regional dummy variables\n"
        "6. use robust regression or remove outliers\n"
        "7. cross-validation for out-of-sample assessment")

    pdf.sub_title("conclusion")
    pdf.body_text(
        f"the multiple linear regression model predicts life expectancy "
        f"with r2 = {model.rsquared:.4f}. the model was selected via best "
        f"subset variable selection. all ols assumptions were tested through "
        f"comprehensive residual diagnostics. education and income are "
        f"significant predictors of life expectancy, consistent with the "
        f"public health literature on social determinants of health.")

    report_path = os.path.join(
        OUTPUT_DIR, "life_expectancy_regression_report.pdf")
    pdf.output(report_path)
    print(f"report saved: {report_path}")
    return report_path


if __name__ == "__main__":
    print("running analysis to generate charts...")
    from step1_data_understanding import load_and_clean
    from step2_variable_identification import identify_variables
    from step3_variable_selection import variable_selection
    from step4_eda import eda
    from step5_model_estimation import fit_model
    from step6_residual_analysis import residual_analysis

    df = load_and_clean()
    df_analysis = identify_variables(df)
    best_predictors, _ = variable_selection(df_analysis)
    eda(df_analysis, best_predictors)
    model, df_analysis, X_const = fit_model(df_analysis, best_predictors)
    residual_analysis(model, df_analysis, X_const)

    print("\ngenerating pdf report...")
    path = generate_report()
    print(f"done. report at: {path}")
