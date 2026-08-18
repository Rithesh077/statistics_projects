"""
main pipeline - runs all 7 analysis steps, generates pdf + html reports,
and opens the html presentation in the browser.
single command: python main.py
"""

import os
import webbrowser
from config import OUTPUT_DIR
from step1_data_understanding import load_and_clean
from step2_variable_identification import identify_variables
from step3_variable_selection import variable_selection
from step4_eda import eda
from step5_model_estimation import fit_model
from step6_residual_analysis import residual_analysis
from step7_discussion import discussion
from generate_report import generate_report
from generate_html import generate_html


def main():
    print("undp life expectancy regression analysis - lrm case study\n")

    # run all analysis steps (generates charts)
    df = load_and_clean()
    df_analysis = identify_variables(df)
    best_predictors, subset_results = variable_selection(df_analysis)
    eda(df_analysis, best_predictors)
    model, df_analysis, X_const = fit_model(df_analysis, best_predictors)
    bp_p, shapiro_p = residual_analysis(model, df_analysis, X_const)
    discussion(model, bp_p, shapiro_p, best_predictors)

    # generate pdf report
    print("\ngenerating pdf report...")
    generate_report()

    # generate html presentation and open in browser
    print("generating html presentation...")
    html_path = generate_html()
    webbrowser.open(f"file:///{os.path.abspath(html_path).replace(os.sep, '/')}")

    print(f"\ndone. all outputs in: {os.path.abspath(OUTPUT_DIR)}")
    for f in sorted(os.listdir(OUTPUT_DIR)):
        print(f"  {f}")


if __name__ == "__main__":
    main()
