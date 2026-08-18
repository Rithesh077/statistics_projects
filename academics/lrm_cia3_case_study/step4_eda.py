"""
step 4: exploratory data analysis

- scatter plots of each predictor vs life expectancy with trend lines
- pearson correlation for each pair
- distribution plots with kde overlay
- observations about patterns and trends
"""

import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats
from config import save_fig


def eda(df_analysis, best_predictors):
    print("\nstep 4: exploratory data analysis\n")

    target = "life_expectancy"
    palette = sns.color_palette("muted")

    # scatter plots with trend lines
    n_pred = len(best_predictors)
    ncols = min(n_pred, 2)
    nrows = (n_pred + ncols - 1) // ncols
    print("scatter plots:")
    fig, axes = plt.subplots(nrows, ncols, figsize=(6.5 * ncols, 5 * nrows))
    if n_pred == 1:
        axes = np.array([axes])
    axes = np.array(axes).flatten()

    for i, pred in enumerate(best_predictors):
        ax = axes[i]
        ax.scatter(df_analysis[pred], df_analysis[target], s=28, alpha=0.55,
                   color=palette[i], edgecolors="white", linewidths=0.3)
        z = np.polyfit(df_analysis[pred], df_analysis[target], 1)
        xs = np.linspace(df_analysis[pred].min(), df_analysis[pred].max(), 100)
        ax.plot(xs, np.poly1d(z)(xs), "r--", lw=1.5)
        r, p = stats.pearsonr(df_analysis[pred], df_analysis[target])
        ax.set_xlabel(pred.replace("_", " "))
        ax.set_ylabel("life expectancy (years)")
        ax.set_title(
            f"{pred.replace('_', ' ')} vs life expectancy  (r={r:.3f}, p={p:.1e})")
        ax.grid(alpha=0.3)
        print(f"  {pred:25s} r = {r:.4f}  p = {p:.2e}")

    # hide unused axes
    for j in range(n_pred, len(axes)):
        axes[j].set_visible(False)

    fig.suptitle("scatter plots: predictors vs life expectancy",
                 fontsize=13, weight="bold")
    plt.tight_layout()
    save_fig(fig, "03_scatter_plots")

    # distribution plots
    print("\ndistribution plots:")
    all_vars = [target] + best_predictors
    fig, axes = plt.subplots(
        1, len(all_vars), figsize=(4 * len(all_vars), 4.5))
    if len(all_vars) == 1:
        axes = [axes]
    for i, col in enumerate(all_vars):
        ax = axes[i]
        vals = df_analysis[col].dropna()
        ax.hist(vals, bins=20, density=True, alpha=0.6,
                color=palette[i % len(palette)], edgecolor="white")
        vals.plot.kde(ax=ax, color="red", lw=1.5)
        ax.set_title(col.replace("_", " "), fontsize=9)
        ax.set_xlabel("")
        ax.grid(alpha=0.3)
    fig.suptitle("variable distributions", fontsize=12, weight="bold")
    plt.tight_layout()
    save_fig(fig, "04_distributions")

    # data-driven observations
    le_skew = stats.skew(df_analysis[target].dropna())
    skew_desc = "left-skewed" if le_skew < - \
        0.5 else "right-skewed" if le_skew > 0.5 else "approximately symmetric"

    print(f"""
observations:
  the scatter plots show relationships between each selected predictor
  and life expectancy across countries.
  life expectancy distribution is {skew_desc} (skewness = {le_skew:.3f}).
  education variables (expected and mean schooling) are expected to show
  positive trends -- countries with higher education levels tend to have
  longer life expectancy due to better health literacy and access to care.
  log(gni) should also show a positive relationship, as wealthier
  countries invest more in healthcare, nutrition, and sanitation.
  the log transformation linearises the income-health relationship.
""")


if __name__ == "__main__":
    from step1_data_understanding import load_and_clean
    from step2_variable_identification import identify_variables
    from step3_variable_selection import variable_selection
    df = load_and_clean()
    df_analysis = identify_variables(df)
    best_predictors, _ = variable_selection(df_analysis)
    eda(df_analysis, best_predictors)
