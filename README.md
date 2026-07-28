# data science lab

this repo is my scratchpad for data science experiments: stats, time series, econometrics, and a bit of ml.
i prototype here, keep notes, and learn libraries properly.
if something turns into a clean, reusable project, it will move into its own dedicated repo.

## current focus: scipy + statsmodels study tools

building small cli-style tools to learn scipy and statsmodels(then scikit-learn and pytorch).
the running notes live in docs/.

- [docs index](docs/)
- [0.1 regression diagnostics engine (statsmodels)](docs/0.1-regression-diagnostics-engine.md)
- [0.2 distribution fitting engine (scipy)](docs/0.2-distribution-fitting-engine.md)
- [0.3 time series diagnostics and forecasting (statsmodels.tsa)](docs/0.3-time-series-diagnostics-forecasting.md)
- [0.4 resampling and power simulation lab (scipy + numpy)](docs/0.4-resampling-power-simulation.md)
- [0.5 optimization trace harness (scipy optimize)](docs/0.5-optimization-trace-harness.md)
- [0.6 linear algebra stability explorer (scipy linalg / sparse)](docs/0.6-linear-algebra-stability.md)
- [0.7 model evaluation and calibration playground (scikit-learn)](docs/0.7-evaluation-calibration.md)
- [0.8 autograd and numerical stability lab (pytorch)](docs/0.8-autograd-numerical-stability.md)

## what's here

- r scripts and a shiny dashboard
- python scripts and small utilities
- generated outputs go under outputs/ (ignored by default)
- local datasets go under data/ (ignored by default)

## older work (mostly r)

- retail demand forecasting and inventory planning (arima)
- sentiment analysis on product reviews (afinn)
- sales data eda and correlation analysis
- probability distribution simulation (clt, lln, goodness-of-fit)
- estimation demos (sampling and density visualizations)
- hypothesis testing on amazon reviews (assumptions checks, t-test vs wilcoxon)

## running

- r: run scripts in the r folder, or run the shiny app
- python: create a venv, install dependencies, run scripts in the python folder


- R_academics TSA lab 9 dataset: https://fred.stlouisfed.org/series/CPIAUCSL 