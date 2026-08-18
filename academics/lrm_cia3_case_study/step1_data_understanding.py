"""
step 1: data understanding

- load the raw undp hdr25 csv
- parse the multi-row header and clean footnotes
- remove section headers (e.g. 'very high human development')
- handle missing values with mean imputation
- report shape, types, missing values, summary statistics
"""

import re
import pandas as pd
from config import DATASET_PATH


def load_and_clean():
    print("\nstep 1: data understanding\n")

    # try utf-8 first, fall back to latin-1 for special characters
    for enc in ["utf-8", "latin-1", "cp1252"]:
        try:
            raw = pd.read_csv(DATASET_PATH, header=None, encoding=enc)
            break
        except UnicodeDecodeError:
            continue

    print(f"raw csv shape: {raw.shape[0]} rows x {raw.shape[1]} columns")

    # rows 0-4 are header/metadata. data starts at row 5.
    # keep only meaningful columns (drop spacer/note columns)
    df_clean = raw.iloc[5:].copy()

    cols_to_keep = [0, 1, 2, 4, 6, 8, 10, 12, 14]
    cols_to_keep = [c for c in cols_to_keep if c < df_clean.shape[1]]
    df_clean = df_clean[df_clean.columns[cols_to_keep]]

    df_clean.columns = [
        "hdi_rank", "country", "hdi_value",
        "life_expectancy", "expected_schooling", "mean_schooling",
        "gni_per_capita", "gni_rank_minus_hdi_rank", "hdi_rank_2022"
    ][:len(cols_to_keep)]

    # clean footnotes from numeric columns (e.g. '85.5 g' -> '85.5')
    # this regex approach is from the dir2 cleaning script
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

    # remove section headers and rows without valid country/hdi
    df_clean = df_clean.dropna(subset=["country", "hdi_value"])
    df_clean["country"] = df_clean["country"].astype(str).str.strip()
    df_clean = df_clean.reset_index(drop=True)

    # report dataset description
    print(f"cleaned dataset: {df_clean.shape[0]} observations x {df_clean.shape[1]} variables\n")
    print("variable types:")
    for col in df_clean.columns:
        vtype = "categorical" if df_clean[col].dtype == "object" else "numeric"
        print(f"  {col}: {vtype}")

    # missing values
    print("\nmissing values:")
    miss = df_clean.isnull().sum()
    print(miss.to_string())

    if miss.sum() > 0:
        # mean imputation for numeric columns (as per assignment instructions)
        print("\nhandling missing values with mean imputation:")
        for col in df_clean.select_dtypes("number").columns:
            n = df_clean[col].isnull().sum()
            if n > 0:
                m = df_clean[col].mean()
                df_clean[col] = df_clean[col].fillna(m)
                print(f"  filled {n} missing in {col} with mean = {m:.4f}")
    else:
        print("  no missing values found")

    # summary statistics
    print("\nsummary statistics:")
    print(df_clean.describe().round(3).to_string())

    return df_clean


if __name__ == "__main__":
    df = load_and_clean()
    print(f"\nfirst 5 rows:\n{df.head().to_string()}")
