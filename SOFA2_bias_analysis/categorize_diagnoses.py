"""
Categorize MIMIC ICU primary diagnoses (ICD-9 + ICD-10) into clinical chapters.

Approach
--------
Each ICD code is mapped to its standard clinical body-system chapter. Because
ICD-9 and ICD-10 share the same conceptual chapter structure, this makes a
single category span "similar diagnoses" across both coding systems
(e.g. ICD-9 410.x and ICD-10 I21.x both -> "Circulatory System").

The top 10 chapters by frequency are kept; everything else becomes "Other".
A new column, `diagnosis_category`, is added to the output.

Usage
-----
    python categorize_diagnoses.py input.csv output.csv

Expects columns: hadm_id, icd_code, icd_version, long_title
"""

import sys
import pandas as pd


def cat_icd9(code: str) -> str:
    """Map an ICD-9-CM code to its clinical chapter."""
    if code.startswith('V'):
        return 'Factors Influencing Health / Health Services (V/Z codes)'
    if code.startswith('E'):
        return 'Injury, Poisoning & External Causes'
    try:
        n = int(code[:3])          # chapter is determined by the first 3 digits
    except ValueError:
        return 'Unmapped'

    if   1   <= n <= 139: return 'Infectious & Parasitic Diseases'
    elif 140 <= n <= 239: return 'Neoplasms'
    elif 240 <= n <= 279: return 'Endocrine, Nutritional & Metabolic'
    elif 280 <= n <= 289: return 'Blood & Blood-forming Organs'
    elif 290 <= n <= 319: return 'Mental & Behavioral Disorders'
    elif 320 <= n <= 389: return 'Nervous System & Sense Organs'
    elif 390 <= n <= 459: return 'Circulatory System'
    elif 460 <= n <= 519: return 'Respiratory System'
    elif 520 <= n <= 579: return 'Digestive System'
    elif 580 <= n <= 629: return 'Genitourinary System'
    elif 630 <= n <= 679: return 'Pregnancy, Childbirth & Puerperium'
    elif 680 <= n <= 709: return 'Skin & Subcutaneous Tissue'
    elif 710 <= n <= 739: return 'Musculoskeletal & Connective Tissue'
    elif 740 <= n <= 759: return 'Congenital Anomalies'
    elif 760 <= n <= 779: return 'Perinatal Conditions'
    elif 780 <= n <= 799: return 'Symptoms, Signs & Ill-defined Conditions'
    elif 800 <= n <= 999: return 'Injury, Poisoning & External Causes'
    return 'Unmapped'


def cat_icd10(code: str) -> str:
    """Map an ICD-10-CM code to its clinical chapter."""
    letter = code[0]
    try:
        num = int(code[1:3])       # 2-digit block after the letter
    except ValueError:
        num = 0

    if letter in ('A', 'B'):       return 'Infectious & Parasitic Diseases'
    if letter == 'C':              return 'Neoplasms'
    if letter == 'D':                                       # D-block splits
        return 'Neoplasms' if num <= 49 else 'Blood & Blood-forming Organs'
    if letter == 'E':              return 'Endocrine, Nutritional & Metabolic'
    if letter == 'F':              return 'Mental & Behavioral Disorders'
    if letter == 'G':              return 'Nervous System & Sense Organs'
    if letter == 'H':              return 'Nervous System & Sense Organs'  # eye/ear
    if letter == 'I':              return 'Circulatory System'
    if letter == 'J':              return 'Respiratory System'
    if letter == 'K':              return 'Digestive System'
    if letter == 'L':              return 'Skin & Subcutaneous Tissue'
    if letter == 'M':              return 'Musculoskeletal & Connective Tissue'
    if letter == 'N':              return 'Genitourinary System'
    if letter == 'O':              return 'Pregnancy, Childbirth & Puerperium'
    if letter == 'P':              return 'Perinatal Conditions'
    if letter == 'Q':              return 'Congenital Anomalies'
    if letter == 'R':              return 'Symptoms, Signs & Ill-defined Conditions'
    if letter in ('S', 'T'):       return 'Injury, Poisoning & External Causes'
    if letter in ('V', 'W', 'X', 'Y'): return 'Injury, Poisoning & External Causes'
    if letter == 'Z':              return 'Factors Influencing Health / Health Services (V/Z codes)'
    if letter == 'U':              return 'Infectious & Parasitic Diseases'  # special/emergency codes
    return 'Unmapped'


def categorize(row) -> str:
    """Dispatch to the correct mapper based on ICD version."""
    code = str(row['icd_code']).strip().upper()
    return cat_icd9(code) if row['icd_version'] == 9 else cat_icd10(code)


def main(in_path: str, out_path: str, n_top: int = 10) -> None:
    # dtype=str is essential: prevents pandas from mangling codes like "V550"
    df = pd.read_csv(in_path, dtype={'icd_code': str})
    df['icd_code'] = df['icd_code'].str.strip().str.upper()

    # 1) map every code to its full clinical chapter
    chapter = df.apply(categorize, axis=1)

    unmapped = (chapter == 'Unmapped').sum()
    if unmapped:
        print(f"WARNING: {unmapped} rows could not be mapped.")

    # 2) keep the top N chapters, collapse the rest into "Other"
    top = chapter.value_counts().head(n_top).index
    df['diagnosis_category'] = chapter.where(chapter.isin(top), 'Other')

    # 3) report and save
    total = len(df)
    print(f"\nTop {n_top} categories + Other  (n = {total:,}):\n")
    vc = df['diagnosis_category'].value_counts()
    for cat, cnt in vc.items():
        print(f"  {cat:<42} {cnt:>7,}  ({cnt / total * 100:5.1f}%)")

    df.to_csv(out_path, index=False)
    print(f"\nSaved -> {out_path}  {df.shape}")


if __name__ == '__main__':
    in_path  = sys.argv[1] if len(sys.argv) > 1 else 'Primary_diag_MIMIC_ICUcsv.csv'
    out_path = sys.argv[2] if len(sys.argv) > 2 else 'Primary_diag_MIMIC_ICU_categorized.csv'
    main(in_path, out_path)
