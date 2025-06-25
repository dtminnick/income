# Income Classification Using Logistic Regression
This project analyzes income prediction using logistic regression on the UCI Adult Income dataset. It explores the impact of class imbalance, feature selection, and decision threshold tuning to build interpretable and balanced classification models.

# Project Structure
The analysis is organized into modular R notebooks:

| Notebook | Description |
|----------|-------------|
| `1_data_preparation.Rmd` | Loads, cleans, and prepares the data for modeling |
| `2_exploratory_analysis.Rmd` | Examines distributions, feature relationships, and imbalance |
| `3_model_training.Rmd` | Trains full and simplified logistic regression models, with and without class weights |
| `4_model_evaluation.Rmd` | Evaluates models using ROC, AUC, Youden’s Index, F1 score, and confusion matrices |
| `5_reporting_summary.Rmd` | Summarizes findings, performance tradeoffs, and business implications |

# Key Techniques Used
- Logistic regression (weighted and unweighted)
- Class imbalance handling via weighting
- ROC and PR curve analysis
- Threshold tuning using Youden’s Index and F1 score
- Sensitivity vs. specificity tradeoff visualization
- Modular, reproducible analysis using RMarkdown

# Dataset
The analysis uses the [UCI Adult Income dataset](https://archive.ics.uci.edu/ml/datasets/adult), which includes demographic and employment attributes to predict whether an individual's income exceeds $50K per year.
