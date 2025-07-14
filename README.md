# Corporate Credit Risk PD Prediction

**Course project for Modelling Market Risk**

This repository contains the implementation and analysis of models predicting the probability of default (PD) for corporate clients in the Middle-Market segment. The project focuses on building and comparing logistic regression, probit regression, and linear regression models to assess credit risk.

For the full project description and requirements, please refer to the [project brief PDF](./credit-risk_project.pdf).

---

## Project Overview

The main objectives of this project are:
- Data preprocessing and exploratory data analysis (EDA)
- Feature selection based on Information Value (IV)
- Variable transformation using Weight of Evidence (WOE) binning
- Building and validating predictive models using:
  - Logistic regression
  - Probit regression
  - Linear regression
- Comparing model performance using metrics such as AUC, KS statistic, Accuracy, and F1-score
- Evaluating an expert score-based model and comparing it with statistical models
- Conducting segmentation analysis by financial holding type (GROUP_FLAG) to assess benefits of bespoke models

---

## Technologies and Tools Used

- **R programming language**
- Key libraries:
  - `readxl` — data import from Excel files
  - `ggplot2` — data visualization
  - `caret` — model training, cross-validation, and evaluation
  - `scorecard` — credit risk-specific tools for IV, WOE, model evaluation
  - `dplyr` — data manipulation
  - `pROC` — ROC curve analysis and threshold selection
  - `forcats` — factor handling
  - `pheatmap` — heatmaps for correlation matrices

---

## Additional Resources

- [Full Project Report (PDF)](./full_project_report.pdf)  
- [Project Presentation (PDF)](./project_presentation.pdf)

---

## Authors

- Your Name  
- Additional Author (if any)

---
