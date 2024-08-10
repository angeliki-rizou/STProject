# Web-Based Data Mining and Analysis Application

# Overview
This project involves the development of a web-based application for data mining and analysis, using either Streamlit or RShiny. The application is designed to load tabular data and support various functionalities such as data visualization, feature selection, and machine learning classification. The project was completed as part of the "Software Technology" course requirements.

# Features
**1. Data Loading**
The application supports loading tabular data in formats such as CSV, Excel, TSV, and more.
The data table is structured to have rows representing samples and columns representing features, with the last column containing the label for each sample.

**2. Data Visualization**
A dedicated tab allows users to perform 2D and 3D visualizations using PCA and UMAP dimensionality reduction algorithms.
Includes at least two exploratory data analysis (EDA) charts for a comprehensive data overview.

**3. Machine Learning Tabs**
Feature Selection Tab: Users can execute a feature selection algorithm of their choice, controlling the number of features remaining in the dataset. The resulting reduced dataset is displayed for further analysis.
Classification Tab: Users can run two supervised machine learning classification algorithms on both the original and reduced datasets. The performance of the algorithms is compared using metrics such as accuracy, F1-score, and ROC-AUC, with one parameter (e.g., 'k' in k-nearest neighbors) being user-configurable.

**4. Results and Comparison**
The application provides a detailed presentation of algorithm results, including performance metrics. It also highlights the best-performing algorithms for the given dataset.

**5. Info Tab**
An information section is included to provide details about the application, its functionality, the development team, and the specific tasks completed by each team member.
