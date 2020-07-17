# Human Decision-making and Computational Complexity: The Knapsack Problem

This repository includes all of the code used for the Open Science Framework project: https://doi.org/10.17605/OSF.IO/T2JV7

The code is divided into four sections (folders). See the CODE-DATA-FLOW pdf for an diagram of the code used for each step of the analysis.

## 1. Instance Generation

The first step in the project involved the random generation of instances of the knapsack problem. Instance generation was done for both the optimisation and the decison variant.

## 2. Instance Selection

After a long list of instances was generated for each variant of the knapsack problem a subset was selected to use as inputs for the human experiments.

## 3. Behavioural Data Analysis

After the human experiments were performed, the data was preprocessed using the file `beh_data_preprocessing.R`. The output of this script was used to generate the relevant data analysis presented in the R notebooks `ResultsV7_supp.nb.html` and `IC_expost_measure_Supp.nb.html`.

- `ResultsV7_supp.nb.html` includes the general descriptive statistics, plots and the analysis related to typical-case complexity (TCC).
- `IC_expost_measure_Supp.nb.html` includes analysis related to instance complexity (IC). This metric is referred to as ICexpost in this script.

## 4. Appendix: Number of Solution-witnesses

This folder includes all the code used in the supplementary section: 'Expected number of solution witnesses and the constrainedness of the solution space'.




