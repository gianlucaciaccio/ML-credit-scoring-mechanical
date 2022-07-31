# ML-credit-scoring-mechanical

This repo contains the source code of my project carried out during my academic experience at the University of Bologna and subsequently updated to improve the results.

## Goal

The primary goal of the project is to predict the probability of default of mechanical sector companies in Italy and to create a rating system to rank companies based on their creditworthiness.

## Data and Tools

Data was collected from the *[AIDA](https://www.bvdinfo.com/it-it/le-nostre-soluzioni/dati/nazionali/aida)* database, published by *[Bureau van Dijk](https://www.bvdinfo.com/en-gb/)*.

The dataset used for the analysis contains the latest available financial statements (from 2012 to 2017) of 4801 Italian companies in the mechanical sector (*ATECO code: 2562*) with legal status both *"In Bonis"* and *"Failed"*. Firms with no missing values were selected.

The analysis was conducted in [R](cran.r-project.org/) using mainly the [`tidyverse`](https://www.tidyverse.org/) and [`caret`](https://topepo.github.io/caret/index.html) libraries
## Overview of the analysis:

#### Data preparation and EDA (features engineering)

- On the basis of related studies and available data, it was decided to calculate the most relevant balance sheet ratios for assessing the profitability, liquidity and solvency of companies.

- Removed outliers with the *[Zscore method](https://en.wikipedia.org/wiki/Standard_score)* (all features with $| Z |> 3$ )

- *[Correlation](https://en.wikipedia.org/wiki/Correlation)* between features and outcome and *[PCA](https://en.wikipedia.org/wiki/Principal_component_analysis)* to explore the relationships in the data.


#### Modeling:

- Splitted the data into training and test samples and using *[10-fold CV](https://en.wikipedia.org/wiki/Cross-validation_(statistics))* repeated 5 times to train the models.

- To deal with the class imbalance problem (only *3%* of companies belonged to the "Failed" status), the *[SMOTE](https://en.wikipedia.org/wiki/Oversampling_and_undersampling_in_data_analysis)* method was applied during the resampling. For tree-based models the class weights on the original sample (without SMOTE) were also evaluated.

- Built and compared supervised learning algorithms for binary classification:

  - *[Logistic Regression](https://en.wikipedia.org/wiki/Logistic_regression)* and *[Linear Discriminant Analysis](https://en.wikipedia.org/wiki/Linear_discriminant_analysis)* using:
  
    - *[Stepwise Feature Selection](https://en.wikipedia.org/wiki/Support-vector_machine)*
    
    - *[PCA](https://en.wikipedia.org/wiki/Principal_component_analysis) Feature Extraction*
    
    - *L1, L2 and Elastic Net [Regularization](https://en.wikipedia.org/wiki/Shrinkage_(statistics))*
    
  - *[Decision Tree](https://en.wikipedia.org/wiki/Principal_component_analysis)* and *[Ensemble Models](https://en.wikipedia.org/wiki/Ensemble_learning)*:
   
    - *[CART](https://en.wikipedia.org/wiki/Decision_tree_learning)*
    
    - *[Random Forest](https://en.wikipedia.org/wiki/Random_forest)*
    
    - *[Stochastic Gradient Boosting](https://en.wikipedia.org/wiki/Gradient_boosting)*
    
  - *[Support Vector Machines with Linear Kernel](https://en.wikipedia.org/wiki/Support-vector_machine)*
 
- During training, the best models were chosen using *[hyperparameter tuning](https://en.wikipedia.org/wiki/Hyperparameter_optimization)* with grid search, maximizing the *[Youden's J statistic](https://en.wikipedia.org/wiki/Youden%27s_J_statistic)*. The other [metrics](https://en.wikipedia.org/wiki/Confusion_matrix) used to evaluate the performance of the models are: *[MCC](https://en.wikipedia.org/wiki/Matthews_correlation_coefficient)*, *Kappa*, *[Log Loss](https://en.wikipedia.org/wiki/Scoring_rule)*, *[Brier Score](https://en.wikipedia.org/wiki/Brier_score)*, *Sensitivity (Recall)*, *Precision*, *Specificity*, *[AUC-PR](http://mlwiki.org/index.php/Precision_and_Recall)* and *[AUC-ROC](https://en.wikipedia.org/wiki/Receiver_operating_characteristic)*.

