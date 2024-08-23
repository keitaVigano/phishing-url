# Phishing or Legitimate?
**Phishing Detection with Machine Learning: A Deep Dive** 🛡️
## Table of Contents
- [Introduction](#Introduction)
   - [Understanding the Threat](#Understanding-the-Threat)
   - [Dataset](#Dataset)
   - [Pre-processing](#Pre-processing)
     - [Binary variable transformation and creation of the score set](#Binary-variable-transformation)
     - [Removal of Problematic Variables](#Removal-of-Problematic-Variables)
     - [Model Selection with Boruta](#Model-Selection)
     - [Verification of Separation for Problematic Variables](#Separtaion)
- [Models Training](#Models-Training)
  - [GLM](#GLM)
  - [KNN-K Nearest Neighbors](#KNN)
  - [Lasso](#Lasso)
  - [PLS](#PLS)
  - [Naive Bayes](#Naive-Bayes)
  - [Tree](#Tree)
  - [Bagging](#Bagging)
  - [Gradeint Boosting](#Gradeint-Boosting)
  - [Random Forest](#Random-Forest)
  - [Stacking](#Stacking)
  - [Neural Network](#Neural-Network)
- [Assessment](#Assessment)
     - [ROC Curves](#ROC-Curves)
     - [Lift Curves](#Lift-Curves)
- [Threshold Selection](#Threshold-Selection)
   - [Confusion Matrix and Metrics with Adjusted Threshold](#New-Metrics)
- [Scoring New Cases](#Scoring-New-Cases)
- [Results](#Results)
     
## Introduction
### Understanding the Threat:
Phishing is a deceptive technique used by cybercriminals to trick individuals into disclosing sensitive information. This is achieved through emails that mimic legitimate sources, directing recipients to fraudulent websites that look strikingly similar to their real counterparts. Unlike complex cyber attacks that require breaking through security measures, phishing exploits human trust.As cybercriminals become more sophisticated, they now employ SSL protections on phishing sites, making them appear secure and legitimate. This evolution has blurred the lines between safe and malicious sites, presenting a challenge for users and security systems alike. 
In response to these evolving threats, machine learning (ML) emerges as a powerful tool in detecting and preventing phishing attempts. ML algorithms analyze vast amounts of data to identify patterns and anomalies indicative of phishing.

**Detection Techniques:**  
1. **URL Analysis:** ML algorithms scrutinize URLs, looking for suspicious patterns such as unfamiliar domain names or misleading subdomains. This analysis includes examining the structure, the use of secure protocols, and the overall composition of the URL to flag potential threats. 
2. **Content Inspection:** Beyond URLs, ML dives into the content of web pages. It evaluates the HTML code for unusual patterns, such as the presence of fake login forms or unexpected redirects. This thorough examination helps in identifying web pages that are designed to deceive. 

This project is committed to harnessing ML for more robust phishing detection. By merging intricate URL and content analyses, we strive to develop a system capable of accurately spotting and marking phishing sites.

### Dataset:
The diagram in Figure 1 illustrates the procedure used to create the dataset in detail.
<div align="center">
  
  <img width="775" alt="Dataset_Construction" src="https://github.com/user-attachments/assets/753836c3-5bf0-4320-afda-1d7983e37ba1" />

  Figure 1: Source: Extract from the paper

</div>


The analyzed dataset consists of 11,430 URLs, each characterized by 87 distinct attributes. This dataset was specifically developed as a benchmark for phishing detection systems that utilize machine learning techniques.

The dataset is balanced, with 50% of the URLs being phishing and 50% legitimate. However, it's important to note that the actual proportion of phishing cases in the general population is much lower, estimated at around 0.01%.

The features in the dataset are grouped into three main categories:
- 56 features derived from the structure and syntax of the URLs;
- 24 features extracted by analyzing the content of the web pages;
- 7 features obtained by querying external services.

The target variable in this dataset is "status," a binary variable indicating whether a website is classified as legitimate or phishing. As shown in Table 1, this variable is balanced, reflecting the equal distribution of phishing and legitimate URLs in the dataset.

<div align="center">

| Status     | Train | Test  |
|------------|-------|-------|
| Phishing   | 3,829 | 1,792 |
| Legitimate | 3,829 | 1,792 | 

Table 1: Distribution of the "status" variable in the train and test datasets.

</div>


### Pre-processing
#### Binary variable transformation
Before starting the analysis, it is necessary to convert the binary variables, as R imports them as categorical variables, although they are actually numeric. The score set is needed for the final step, where predictions are made on new observations for which the target variable, status, is unknown. The score dataset was created by considering 5% of the test score.

#### Missing Data:  
There are no missing values in the train or test datasets

#### Removal of Problematic Variables
The variables displayed in Figure 3 were removed from the analysis because they had only one category, making them uninformative for the model. Additionally, the variable 'statistical report' was found to have an issue: although it appears with three categories, it was discovered from the original publication that it should be binary. Therefore, the extra values are considered errors, and the variable will be excluded from the predictors.

<div align="center">
  <img width="713" alt="Problematic-Variables" src="https://github.com/user-attachments/assets/9eec250c-8a93-4897-b18b-ca15c9ca475e">

  Figure 3: Problematic Variables

</div>

#### Model Selection with Boruta 
The Boruta algorithm was used for variable selection due to its robustness and ability to provide more reliable results than other methods like Decision Tree or Random Forest. Boruta's iterative methodology evaluates the relevance of variables through multiple iterations, offering a deeper understanding of variable importance in the dataset. After applying Boruta, a subset of the original dataset was created, including only the 68 variables identified as relevant by the algorithm, along with the target variable. This new dataset will be used for training machine learning models that require a preliminary model selection phase, utilizing the accurate variable selection to improve model effectiveness.

<div align="center">
  <img width="983" alt="Boruta_model_selection" src="https://github.com/user-attachments/assets/da4f9b8d-9157-40b9-bd18-cef869c3ff5d">

  Figure 4: Boruta Model Selection

</div>

Three variables stood out in terms of importance: 'google index,' 'page rank,' and 'nb hyperlinks.'

<div align="center">

| Feature       | meanImp    | medianImp  | minImp     | maxImp     | decision  |
|---------------|------------|------------|------------|------------|-----------|
| google index  | 43.3923540 | 43.407850030 | 40.473236069 | 46.661588 | Confirmed |
| page rank     | 42.6068700 | 42.653352067 | 39.644587797 | 45.602508 | Confirmed |
| nb hyperlinks | 37.2520280 | 37.315795790 | 34.500929467 | 40.575561 | Confirmed |
| domain age    | 28.83370859 | 28.731570436 | 25.735769316 | 31.050372 | Confirmed |
| web traffic   | 28.28171636 | 28.291790564 | 26.563648663 | 30.035131 0 | Confirmed |
| nb www        | 25.30681692 | 25.329973224 | 23.114021043 | 27.378201 | Confirmed |

</div>


#### Verification of Separation for Problematic Variables
During the analysis of the distribution of all variables concerning the target variable, it was found that 'page rank' and 'google index' exhibit the phenomenon of separation. Specifically, 'page rank' reflects a page's position in Google search results, where pages with a very low rank tend to be poorly referenced by Google and are often associated with phishing sites, while those appearing at the top are generally considered legitimate. Regarding 'google index,' it takes the value 1 when a site is indexed by Google and 0 otherwise. Non-indexed sites are often those that Google has identified as phishing, making them invisible in search results.
<div align="center">
  <img width="1027" alt="separation" src="https://github.com/user-attachments/assets/88449b38-8ff0-4266-b871-96153afe3edf">

  Figure 5: Problematic Variables

</div>

## Models Training
**Tuning with Sensitivity**: In our analysis, we prioritize sensitivity for hyperparameter tuning. This focus is crucial for minimizing false negatives, where phishing sites are wrongly deemed legitimate, potentially leading to significant user risks like fraud or identity theft. Conversely, while false positives—legitimate sites flagged as phishing—may cause inconvenience, they bear less severe consequences. Thus, orienting our model towards "caution" reduces the likelihood of overlooking harmful sites, even at the expense of increasing false positives.

**Adjusting Priors**: In phishing detection, balancing our dataset to equate rare events (phishing attempts) with common ones doesn't reflect reality, where phishing is much less frequent (e.g., 1%). Adjusting our model's priors to match the real distribution (99% legitimate, 1% phishing) is essential. This recalibration ensures our model realistically predicts phishing attempts, enhancing its efficacy and representation of actual conditions.

**Confusion Matrix and Classification Metrics**: Adjusting the model with balanced priors necessitates recalibrating the confusion matrix and classification metrics. This adjustment aligns the metrics with real-world class distributions, ensuring they accurately reflect the model's true performance.

**Predicted Probabilities (Posteriors)**: We must correct the initially predicted probabilities to achieve adjusted posteriors. This crucial step recalibrates the model's output to mirror the actual scenario, enabling more precise decision-making and aligning predictions with the genuine likelihood of classes.


These adjustments are pivotal for deploying ML models in practical settings, like phishing detection, where class imbalance is pronounced. Correcting both the confusion matrix and predicted probabilities ensures the model's metrics and predictions are attuned to real-world conditions, significantly improving its utility and accuracy in identifying rare but critical events.

### `GLM`

#### Pre-Processing

The selected dataset for training includes 68 features, with no missing values. Standardization was applied, and collinearity was checked to ensure data quality.

#### Tuning Parameters

No tuning parameters were applied during the training process.

<div style="display: flex; justify-content: space-between;">

  <div style="flex: 1; padding-right: 10px;">
  
  #### Model Evaluation

  | Metric           | Score    |
  |------------------|----------|
  | **Accuracy (Train)**  | 0.9033   |
  | **Accuracy (Test)**   | 0.9111   |
  | **Sensitivity (Test)**| 0.9029   |
  | **Specificity (Test)**| 0.9112   |

  </div>

  <div style="flex: 1; padding-left: 10px;">
  
  #### Adjusted Confusion Matrix (Test)

  |                 | **Phishing** | **Legitimate** |
  |-----------------|--------------|----------------|
  | **Phishing**    | 32.36        | 3.48           |
  | **Legitimate**  | 314.82       | 3233.34        |

  </div>
  
</div>


### `KNN-K Nearest Neighbors`

#### Pre-Processing

The selected dataset for training includes 68 features. There are no missing values, and standardization was applied to ensure consistency in the data. Collinearity was checked to avoid issues with correlated variables.

#### Train Control and Tuning Parameters

The training process utilized a cross-validation method with 10 folds. A grid search was conducted to find the optimal parameter, specifically tuning the number of neighbors (K). The optimaL K is 5
<div align="center">
   <img width="633" alt="Tuning-KNN" src="https://github.com/user-attachments/assets/b031c105-fb09-48df-aa7a-dbb0c5dcff44">

  Figure 6: Tuning K

</div>

#### Model Evaluation



| Metric               | Value   |
|----------------------|---------|
| Accuracy (Train)     | 0.9521  |
| Accuracy (Test)      | 0.9310  |
| Sensitivity (Test)   | 0.9012  |
| Specificity (Test)   | 0.9313  |



#### Adjusted Confusion Matrix (Test)

|                 | Phishing | Legitimate |
|-----------------|----------|------------|
| **Phishing**    | 32.3     | 3.54       |
| **Legitimate**  | 243.54   | 3304.62    |


### `Lasso`

#### Pre-Processing
The selected dataset for training includes 79 features. There are no missing values, and standardization was applied to ensure consistency in the data.

#### Train Control and Tuning Parameters

The training process used a cross-validation method with 10 folds. A grid search was conducted to find the optimal parameter, specifically tuning the effect of regularization (λ).
<div align="center">
   <img width="553" alt="Tuning λ" src="https://github.com/user-attachments/assets/b25a91ac-c2b5-444f-8d07-ea39c7afd8a2">

  Figure 7: Tuning λ 

</div>

#### Model Evaluation

| Metric               | Value   |
|----------------------|---------|
| Accuracy (Train)     | 0.9030  |
| Accuracy (Test)      | 0.9111  |
| Sensitivity (Test)   | 0.9039  |
| Specificity (Test)   | 0.9112  |

#### Confusion Matrix (Test)

|                 | Phishing | Legitimate |
|-----------------|----------|------------|
| **Phishing**    | 32.36    | 3.48       |
| **Legitimate**  | 314.82   | 3233.34    |


### `PLS`

#### Pre-Processing
The selected dataset for training includes 79 features. There are no missing values, and standardization was applied to ensure data consistency.

#### Train Control and Tuning Parameters
The training process utilized a cross-validation method with 10 folds. A grid search was conducted to find the optimal parameter, specifically tuning the number of PLS scores (K).
<div align="center">
   <img width="617" alt="Tuning ncomp" src="https://github.com/user-attachments/assets/2264949e-3861-415b-99c1-8fb9f53b514e">

  Figure 8: Tuning n° PLS scores

</div>

#### Model Evaluation

| Metric               | Value   |
|----------------------|---------|
| Accuracy (Train)     | 0.8931  |
| Accuracy (Test)      | 0.8971  |
| Sensitivity (Test)   | 0.8772  |
| Specificity (Test)   | 0.8973  |

#### Confusion Matrix (Test)

|                 | Phishing | Legitimate |
|-----------------|----------|------------|
| **Phishing**    | 31.44    | 4.4        |
| **Legitimate**  | 364.32   | 3183.84    |


### `Naive Bayes`
#### Pre-Processing

The dataset selected for training includes 68 features. Pre-processing involved handling collinearity and zero-frequency issues to prepare the data for model training.

#### Train Control and Tuning Parameters

The training process used cross-validation with 10 folds. A grid search was employed to optimize the model, specifically adjusting the parameters: laplace set to 0, adjust set to 1, and usekernel set to True.

<div align="center">
   <img width="537" alt="Tuning-NaiveBayes" src="https://github.com/user-attachments/assets/edf69320-291e-4b5c-b843-a54fe69c21f2">


  Figure 9: Tuning use-Kernel

</div>

#### Model Evaluation

| Metric               | Value   |
|----------------------|---------|
| Accuracy (Train)     | 0.8966  |
| Accuracy (Test)      | 0.9029  |
| Sensitivity (Test)   | 0.6780  |
| Specificity (Test)   | 0.9029  |

#### Confusion Matrix (Test)

|                 | Phishing | Legitimate |
|-----------------|----------|------------|
| **Phishing**    | 24.3     | 11.54      |
| **Legitimate**  | 344.32   | 3203.64    |


### `Tree`
#### Pre-Processing
The dataset selected for training includes 87 features. The training process was conducted without any pre-processing, focusing on leveraging strong predictors. 

#### Train Control and Tuning Parameters

The training method employed cross-validation with 10 folds. A grid search was used to fine-tune the model parameters, with pruning applied to optimize the final model.
<div align="center">
   <img width="568" alt="Pruning-Tree" src="https://github.com/user-attachments/assets/562b4e2d-f3ef-4d70-acf6-bb3547dcf145">

  Figure 10: Pruning Tree

</div>


#### Model Evaluation

| Metric               | Value   |
|----------------------|---------|
| Accuracy (Train)     | 0.9457  |
| Accuracy (Test)      | 0.9130  |
| Sensitivity (Test)   | 0.9263  |
| Specificity (Test)   | 0.9129  |

#### Confusion Matrix (Test)

|                 | Phishing | Legitimate |
|-----------------|----------|------------|
| **Phishing**    | 33.2     | 2.64       |
| **Legitimate**  | 308.88   | 3239.28    |


### `Bagging`


#### Pre-Processing

The dataset used for training consists of 87 features. The training process focused on strong predictors without any pre-processing steps. 

#### Train Control and Tuning Parameters

The model was trained using cross-validation with 10 folds. A grid search was conducted to optimize the model, with the number of trees (`ntree`) set to 250.

#### Model Evaluation

| Metric               | Value   |
|----------------------|---------|
| Accuracy (Train)     | 0.9831  |
| Accuracy (Test)      | 0.9359  |
| Sensitivity (Test)   | 0.9441  |
| Specificity (Test)   | 0.9358  |

#### Confusion Matrix (Test)

|                 | Phishing | Legitimate |
|-----------------|----------|------------|
| **Phishing**    | 33.84    | 2          |
| **Legitimate**  | 227.7    | 3320.46    |


### `Gradient Boosting`

#### Pre-Processing

The dataset used for training includes 87 features. The training process focused on strong predictors without any pre-processing steps.

#### Train Control and Tuning Parameters

The model was trained using cross-validation with 10 folds. A grid search was employed to optimize the following parameters:
- `n.trees = 500`: Sets 500 trees in the model.
- `interaction.depth = 4`: Limits trees to 4 levels deep.
- `shrinkage = 0.1`: Controls learning speed.
- `n.minobsinnode = 10`: Minimum 10 samples per tree node.

#### Model Evaluation

| Metric               | Value   |
|----------------------|---------|
| Accuracy (Train)     | 0.9796  |
| Accuracy (Test)      | 0.9488  |
| Sensitivity (Test)   | 0.9642  |
| Specificity (Test)   | 0.9486  |

#### Confusion Matrix (Test)

|                 | Phishing | Legitimate |
|-----------------|----------|------------|
| **Phishing**    | 34.5     | 1.34       |
| **Legitimate**  | 180.18   | 3367.98    |


### `Random Forest`

#### Pre-Processing

The dataset selected for training includes 87 features. The training process emphasized strong predictors without applying any pre-processing steps.

#### Train Control and Tuning Parameters

The model was trained using cross-validation with 10 folds. A grid search was used to optimize the parameter `mtry`, which determines the number of features considered for deciding a split at each tree node.
<div align="center">
   <img width="562" alt="Tuning Random Forest" src="https://github.com/user-attachments/assets/9990d178-c6b6-4c66-8519-49e9760c9581">

  Figure 11: Tuning n° of features considered for deciding a split at each tree node.

</div>

#### Model Evaluation

| Metric               | Value   |
|----------------------|---------|
| Accuracy (Train)     | 0.9931  |
| Accuracy (Test)      | 0.9492  |
| Sensitivity (Test)   | 0.9542  |
| Specificity (Test)   | 0.9492  |

#### Confusion Matrix (Test)

|                 | Phishing | Legitimate |
|-----------------|----------|------------|
| **Phishing**    | 34.22    | 1.62       |
| **Legitimate**  | 182.16   | 3366       |


### `Stacking`

#### Pre-Processing
The dataset selected for training includes 87 features. No pre-processing steps were applied in this process.

#### Train Control and Tuning Parameters

The model was trained using cross-validation with 10 folds. A grid search was conducted to optimize the parameters. The meta-classifier used was Logistic Regression, combining models including `glm`, `knn`, `naive bayes`, and `rf`.
<div align="center">
   <img width="621" alt="Stacking" src="https://github.com/user-attachments/assets/d0c68373-63f0-46ee-8d88-9b9248764c8d">

  Figure 12: Stacking

</div>

#### Model Evaluation

| Metric               | Value   |
|----------------------|---------|
| Accuracy (Train)     | 0.9267  |
| Accuracy (Test)      | 0.9526  |
| Sensitivity (Test)   | 0.9564  |
| Specificity (Test)   | 0.9526  |

#### Confusion Matrix (Test)

|                 | Phishing | Legitimate |
|-----------------|----------|------------|
| **Phishing**    | 34.28    | 1.56       |
| **Legitimate**  | 168.3    | 3379.86    |


### `Neural Network`

#### Pre-Processing

The dataset selected for training includes 68 features. The pre-processing steps involved handling collinearity, missing values, and standardization. Zero variance features were removed to ensure the dataset was optimized for training.

#### Train Control and Tuning Parameters

The model was trained using cross-validation with 10 folds. A grid search was conducted to fine-tune the parameters for a single-hidden-layer neural network.

<div align="center">
   <img width="509" alt="Tuning Neural Network" src="https://github.com/user-attachments/assets/a8061af8-0ee8-41ff-8821-300aa047ca4f">

  Figure 13: Single-hidden-layer neural network

</div>

####  Model Evaluation

| Metric               | Value   |
|----------------------|---------|
| Accuracy (Train)     | 0.9428  |
| Accuracy (Test)      | 0.9237  |
| Sensitivity (Test)   | 0.9375  |
| Specificity (Test)   | 0.9235  |

#### Confusion Matrix (Test)

|                 | Phishing | Legitimate |
|-----------------|----------|------------|
| **Phishing**    | 33.6     | 2.24       |
| **Legitimate**  | 271.26   | 3276.9     |

## Assesment
The goal of this phase is to evaluate the classification performance of the various classifiers that were previously trained. To do this, evaluation metrics are introduced that allow for the comparison of classifiers by addressing the threshold problem. Metrics like sensitivity or accuracy cannot be directly compared because they are calculated for a single threshold value, whatever that may be. In other words, evaluation metrics should not depend on the threshold.

### ROC Curves
Initially, the ROC (Receiver Operating Characteristic) curve method is used. ROC curves show how the probability of correctly classifying events (True Positive Rate or sensitivity) changes as the incorrect classification of non-events (False Positive Rate or 1-specificity) varies for each threshold (point on the ROC curve). The steeper the curve, the faster the correct classification of events, while minimizing errors on non-events (1-specificity). It's important to note that the first part of the ROC curve corresponds to high posterior thresholds, while the second part corresponds to low posterior thresholds.
<div align="center">
   <img width="689" alt="ROC Curves" src="https://github.com/user-attachments/assets/d19522c2-196e-4bfd-9f72-28efaae8fbee">

  Figure 14: ROC Curves

</div>

From the analysis of Figure 14, it is clear that most of the trained classifiers demonstrate good or at least decent event classification capabilities, with a low percentage of non-event misclassification. However, the graph does not clearly distinguish a superior model, as no specific model's curve stands out across the entire X-axis for all considered thresholds. To better assess the discriminative ability of the models, the Area Under the ROC Curve (AUC) is used, ranging from 0.5 to 1. The comparison of AUCs, presented in Table below, highlights the difficulty in selecting a winning model, as most classifiers achieve excellent results in terms of AUC. There are three models as top contenders: Gradient Boosting, Random Forest, and Stacking.

<div align="center">


| Algorithm          | AUC Value  | Algorithm      | AUC Value  |
|--------------------|------------|----------------|------------|
| Logit              | 0.9669143  | GLM Stack      | 0.9892422  |
| Gradient Boosting  | 0.9879352  | Lasso          | 0.9668068  |
| Random Forest      | 0.9891493  | PLS            | 0.9545042  |
| Neural Network     | 0.9751982  | Bagging        | 0.9832734  |
| KNN                | 0.9631318  |                |            |

</div>

### Lift Curves

In such cases, Lift curves are used for further evaluation. Lift curves assess the performance of classification models by comparing the cumulative percentage of correctly classified events to the expected percentage under random conditions. A well-positioned Lift curve indicates that the model outperforms random selection, while a curve near the reference line suggests modest performance.
- Lift Curve - **Random Forest**: In the first three deciles, the model captures 59.9% of true phishing URLs, with a Lift score of 2 in this decile (Left).
- Lift Curve - **Gradient Boosting**: In the first three deciles, the model captures 59.8% of true phishing URLs, with a Lift score of 1.99 in this decile (Center).
- Lift Curve - **Stacking**: In the first three deciles, the model captures 59.8% of true phishing URLs, with a Lift score of 1.99 in this decile (Right).

<div align="center">
   
   <img width="1008" alt="Lift" src="https://github.com/user-attachments/assets/cae1330d-8600-423d-9051-fac8cf83b8da">

  Figure 15: Lift Curves

</div>

## Threshold Selection

In this step, the classification performance of the best model was analyzed, with a focus on identifying an optimal threshold for application to new data. The primary goal was to optimize the sensitivity of the most effective model, Random Forest. To achieve this, methodologies based on statistical criteria were employed, aimed at maximizing sensitivity within the selected model.

<div align="center">
   <img width="694" alt="Optimal Threshold" src="https://github.com/user-attachments/assets/9b4b0730-6e3b-42e5-b06c-7b9b2c9e2713">

  Figure 16: Optimal Threshold

</div>

The analysis of the graph revealed that the optimal threshold value is 0.4. This determination is based on the observation that this value corresponds to a high level of sensitivity, which is crucial for ensuring accurate predictions of phishing sites. At this threshold, significant specificity values were also observed.

### Confusion Matrix and Metrics with Adjusted Threshold
With the adjustment of the threshold to 0.4, new confusion matrix was constructed to better reflect the model's performance under this optimized setting. The following tables present the updated confusion matrix and performance metrics for both the 0.4 and 0.5 thresholds, highlighting the impact of threshold selection on model accuracy and classification effectiveness.
After adjusting the threshold to 0.4, the confusion matrix shows an increase in values, particularly for sensitivity.

<div align="center">


|                    | **Predicted Phishing** | **Predicted Legitimate** |
|--------------------|------------------------|--------------------------|
| **Actual Phishing**     | 34.88                  | 0.96                     |
| **Actual Legitimate**   | 273.24                 | 3274.92                  |

Table: New Adjusted Confusion Matrix



| Metric      | Threshold 0.4 | Threshold 0.5 |
|-------------|---------------|---------------|
| **Accuracy**    | 0.9235        | 0.9492        |
| **Sensitivity** | 0.9732        | 0.9542        |
| **Specificity** | 0.9229        | 0.9492        |

Table: Performance Metrics at Different Thresholds
</div>

## Scoring New Cases

The results from the previous analysis step are satisfactory, so the score set created at the beginning of the project, consisting of 174 observations, will be used.
The threshold (0.4) obtained in the previous step will be applied to the new cases. In the end, out of 174 observations, 145 were classified as legitimate and 29 as phishing.
<div align="center">
   <img width="835" alt="Predict new data" src="https://github.com/user-attachments/assets/6511d9c8-469e-4d8b-b5fe-64f754fbdb97">
   
   Figure 16: Scoring new data
</div>

## Results
<div align="center">
   <img width="905" alt="project at glance" src="https://github.com/user-attachments/assets/914a57d2-0d12-4dc3-8f6f-3a70986695fb">
   
   Figure 17: Project at Glance
</div>

The hypothesis of using a Random Forest model trained to identify phishing sites in corporate environments could significantly enhance cybersecurity. Integrating this model into an email protection system could provide an effective method to counter one of the most common and insidious attack vectors: email phishing.

In this hypothetical implementation, the model would be integrated with corporate email systems, offering a proactive solution to detect and neutralize phishing threats. Real-time analysis of links in emails could immediately identify phishing attempts, preventing damage before it occurs. This approach is efficient as it reduces the need for manual intervention by IT staff, automating threat detection.

### Tips for Identifying Phishing Sites
- Legitimate pages usually use hyperlinks with the same base domain, while phishing pages often use multiple external hyperlinks pointing to target websites. The ratio of internal to external hyperlinks is considered a phishing indicator.
- Phishing websites generally have fewer visitors than legitimate ones. Alexa is used to identify web traffic for URLs.
- Since phishing websites are short-lived, the age of the domain is considered a phishing indicator.
- Legitimate websites typically consist of more pages than phishing sites. Therefore, the number of links in the content of web pages is considered to distinguish phishing sites.
- Common terms in URLs, like "www," are used only once in legitimate URLs but are often used more than once in phishing URLs.


