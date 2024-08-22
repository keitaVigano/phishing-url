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

### GLM
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




1. **Confusion Matrix and Classification Metrics**: Adjusting the model with balanced priors necessitates recalibrating the confusion matrix and classification metrics. This adjustment aligns the metrics with real-world class distributions, ensuring they accurately reflect the model's true performance.
2. **Predicted Probabilities (Posteriors)**: We must correct the initially predicted probabilities to achieve adjusted posteriors. This crucial step recalibrates the model's output to mirror the actual scenario, enabling more precise decision-making and aligning predictions with the genuine likelihood of classes.

These adjustments are pivotal for deploying ML models in practical settings, like phishing detection, where class imbalance is pronounced. Correcting both the confusion matrix and predicted probabilities ensures the model's metrics and predictions are attuned to real-world conditions, significantly improving its utility and accuracy in identifying rare but critical events.

By leveraging strong predictors and refining feature selection methods, our approach not only enhances detection accuracy but also underscores the power of ML in creating a safer digital environment. Let's join forces to outsmart phishing attempts and safeguard our digital frontiers. 🌐
