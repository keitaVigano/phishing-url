# Librerie ----------------------------------------------------------------
library(Boruta)
library(breakDown)
library(caTools)
library(car)
library(caret)
library(caretEnsemble)
library(DALEX)
library(dplyr)
library(factorMerger)
library(funModeling)
library(gbm)
library(ggplot2)
library(gridExtra)
library(MASS)
library(mice)
library(naivebayes)
library(patchwork)
library(plyr)
library(pls)
library(randomForest)
library(reshape2)
library(ROCR)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(VIM)

# Importazione dati ---------------------------------------
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Trasformare le variabili binarie nel train
binary_vars <- sapply(train, function(x) all(x %in% c(0, 1)))
train[binary_vars] <- lapply(train[binary_vars], factor, levels = c(0, 1), labels = c(0, 1))
train$status <- factor(train$status)

# Trasformare le variabili binarie nel test
binary_vars <- sapply(test, function(x) all(x %in% c(0, 1)))
test[binary_vars] <- lapply(test[binary_vars], factor, levels = c(0, 1), labels = c(0, 1))
test$status <- factor(test$status)

# Creazione Score Set
Trainindex = createDataPartition(y = test$status, p = .95, list = FALSE)
test = test[Trainindex,]
score = test[-Trainindex,]

# Rinominazione Target e livelli
colnames(train)[colnames(train)=="status"] <- "target" 
colnames(test)[colnames(test)=="status"] <- "target" 

train$target <- factor(train$target, levels = c("phishing", "legitimate"))
test$target <- factor(test$target, levels = c("phishing", "legitimate"))




# Distribuzione variabile target ------------------------------------------
class(train$target)
table(train$target) / nrow(train)

# Funzioni per fare i grafici
plot_gg1 = function(column){
  ggplot(data = train, mapping = aes(x = {{column}})) +
    geom_bar(position = 'dodge') +
    scale_fill_manual('Legenda', values = c("lightblue", "blue"))
}

plot_gg = function(column){
  ggplot(data = train, mapping = aes(x = {{column}}, fill = target)) +
    geom_bar(position = 'dodge') +
    scale_fill_manual('Legenda', values = c("lightblue", "blue"))
}

plot_gg(target) + 
  ggtitle("Phishing and Legitimate website")

# Dati mancanti -----------------------------------------------------------

# Train
missing_data <- train %>% 
  summarise_all(function(x) sum(is.na(x) | x == "")) %>% 
  gather(variable, missing_count)

missingness = aggr(train,
                   col=c('navyblue','yellow'),numbers=TRUE,sortVars=TRUE,
                   cex.axis=.7,gap=2) 

# Test 
missing_data <- test %>% 
  summarise_all(function(x) sum(is.na(x) | x == "")) %>% 
  gather(variable, missing_count)

missingness = aggr(test,
                   col=c('navyblue','yellow'),numbers=TRUE,sortVars=TRUE,
                   cex.axis=.7,gap=2) 

# Variabili problematiche
# Lista di grafici ggplot
p1 <- plot_gg1(statistical_report)
p2 <- plot_gg1(ratio_nullHyperlinks)
p3 <- plot_gg1(ratio_intErrors)
p4 <- plot_gg1(ratio_intRedirection)
p5 <- plot_gg1(submit_email)
p6 <- plot_gg1(sfh)

# Combina i grafici in una griglia
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)

#Eliminazioni delle variabili problematiche
train <- subset(train, select = -c(statistical_report, ratio_nullHyperlinks, ratio_intErrors,ratio_intRedirection,submit_email,sfh,url ))
test <- subset(test, select = -c(statistical_report, ratio_nullHyperlinks, ratio_intErrors,ratio_intRedirection,submit_email,sfh,url ))


# Model selection ----------------------------------------------------------
#Boruta
boruta.train = Boruta(status ~., data = train, doTrace = 1)
plot(boruta.train, xlab = "features", xaxt = "n", ylab="MDI")

print(boruta.train)

boruta.metrics = attStats(boruta.train)
head(boruta.metrics)
table(boruta.metrics$decision)

vi_bo = subset(boruta.metrics, decision == "Confirmed")
head(vi_bo)  
viname_bo = rownames(vi_bo)

# Scelta delle features da mantenere in analisi
selected = c("target","google_index", "page_rank", "nb_hyperlinks", "domain_age", "web_traffic", "nb_www", 
             "phish_hints", "length_url", "longest_word_path", "length_hostname", "nb_hyphens", "ratio_intHyperlinks","safe_anchor","domain_registration_length","length_words_raw", "longest_words_raw","ratio_extHyperlinks","ratio_digits_host","nb_slash","avg_word_path")
selected_s=c("google_index", "page_rank", "nb_hyperlinks", "domain_age", "web_traffic", "nb_www", 
                        "phish_hints", "length_url", "longest_word_path", "length_hostname", "nb_hyphens", "ratio_intHyperlinks","safe_anchor","domain_registration_length","length_words_raw", "longest_words_raw","ratio_extHyperlinks","ratio_digits_host","nb_slash","avg_word_path")
train_selected = train[,selected]
test_selected= test[,selected]
score_selected=score[,selected_s]


# Verifica Separation -----------------------------------------------------

# Funzione per creare griglie di grafici 3x3 per tutte le variabili
plot_all_variables_in_grids <- function(data, target_var) {
  # Filtra le variabili da visualizzare (escludendo la variabile target)
  variables <- setdiff(names(data), target_var)
  
  # Numero di grafici per griglia
  n_per_page <- 9
  
  # Calcola il numero totale di pagine necessarie
  total_pages <- ceiling(length(variables) / n_per_page)
  
  for (page in 1:total_pages) {
    # Seleziona le variabili per la pagina corrente
    vars_for_page <- variables[((page - 1) * n_per_page + 1):min(page * n_per_page, length(variables))]
    
    # Crea una lista di grafici per questa pagina
    plots <- lapply(vars_for_page, function(var) {
      if (is.numeric(data[[var]])) {
        ggplot(data, aes_string(x = target_var, y = var)) +
          geom_boxplot() +
          labs(title = paste("Boxplot of", var, "by", target_var))
      } else {
        ggplot(data, aes_string(x = var, fill = target_var)) +
          geom_bar(position = "dodge") +
          labs(title = paste("Barplot of", var, "by", target_var))
      }
    })
    
    # Mostra la griglia di grafici
    grid.arrange(grobs = plots, ncol = 3)
    if (page < total_pages) {
      # Aggiunge una pausa se ci sono altre pagine
      cat("Premi [Enter] per la prossima pagina...\n")
      readline()
    }
  }
}

# Utilizzo della funzione
plot_all_variables_in_grids(train, "status")


# Step 1 ------------------------------------------------------------------
# GLM (Logistic Classifier) -----------------------------------------------
set.seed(9999)
ctrl = trainControl(method = "cv",
                    number = 10,
                    search = "grid", 
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary) 

glm = train(target ~ ., 
            data = train_selected, 
            method = "glm", 
            preProcess = c("corr", "nzv"),
            trControl = ctrl,
            tuneLength = 5, 
            trace = TRUE,
            metric = "Sens") 

glm 
confusionMatrix(glm)

glmpred = predict(glm, test_selected)
glmpred_p = predict(glm, test_selected, type = c("prob"))
confusionMatrix(glmpred, test_selected$target)




# K-Nearest Neightbour ----------------------------------------------------

set.seed(9999)
ctrl = trainControl(method = "cv", 
                    number = 10,
                    search = "grid",
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary) 

knn = train(target ~ ., 
            data = train_selected,
            method = "knn",
            trControl = ctrl,
            tuneLength = 10, 
            preProcess = c("center", "scale", "corr", "nzv"), 
            metric = "Sens") 
knn
plot(knn)
confusionMatrix(knn)

knnpred = predict(knn, test_selected)
knnpred_p = predict(knn, test_selected, type = c("prob"))
confusionMatrix(knnpred, test_selected$target)

# LASSO -------------------------------------------------------------------

set.seed(9999)
ctrl = trainControl(method = "cv", 
                    number = 10,
                    classProbs = TRUE,
                    search = "grid", 
                    summaryFunction = twoClassSummary)



lasso = train(target ~ ., 
              data = train, 
              method = "glmnet",
              family = "binomial", 
              preProcess = c("corr", "nzv","center", "scale"), 
              metric = "Sens",
              trControl = ctrl,
              tuneLength = 10) 
lasso
plot(lasso)
confusionMatrix(lasso)

lassopred = predict(lasso, test)
lassopred_p = predict(lasso, test, type = c("prob"))
confusionMatrix(lassopred, test$target)

# PLS ---------------------------------------------------------------------

set.seed(9999)
ctrl = trainControl(method = "cv", 
                    number = 10,
                    search = "grid", 
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary)

pls = train(target ~ .,
            data = train, 
            method = "pls",
            preProcess = c("corr", "nzv","center", "scale"), 
            metric = "Sens",
            trControl = ctrl,
            tuneLength = 10) 

pls
plot(pls)
confusionMatrix(pls)

plspred = predict(pls, test)
plspred_p = predict(pls, test, type = c("prob"))
confusionMatrix(plspred, test$target)

# Naive Bayes -------------------------------------------------------------
set.seed(9999)
ctrl = trainControl(method = "cv", 
                    number = 10,
                    search = "grid", 
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary)

naivebayes = train(target ~ ., 
                   data = train_selected, 
                   method = "naive_bayes",
                   trControl = ctrl,
                   tuneLength = 10,
                   preProcess = c("corr", "nzv"), 
                   metric = "Sens")

naivebayes
plot(naivebayes)
confusionMatrix(naivebayes)

nbpred = predict(naivebayes, test_selected)
nbpred_p = predict(naivebayes, test_selected, type = c("prob"))
confusionMatrix(nbpred, test_selected$target)

# Decision Tree ----------------------------------------
tree_rpart = rpart(target ~ ., 
                   data = train_selected, 
                   method = "class",
                   cp = 0, 
                   minsplit = 1)
tree_rpart$cptable
rpart.plot(tree_rpart, type = 4, extra = 1)

tree_pruned = prune(tree_rpart, cp=  
                      tree_rpart$cptable[which.min(tree_rpart$cptable[,"xerror"]),"CP"])
rpart.plot(tree_pruned, type = 4, extra = 1)

treePred_pruned_p = predict(tree_pruned, test, type = c("prob"))
treePred_pruned = predict(tree_pruned, test, type = c("class"))

confusionMatrix(treePred_pruned, test$target)







# Bagging -----------------------------------------------------------------
set.seed(9999)
ctrl = trainControl(method = "boot",
                    number = 10,
                    searc = "grid", 
                    summaryFunction = twoClassSummary, 
                    classProbs = TRUE)

bagging = train(target ~ ., 
                data = train_selected, 
                method = "treebag",
                ntree = 250,
                trControl = ctrl)

bagging
confusionMatrix(bagging)

baggpred = predict(bagging, test)
baggpred_p = predict(bagging, test, type = c("prob"))
confusionMatrix(baggpred, test$target)

# Gradient Boosting -------------------------------------------------------

set.seed(9999)
ctrl = trainControl(method = "boot",
                    number = 10,
                    searc = "grid", 
                    summaryFunction = twoClassSummary, 
                    classProbs = TRUE)

gbm_tune = expand.grid(n.trees = 500,
                       interaction.depth = 4,
                       shrinkage = 0.1,
                       n.minobsinnode = 10)

gb = train(target ~., 
           data = train, 
           method = "gbm",
           tuneLength = 10,
           metric ="Sens",
           tuneGrid = gbm_tune,
           trControl = ctrl)

gb
confusionMatrix(gb)

gbpred = predict(gb, test)
gbpred_p = predict(gb, test, type = c("prob"))
confusionMatrix(gbpred, test$target)

# Random Forest -----------------------------------------------------------

set.seed(9999)
ctrl <- trainControl(method = "cv",
                     number = 10,
                     search = "grid",
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

rf <- train(target ~ ., 
            data = train, 
            method = "rf",
            metric = "Sens",
            tuneLength = 10,
            trControl = ctrl,
            verbose = FALSE) 

rf
plot(rf)
confusionMatrix(rf)

rfpred = predict(rf, test)
rfpred_p = predict(rf, test, type = c("prob"))
confusionMatrix(rfpred, test$target)


# Permutation Importance
vImp = varImp(rf)
plot(varImp(object = rf), main = "Train Tuned - Variable Importnace")
vImp = data.frame(vImp[1])
vImp$var=row.names(vImp)
head(vImp)

# Select covariate with Importance > 30% than most important
vImp2 = vImp[vImp$Overall>30,]
head(vImp2)

# NN ----------------------------------------------------------------------

cvCtrl = trainControl(method = "boot", 
                      number=10, searc="grid", 
                      summaryFunction = twoClassSummary, 
                      classProbs = TRUE)

#nn 
nn = train(target ~., data=train_selected,
           method = "nnet",
           preProcess = c("scale", "corr", "nzv"), 
           tuneLength = 5, metric="Sens", trControl=cvCtrl, trace = TRUE,
           maxit = 100)

plot(nn)
print(nn)
getTrainPerf(nn)
confusionMatrix(nn)

nnPred_p = predict(nn, test_selected, type = c("prob"))
nnPred = predict(nn, test_selected)

confusionMatrix(nnPred, test_selected$target)

#nn tuned
cvCtrl = trainControl(method = "cv", 
                      number=10, 
                      searc="grid", 
                      summaryFunction = twoClassSummary, 
                      classProbs = TRUE)

tunegrid = expand.grid(size=c(1:5), decay = c(0.001, 0.01, 0.05 , .1, .3))

nn_tuned = train(target ~., data=train_selected,
                 method = "nnet",
                 preProcess =  c("scale", "corr", "nzv"), 
                 tuneLength = 10, 
                 metric= "Sens", 
                 trControl=cvCtrl, 
                 tuneGrid=tunegrid,
                 trace = TRUE,
                 maxit = 300)

plot(nn_tuned)
print(nn_tuned)
getTrainPerf(nn_tuned)

confusionMatrix(nn_tuned)

nn_tunedPred_p = predict(nn_tuned, test_selected, type = c("prob"))
nn_tunedPred = predict(nn_tuned, test_selected)

confusionMatrix(nn_tunedPred, test_selected$target)


# Stacking ----------------------------------------------------------------
#GLM
cvCtrl = trainControl(method = "cv", 
                      number=10, 
                      searc="grid", 
                      summaryFunction = twoClassSummary, 
                      classProbs = TRUE)

model_list = caretList(target ~.,
                       data = train_selected,
                       trControl = cvCtrl,
                       methodList = c("glm", "knn", "naive_bayes","rf", "nne")
)

glm_ensemble = caretStack(
  model_list,
  method="glm",
  metric="Sens",
  trControl = cvCtrl
)

model_preds = lapply(model_list, predict, newdata = test_selected, type="prob")
model_preds2 = model_preds
model_preds$ensemble = predict(glm_ensemble, newdata = test_selected, type="prob")
model_preds2$ensemble = predict(glm_ensemble, newdata = test_selected)
CF = coef(glm_ensemble$ens_model$finalModel)[-1]
colAUC(model_preds$ensemble, test_selected$target)
confusionMatrix(model_preds2$ensemble, test_selected$target)

#GBM
gbm_ensemble = caretStack(
  model_list,
  method="gbm",
  metric="Sens",
  trControl = cvCtrl
)

model_preds3 = model_preds
model_preds4 = model_preds
model_preds3$ensemble = predict(gbm_ensemble, newdata=test_selected, type="prob")
model_preds4$ensemble = predict(gbm_ensemble, newdata=test_selected)
colAUC(model_preds3$ensemble, test_selected$target)
confusionMatrix(model_preds4$ensemble, test_selected$target)


# Step 2 ------------------------------------------------------------------

#GLM 
y = test$target
glmpredR = prediction(glmpred_p[,1], y)
roc_log = performance(glmpredR, measure = "tpr", x.measure = "fpr")
plot(roc_log)
abline(a=0, b= 1)

# KNN  
knnpredR = prediction(knnpred_p[,1], y)
roc_knn = performance(knnpredR, measure = "tpr", x.measure = "fpr")
plot(roc_knn)
abline(a=0, b= 1)

# LASSO 
lassoPredR = prediction(lassopred_p[,1], y)
roc_lasso = performance(lassoPredR, measure = "tpr", x.measure = "fpr")
plot(roc_lasso)
abline(a=0, b= 1)

# PLS 
plsPredR = prediction(plspred_p[,1], y)
roc_pls = performance(plsPredR, measure = "tpr", x.measure = "fpr")
plot(roc_pls)
abline(a=0, b= 1)

# Naive Bayes 
naivePredR = prediction(nbpred_p[,1], y)
roc_naive = performance(naivePredR, measure = "tpr", x.measure = "fpr")
plot(roc_naive)
abline(a=0, b= 1)

# Tree
treePredR = prediction(treePred_pruned_p[,1], y)
roc_tree = performance(treePredR, measure = "tpr", x.measure = "fpr")
plot(roc_tree)
abline(a=0, b= 1)

# Gradient Boosting 
gbPredR = prediction(gbpred_p[,1], y)
roc_gb = performance(gbPredR, measure = "tpr", x.measure = "fpr")
plot(roc_gb)
abline(a=0, b= 1)

# Bagging 
baggingPredR = prediction(baggpred_p[,1], y)
roc_bagging = performance(baggingPredR, measure = "tpr", x.measure = "fpr")
plot(roc_bagging)
abline(a=0, b= 1)

# Random Forest
rfPredR = prediction(rfpred_p[,1], y)
roc_rf = performance(rfPredR, measure = "tpr", x.measure = "fpr")
plot(roc_rf)
abline(a=0, b= 1)

# Neural Network 
nnPredR = prediction(nnPred_p[,1], y)
roc_nn = performance(nnPredR, measure = "tpr", x.measure = "fpr")
plot(roc_nn)
abline(a=0, b= 1)

# Neural Network (Tuned Version) 
nn_tunedPredR = prediction(nn_tunedPred_p[,1], y)
roc_nn_tuned = performance(nn_tunedPredR, measure = "tpr", x.measure = "fpr")
plot(roc_nn_tuned)
abline(a=0, b= 1)

# Stacking (GLM) 
glm_sPredR = prediction(model_preds$ensemble, y)
roc_glm_s = performance(glm_sPredR, measure = "tpr", x.measure = "fpr")
plot(roc_glm_s)
abline(a=0, b= 1)


# Stacking (GBM) 
gbm_sPredR = prediction(model_preds3$ensemble, y)
roc_gbm_s = performance(gbm_sPredR, measure = "tpr", x.measure = "fpr")
plot(roc_gbm_s)
abline(a=0, b= 1)


library(pROC)
library(ggplot2)

# Combine all ROC curves into a data frame --------------------------------
#glm
# Assuming roc_log is a performance object from ROCR
# Extract false positive rate (FPR) and true positive rate (TPR)
fpr_log <- roc_log@x.values[[1]]
tpr_log <- roc_log@y.values[[1]]

# Create a data frame
df_log <- data.frame(method = "logit", FPR = fpr_log, TPR = tpr_log)

#gb
fpr_gb <- roc_gb@x.values[[1]]
tpr_gb <- roc_gb@y.values[[1]]
df_gb <- data.frame(method = "logit", FPR = fpr_gb, TPR = tpr_gb)

#rf
fpr_rf <- roc_rf@x.values[[1]]
tpr_rf <- roc_rf@y.values[[1]]
df_rf <- data.frame(method = "logit", FPR = fpr_rf, TPR = tpr_rf)


#nn
fpr_nn <- roc_nn@x.values[[1]]
tpr_nn <- roc_nn@y.values[[1]]
df_nn <- data.frame(method = "logit", FPR = fpr_nn, TPR = tpr_nn)


#knn
fpr_knn <- roc_knn@x.values[[1]]
tpr_knn <- roc_knn@y.values[[1]]
df_knn <- data.frame(method = "logit", FPR = fpr_knn, TPR = tpr_knn)

#knn
fpr_glm_s <- roc_glm_s@x.values[[1]]
tpr_glm_s <- roc_glm_s@y.values[[1]]
df_glm_s <- data.frame(method = "logit", FPR = fpr_glm_s, TPR = tpr_glm_s)




# Combine all data frames
roc_data <- rbind(df_log, df_gb, df_rf, df_nn, df_knn, df_glm_s)

roc_data <- rbind(
  data.frame(method = "logit", roc_log),
  data.frame(method = "gb", roc_gb),
  data.frame(method = "rf", roc_rf),
  data.frame(method = "nn", roc_nn),
  data.frame(method = "knn", roc_knn),
  data.frame(method = "glmstack", roc_glm_s)
)

# Plot ROC curves ---------------------------------------------------------
ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity, color = method)) +
  geom_line(size = 2) +
  theme_minimal() +  # You can customize the theme as needed
  labs(title = "ROC Curves",
       x = "False Positive Rate",
       y = "True Positive Rate") +
  scale_color_manual(values = c("dodgerblue", "darkorange", "green", "purple", "yellow4", "red")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key.size = unit(1.2, "lines"))





# Assuming roc_log is a performance object with ROC data
# Extract false positive rate, true positive rate, and thresholds
fpr <- roc_log@x.values[[1]]
tpr <- roc_log@y.values[[1]]
thresholds <- roc_log@alpha.values[[1]]

# Create a dataframe
roc_log_df <- data.frame(method = "logit", FPR = fpr, TPR = tpr, Threshold = thresholds)

# Repeat for other methods (roc_gb, roc_rf, etc.) and bind them together





plot(roc_log, col = "dodgerblue", lwd = 2) 
par(new = TRUE)
plot(roc_gb, col = "darkorange", lwd = 2) 
par(new = TRUE)
plot(roc_rf, col = "green", lwd = 2) 
par(new = TRUE)
plot(roc_nn, col = "purple", lwd = 2) 
par(new = TRUE)
plot(roc_knn, col = "yellow4", lwd = 2)
par(new = TRUE)
plot(roc_glm_s, col = "red", lwd = 2) 
par(new = TRUE)


legend("bottomright", legend=c("logit", "gb", "rf", "nn", "knn", "glmstack"),
       col=c("dodgerblue", "darkorange", "green", "purple", "yellow4", "red"),
       lty = 1, cex = 0.7, text.font=4, y.intersp=0.5, x.intersp=0.1, lwd = 3)




# met2 --------------------------------------------------------------------
# Function to extract data from a ROC object
extract_roc_data <- function(roc_obj, method) {
  data.frame(method = method,
             specificity = unlist(roc_obj@x.values),
             sensitivity = unlist(roc_obj@y.values))
}

# Extract data from each ROC object
roc_data_log <- extract_roc_data(roc_log, "logit")
roc_data_gb <- extract_roc_data(roc_gb, "gb")
roc_data_rf <- extract_roc_data(roc_rf, "rf")
roc_data_nn <- extract_roc_data(roc_nn, "nn")
roc_data_knn <- extract_roc_data(roc_knn, "knn")
roc_data_glm_s <- extract_roc_data(roc_glm_s, "glmstack")

# Combine all data into one dataframe
roc_data <- rbind(roc_data_log, roc_data_gb, roc_data_rf, roc_data_nn, roc_data_knn, roc_data_glm_s)

# Plotting the ROC curves using ggplot
library(ggplot2)
ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity, color = method)) +
  geom_line(size = 2) +
  theme_minimal() +
  labs(title = "ROC Curves",
       x = "False Positive Rate",
       y = "True Positive Rate") +
  scale_color_manual(values = c("dodgerblue", "darkorange", "green", "purple", "yellow4", "red")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key.size = unit(1.2, "lines"))


