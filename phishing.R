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
library(pROC)
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

train$status <- factor(train$status, levels = c("phishing", "legitimate"))
test$status <- factor(test$status, levels = c("phishing", "legitimate"))

# True priors
rho1 = 0.5; rho0 = 0.5; true0 = 0.99; true1 = 0.01 

# Distribuzione variabile status ------------------------------------------

class(train$status)
table(train$status) / nrow(train)

# Funzioni per fare i grafici
plot_gg1 = function(column){
  ggplot(data = train, mapping = aes(x = {{column}})) +
    geom_bar(position = 'dodge') +
    scale_fill_manual('Legenda', values = c("lightblue", "blue"))
}

plot_gg = function(column){
  ggplot(data = train, mapping = aes(x = {{column}}, fill = status)) +
    geom_bar(position = 'dodge') +
    scale_fill_manual('Legenda', values = c("lightblue", "blue"))
}

plot_gg(status) + 
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
p7 <- plot_gg1(nb_or)

# Combina i grafici in una griglia
grid.arrange(p1, p2, p3, p4, p5, p6, p7, ncol = 3)

#Eliminazioni delle variabili problematiche
train <- subset(train, select = -c(statistical_report, ratio_nullHyperlinks, nb_or, ratio_intErrors,ratio_intRedirection,submit_email,sfh,url))
score <- subset(score, select = -c(status))


# Model selection ----------------------------------------------------------

set.seed(9999)
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
vars_list <- c(
  "length_url",
  "length_hostname",
  "ip",
  "nb_dots",
  "nb_hyphens",
  "nb_at",
  "nb_qm",
  "nb_and",
  "nb_eq",
  "nb_underscore",
  "nb_percent",
  "nb_slash",
  "nb_colon",
  "nb_semicolumn",
  "nb_space",
  "nb_www",
  "nb_com",
  "nb_dslash",
  "http_in_path",
  "https_token",
  "ratio_digits_url",
  "ratio_digits_host",
  "port",
  "tld_in_path",
  "tld_in_subdomain",
  "abnormal_subdomain",
  "nb_subdomains",
  "prefix_suffix",
  "random_domain",
  "shortening_service",
  "nb_redirection",
  "length_words_raw",
  "char_repeat",
  "shortest_words_raw",
  "shortest_word_host",
  "shortest_word_path",
  "longest_words_raw",
  "longest_word_host",
  "longest_word_path",
  "avg_words_raw",
  "avg_word_host",
  "avg_word_path",
  "phish_hints",
  "domain_in_brand",
  "brand_in_path",
  "suspecious_tld",
  "nb_hyperlinks",
  "ratio_intHyperlinks",
  "ratio_extHyperlinks",
  "nb_extCSS",
  "ratio_extRedirection",
  "ratio_extErrors",
  "login_form",
  "external_favicon",
  "links_in_tags",
  "ratio_intMedia",
  "ratio_extMedia",
  "safe_anchor",
  "empty_title",
  "domain_in_title",
  "domain_with_copyright",
  "whois_registered_domain",
  "domain_registration_length",
  "domain_age",
  "web_traffic",
  "dns_record",
  #"google_index",
  #"page_rank",
  'status'
)

train_selected = train[,vars_list]
train <- subset(train, select = -c(google_index,page_rank))

# Verifica Separation -----------------------------------------------------

# Funzione per creare griglie di grafici 3x3 per tutte le variabili
plot_all_variables_in_grids <- function(data, status_var) {
  # Filtra le variabili da visualizzare (escludendo la variabile status)
  variables <- setdiff(names(data), status_var)
  
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
        ggplot(data, aes_string(x = status_var, y = var)) +
          geom_boxplot() +
          labs(title = paste("Boxplot of", var, "by", status_var))
      } else {
        ggplot(data, aes_string(x = var, fill = status_var)) +
          geom_bar(position = "dodge") +
          labs(title = paste("Barplot of", var, "by", status_var))
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

# google_index
ggplot(train, aes(x = status, fill = google_index)) +
  geom_bar(position = "dodge") 

# page_rank
ggplot(train, aes(x = status, y = page_rank)) +
  geom_boxplot() +
  theme(plot.title = element_text(size = 20), 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12))

# nb_hyperlinks
ggplot(train, aes(x = status, y = nb_hyperlinks)) +
  geom_boxplot() +
  theme(plot.title = element_text(size = 20), 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12))

# Funzione matrice confusione corretta ------------------------------------

adjusted_confusion_matrix <- function(model, test_data, soglia) {
  set.seed(9999)
  glmpred_probs <- predict(model, test_data, type = "prob")
  
  glmpred <- glmpred_probs[, 1]
  
  test_data$pred_y_glm_adj <- ifelse(glmpred > soglia, "phishing", "legitimate")
  test_data$pred_y_glm_adj <- factor(test_data$pred_y_glm_adj, levels = c("phishing", "legitimate"))
  
  conf_mat <- confusionMatrix(test_data$pred_y_glm_adj, test_data$status)
  true_positives <- conf_mat$table[1, 1]
  false_positives <- conf_mat$table[1, 2]
  false_negatives <- conf_mat$table[2, 1]
  true_negatives <- conf_mat$table[2, 2]
  
  TP_adj <- (true1 / rho1) * true_positives
  FP_adj <- (true0 / rho0) * false_positives
  TN_adj <- (true0 / rho0) * true_negatives
  FN_adj <- (true1 / rho1) * false_negatives
  accuracy_adj <- (TP_adj + TN_adj) / (TP_adj + FP_adj + TN_adj + FN_adj)
  
  sensitivity_adj <- TP_adj / (TP_adj + FN_adj)
  specificity_adj <- TN_adj / (TN_adj + FP_adj)
  
  cat("Matrice di Confusione aggiustata:\n",
      "               Predetto\n",
      "Reale   Phishing Legitimate\n",
      "Phishing ", TP_adj, "      ", FN_adj, "\n",
      "Legitimate ", FP_adj, "      ", TN_adj, "\n\n",
      "Accuracy: ", accuracy_adj, "\n",
      "Sensitivity: ", sensitivity_adj, "\n",
      "Specificity: ", specificity_adj, "\n")
}

adjusted_confusion_matrix_stacking<- function(model, test_data, soglia) {
  set.seed(9999)
  glmpred_probs <- predict(model, test_data, type = "prob")
  
  glmpred <- glmpred_probs
  
  test_data$pred_y_glm_adj <- ifelse(glmpred > soglia, "phishing", "legitimate")
  test_data$pred_y_glm_adj <- factor(test_data$pred_y_glm_adj, levels = c("phishing", "legitimate"))
  
  conf_mat <- confusionMatrix(test_data$pred_y_glm_adj, test_data$status)
  true_positives <- conf_mat$table[1, 1]
  false_positives <- conf_mat$table[1, 2]
  false_negatives <- conf_mat$table[2, 1]
  true_negatives <- conf_mat$table[2, 2]
  
  TP_adj <- (true1 / rho1) * true_positives
  FP_adj <- (true0 / rho0) * false_positives
  TN_adj <- (true0 / rho0) * true_negatives
  FN_adj <- (true1 / rho1) * false_negatives
  accuracy_adj <- (TP_adj + TN_adj) / (TP_adj + FP_adj + TN_adj + FN_adj)
  
  sensitivity_adj <- TP_adj / (TP_adj + FN_adj)
  specificity_adj <- TN_adj / (TN_adj + FP_adj)
  
  cat("Matrice di Confusione aggiustata:\n",
      "               Predetto\n",
      "Reale   Phishing Legitimate\n",
      "Phishing ", TP_adj, "      ", FN_adj, "\n",
      "Legitimate ", FP_adj, "      ", TN_adj, "\n\n",
      "Accuracy: ", accuracy_adj, "\n",
      "Sensitivity: ", sensitivity_adj, "\n",
      "Specificity: ", specificity_adj, "\n")
}

# GLM  -----------------------------------------------
set.seed(9999)
ctrl = trainControl(method = "cv",
                    number = 10,
                    search = "grid", 
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary) 

glm = train(status ~ ., 
            data = train_selected, 
            method = "glm", 
            preProcess = c("corr", "nzv"),
            trControl = ctrl,
            tuneLength = 5, 
            trace = TRUE,
            metric = "Sens") 

glm 

glmpred = predict(glm, test)
glmpred_p = predict(glm, test, type = c("prob"))

confusionMatrix(glmpred, test$status)

adjusted_confusion_matrix(glm, test, 0.5)
adjusted_confusion_matrix(glm, train, 0.5)

# K-Nearest Neightbour ----------------------------------------------------

set.seed(9999)
ctrl = trainControl(method = "cv", 
                    number = 10,
                    search = "grid",
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary) 
grid = expand.grid(k = seq(5, 20, 3))
knn = train(status ~ ., 
            data = train_selected,
            method = "knn",
            trControl = ctrl,
            tuneLength = 10, 
            tuneGrid = grid,
            preProcess = c("center", "scale", "corr", "nzv"), 
            metric = "Sens") 
knn

knnpred = predict(knn, test)
knnpred_p = predict(knn, test, type = c("prob"))
adjusted_confusion_matrix(knn, test, 0.5)
adjusted_confusion_matrix(knn, train, 0.5)

# LASSO -------------------------------------------------------------------

set.seed(9999)
ctrl = trainControl(method = "cv", 
                    number = 10,
                    classProbs = TRUE,
                    search = "grid", 
                    summaryFunction = twoClassSummary)
grid = expand.grid(.alpha = 1,
                   .lambda = seq(0, 1, by = 0.01))
lasso = train(status ~ ., 
              data = train, 
              method = "glmnet",
              family = "binomial",
              tuneGrid = grid,
              preProcess = c("corr", "nzv","center", "scale"), 
              metric = "Sens",
              trControl = ctrl,
              tuneLength = 10) 
lasso

lassopred = predict(lasso, test)
lassopred_p = predict(lasso, test, type = c("prob"))

adjusted_confusion_matrix(lasso, test, 0.5)
adjusted_confusion_matrix(lasso, train, 0.5)

# PLS ---------------------------------------------------------------------

set.seed(9999)
ctrl = trainControl(method = "cv", 
                    number = 10,
                    search = "grid", 
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary)

pls = train(status ~ .,
            data = train, 
            method = "pls",
            preProcess = c("corr", "nzv","center", "scale"), 
            metric = "Sens",
            trControl = ctrl,
            tuneLength = 10) 

pls

plspred = predict(pls, test)
plspred_p = predict(pls, test, type = c("prob"))


adjusted_confusion_matrix(pls, test, 0.5)
adjusted_confusion_matrix(pls, train, 0.5)

# Naive Bayes -------------------------------------------------------------
set.seed(9999)
ctrl = trainControl(method = "cv", 
                    number = 10,
                    search = "grid", 
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary)

naivebayes = train(status ~ ., 
                   data = train_selected, 
                   method = "naive_bayes",
                   trControl = ctrl,
                   tuneLength = 10,
                   preProcess = c("corr", "nzv"), 
                   metric = "Sens")

naivebayes

nbpred = predict(naivebayes, test)
nbpred_p = predict(naivebayes, test, type = c("prob"))

adjusted_confusion_matrix(naivebayes, test, 0.5)
adjusted_confusion_matrix(naivebayes, train, 0.5)

# Decision Tree ----------------------------------------
tree_rpart = rpart(status ~ ., 
                   data = train, 
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

confusionMatrix(treePred_pruned, test$status)

adjusted_confusion_matrix(tree_pruned, test, 0.5)
adjusted_confusion_matrix(tree_pruned, train, 0.5)

# Bagging -----------------------------------------------------------------
set.seed(9999)
ctrl = trainControl(method = "cv",
                    number = 10,
                    searc = "grid", 
                    summaryFunction = twoClassSummary, 
                    classProbs = TRUE, savePredictions = TRUE)

bagging = train(status ~ ., 
                data = train, 
                method = "treebag",
                ntree = 250,
                trControl = ctrl)

bagging

baggpred = predict(bagging, test)
baggpred_p = predict(bagging, test, type = c("prob"))

confusionMatrix(baggpred, test$status)

adjusted_confusion_matrix(bagging, test, 0.5)
adjusted_confusion_matrix(bagging, train, 0.5)

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

gb = train(status ~., 
           data = train, 
           method = "gbm",
           tuneLength = 10,
           metric ="Sens",
           tuneGrid = gbm_tune,
           trControl = ctrl)

gb

gbpred = predict(gb, test)
gbpred_p = predict(gb, test, type = c("prob"))

adjusted_confusion_matrix(gb, test, 0.5)
adjusted_confusion_matrix(gb, train, 0.5)

# True priors
gb_old_pr1 = predict(gb, test, "prob")[,1] 
pred_r1_gb<-as.numeric(gb_old_pr1) 
pred_r0_gb = 1 - pred_r1_gb 
den_gb = pred_r1_gb*(true1/rho1)+pred_r0_gb*(true0/rho0) 
pred1_true_gb = pred_r1_gb*(true1/rho1)/den_gb 
pred0_true_gb = pred_r0_gb*(true0/rho0)/den_gb

# Random Forest -----------------------------------------------------------

set.seed(9999)
ctrl <- trainControl(method = "cv",
                     number = 10,
                     search = "grid",
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

rf <- train(status ~ ., 
            data = train, 
            method = "rf",
            metric = "Sens",
            tuneLength = 10,
            trControl = ctrl,
            verbose = FALSE) 

rf
plot(rf)

rfpred = predict(rf, test)
rfpred_p = predict(rf, test, type = c("prob"))


adjusted_confusion_matrix(rf, test, 0.5)
adjusted_confusion_matrix(rf, train, 0.5)

# True priors
rf_old_pr1 = predict(rf, test, "prob")[,1] 
pred_r1_rf <-as.numeric(rf_old_pr1) 
pred_r0_rf = 1 - pred_r1_rf
den_rf = pred_r1_rf*(true1/rho1)+pred_r0_rf*(true0/rho0) 
pred1_true_rf = pred_r1_rf*(true1/rho1)/den_rf 
pred0_true_rf = pred_r0_rf*(true0/rho0)/den_rf

# NN ----------------------------------------------------------------------

cvCtrl = trainControl(method = "boot", 
                      number=10,
                      searc="grid", 
                      summaryFunction = twoClassSummary, 
                      classProbs = TRUE)

#nn 
nn = train(status ~., data=train_selected,
           method = "nnet",
           preProcess = c("scale", "corr", "nzv"), 
           tuneLength = 5,
           metric="Sens",
           trControl=cvCtrl, trace = TRUE,
           maxit = 100)

plot(nn)
print(nn)
getTrainPerf(nn)

nnPred_p = predict(nn, test, type = c("prob"))
nnPred = predict(nn,test)

adjusted_confusion_matrix(nn, test, 0.5)
adjusted_confusion_matrix(nn, train, 0.5)

#nn tuned
cvCtrl = trainControl(method = "cv", 
                      number=10, 
                      searc="grid", 
                      summaryFunction = twoClassSummary, 
                      classProbs = TRUE)

tunegrid = expand.grid(size=c(1:5), decay = c(0.001, 0.01, 0.05 , .1, .3))

nn_tuned = train(status ~., data=train_selected,
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

nn_tunedPred_p = predict(nn_tuned, test, type = c("prob"))
nn_tunedPred = predict(nn_tuned, test)

confusionMatrix(nn_tunedPred, test$status)

adjusted_confusion_matrix(nn_tuned, test, 0.5)
adjusted_confusion_matrix(nn_tuned, train, 0.5)

# Stacking ----------------------------------------------------------------
#GLM

cvCtrl = trainControl(method = "cv",
                      number=10,
                      searc="grid",
                      summaryFunction = twoClassSummary,
                      classProbs = TRUE)

model_list = caretList(status ~.,
                       data = train,
                       trControl = cvCtrl,
                       methodList = c("glm", "knn", "naive_bayes","rf")
 )

# modelli <- list("rf"=rf, "glm"=glm, "nne"=nn, "naive_bayes"=naivebayes, "knn"=knn)
# class(modelli) <- "caretList"
 
glm_ensemble = caretStack(
  model_list,
  method="glm",
  metric="Sens",
  trControl = cvCtrl
)

set.seed(42)
model_preds = lapply(model_list, predict, newdata = test, type="prob")
model_preds2 = model_preds
model_preds$ensemble = predict(glm_ensemble, newdata = test, type="prob" )
model_preds2$ensemble = predict(glm_ensemble, newdata = test)
CF = coef(glm_ensemble$ens_model$finalModel)[-1]
#colAUC(model_preds$ensemble, test_selected$status)
#confusionMatrix(model_preds2$ensemble, test_selected$status)

#confusionMatrix(model_preds2$ensemble, test$status)

adjusted_confusion_matrix_stacking(glm_ensemble, test, 0.5)
adjusted_confusion_matrix_stacking(glm_ensemble, train, 0.5)
# True priors
set.seed(42)
ens_old_pr1 = model_preds$ensemble
pred_r1_stack<-as.numeric(ens_old_pr1) 
pred_r0 = 1 - pred_r1_stack 
den_stack = pred_r1_stack*(true1/rho1)+pred_r0*(true0/rho0) 
pred1_true_ens = pred_r1_stack*(true1/rho1)/den_stack 
pred0_true_ens = pred_r0*(true0/rho0)/den_stack

# Confronto modelli ROC ------------------------------------------------------------------

# Curve Roc

#GLM 
y = test$status
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
roc_data_nnt <- extract_roc_data(roc_nn_tuned, "nnt")
roc_data_knn <- extract_roc_data(roc_knn, "knn")
roc_data_bag <- extract_roc_data(roc_bagging, "bagg")
roc_data_lasso <- extract_roc_data(roc_lasso, "lasso")
roc_data_pls <- extract_roc_data(roc_pls, "pls")
roc_data_glm_s <- extract_roc_data(roc_glm_s, "glmstack")

# Combine all data into one dataframe
roc_data <- rbind(roc_data_log, roc_data_gb, roc_data_rf, roc_data_nnt, roc_data_knn, roc_data_bag,roc_data_lasso,roc_data_pls,roc_data_glm_s)

# Plot ROC curves 
ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity, color = method)) +
  geom_line(size = 1.2) +
  theme_minimal() + 
  labs(title = "ROC Curves",
       x = "False Positive Rate",
       y = "True Positive Rate") +
  scale_color_manual(values = c("yellow", "darkorange", "green", "purple", "yellow4", "red",'pink','dodgerblue','darkblue')) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key.size = unit(1.2, "lines"))

# met2
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
plot(roc_lasso, col = "pink", lwd = 2) 
par(new = TRUE)
plot(roc_pls, col = "yellow", lwd = 2) 
par(new = TRUE)
plot(roc_bagging, col = "darkblue", lwd = 2) 
par(new = TRUE)

legend("bottomright", legend=c("logit", "gb", "rf", "nn", "knn", "glmstack","Lasso","Pls","Bagging"),
       col=c("dodgerblue", "darkorange", "green", "purple", "yellow4", "red","pink","yellow",'darkblue'),
       lty = 1, cex = 0.7, text.font=4, y.intersp=0.5, x.intersp=0.1, lwd = 3)

# AUC ---------------------------------------------------------------------

AUClogit = performance(glmpredR, measure = "auc")
AUCgb = performance(gbPredR, measure = "auc")
AUCrf = performance(rfPredR, measure = "auc")
AUCnn = performance(nnPredR, measure = "auc")
AUCknn = performance(knnpredR, measure = "auc")
AUCglmstack = performance(glm_sPredR, measure = "auc")
AUCLasso = performance(lassoPredR, measure = "auc")
AUCPls = performance(plsPredR, measure = "auc")
AUCBagging = performance(baggingPredR, measure = "auc")

AUClogit = AUClogit@y.values[[1]]
AUCgb = AUCgb@y.values[[1]]
AUCrf = AUCrf@y.values[[1]]
AUCnn = AUCnn@y.values[[1]]
AUCknn = AUCknn@y.values[[1]]
AUCglmstack = AUCglmstack@y.values[[1]]
AUCLasso = AUCLasso@y.values[[1]]
AUCPls = AUCPls@y.values[[1]]
AUCBagging = AUCBagging@y.values[[1]]

# Dati delle AUC
AUC_values <- c(AUClogit, AUCgb, AUCrf, AUCnn, AUCknn, AUCglmstack, AUCLasso, AUCPls, AUCBagging)
Algoritmi <- c("Logit", "Gradient Boosting", "Random Forest", "Neural Network", "KNN", "GLM Stack", "Lasso", "PLS", "Bagging")

# Crea un dataframe
dfAUC <- data.frame(Algoritmi, AUC_values)

# Crea l'istogramma con ggplot2
histAUC <- ggplot(dfAUC, aes(x = Algoritmi, y = AUC_values, fill = Algoritmi)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Confronto delle AUC tra i modelli",
       x = "Algoritmo",
       y = "AUC") +
  theme_minimal() +
  theme(legend.position="top") +
  guides(fill=guide_legend(title="Algoritmo")) +
  coord_cartesian(ylim = c(0.5, 1))

histAUC

# Confronto modelli LIFT curve --------------------------------------------------------------------

copy = test
colnames(copy)[colnames(copy)=="status"] <- "status" #va lasciata, altrimenti il nome status porta ad un conflitto con gain_lift

# LIFT - Gradient Boosting 

copy$gb = pred0_true_gb
gain_lift(data = copy, score = 'gb', target = 'status')


# LIFT - STACKING (GLM) 
copy$glm_s = pred0_true_ens
gain_lift(data = copy, score='glm_s', target='status')


# LIFT - RANDOM FOREST

copy$rf= pred0_true_rf
gain_lift(data = copy, score='rf', target='status')

# Step 3 ------------------------------------------------------------------

# Random Forest
y = test$status
rfPredR = prediction(rfpred_p[,1], y)
predRoc <- rfPredR

# Calcolo delle performance
acc.perf = performance(predRoc, measure = "acc")
spec.perf = performance(predRoc, measure = "spec")
sens.perf = performance(predRoc, measure = "sens")

# Estrazione dei valori per il plot
acc.values = acc.perf@y.values[[1]]
spec.values = spec.perf@y.values[[1]]
sens.values = sens.perf@y.values[[1]]
thresholds = acc.perf@x.values[[1]]

# Creazione del grafico
plot(thresholds, spec.values, type="l", col="dodgerblue", 
     main="Performance Metrics vs Threshold", xlab="Threshold", ylab="Metrics",
     ylim=range(c(acc.values, spec.values, sens.values)))
#lines(thresholds, spec.values, col="darkorange")
lines(thresholds, sens.values, col="green")

# Aggiunta della legenda
legend("bottomright", legend=c("Specificity", "Sensitivity"),
       col=c("dodgerblue", "green"),
       lty=1, cex=0.8, text.font=4, y.intersp=1.2, x.intersp=1.2, lwd=2)
abline(v = 0.4, lwd = 2, col = "red") 
text(x = 0.6, y = 0.5, labels = "Optimal Threshold", col = "red") 

adjusted_confusion_matrix(rf, test, 0.4)

# Importanza delle variabili
Vimportance <- varImp(rf)
plot(Vimportance, top=10)

# Step 4 ------------------------------------------------------------------

rf_old_pr1_score = predict(rf, score, "prob")[,1] 
pred_r1_score<-as.numeric(rf_old_pr1_score) 
pred_r0_score = 1 - pred_r1_score 
den = pred_r1_score*(true1/rho1)+pred_r0_score*(true0/rho0) 
pred1_true_rf_score = pred_r1_score*(true1/rho1)/den 
pred0_true_rf_score = pred_r0_score*(true0/rho0)/den
score$pred_y=ifelse(pred1_true_rf_score>0.4, "phishing","legitimate")
head(score)

# Grafico
ggplot(score, aes(x = pred_y, fill = pred_y)) +
  geom_bar(width = 0.5) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, position = position_stack(vjust = 0.5)) +
  theme_minimal() 