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

# Combina i grafici in una griglia
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)

#Eliminazioni delle variabili problematiche
train <- subset(train, select = -c(statistical_report, ratio_nullHyperlinks, ratio_intErrors,ratio_intRedirection,submit_email,sfh,url, google_index ))
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
  "page_rank",'status'
)


# Scelta delle features da mantenere in analisi
#selected20 = c("status","google_index", "page_rank", "nb_hyperlinks", "domain_age", "web_traffic", "nb_www", 
#             "phish_hints", "length_url", "longest_word_path", "length_hostname", "nb_hyphens", "ratio_intHyperlinks","safe_anchor","domain_registration_length","length_words_raw", "longest_words_raw","ratio_extHyperlinks","ratio_digits_host","nb_slash","avg_word_path")

train_selected = train[,vars_list]

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

# Addestramento e Tuning Modelli 
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

# Train
# True priors
glm_old_pr1_tr = predict(glm, train, "prob")[,1] 
pred_r1_glm_tr<-as.numeric(glm_old_pr1_tr) 
pred_r0_glm_tr = 1 - pred_r1_glm_tr 
den = pred_r1_glm_tr*(true1/rho1)+pred_r0_glm_tr*(true0/rho0) 
pred1_true_glm_tr = pred_r1_glm_tr*(true1/rho1)/den 
pred0_true_glm_tr = pred_r0_glm_tr*(true0/rho0)/den

#Adjusted
train$pred_y_glm_adj_tr=ifelse(pred1_true_glm_tr>0.4, "phishing","legitimate")
train$pred_y_glm_adj_tr <- factor(train$pred_y_glm_adj_tr, levels = c("phishing", "legitimate"))
TP_glm_tr <- sum(train$status == "phishing" & train$pred_y_glm_adj_tr == "phishing")
TP_adj_glm_tr <- (true0/rho0)*TP_glm_tr
FP_glm_tr <- sum(train$status == "legitimate" & train$pred_y_glm_adj_tr == "phishing")
FP_adj_glm_tr <- (true0/rho0)*FP_glm_tr
TN_glm_tr <- sum(train$status == "legitimate" & train$pred_y_glm_adj_tr == "legitimate")
TN_adj_glm_tr <- (true1/rho1)*TN_glm_tr
FN_glm_tr <- sum(train$status == "phishing" & train$pred_y_glm_adj_tr == "legitimate")
FN_adj_glm_tr <- (true1/rho1)*FN_glm_tr
accuracy_adj_glm_tr <- (TP_adj_glm_tr + TN_adj_glm_tr) / (TP_adj_glm_tr + FP_adj_glm_tr + TN_adj_glm_tr + FN_adj_glm_tr)

# Stampa della matrice di confusione e delle metriche
cat("Matrice di Confusione aggiustata:\n",
    "               Predetto\n",
    "Reale   Phishing Legitimate\n",
    "Phishing ", TP_adj_glm_tr, "      ", FN_adj_glm_tr, "\n",
    "Legitimate ", FP_adj_glm_tr, "      ", TN_adj_glm_tr, "\n\n",
    "Accuracy: ", accuracy_adj_glm_tr, "\n")

# Test
# True priors
glm_old_pr1 = predict(glm, test, "prob")[,1] 
pred_r1<-as.numeric(glm_old_pr1) 
pred_r0 = 1 - pred_r1 
den = pred_r1*(true1/rho1)+pred_r0*(true0/rho0) 
pred1_true_glm = pred_r1*(true1/rho1)/den 
pred0_true_glm = pred_r0*(true0/rho0)/den

#Adjusted
test$pred_y_glm_adj=ifelse(pred1_true_glm>0.4, "phishing","legitimate")
test$pred_y_glm_adj <- factor(test$pred_y_glm_adj, levels = c("phishing", "legitimate"))
TP_glm <- sum(test$status == "phishing" & test$pred_y_glm_adj == "phishing")
TP_adj_glm <- (true0/rho0)*TP_glm
FP_glm <- sum(test$status == "legitimate" & test$pred_y_glm_adj == "phishing")
FP_adj_glm <- (true0/rho0)*FP_glm
TN_glm <- sum(test$status == "legitimate" & test$pred_y_glm_adj == "legitimate")
TN_adj_glm <- (true1/rho1)*TN_glm
FN_glm <- sum(test$status == "phishing" & test$pred_y_glm_adj == "legitimate")
FN_adj_glm <- (true1/rho1)*FN_glm
accuracy_adj_glm <- (TP_adj_glm + TN_adj_glm) / (TP_adj_glm + FP_adj_glm + TN_adj_glm + FN_adj_glm)

# Stampa della matrice di confusione e delle metriche
cat("Matrice di Confusione aggiustata:\n",
    "               Predetto\n",
    "Reale   Phishing Legitimate\n",
    "Phishing ", TP_adj_glm, "      ", FN_adj_glm, "\n",
    "Legitimate ", FP_adj_glm, "      ", TN_adj_glm, "\n\n",
    "Accuracy: ", accuracy_adj_glm, "\n")

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


# Train
# True priors
knn_old_pr1_tr = predict(knn, train, "prob")[,1] 
pred_r1_knn_tr<-as.numeric(knn_old_pr1_tr) 
pred_r0_knn_tr = 1 - pred_r1_knn_tr 
den = pred_r1_knn_tr*(true1/rho1)+pred_r0_knn_tr*(true0/rho0) 
pred1_true_knn_tr = pred_r1_knn_tr*(true1/rho1)/den 
pred0_true_knn_tr = pred_r0_knn_tr*(true0/rho0)/den

#Adjusted
train$pred_y_knn_adj_tr=ifelse(pred1_true_knn_tr>0.4, "phishing","legitimate")
train$pred_y_knn_adj_tr <- factor(train$pred_y_knn_adj_tr, levels = c("phishing", "legitimate"))
TP_knn_tr <- sum(train$status == "phishing" & train$pred_y_knn_adj_tr == "phishing")
TP_adj_knn_tr <- (true0/rho0)*TP_knn_tr
FP_knn_tr <- sum(train$status == "legitimate" & train$pred_y_knn_adj_tr == "phishing")
FP_adj_knn_tr <- (true0/rho0)*FP_knn_tr
TN_knn_tr <- sum(train$status == "legitimate" & train$pred_y_knn_adj_tr == "legitimate")
TN_adj_knn_tr <- (true1/rho1)*TN_knn_tr
FN_knn_tr <- sum(train$status == "phishing" & train$pred_y_knn_adj_tr == "legitimate")
FN_adj_knn_tr <- (true1/rho1)*FN_knn_tr
accuracy_adj_knn_tr <- (TP_adj_knn_tr + TN_adj_knn_tr) / (TP_adj_knn_tr + FP_adj_knn_tr + TN_adj_knn_tr + FN_adj_knn_tr)

# Stampa della matrice di confusione e delle metriche
cat("Matrice di Confusione aggiustata:\n",
    "               Predetto\n",
    "Reale   Phishing Legitimate\n",
    "Phishing ", TP_adj_knn_tr, "      ", FN_adj_knn_tr, "\n",
    "Legitimate ", FP_adj_knn_tr, "      ", TN_adj_knn_tr, "\n\n",
    "Accuracy: ", accuracy_adj_knn_tr, "\n")

# Test
# True priors
knn_old_pr1 = predict(knn, test, "prob")[,1] 
pred_r1<-as.numeric(knn_old_pr1) 
pred_r0 = 1 - pred_r1 
den = pred_r1*(true1/rho1)+pred_r0*(true0/rho0) 
pred1_true_knn = pred_r1*(true1/rho1)/den 
pred0_true_knn = pred_r0*(true0/rho0)/den

#Adjusted
test$pred_y_knn_adj=ifelse(pred1_true_knn>0.4, "phishing","legitimate")
test$pred_y_knn_adj <- factor(test$pred_y_knn_adj, levels = c("phishing", "legitimate"))
TP_knn <- sum(test$status == "phishing" & test$pred_y_knn_adj == "phishing")
TP_adj_knn <- (true0/rho0)*TP_knn
FP_knn <- sum(test$status == "legitimate" & test$pred_y_knn_adj == "phishing")
FP_adj_knn <- (true0/rho0)*FP_knn
TN_knn <- sum(test$status == "legitimate" & test$pred_y_knn_adj == "legitimate")
TN_adj_knn <- (true1/rho1)*TN_knn
FN_knn <- sum(test$status == "phishing" & test$pred_y_knn_adj == "legitimate")
FN_adj_knn <- (true1/rho1)*FN_knn
accuracy_adj_knn <- (TP_adj_knn + TN_adj_knn) / (TP_adj_knn + FP_adj_knn + TN_adj_knn + FN_adj_knn)

# Stampa della matrice di confusione e delle metriche
cat("Matrice di Confusione aggiustata:\n",
    "               Predetto\n",
    "Reale   Phishing Legitimate\n",
    "Phishing ", TP_adj_knn, "      ", FN_adj_knn, "\n",
    "Legitimate ", FP_adj_knn, "      ", TN_adj_knn, "\n\n",
    "Accuracy: ", accuracy_adj_knn, "\n")

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
              data = numeric_train, 
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

# Train
# True priors
lasso_old_pr1_tr = predict(lasso, train, "prob")[,1] 
pred_r1_lasso_tr<-as.numeric(lasso_old_pr1_tr) 
pred_r0_lasso_tr = 1 - pred_r1_lasso_tr 
den = pred_r1_lasso_tr*(true1/rho1)+pred_r0_lasso_tr*(true0/rho0) 
pred1_true_lasso_tr = pred_r1_lasso_tr*(true1/rho1)/den 
pred0_true_lasso_tr = pred_r0_lasso_tr*(true0/rho0)/den

#Adjusted
train$pred_y_lasso_adj_tr=ifelse(pred1_true_lasso_tr>0.4, "phishing","legitimate")
train$pred_y_lasso_adj_tr <- factor(train$pred_y_lasso_adj_tr, levels = c("phishing", "legitimate"))
TP_lasso_tr <- sum(train$status == "phishing" & train$pred_y_lasso_adj_tr == "phishing")
TP_adj_lasso_tr <- (true0/rho0)*TP_lasso_tr
FP_lasso_tr <- sum(train$status == "legitimate" & train$pred_y_lasso_adj_tr == "phishing")
FP_adj_lasso_tr <- (true0/rho0)*FP_lasso_tr
TN_lasso_tr <- sum(train$status == "legitimate" & train$pred_y_lasso_adj_tr == "legitimate")
TN_adj_lasso_tr <- (true1/rho1)*TN_lasso_tr
FN_lasso_tr <- sum(train$status == "phishing" & train$pred_y_lasso_adj_tr == "legitimate")
FN_adj_lasso_tr <- (true1/rho1)*FN_lasso_tr
accuracy_adj_lasso_tr <- (TP_adj_lasso_tr + TN_adj_lasso_tr) / (TP_adj_lasso_tr + FP_adj_lasso_tr + TN_adj_lasso_tr + FN_adj_lasso_tr)

# Stampa della matrice di confusione e delle metriche
cat("Matrice di Confusione aggiustata:\n",
    "               Predetto\n",
    "Reale   Phishing Legitimate\n",
    "Phishing ", TP_adj_lasso_tr, "      ", FN_adj_lasso_tr, "\n",
    "Legitimate ", FP_adj_lasso_tr, "      ", TN_adj_lasso_tr, "\n\n",
    "Accuracy: ", accuracy_adj_lasso_tr, "\n")

# Test
# True priors
lasso_old_pr1 = predict(lasso, test, "prob")[,1] 
pred_r1<-as.numeric(lasso_old_pr1) 
pred_r0 = 1 - pred_r1 
den = pred_r1*(true1/rho1)+pred_r0*(true0/rho0) 
pred1_true_lasso = pred_r1*(true1/rho1)/den 
pred0_true_lasso = pred_r0*(true0/rho0)/den

#Adjusted
test$pred_y_lasso_adj=ifelse(pred1_true_lasso>0.4, "phishing","legitimate")
test$pred_y_lasso_adj <- factor(test$pred_y_lasso_adj, levels = c("phishing", "legitimate"))
TP_lasso <- sum(test$status == "phishing" & test$pred_y_lasso_adj == "phishing")
TP_adj_lasso <- (true0/rho0)*TP_lasso
FP_lasso <- sum(test$status == "legitimate" & test$pred_y_lasso_adj == "phishing")
FP_adj_lasso <- (true0/rho0)*FP_lasso
TN_lasso <- sum(test$status == "legitimate" & test$pred_y_lasso_adj == "legitimate")
TN_adj_lasso <- (true1/rho1)*TN_lasso
FN_lasso <- sum(test$status == "phishing" & test$pred_y_lasso_adj == "legitimate")
FN_adj_lasso <- (true1/rho1)*FN_lasso
accuracy_adj_lasso <- (TP_adj_lasso + TN_adj_lasso) / (TP_adj_lasso + FP_adj_lasso + TN_adj_lasso + FN_adj_lasso)

# Stampa della matrice di confusione e delle metriche
cat("Matrice di Confusione aggiustata:\n",
    "               Predetto\n",
    "Reale   Phishing Legitimate\n",
    "Phishing ", TP_adj_lasso, "      ", FN_adj_lasso, "\n",
    "Legitimate ", FP_adj_lasso, "      ", TN_adj_lasso, "\n\n",
    "Accuracy: ", accuracy_adj_lasso, "\n")

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

# Train
# True priors
pls_old_pr1_tr = predict(pls, train, "prob")[,1] 
pred_r1_pls_tr<-as.numeric(pls_old_pr1_tr) 
pred_r0_pls_tr = 1 - pred_r1_pls_tr 
den = pred_r1_pls_tr*(true1/rho1)+pred_r0_pls_tr*(true0/rho0) 
pred1_true_pls_tr = pred_r1_pls_tr*(true1/rho1)/den 
pred0_true_pls_tr = pred_r0_pls_tr*(true0/rho0)/den

#Adjusted
train$pred_y_pls_adj_tr=ifelse(pred1_true_pls_tr>0.4, "phishing","legitimate")
train$pred_y_pls_adj_tr <- factor(train$pred_y_pls_adj_tr, levels = c("phishing", "legitimate"))
TP_pls_tr <- sum(train$status == "phishing" & train$pred_y_pls_adj_tr == "phishing")
TP_adj_pls_tr <- (true0/rho0)*TP_pls_tr
FP_pls_tr <- sum(train$status == "legitimate" & train$pred_y_pls_adj_tr == "phishing")
FP_adj_pls_tr <- (true0/rho0)*FP_pls_tr
TN_pls_tr <- sum(train$status == "legitimate" & train$pred_y_pls_adj_tr == "legitimate")
TN_adj_pls_tr <- (true1/rho1)*TN_pls_tr
FN_pls_tr <- sum(train$status == "phishing" & train$pred_y_pls_adj_tr == "legitimate")
FN_adj_pls_tr <- (true1/rho1)*FN_pls_tr
accuracy_adj_pls_tr <- (TP_adj_pls_tr + TN_adj_pls_tr) / (TP_adj_pls_tr + FP_adj_pls_tr + TN_adj_pls_tr + FN_adj_pls_tr)

# Stampa della matrice di confusione e delle metriche
cat("Matrice di Confusione aggiustata:\n",
    "               Predetto\n",
    "Reale   Phishing Legitimate\n",
    "Phishing ", TP_adj_pls_tr, "      ", FN_adj_pls_tr, "\n",
    "Legitimate ", FP_adj_pls_tr, "      ", TN_adj_pls_tr, "\n\n",
    "Accuracy: ", accuracy_adj_pls_tr, "\n")

# Test
# True priors
pls_old_pr1 = predict(pls, test, "prob")[,1] 
pred_r1<-as.numeric(pls_old_pr1) 
pred_r0 = 1 - pred_r1 
den = pred_r1*(true1/rho1)+pred_r0*(true0/rho0) 
pred1_true_pls = pred_r1*(true1/rho1)/den 
pred0_true_pls = pred_r0*(true0/rho0)/den

#Adjusted
test$pred_y_pls_adj=ifelse(pred1_true_pls>0.4, "phishing","legitimate")
test$pred_y_pls_adj <- factor(test$pred_y_pls_adj, levels = c("phishing", "legitimate"))
TP_pls <- sum(test$status == "phishing" & test$pred_y_pls_adj == "phishing")
TP_adj_pls <- (true0/rho0)*TP_pls
FP_pls <- sum(test$status == "legitimate" & test$pred_y_pls_adj == "phishing")
FP_adj_pls <- (true0/rho0)*FP_pls
TN_pls <- sum(test$status == "legitimate" & test$pred_y_pls_adj == "legitimate")
TN_adj_pls <- (true1/rho1)*TN_pls
FN_pls <- sum(test$status == "phishing" & test$pred_y_pls_adj == "legitimate")
FN_adj_pls <- (true1/rho1)*FN_pls
accuracy_adj_pls <- (TP_adj_pls + TN_adj_pls) / (TP_adj_pls + FP_adj_pls + TN_adj_pls + FN_adj_pls)

# Stampa della matrice di confusione e delle metriche
cat("Matrice di Confusione aggiustata:\n",
    "               Predetto\n",
    "Reale   Phishing Legitimate\n",
    "Phishing ", TP_adj_pls, "      ", FN_adj_pls, "\n",
    "Legitimate ", FP_adj_pls, "      ", TN_adj_pls, "\n\n",
    "Accuracy: ", accuracy_adj_pls, "\n")

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

nbpred = predict(naivebayes, test_selected)
nbpred_p = predict(naivebayes, test_selected, type = c("prob"))

# Train
# True priors
nb_old_pr1_tr = predict(naivebayes, train, "prob")[,1] 
pred_r1_nb_tr<-as.numeric(nb_old_pr1_tr) 
pred_r0_nb_tr = 1 - pred_r1_nb_tr 
den = pred_r1_nb_tr*(true1/rho1)+pred_r0_nb_tr*(true0/rho0) 
pred1_true_nb_tr = pred_r1_nb_tr*(true1/rho1)/den 
pred0_true_nb_tr = pred_r0_nb_tr*(true0/rho0)/den

#Adjusted
train$pred_y_nb_adj_tr=ifelse(pred1_true_nb_tr>0.4, "phishing","legitimate")
train$pred_y_nb_adj_tr <- factor(train$pred_y_nb_adj_tr, levels = c("phishing", "legitimate"))
TP_nb_tr <- sum(train$status == "phishing" & train$pred_y_nb_adj_tr == "phishing")
TP_adj_nb_tr <- (true0/rho0)*TP_nb_tr
FP_nb_tr <- sum(train$status == "legitimate" & train$pred_y_nb_adj_tr == "phishing")
FP_adj_nb_tr <- (true0/rho0)*FP_nb_tr
TN_nb_tr <- sum(train$status == "legitimate" & train$pred_y_nb_adj_tr == "legitimate")
TN_adj_nb_tr <- (true1/rho1)*TN_nb_tr
FN_nb_tr <- sum(train$status == "phishing" & train$pred_y_nb_adj_tr == "legitimate")
FN_adj_nb_tr <- (true1/rho1)*FN_nb_tr
accuracy_adj_nb_tr <- (TP_adj_nb_tr + TN_adj_nb_tr) / (TP_adj_nb_tr + FP_adj_nb_tr + TN_adj_nb_tr + FN_adj_nb_tr)

# Stampa della matrice di confusione e delle metriche
cat("Matrice di Confusione aggiustata:\n",
    "               Predetto\n",
    "Reale   Phishing Legitimate\n",
    "Phishing ", TP_adj_nb_tr, "      ", FN_adj_nb_tr, "\n",
    "Legitimate ", FP_adj_nb_tr, "      ", TN_adj_nb_tr, "\n\n",
    "Accuracy: ", accuracy_adj_nb_tr, "\n")

# Test
# True priors
nb_old_pr1 = predict(naivebayes, test, "prob")[,1] 
pred_r1<-as.numeric(nb_old_pr1) 
pred_r0 = 1 - pred_r1 
den = pred_r1*(true1/rho1)+pred_r0*(true0/rho0) 
pred1_true_nb = pred_r1*(true1/rho1)/den 
pred0_true_nb = pred_r0*(true0/rho0)/den

#Adjusted
test$pred_y_nb_adj=ifelse(pred1_true_nb>0.4, "phishing","legitimate")
test$pred_y_nb_adj <- factor(test$pred_y_nb_adj, levels = c("phishing", "legitimate"))
TP_nb <- sum(test$status == "phishing" & test$pred_y_nb_adj == "phishing")
TP_adj_nb <- (true0/rho0)*TP_nb
FP_nb <- sum(test$status == "legitimate" & test$pred_y_nb_adj == "phishing")
FP_adj_nb <- (true0/rho0)*FP_nb
TN_nb <- sum(test$status == "legitimate" & test$pred_y_nb_adj == "legitimate")
TN_adj_nb <- (true1/rho1)*TN_nb
FN_nb <- sum(test$status == "phishing" & test$pred_y_nb_adj == "legitimate")
FN_adj_nb <- (true1/rho1)*FN_nb
accuracy_adj_nb <- (TP_adj_nb + TN_adj_nb) / (TP_adj_nb + FP_adj_nb + TN_adj_nb + FN_adj_nb)

# Stampa della matrice di confusione e delle metriche
cat("Matrice di Confusione aggiustata:\n",
    "               Predetto\n",
    "Reale   Phishing Legitimate\n",
    "Phishing ", TP_adj_nb, "      ", FN_adj_nb, "\n",
    "Legitimate ", FP_adj_nb, "      ", TN_adj_nb, "\n\n",
    "Accuracy: ", accuracy_adj_nb, "\n")

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

# Train
# True priors
tree_old_pr1_tr = predict(tree_pruned, train, "prob")[,1] 
pred_r1_tree_tr<-as.numeric(tree_old_pr1_tr) 
pred_r0_tree_tr = 1 - pred_r1_tree_tr 
den = pred_r1_tree_tr*(true1/rho1)+pred_r0_tree_tr*(true0/rho0) 
pred1_true_tree_tr = pred_r1_tree_tr*(true1/rho1)/den 
pred0_true_tree_tr = pred_r0_tree_tr*(true0/rho0)/den

#Adjusted
train$pred_y_tree_adj_tr=ifelse(pred1_true_tree_tr>0.4, "phishing","legitimate")
train$pred_y_tree_adj_tr <- factor(train$pred_y_tree_adj_tr, levels = c("phishing", "legitimate"))
TP_tree_tr <- sum(train$status == "phishing" & train$pred_y_tree_adj_tr == "phishing")
TP_adj_tree_tr <- (true0/rho0)*TP_tree_tr
FP_tree_tr <- sum(train$status == "legitimate" & train$pred_y_tree_adj_tr == "phishing")
FP_adj_tree_tr <- (true0/rho0)*FP_tree_tr
TN_tree_tr <- sum(train$status == "legitimate" & train$pred_y_tree_adj_tr == "legitimate")
TN_adj_tree_tr <- (true1/rho1)*TN_tree_tr
FN_tree_tr <- sum(train$status == "phishing" & train$pred_y_tree_adj_tr == "legitimate")
FN_adj_tree_tr <- (true1/rho1)*FN_tree_tr
accuracy_adj_tree_tr <- (TP_adj_tree_tr + TN_adj_tree_tr) / (TP_adj_tree_tr + FP_adj_tree_tr + TN_adj_tree_tr + FN_adj_tree_tr)

# Stampa della matrice di confusione e delle metriche
cat("Matrice di Confusione aggiustata:\n",
    "               Predetto\n",
    "Reale   Phishing Legitimate\n",
    "Phishing ", TP_adj_tree_tr, "      ", FN_adj_tree_tr, "\n",
    "Legitimate ", FP_adj_tree_tr, "      ", TN_adj_tree_tr, "\n\n",
    "Accuracy: ", accuracy_adj_tree_tr, "\n")

# Test
# True priors
tree_old_pr1 = predict(tree_pruned, test, "prob")[,1] 
pred_r1<-as.numeric(tree_old_pr1) 
pred_r0 = 1 - pred_r1 
den = pred_r1*(true1/rho1)+pred_r0*(true0/rho0) 
pred1_true_tree = pred_r1*(true1/rho1)/den 
pred0_true_tree = pred_r0*(true0/rho0)/den

#Adjusted
test$pred_y_tree_adj=ifelse(pred1_true_tree>0.4, "phishing","legitimate")
test$pred_y_tree_adj <- factor(test$pred_y_tree_adj, levels = c("phishing", "legitimate"))
TP_tree <- sum(test$status == "phishing" & test$pred_y_tree_adj == "phishing")
TP_adj_tree <- (true0/rho0)*TP_tree
FP_tree <- sum(test$status == "legitimate" & test$pred_y_tree_adj == "phishing")
FP_adj_tree <- (true0/rho0)*FP_tree
TN_tree <- sum(test$status == "legitimate" & test$pred_y_tree_adj == "legitimate")
TN_adj_tree <- (true1/rho1)*TN_tree
FN_tree <- sum(test$status == "phishing" & test$pred_y_tree_adj == "legitimate")
FN_adj_tree <- (true1/rho1)*FN_tree
accuracy_adj_tree <- (TP_adj_tree + TN_adj_tree) / (TP_adj_tree + FP_adj_tree + TN_adj_tree + FN_adj_tree)

# Stampa della matrice di confusione e delle metriche
cat("Matrice di Confusione aggiustata:\n",
    "               Predetto\n",
    "Reale   Phishing Legitimate\n",
    "Phishing ", TP_adj_tree, "      ", FN_adj_tree, "\n",
    "Legitimate ", FP_adj_tree, "      ", TN_adj_tree, "\n\n",
    "Accuracy: ", accuracy_adj_tree, "\n")

# Bagging -----------------------------------------------------------------
set.seed(9999)
ctrl = trainControl(method = "boot",
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


# Train
# True priors
bg_old_pr1_tr = predict(bagging, train, "prob")[,1] 
pred_r1_bg_tr<-as.numeric(bg_old_pr1_tr) 
pred_r0_bg_tr = 1 - pred_r1_bg_tr 
den = pred_r1_bg_tr*(true1/rho1)+pred_r0_bg_tr*(true0/rho0) 
pred1_true_bg_tr = pred_r1_bg_tr*(true1/rho1)/den 
pred0_true_bg_tr = pred_r0_bg_tr*(true0/rho0)/den

#Adjusted
train$pred_y_bg_adj_tr=ifelse(pred1_true_bg_tr>0.4, "phishing","legitimate")
train$pred_y_bg_adj_tr <- factor(train$pred_y_bg_adj_tr, levels = c("phishing", "legitimate"))
TP_bg_tr <- sum(train$status == "phishing" & train$pred_y_bg_adj_tr == "phishing")
TP_adj_bg_tr <- (true0/rho0)*TP_bg_tr
FP_bg_tr <- sum(train$status == "legitimate" & train$pred_y_bg_adj_tr == "phishing")
FP_adj_bg_tr <- (true0/rho0)*FP_bg_tr
TN_bg_tr <- sum(train$status == "legitimate" & train$pred_y_bg_adj_tr == "legitimate")
TN_adj_bg_tr <- (true1/rho1)*TN_bg_tr
FN_bg_tr <- sum(train$status == "phishing" & train$pred_y_bg_adj_tr == "legitimate")
FN_adj_bg_tr <- (true1/rho1)*FN_bg_tr
accuracy_adj_bg_tr <- (TP_adj_bg_tr + TN_adj_bg_tr) / (TP_adj_bg_tr + FP_adj_bg_tr + TN_adj_bg_tr + FN_adj_bg_tr)

# Stampa della matrice di confusione e delle metriche
cat("Matrice di Confusione aggiustata:\n",
    "               Predetto\n",
    "Reale   Phishing Legitimate\n",
    "Phishing ", TP_adj_bg_tr, "      ", FN_adj_bg_tr, "\n",
    "Legitimate ", FP_adj_bg_tr, "      ", TN_adj_bg_tr, "\n\n",
    "Accuracy: ", accuracy_adj_bg_tr, "\n")

# Test
# True priors
bg_old_pr1 = predict(bagging, test, "prob")[,1] 
pred_r1<-as.numeric(bg_old_pr1) 
pred_r0 = 1 - pred_r1 
den = pred_r1*(true1/rho1)+pred_r0*(true0/rho0) 
pred1_true_bg = pred_r1*(true1/rho1)/den 
pred0_true_bg = pred_r0*(true0/rho0)/den

#Adjusted
test$pred_y_bg_adj=ifelse(pred1_true_bg>0.4, "phishing","legitimate")
test$pred_y_bg_adj <- factor(test$pred_y_bg_adj, levels = c("phishing", "legitimate"))
TP_bg <- sum(test$status == "phishing" & test$pred_y_bg_adj == "phishing")
TP_adj_bg <- (true0/rho0)*TP_bg
FP_bg <- sum(test$status == "legitimate" & test$pred_y_bg_adj == "phishing")
FP_adj_bg <- (true0/rho0)*FP_bg
TN_bg <- sum(test$status == "legitimate" & test$pred_y_bg_adj == "legitimate")
TN_adj_bg <- (true1/rho1)*TN_bg
FN_bg <- sum(test$status == "phishing" & test$pred_y_bg_adj == "legitimate")
FN_adj_bg <- (true1/rho1)*FN_bg
accuracy_adj_bg <- (TP_adj_bg + TN_adj_bg) / (TP_adj_bg + FP_adj_bg + TN_adj_bg + FN_adj_bg)

# Stampa della matrice di confusione e delle metriche
cat("Matrice di Confusione aggiustata:\n",
    "               Predetto\n",
    "Reale   Phishing Legitimate\n",
    "Phishing ", TP_adj_bg, "      ", FN_adj_bg, "\n",
    "Legitimate ", FP_adj_bg, "      ", TN_adj_bg, "\n\n",
    "Accuracy: ", accuracy_adj_bg, "\n")

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

# Train
# True priors
gb_old_pr1_tr = predict(gb, train, "prob")[,1] 
pred_r1_gb_tr<-as.numeric(gb_old_pr1_tr) 
pred_r0_gb_tr = 1 - pred_r1_gb_tr 
den = pred_r1_gb_tr*(true1/rho1)+pred_r0_gb_tr*(true0/rho0) 
pred1_true_gb_tr = pred_r1_gb_tr*(true1/rho1)/den 
pred0_true_gb_tr = pred_r0_gb_tr*(true0/rho0)/den

#Adjusted
train$pred_y_gb_adj_tr=ifelse(pred1_true_gb_tr>0.4, "phishing","legitimate")
train$pred_y_gb_adj_tr <- factor(train$pred_y_gb_adj_tr, levels = c("phishing", "legitimate"))
TP_gb_tr <- sum(train$status == "phishing" & train$pred_y_gb_adj_tr == "phishing")
TP_adj_gb_tr <- (true0/rho0)*TP_gb_tr
FP_gb_tr <- sum(train$status == "legitimate" & train$pred_y_gb_adj_tr == "phishing")
FP_adj_gb_tr <- (true0/rho0)*FP_gb_tr
TN_gb_tr <- sum(train$status == "legitimate" & train$pred_y_gb_adj_tr == "legitimate")
TN_adj_gb_tr <- (true1/rho1)*TN_gb_tr
FN_gb_tr <- sum(train$status == "phishing" & train$pred_y_gb_adj_tr == "legitimate")
FN_adj_gb_tr <- (true1/rho1)*FN_gb_tr
accuracy_adj_gb_tr <- (TP_adj_gb_tr + TN_adj_gb_tr) / (TP_adj_gb_tr + FP_adj_gb_tr + TN_adj_gb_tr + FN_adj_gb_tr)

# Stampa della matrice di confusione e delle metriche
cat("Matrice di Confusione aggiustata:\n",
    "               Predetto\n",
    "Reale   Phishing Legitimate\n",
    "Phishing ", TP_adj_gb_tr, "      ", FN_adj_gb_tr, "\n",
    "Legitimate ", FP_adj_gb_tr, "      ", TN_adj_gb_tr, "\n\n",
    "Accuracy: ", accuracy_adj_gb_tr, "\n")

# Test
# True priors
gb_old_pr1 = predict(gb, test, "prob")[,1] 
pred_r1<-as.numeric(gb_old_pr1) 
pred_r0 = 1 - pred_r1 
den = pred_r1*(true1/rho1)+pred_r0*(true0/rho0) 
pred1_true_gb = pred_r1*(true1/rho1)/den 
pred0_true_gb = pred_r0*(true0/rho0)/den

#Adjusted
test$pred_y_gb_adj=ifelse(pred1_true_gb>0.4, "phishing","legitimate")
test$pred_y_gb_adj <- factor(test$pred_y_gb_adj, levels = c("phishing", "legitimate"))
TP_gb <- sum(test$status == "phishing" & test$pred_y_gb_adj == "phishing")
TP_adj_gb <- (true0/rho0)*TP_gb
FP_gb <- sum(test$status == "legitimate" & test$pred_y_gb_adj == "phishing")
FP_adj_gb <- (true0/rho0)*FP_gb
TN_gb <- sum(test$status == "legitimate" & test$pred_y_gb_adj == "legitimate")
TN_adj_gb <- (true1/rho1)*TN_gb
FN_gb <- sum(test$status == "phishing" & test$pred_y_gb_adj == "legitimate")
FN_adj_gb <- (true1/rho1)*FN_gb
accuracy_adj_gb <- (TP_adj_gb + TN_adj_gb) / (TP_adj_gb + FP_adj_gb + TN_adj_gb + FN_adj_gb)

# Stampa della matrice di confusione e delle metriche
cat("Matrice di Confusione aggiustata:\n",
    "               Predetto\n",
    "Reale   Phishing Legitimate\n",
    "Phishing ", TP_adj_gb, "      ", FN_adj_gb, "\n",
    "Legitimate ", FP_adj_gb, "      ", TN_adj_gb, "\n\n",
    "Accuracy: ", accuracy_adj_gb, "\n")

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

# Train
# True priors
rf_old_pr1_tr = predict(rf, train, "prob")[,1] 
pred_r1_rf_tr<-as.numeric(rf_old_pr1_tr) 
pred_r0_rf_tr = 1 - pred_r1_rf_tr 
den = pred_r1_rf_tr*(true1/rho1)+pred_r0_rf_tr*(true0/rho0) 
pred1_true_rf_tr = pred_r1_rf_tr*(true1/rho1)/den 
pred0_true_rf_tr = pred_r0_rf_tr*(true0/rho0)/den

#Adjusted
train$pred_y_rf_adj_tr=ifelse(pred1_true_rf_tr>0.4, "phishing","legitimate")
train$pred_y_rf_adj_tr <- factor(train$pred_y_rf_adj_tr, levels = c("phishing", "legitimate"))
TP_rf_tr <- sum(train$status == "phishing" & train$pred_y_rf_adj_tr == "phishing")
TP_adj_rf_tr <- (true0/rho0)*TP_rf_tr
FP_rf_tr <- sum(train$status == "legitimate" & train$pred_y_rf_adj_tr == "phishing")
FP_adj_rf_tr <- (true0/rho0)*FP_rf_tr
TN_rf_tr <- sum(train$status == "legitimate" & train$pred_y_rf_adj_tr == "legitimate")
TN_adj_rf_tr <- (true1/rho1)*TN_rf_tr
FN_rf_tr <- sum(train$status == "phishing" & train$pred_y_rf_adj_tr == "legitimate")
FN_adj_rf_tr <- (true1/rho1)*FN_rf_tr
accuracy_adj_rf_tr <- (TP_adj_rf_tr + TN_adj_rf_tr) / (TP_adj_rf_tr + FP_adj_rf_tr + TN_adj_rf_tr + FN_adj_rf_tr)

# Stampa della matrice di confusione e delle metriche
cat("Matrice di Confusione aggiustata:\n",
    "               Predetto\n",
    "Reale   Phishing Legitimate\n",
    "Phishing ", TP_adj_rf_tr, "      ", FN_adj_rf_tr, "\n",
    "Legitimate ", FP_adj_rf_tr, "      ", TN_adj_rf_tr, "\n\n",
    "Accuracy: ", accuracy_adj_rf_tr, "\n")

# Test
# True priors
rf_old_pr1 = predict(rf, test, "prob")[,1] 
pred_r1<-as.numeric(rf_old_pr1) 
pred_r0 = 1 - pred_r1 
den = pred_r1*(true1/rho1)+pred_r0*(true0/rho0) 
pred1_true_rf = pred_r1*(true1/rho1)/den 
pred0_true_rf = pred_r0*(true0/rho0)/den

#Adjusted
test$pred_y_rf_adj=ifelse(pred1_true_rf>0.4, "phishing","legitimate")
test$pred_y_rf_adj <- factor(test$pred_y_rf_adj, levels = c("phishing", "legitimate"))
TP_rf <- sum(test$status == "phishing" & test$pred_y_rf_adj == "phishing")
TP_adj_rf <- (true0/rho0)*TP_rf
FP_rf <- sum(test$status == "legitimate" & test$pred_y_rf_adj == "phishing")
FP_adj_rf <- (true0/rho0)*FP_rf
TN_rf <- sum(test$status == "legitimate" & test$pred_y_rf_adj == "legitimate")
TN_adj_rf <- (true1/rho1)*TN_rf
FN_rf <- sum(test$status == "phishing" & test$pred_y_rf_adj == "legitimate")
FN_adj_rf <- (true1/rho1)*FN_rf
accuracy_adj_rf <- (TP_adj_rf + TN_adj_rf) / (TP_adj_rf + FP_adj_rf + TN_adj_rf + FN_adj_rf)

# Stampa della matrice di confusione e delle metriche
cat("Matrice di Confusione aggiustata:\n",
    "               Predetto\n",
    "Reale   Phishing Legitimate\n",
    "Phishing ", TP_adj_rf, "      ", FN_adj_rf, "\n",
    "Legitimate ", FP_adj_rf, "      ", TN_adj_rf, "\n\n",
    "Accuracy: ", accuracy_adj_rf, "\n")

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

nnPred_p = predict(nn, test_selected, type = c("prob"))
nnPred = predict(nn, test_selected)

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

nn_tunedPred_p = predict(nn_tuned, test_selected, type = c("prob"))
nn_tunedPred = predict(nn_tuned, test_selected)

# Train
# True priors
nn_old_pr1_tr = predict(nn_tuned, train, "prob")[,1] 
pred_r1_nn_tr<-as.numeric(nn_old_pr1_tr) 
pred_r0_nn_tr = 1 - pred_r1_nn_tr 
den = pred_r1_nn_tr*(true1/rho1)+pred_r0_nn_tr*(true0/rho0) 
pred1_true_nn_tr = pred_r1_nn_tr*(true1/rho1)/den 
pred0_true_nn_tr = pred_r0_nn_tr*(true0/rho0)/den

#Adjusted
train$pred_y_nn_adj_tr=ifelse(pred1_true_nn_tr>0.4, "phishing","legitimate")
train$pred_y_nn_adj_tr <- factor(train$pred_y_nn_adj_tr, levels = c("phishing", "legitimate"))
TP_nn_tr <- sum(train$status == "phishing" & train$pred_y_nn_adj_tr == "phishing")
TP_adj_nn_tr <- (true0/rho0)*TP_nn_tr
FP_nn_tr <- sum(train$status == "legitimate" & train$pred_y_nn_adj_tr == "phishing")
FP_adj_nn_tr <- (true0/rho0)*FP_nn_tr
TN_nn_tr <- sum(train$status == "legitimate" & train$pred_y_nn_adj_tr == "legitimate")
TN_adj_nn_tr <- (true1/rho1)*TN_nn_tr
FN_nn_tr <- sum(train$status == "phishing" & train$pred_y_nn_adj_tr == "legitimate")
FN_adj_nn_tr <- (true1/rho1)*FN_nn_tr
accuracy_adj_nn_tr <- (TP_adj_nn_tr + TN_adj_nn_tr) / (TP_adj_nn_tr + FP_adj_nn_tr + TN_adj_nn_tr + FN_adj_nn_tr)

# Stampa della matrice di confusione e delle metriche
cat("Matrice di Confusione aggiustata:\n",
    "               Predetto\n",
    "Reale   Phishing Legitimate\n",
    "Phishing ", TP_adj_nn_tr, "      ", FN_adj_nn_tr, "\n",
    "Legitimate ", FP_adj_nn_tr, "      ", TN_adj_nn_tr, "\n\n",
    "Accuracy: ", accuracy_adj_nn_tr, "\n")

# Test
# True priors
nn_old_pr1 = predict(nn_tuned, test, "prob")[,1] 
pred_r1<-as.numeric(nn_old_pr1) 
pred_r0 = 1 - pred_r1 
den = pred_r1*(true1/rho1)+pred_r0*(true0/rho0) 
pred1_true_nn = pred_r1*(true1/rho1)/den 
pred0_true_nn = pred_r0*(true0/rho0)/den

#Adjusted
test$pred_y_nn_adj=ifelse(pred1_true_nn>0.4, "phishing","legitimate")
test$pred_y_nn_adj <- factor(test$pred_y_nn_adj, levels = c("phishing", "legitimate"))
TP_nn <- sum(test$status == "phishing" & test$pred_y_nn_adj == "phishing")
TP_adj_nn <- (true0/rho0)*TP_nn
FP_nn <- sum(test$status == "legitimate" & test$pred_y_nn_adj == "phishing")
FP_adj_nn <- (true0/rho0)*FP_nn
TN_nn <- sum(test$status == "legitimate" & test$pred_y_nn_adj == "legitimate")
TN_adj_nn <- (true1/rho1)*TN_nn
FN_nn <- sum(test$status == "phishing" & test$pred_y_nn_adj == "legitimate")
FN_adj_nn <- (true1/rho1)*FN_nn
accuracy_adj_nn <- (TP_adj_nn + TN_adj_nn) / (TP_adj_nn + FP_adj_nn + TN_adj_nn + FN_adj_nn)

# Stampa della matrice di confusione e delle metriche
cat("Matrice di Confusione aggiustata:\n",
    "               Predetto\n",
    "Reale   Phishing Legitimate\n",
    "Phishing ", TP_adj_nn, "      ", FN_adj_nn, "\n",
    "Legitimate ", FP_adj_nn, "      ", TN_adj_nn, "\n\n",
    "Accuracy: ", accuracy_adj_nn, "\n")

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
                       methodList = c("glm", "knn", "naive_bayes","rf", "nne")
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
colAUC(model_preds$ensemble, test_selected$status)
confusionMatrix(model_preds2$ensemble, test_selected$status)

# Train
# True priors
ens_old_pr1_tr = predict(glm_ensemble, train, "prob")[,1] 
pred_r1_ens_tr<-as.numeric(ens_old_pr1_tr) 
pred_r0_ens_tr = 1 - pred_r1_ens_tr 
den = pred_r1_ens_tr*(true1/rho1)+pred_r0_ens_tr*(true0/rho0) 
pred1_true_ens_tr = pred_r1_ens_tr*(true1/rho1)/den 
pred0_true_ens_tr = pred_r0_ens_tr*(true0/rho0)/den

#Adjusted
train$pred_y_ens_adj_tr=ifelse(pred1_true_ens_tr>0.4, "phishing","legitimate")
train$pred_y_ens_adj_tr <- factor(train$pred_y_ens_adj_tr, levels = c("phishing", "legitimate"))
TP_ens_tr <- sum(train$status == "phishing" & train$pred_y_ens_adj_tr == "phishing")
TP_adj_ens_tr <- (true0/rho0)*TP_ens_tr
FP_ens_tr <- sum(train$status == "legitimate" & train$pred_y_ens_adj_tr == "phishing")
FP_adj_ens_tr <- (true0/rho0)*FP_ens_tr
TN_ens_tr <- sum(train$status == "legitimate" & train$pred_y_ens_adj_tr == "legitimate")
TN_adj_ens_tr <- (true1/rho1)*TN_ens_tr
FN_ens_tr <- sum(train$status == "phishing" & train$pred_y_ens_adj_tr == "legitimate")
FN_adj_ens_tr <- (true1/rho1)*FN_ens_tr
accuracy_adj_ens_tr <- (TP_adj_ens_tr + TN_adj_ens_tr) / (TP_adj_ens_tr + FP_adj_ens_tr + TN_adj_ens_tr + FN_adj_ens_tr)

# Stampa della matrice di confusione e delle metriche
cat("Matrice di Confusione aggiustata:\n",
    "               Predetto\n",
    "Reale   Phishing Legitimate\n",
    "Phishing ", TP_adj_ens_tr, "      ", FN_adj_ens_tr, "\n",
    "Legitimate ", FP_adj_ens_tr, "      ", TN_adj_ens_tr, "\n\n",
    "Accuracy: ", accuracy_adj_ens_tr, "\n")

# Test
# True priors
ens_old_pr1 = predict(glm_ensemble, test, "prob")[,1] 
pred_r1<-as.numeric(ens_old_pr1) 
pred_r0 = 1 - pred_r1 
den = pred_r1*(true1/rho1)+pred_r0*(true0/rho0) 
pred1_true_ens = pred_r1*(true1/rho1)/den 
pred0_true_ens = pred_r0*(true0/rho0)/den

#Adjusted
test$pred_y_ens_adj=ifelse(pred1_true_ens>0.4, "phishing","legitimate")
test$pred_y_ens_adj <- factor(test$pred_y_ens_adj, levels = c("phishing", "legitimate"))
TP_ens <- sum(test$status == "phishing" & test$pred_y_ens_adj == "phishing")
TP_adj_ens <- (true0/rho0)*TP_ens
FP_ens <- sum(test$status == "legitimate" & test$pred_y_ens_adj == "phishing")
FP_adj_ens <- (true0/rho0)*FP_ens
TN_ens <- sum(test$status == "legitimate" & test$pred_y_ens_adj == "legitimate")
TN_adj_ens <- (true1/rho1)*TN_ens
FN_ens <- sum(test$status == "phishing" & test$pred_y_ens_adj == "legitimate")
FN_adj_ens <- (true1/rho1)*FN_ens
accuracy_adj_ens <- (TP_adj_ens + TN_adj_ens) / (TP_adj_ens + FP_adj_ens + TN_adj_ens + FN_adj_ens)

# Stampa della matrice di confusione e delle metriche
cat("Matrice di Confusione aggiustata:\n",
    "               Predetto\n",
    "Reale   Phishing Legitimate\n",
    "Phishing ", TP_adj_ens, "      ", FN_adj_ens, "\n",
    "Legitimate ", FP_adj_ens, "      ", TN_adj_ens, "\n\n",
    "Accuracy: ", accuracy_adj_ens, "\n")


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
rfPredR = prediction(rfpred_p[,1], y)
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
plot(thresholds, acc.values, type="l", col="dodgerblue", 
     main="Performance Metrics vs Threshold", xlab="Threshold", ylab="Metrics",
     ylim=range(c(acc.values, spec.values, sens.values)))
lines(thresholds, spec.values, col="darkorange")
lines(thresholds, sens.values, col="green")

# Aggiunta della legenda
legend("bottomright", legend=c("Accuracy", "Specificity", "Sensitivity"),
       col=c("dodgerblue", "darkorange", "green"),
       lty=1, cex=0.8, text.font=4, y.intersp=1.2, x.intersp=1.2, lwd=2)

# Unadjusted
test$pred_y=ifelse(rfpred_p[,1]>0.4, "phishing","legitimate")
test$pred_y <- factor(test$pred_y, levels = c("phishing", "legitimate"))
confusionMatrix(test$pred_y, test$status)

#Adjusted
test$pred_y_adj=ifelse(pred1_true_rf>0.4, "phishing","legitimate")
test$pred_y_adj <- factor(test$pred_y_adj, levels = c("phishing", "legitimate"))
TP <- sum(test$status == "phishing" & test$pred_y == "phishing")
TP_adj <- (true0/rho0)*TP
FP <- sum(test$status == "legitimate" & test$pred_y == "phishing")
FP_adj <- (true0/rho0)*FP
TN <- sum(test$status == "legitimate" & test$pred_y == "legitimate")
TN_adj <- (true1/rho1)*TN
FN <- sum(test$status == "phishing" & test$pred_y == "legitimate")
FN_adj <- (true1/rho1)*FN
accuracy_adj <- (TP_adj + TN_adj) / (TP_adj + FP_adj + TN_adj + FN_adj )

# Stampa della matrice di confusione e delle metriche
cat("Matrice di Confusione aggiustata:\n")
cat("               Predetto\n")
cat("Reale   Phishing Legitimate\n")
cat("Phishing ", TP_adj, "      ", FN_adj, "\n")
cat("Legitimate ", FP_adj, "      ", TN_adj, "\n\n")

cat("Accuracy: ", accuracy_adj, "\n")

# Step 4 ------------------------------------------------------------------

rf_old_pr1_score = predict(rf, score, "prob")[,1] 
pred_r1_score<-as.numeric(rf_old_pr1_score) 
pred_r0_score = 1 - pred_r1_score 
den = pred_r1_score*(true1/rho1)+pred_r0_score*(true0/rho0) 
pred1_true_rf_score = pred_r1_score*(true1/rho1)/den 
pred0_true_rf_score = pred_r0_score*(true0/rho0)/den
score$pred_y=ifelse(pred1_true_rf_score>0.4, "phishing","legitimate")
head(score)
