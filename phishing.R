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

# Trasformare le variabili binarie nel dataset
binary_vars <- sapply(train, function(x) all(x %in% c(0, 1)))
train[binary_vars] <- lapply(train[binary_vars], factor, levels = c(0, 1), labels = c(0, 1))
train$status <- factor(train$status)

# Test
test <- read.csv("test.csv")

# Trasformare le variabili binarie nel dataset
binary_vars <- sapply(test, function(x) all(x %in% c(0, 1)))
test[binary_vars] <- lapply(test[binary_vars], factor, levels = c(0, 1), labels = c(0, 1))
test$status <- factor(test$status)

# Distribuzione variabile target ------------------------------------------
class(train$status)
table(train$status) / nrow(train)

# Funzione per il grafico

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

# Variabili grafiche
# Crea una lista di grafici ggplot
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

# Conto valori mancanti per variabile per fare grafico 
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

# Model selection ----------------------------------------------------------

# Tree
cvCtrl = trainControl(method = "cv", number=10, search="grid", classProbs = TRUE)
rpartTuneCvA = train(status ~ ., data = train, method = "rpart",
                     tuneLength = 10,
                     trControl = cvCtrl)

rpartTuneCvA
getTrainPerf(rpartTuneCvA)

plot(varImp(object=rpartTuneCvA),main="train tuned - Variable Importance")
plot(rpartTuneCvA)

vi_t = as.data.frame(rpartTuneCvA$finalModel$variable.importance)
viname_t = row.names(vi_t)
head(viname_t)

#Random Forest
rfTune = train(status ~ ., data = train, method = "rf",
               tuneLength = 10,
               trControl = cvCtrl)

rfTune
getTrainPerf(rfTune)

plot(varImp(object=rfTune),main="train tuned - Variable Importance")
plot(rfTune)

vi_rf = data.frame(varImp(rfTune)[1])
vi_rf$var = row.names(vi_rf)
head(vi_rf)
viname_rf = vi_rf[,2]

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

viname_t
viname_rf
viname_bo

# Scelta delle features da mantenere in analisi
selected = c("status","google_index", "page_rank", "nb_hyperlinks", "domain_age", "web_traffic", "nb_www", 
             "phish_hints", "length_url", "longest_word_path", "length_hostname", "nb_hyphens", "ratio_intHyperlinks","safe_anchor","domain_registration_length","length_words_raw", "longest_words_raw","ratio_extHyperlinks","ratio_digits_host","nb_slash","avg_word_path")

train = train[,selected]
test= test[,selected]

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
colnames(train)[colnames(train)=="status"] <- "target" 
colnames(test)[colnames(test)=="status"] <- "target" 

# GLM (Logistic Classifier) -----------------------------------------------

set.seed(9999)
ctrl = trainControl(method = "boot", # prof. use method = "cv" - Carlotta use method = "boot"
                    number = 10,
                    search = "grid", # prof. not use search in GLM - Carlotta yes 
                    classProbs = TRUE) 
#                       summaryFunction = twoClassSummary) # Carlotta not use summaryFunction in GLM - prof. yes 

glm = train(target ~ ., # !!! adjust/modify/change/adapt with our target !!!
            data = train, # !!! adjust/modify/change/adapt with our train data !!!
            method = "glm", 
            preProcess = c("corr", "nzv"), # we can also add BoxCox as follow: preProcess = c("corr", "nzv", "BoxCox")
            trControl = ctrl,
            #            tuneLength = 5, ??? No if we use (like Carlotta do) method = "boot" ???
            #            trace = TRUE, # Carlotta not use trace in GLM - prof. yes
            metric = "Sens") # n.b.: prof. use metric = "ROC"

glm 
confusionMatrix(glm)

glmpred = predict(glm, test)
glmpred_p = predict(glm, test, type = c("prob"))
confusionMatrix(glmpred, test$target)

# K-Nearest Neightbour ----------------------------------------------------

set.seed(9999)
ctrl = trainControl(method = "boot", # prof. use method = "cv" - Carlotta use method = "boot"
                    number = 10,
                    search = "grid", # prof. not use search in KNN - Carlotta yes
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary) 

#grid = expand.grid(k = seq(5, 20, 3)) # Carlotta not use grid = expand.grid(k = seq(from = ..., to = ..., by = ...)) in KNN - prof. yes 

knn = train(target ~ ., # !!! adjust/modify/change/adapt with our target !!!
            data = train, # !!! adjust/modify/change/adapt with our train data !!!
            method = "knn",
            trControl = ctrl,
            tuneLength = 10, # prof. use tuneLength = 5 - Carlotta use tuneLength = 10
            #            tuneGrid = grid, # Carlotta not use grid in KNN , so neither tuneGrid - prof. yes
            preProcess = c("center", "scale", "corr", "nzv"), # n.b.: prof. use only preProcess = c("scale", "corr")
            metric = "Sens") 
knn
plot(knn)
confusionMatrix(knn)

knnpred = predict(knn, test)
knnpred_p = predict(knn, test, type = c("prob"))
confusionMatrix(knnpred, test$target)

# LASSO -------------------------------------------------------------------

set.seed(9999)
ctrl = trainControl(method = "boot", # prof. use method = "cv" - Carlotta use method = "boot"
                    number = 10,
                    classProbs = TRUE,
                    search = "grid", # prof. not use search in LASSO - Carlotta yes
                    summaryFunction = twoClassSummary)

# Carlotta not use grid = expand.grid(.alpha = 1, .lambda = seq(0, 1, by = 0.01)) in LASSO - prof. yes
#grid = expand.grid(.alpha = 1,
#                   .lambda = seq(0, 1, by = 0.01))

lasso = train(target ~ ., # !!! adjust/modify/change/adapt with our target !!!
              data = train, # !!! adjust/modify/change/adapt with our train data !!!
              method = "glmnet",
              #              family = "binomial", # Carlotta not use family = "binomial" in LASSO - prof. yes
              preProcess = c("corr", "nzv"), # prof. not use preProcess in LASSO - Carlotta yes
              metric = "Sens",
              trControl = ctrl,
              tuneLength = 10) # prof. use tuneLength = 5 - Carlotta use tuneLength = 10
#              tuneGrid = grid) # Carlotta not use grid in LASSO , so neither tuneGrid - prof. yes

lasso
plot(lasso)
confusionMatrix(lasso)

lassopred = predict(lasso, test)
lassopred_p = predict(lasso, test, type = c("prob"))
confusionMatrix(lassopred, test$target)

# PLS ---------------------------------------------------------------------

set.seed(9999)
ctrl = trainControl(method = "boot", # prof. use method = "cv" - Carlotta use method = "boot"
                    number = 10,
                    search = "grid", # prof. not use search in PLS - Carlotta yes
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary)

pls = train(target ~ ., # !!! adjust/modify/change/adapt with our target !!!
            data = train, # !!! adjust/modify/change/adapt with our train data !!!
            method = "pls",
            preProcess = c("center"), # prof. not use preProcess in PLS - Carlotta yes
            metric = "Sens",
            trControl = ctrl,
            tuneLength = 10) # prof. use tuneLength = 5 - Carlotta use tuneLength = 10

pls
plot(pls)
confusionMatrix(pls)

plspred = predict(pls, test)
plspred_p = predict(pls, test, type = c("prob"))
confusionMatrix(plspred, test$target)

# Naive Bayes -------------------------------------------------------------

set.seed(9999)
ctrl = trainControl(method = "boot", # prof. use method = "cv" - Carlotta use method = "boot"
                    number = 10,
                    search = "grid", # prof. not use search in Naive Bayes - Carlotta yes
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary)

naivebayes = train(target ~ ., # !!! adjust/modify/change/adapt with our target !!!
                   data = train, # !!! adjust/modify/change/adapt with our train data !!!
                   method = "naive_bayes",
                   trControl = ctrl,
                   tuneLength = 10, # prof. use tuneLength = 5 - Carlotta use tuneLength = 10
                   preProcess = c("corr", "nzv"), # prof. not use preProcess in Naive Bayes - Carlotta yes
                   metric = "Sens")

naivebayes
plot(naivebayes)
confusionMatrix(naivebayes)

nbpred = predict(naivebayes, test)
nbpred_p = predict(naivebayes, test, type = c("prob"))
confusionMatrix(nbpred, test$target)

# Decision Tree (prof. version) -------------------------------------------

set.seed(9999)
ctrl = trainControl(method = "cv", 
                    number = 10, 
                    search = "grid",
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary)

tree = train(target ~ .,# !!! adjust/modify/change/adapt with our target !!!
             data = train, # !!! adjust/modify/change/adapt with our train data !!!
             method = "rpart",
             #             metric = "Sens",
             tuneLength = 10,
             trControl = ctrl)
# !!! N.B.: Best accuracy using best cp !!!
tree
# Final Model
getTrainPerf(tree)
plot(tree)
# !!! N.B.: Var Imp of the Tree - With the Tree choose most important variables !!!
varImp(object = tree)
plot(varImp(object = tree), main = "Train Tuned - Variable Importance")
# !!! N.B.: Object saved !!!
ls(tree)
ls(tree$finalModel)
# !!! N.B.: Select only important variables !!!
vi = as.data.frame(tree$finalModel$variable.importance)
vi
dim(vi)
# !!! N.B.: Select most important variables from a Tree !!!
viname = row.names(vi)
viname

# Decision Tree (Carlotta version) ----------------------------------------

tree_rpart = rpart(target ~ ., # !!! adjust/modify/change/adapt with our target !!!
                   data = train, # !!! adjust/modify/change/adapt with our train data !!!
                   method = "class",
                   cp = 0, 
                   minsplit = 1)

tree_rpart$cptable
rpart.plot(tree_rpart, type = 4, extra = 1)
confusionMatrix(tree_rpart)

tree_pruned = prune(tree_rpart,
                    cp = tree_rpart$cptable[which.min(tree_rpart$cptable[,"xerror"]),"CP"])

rpart.plot(tree_pruned, type = 4, extra = 1)
confusionMatrix(tree_pruned)

treepred = predict(tree_pruned, test)
treepred_p = predict(tree_pruned, test, type = c("prob"))
confusionMatrix(treepred, test$target)

# Bagging -----------------------------------------------------------------

ctrl = trainControl(method = "boot",
                    number = 10,
                    searc = "grid", 
                    summaryFunction = twoClassSummary, 
                    classProbs = TRUE)

bagging = train(target ~ ., # !!! adjust/modify/change/adapt with our target !!!
                data = train, # !!! adjust/modify/change/adapt with our train data !!!
                method = "treebag",
                ntree = 250,
                trControl = ctrl)

bagging
plot(bagging)
confusionMatrix(bagging)

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

gb = train(target ~., # !!! adjust/modify/change/adapt with our target !!!
           data = train, # !!! adjust/modify/change/adapt with our train data !!!
           method = "gbm",
           tuneLength = 10,
           metric ="Sens",
           tuneGrid = gbm_tune,
           trControl = ctrl)

gb
plot(gb)
confusionMatrix(gb)

gbpred = predict(gb, test)
gbpred_p = predict(gb, test, type = c("prob"))
confusionMatrix(gbpred_p, test$target)


# Random Forest -----------------------------------------------------------

set.seed(9999)
ctrl <- trainControl(method = "boot", # prof. use method = "cv" - Carlotta use method = "boot"
                     number = 10,
                     search = "grid",
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

rf <- train(target ~ ., # !!! adjust/modify/change/adapt with our target !!!
            data = train, # !!! adjust/modify/change/adapt with our train data !!!
            method = "rf",
            metric = "Sens",
            tuneLength = 10,# prof. use tuneLength = 5 - Carlotta use tuneLength = 10
            trControl = ctrl,
            verbose = FALSE) # prof. not use verbose in Random Forest - Carlotta yes

rf
plot(rf)
confusionMatrix(rf)

# Permutation Importance
vImp = varImp(rf)
plot(varImp(object = rf), main = "Train Tuned - Variable Importnace")
vImp = data.frame(vImp[1])
vImp$var=row.names(vImp)
head(vImp)

# Select covariate with Importance > 30% than most important
vImp2 = vImp[vImp$Overall>30,]
head(vImp2)
