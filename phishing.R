# Librerie ----------------------------------------------------------------
library(patchwork)
library(reshape2)
library(ROCR)
library(MASS)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plyr)
library(VIM)
library(mice)
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(Boruta)
library(car)
library(factorMerger)
library(caretEnsemble)
library(caTools)
library(funModeling)
library(DALEX)
library(breakDown)
library(gridExtra)

# Importazione dati e prime analisi ---------------------------------------
train <- read.csv("train.csv")

# Trasformare le variabili binarie nel dataset
binary_vars <- sapply(train, function(x) all(x %in% c(0, 1)))
train[binary_vars] <- lapply(train[binary_vars], factor, levels = c(0, 1), labels = c(0, 1))

# Rimozione variabili problematiche

plot_gg1 = function(column){
  ggplot(data = train, mapping = aes(x = {{column}})) +
    geom_bar(position = 'dodge') +
    scale_fill_manual('Legenda', values = c("lightblue", "blue"))
}

# Crea una lista di grafici ggplot
p1 <- plot_gg1(statistical_report)
p2 <- plot_gg1(ratio_nullHyperlinks)
p3 <- plot_gg1(ratio_intErrors)
p4 <- plot_gg1(ratio_intRedirection)
p5 <- plot_gg1(submit_email)
p6 <- plot_gg1(sfh)

# Combina i grafici in una griglia
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)

# Distribuzione variabile target ------------------------------------------
class(train$status)
table(train$status) / nrow(train)

plot_gg = function(column){
  ggplot(data = train, mapping = aes(x = {{column}}, fill = status)) +
    geom_bar(position = 'dodge') +
    scale_fill_manual('Legenda', values = c("lightblue", "blue"))
}

plot_gg(status) + 
  ggtitle("Phishing and Legitimate website")

# Dati mancanti -----------------------------------------------------------

#Eliminazioni delle variabili superflue
train <- subset(train, select = -c(statistical_report, ratio_nullHyperlinks, ratio_intErrors,ratio_intRedirection,submit_email,sfh,url ))

# Conto valori mancanti per variabile per fare grafico 
missing_data <- train %>% 
  summarise_all(function(x) sum(is.na(x) | x == "")) %>% 
  gather(variable, missing_count)

missingness = aggr(train,
                   col=c('navyblue','yellow'),numbers=TRUE,sortVars=TRUE,
                   cex.axis=.7,gap=2) 

# MODEL SELCTION ----------------------------------------------------------

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

train_selected = train[,selected]
#validation_selected = validation[,selected]

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
plot_all_variables_in_grids(train_selected, "status")