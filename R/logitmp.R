logitmp <- function(list_of_models,
                             plots = TRUE,
                             abbreviations = TRUE,
                             abbreviations_names = "data",
                             abbreviations_id = "numeric",
                             percentual = TRUE,
                             annotations = FALSE,
                             color = FALSE){
  # part one: define some metainformation for upcoming functions
  nr_models <- length(list_of_models)
  model_vector <- 1:nr_models
  # name the models:
  orig_names_temp <- as.character(substitute(list_of_models)) # get original model names
  if (abbreviations == TRUE) {
    if(abbreviations_id == "letters"){
    model_names <- paste(abbreviations_names, letters[model_vector], sep=" ")
    }
    else if(abbreviations_id == "LETTERS"){
      model_names <- paste(abbreviations_names, LETTERS[model_vector], sep=" ")
    }
  else{
    model_names <- paste(abbreviations_names, model_vector, sep=" ")
  }
  orig_names <- orig_names_temp[2:length(orig_names_temp)]
  cat("________________________________________________________________________________\n USED MODELS \n")
  print(data.frame(abbreviations = model_names, original = orig_names),
        row.names = FALSE)
  }
  else{
  model_names <- orig_names_temp[2:length(orig_names_temp)]
  }
  # part two: compute  contingency tables
  confusion_matrix_func <- function(model, rel = FALSE){
    confusion_matrix <- table(Predicted = round(model$fitted.values),
                              Actual = model$y,
                              useNA = "always")[-3,-3]
    if (sum(na.omit(as.integer(dimnames(confusion_matrix)$Predicted) + 1)) == 2) {
      dimnames(confusion_matrix)$Predicted <- c("1", "0")
    }
    else{
      dimnames(confusion_matrix)$Predicted <- c("0", "1")
    }
    dimnames(confusion_matrix)$Actual <- c("0", "1")
        TN <- confusion_matrix[which(as.numeric(row.names(confusion_matrix)) == 0),1]
        FN <- confusion_matrix[which(as.numeric(row.names(confusion_matrix)) == 0),2]
        FP <- confusion_matrix[which(as.numeric(row.names(confusion_matrix)) == 1),1]
        TP <- confusion_matrix[which(as.numeric(row.names(confusion_matrix)) == 1),2]
        N <- TP+FP+FN+TN
        if (rel == TRUE) return(c(TN/N, FP/N, FN/N, TP/N))
        else return(c(TN,FP,FN,TP,N))
  }
  confma_list <- data.frame(sapply(list_of_models,
                                   confusion_matrix_func,
                                   rel = FALSE))
  row.names(confma_list) <- c("predicted and actual -",
                              "predicted + but actual -",
                              "predicted - but actual +",
                              "predicted and actual +",
                              "sample number")
  names(confma_list) <- model_names
  # print contingency table and beautify output:
  cat("________________________________________________________________________________\n CONTINGENCY TABLE \n")
  print(confma_list)
  if (percentual == TRUE){
  # second contingency table with relative values
  confma_list_rel <- data.frame(sapply(list_of_models,
                                       confusion_matrix_func,
                                       rel = TRUE))
  row.names(confma_list_rel) <- c("predicted and actual -",
                                  "predicted + but actual -",
                                  "predicted - but actual +",
                                  "predicted and actual +")
  names(confma_list_rel) <- model_names
  # print relational contingency table and beautify output:
  cat("________________________________________________________________________________\n CONTINGENCY TABLE (percentual) \n")
  print(100*confma_list_rel, digits = 2)
  }
  cat("________________________________________________________________________________\n MEASURES FOR CLASSIFICATION QUALITY \n")
  # part three: compute some measures for classification quality
  accuracy_func <- function (model){
    # compute a confusion matrix again:
    confusion_matrix <- table(Predicted = round(model$fitted.values),
                              Actual = model$y,
                              useNA = "always")[-3,-3]
    if (sum(na.omit(as.integer(dimnames(confusion_matrix)$Predicted) + 1)) == 2) {
      dimnames(confusion_matrix)$Predicted <- c("1", "0")
    }
    else{
      dimnames(confusion_matrix)$Predicted <- c("0", "1")
    }
    dimnames(confusion_matrix)$Actual <- c("0", "1")
    TN <- confusion_matrix[which(as.numeric(row.names(confusion_matrix)) == 0),1]
    FN <- confusion_matrix[which(as.numeric(row.names(confusion_matrix)) == 0),2]
    FP <- confusion_matrix[which(as.numeric(row.names(confusion_matrix)) == 1),1]
    TP <- confusion_matrix[which(as.numeric(row.names(confusion_matrix)) == 1),2]
    N <- TP+FP+FN+TN
    # define the measures
    # Fielding and Bell 1997
    prevalence <- sum(model$y)/N
    kappa <- ((TP+TN) -
                (((TP+FN)*(TP+FP) + (FP+TN)*(FN+TN))/N)) /
                (N - (((TP+FN)*(TP+FP) + (FP+TN)*(FN+TN))/N))
    nmi <- (-TP*log(TP) - FP*log(FP) -
              FN*log(FN) - TN*log(TN) +
              (TP+FP)*log(TP+FP) +
              (FN+TN)*log(FN+TN)) / (N*log(N) -
              ((TP+FN)*log(TP+FN) +
                 (FP+TN)*log(FP+TN))) #normalized_mutual_information_statistic
    misclassification_rate <- (FP+FN)/N
    odds_ratio <- (TP*TN)/(FN*FP)
    # Powers 2011
    tpr <- TP/(TP+FN)   # Recall / True positive Rate / sensitivity
    tpa <- TP/(TP+FP) # precision / Confidence / positive_predictive_power
    tnr <- TN/(FP+TN)   # Inverse Recall / True negative Rate / specifity
    tna <- TN/(FN+TN)  # Inverse Precision / True negative Accuracy / negative_predictive_power
    fpr <- FP/(FP+TN)  # fallout
    fnr <- FN/(TP+FN) # miss_rate
    tcr <- (TP+TN)/N  # accuracy / tca / correct_classification_rate
    dice <- TP/(TP+(FN+FP)/2)   # F1 # F-measure # = 2/((1/tpa)+(1/tpr)) (fawcett 2006)
    jaccard <- TP/(N-TN)
    Informedness <-  tpr-fpr   # true_skill_statistic <- tpr + tnr - 1 ( Allouche et al 2006)
    Markedness <-  tpa + tna -1 # DeltaP in Psychology - "the normative measure of contingency"
    #Crawley
    overdispersion_factor <- model$deviance/model$df.residual
    # miscellanous
    dof <- model$df.null - model$df.residual
    null_deviance <- model$null.deviance
    resid_deviance <- model$deviance
    deviance_diff <- resid_deviance - null_deviance
    AIC <- model$aic
    AUC <- as.numeric(pROC::auc(pROC::roc(response = model$y, predictor = fitted(model))))
    Nagel_R2 <- fmsb::NagelkerkeR2(model)$R2
    if (TN + FN != 0) {
      Hoslem_p_value <-ResourceSelection::hoslem.test(model$y, fitted(model),g=10)$p.value
    }else
    {
      Hoslem_p_value <- NaN
    }
    # trim the measure output to three digits:
    accuracy <- round(c(
      dof,
      prevalence,
      null_deviance,
      resid_deviance,
      deviance_diff,
      AIC,
      overdispersion_factor,
      odds_ratio,
      misclassification_rate,
      tcr,
      tpr,
      tnr,
      tpa,
      tna,
      fpr,
      fnr,
      dice,
      jaccard,
      Informedness,
      Markedness,
      Hoslem_p_value,
      nmi,
      kappa,
      Nagel_R2,
      AUC), digits=3)
    # return the output
    return(accuracy)
  }
  # apply the function, define the names of measures and print the table:
  final_data_frame <- data.frame(lapply(list_of_models, accuracy_func))
  names(final_data_frame)<- model_names
  row.names(final_data_frame) <- c("degrees of freedom",
                                   "prevalence",
                                   "null deviance",
                                   "residual deviance",
                                   "difference in deviance",
                                   "AIC",
                                   "overdispersion factor",
                                   "odds ratio",
                                   "misclassification rate",
                                   "tcr (accuracy)",
                                   "tpr (sensitivity)",
                                   "tnr (specifity)",
                                   "tpa (precision)",
                                   "tna (inverse precision)",
                                   "fpr (fallout)",
                                   "fnr (miss rate)",
                                   "dice (F-measure)",
                                   "jaccard",
                                   "Informedness (TSS)",
                                   "Markedness",
                                   "Hoslem p-value",
                                   "NMI",
                                   "kappa",
                                   "Nagelkerke R\u00B2",
                                   "AUC")
  print(final_data_frame)
  # definitions and annotations
  if (annotations == FALSE){
    cat("________________________________________________________________________________\n NOTE: For further information choose option \"annotations = TRUE\" \n")
  }
  else{
    cat("________________________________________________________________________________\n ANNOTATIONS:
For further information please refer to:
  (1) Crawley, Michael J. 2007. The R Book. Chichester, England; Hoboken, N.J.: Wiley.
  (2) Fielding, Alan H., and John F. Bell. 1997. 'A Review of Methods for the Assessment of Prediction Errors in Conservation Presence/absence Models.' Environmental Conservation 24 (01): 38\u002D49.
  (3) Powers, David Martin. 2011. 'Evaluation: From Precision, Recall and F-Measure to ROC, Informedness, Markedness and Correlation.' Journal of Machine Learning Technologies 2 (1): 37\u002D63.
  (4) Allouche, Omri, Asaf Tsoar, and Ronen Kadmon. 2006. 'Assessing the Accuracy of Species Distribution Models: Prevalence, Kappa and the True Skill Statistic (TSS).' Journal of Applied Ecology 43 (6): 1223\u002D32.
  (5) Subhash R. Lele, Jonah L. Keim and Peter Solymos (2014). ResourceSelection: Resource Selection (Probability) Functions for Use-Availability Data. R package version 0.2\u002D4.
  (6) Minato Nakazawa (2014). fmsb: Functions for medical statistics book with some demographic data. R package version 0.4.4.
  (7) Xavier Robin, Natacha Turck, Alexandre Hainard, Natalia Tiberti, Fr\u00E9d\u00E9rique Lisacek, Jean-Charles Sanchez and Markus M\u00fcller (2011). pROC: an open-source package for R and S+ to analyze and compare ROC curves. BMC Bioinformatics, 12, p. 77

  predicted and actual -    = true negative = TN
  predicted + but actual -  = false positive = FP
  predicted - but actual +  = false negative = FN
  predicted and actual +    = true positive = TP
  N <- TP+FP+FN+TN

    overdispersion factor   = deviance/df.residual (1)
    odds ratio              <- (TP*TN)/(FN*FP) (2)
    misclassification_rate  <- (FP+FN)/N (2)
    tcr (accuracy)          <- (TP+TN)/N (3)
    tpr (sensitivity)       <- TP/(TP+FN) (3)
    tnr (specifity)         <- TN/(FP+TN) (3)
    tpa (precision)         <- TP/(TP+FP) (3)
    tna (inverse precision) <- TN/(FN+TN) (3)
    fpr (fallout)           <- FP/(FP+TN) (3)
    fnr (miss rate)         <- FN/(TP+FN) (3)
    dice (F-measure)        <- TP/(TP+(FN+FP)/2) (3)
    jaccard                 <- TP/(N-TN) (3)
    Informedness (TSS)      <- tpr-fpr (3); see (4) for TSS
    Markedness              <- tpa + tna -1 (3)
    Hoslem p-value:         computed with R-package ResourceSelection (5)
    NMI                     = normalized mutual information statistic (2)
    Nagelkerke R\u00B2:     computed with formula from R-package fmsb (6)
    AUC:                    computed with R-package pROC (7) \n")
  }
  # part four: compute the plots
  # the calibration plot
  # compute the data.frame that is needed for the plot:
  calplot_basic <- function(model, n_bins = 10){
    bin_cuts <- (0:n_bins)/n_bins
    bin_centers <- ((1:n_bins - 0.5))/n_bins
    data_breaks <- cut(model$fitted.values,
                       breaks = bin_cuts,
                       labels = 1:n_bins)
    n_total <- tapply(model$y,
                      data_breaks,
                      length)
    n_presence <- tapply(model$y,
                         data_breaks,
                         sum)
    observed_prob <- n_presence/n_total
    data.frame(bin_centers, observed_prob)
  }
  calplot_data <- lapply(list_of_models, calplot_basic)
  names(calplot_data) <- model_names
  # the ROC-plot
  # compute the data.frame that is needed for the plot:
  roc_basic <- function (rocmod){
    pROC::roc(response = rocmod$y,
        predictor = fitted(rocmod))
  }
  roc_data <- lapply(list_of_models, roc_basic)
  names(roc_data) <- model_names
  if (plots==TRUE){
  # output an empty plot-structure:
  plot(c(-0.05, 1.05), c(-0.05, 1.05),
       type = "n",
       ylab="Observed Occurrences",
       xlab="Predicted Probability",
       main="")
  abline(a = 0,
         b = 1,
         lty = 3,
         col = "darkgrey")
  if(color == TRUE){
  legend("bottomright",
         model_names,
         pch = 1,
         col = c(2:(nr_models+1)),
         cex=0.7)
  # function to plot the lines:
  calplot_lines_func <- function(movect){
    points(calplot_data[[movect]]$bin_centers,
           calplot_data[[movect]]$observed_prob,
           col = movect+1,
           type = "b",
           cex = 0.8)
  }
  # plot all models:
  invisible(lapply(model_vector, calplot_lines_func))
  }else
  {
    legend("bottomright",
           model_names,
           pch = c(2:(nr_models+1)),
           cex=0.7)
    # function to plot the lines:
    calplot_lines_func <- function(movect){
      points(calplot_data[[movect]]$bin_centers,
             calplot_data[[movect]]$observed_prob,
             pch = movect+1,
             type = "b",
             cex = 0.8)
    }
    # plot all models:
    invisible(lapply(model_vector, calplot_lines_func))
  }
  # output an empty plot-structure:
  plot(c(-0.05, 1.05), c(-0.05, 1.05),
       type = "n",
       xlab = "1-Specificity",
       ylab = "Sensitivity",
       main = "")
  abline(a = 0,
         b = 1,
         lty = 3,
         col = "darkgrey")
  if(color==TRUE){
  legend("bottomright",
         model_names,
         lty = 1,
         col = c(2:(nr_models+1)),
         cex=0.7)
  # function to plot the lines:
  roc_plot_lines_func <- function(move){
  lines(1-roc_data[[move]]$specificities,
        roc_data[[move]]$sensitivities,
        col = move+1)
  }
  # plot all models without printing the output in the console:
  invisible(lapply(model_vector, roc_plot_lines_func))
  }else
  {
    legend("bottomright",
           model_names,
           lty = c(1:nr_models),
           cex=0.7)
    # function to plot the lines:
    roc_plot_lines_func <- function(move){
      lines(1-roc_data[[move]]$specificities,
            roc_data[[move]]$sensitivities,
            lty = move,
            lwd = 1.5)
    }
    # plot all models without printing the output in the console:
    invisible(lapply(model_vector, roc_plot_lines_func))
  }
  }
  invisible(list(confusion_matrix = confma_list,
                 calibration_plot_data = calplot_data,
                 ROC_data = roc_data,
                 measures = final_data_frame))
}
