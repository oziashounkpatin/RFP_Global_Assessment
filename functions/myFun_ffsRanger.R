ffsRanger <- function (predictors,
                 response,
                 method = "rf",
                 metric = ifelse(is.factor(response), "Accuracy", "RMSE"),
                 maximize = ifelse(metric == "RMSE", FALSE, TRUE),
                 withinSE = FALSE,
                 minVar = 2,
                 trControl = caret::trainControl(),
                 tuneLength = 3,
                 tuneGrid = NULL,
                 seed = sample(1:1000, 1),
                 verbose=TRUE,
                 ...){
  trControl$returnResamp <- "final"
  if(class(response)=="character"){
    response <- factor(response)
    if(metric=="RMSE"){
      metric <- "Accuracy"
      maximize <- TRUE
    }
  }
  if (trControl$method=="LOOCV"){
    if (withinSE==TRUE){
      print("warning: withinSE is set to FALSE as no SE can be calculated using method LOOCV")
      withinSE <- FALSE
    }}
  se <- function(x){sd(x, na.rm = TRUE)/sqrt(length(na.exclude(x)))}
  n <- length(names(predictors))
  
  acc <- 0
  perf_all <- data.frame(matrix(ncol=length(predictors)+3,
                                nrow=choose(n, minVar)+(n-minVar)*(n-minVar+1)/2))
  names(perf_all) <- c(paste0("var",1:length(predictors)),metric,"SE","nvar")
  if(maximize) evalfunc <- function(x){max(x,na.rm=TRUE)}
  if(!maximize) evalfunc <- function(x){min(x,na.rm=TRUE)}
  isBetter <- function (actmodelperf,bestmodelperf,
                        bestmodelperfSE=NULL,
                        maximization=FALSE,
                        withinSE=FALSE){
    if(withinSE){
      result <- ifelse (!maximization, actmodelperf < bestmodelperf-bestmodelperfSE,
                        actmodelperf > bestmodelperf+bestmodelperfSE)
    }else{
      result <- ifelse (!maximization, actmodelperf < bestmodelperf,
                        actmodelperf > bestmodelperf)
    }
    return(result)
  }
  #### chose initial best model from all combinations of two variables
  minGrid <- t(data.frame(combn(names(predictors),minVar)))
  for (i in 1:nrow(minGrid)){
    if (verbose){
      print(paste0("model using ",paste0(minGrid[i,],collapse=","), " will be trained now..." ))
    }
    set.seed(seed)
    #adaptations for pls:
    tuneGrid_orig <- tuneGrid
    if(method=="pls"&!is.null(tuneGrid)&any(tuneGrid$ncomp>minVar)){
      tuneGrid <- data.frame(ncomp=tuneGrid[tuneGrid$ncomp<=minVar,])
      if(verbose){
        print(paste0("note: maximum ncomp is ", minVar))
      }
    }
    #adaptations for tuning of ranger:
    if(method=="ranger"&!is.null(tuneGrid)&any(tuneGrid$mtry>minVar)){
      tuneGrid$mtry <- minVar
      if(verbose){
        print("invalid value for mtry. Reset to valid range.")
      }
    }
    
    #train model:
    model <- caret::train(predictors[,minGrid[i,]],
                          response,
                          method=method,
                          metric=metric,
                          trControl=trControl,
                          tuneLength = tuneLength,
                          tuneGrid = tuneGrid,
                          ...)
    
    tuneGrid <- tuneGrid_orig
    
    ### compare the model with the currently best model
    actmodelperf <- evalfunc(model$results[,names(model$results)==metric])
    actmodelperfSE <- se(
      sapply(unique(model$resample$Resample),
             FUN=function(x){mean(model$resample[model$resample$Resample==x,
                                                 metric],na.rm=TRUE)}))
    if (i == 1){
      bestmodelperf <- actmodelperf
      bestmodelperfSE <- actmodelperfSE
      bestmodel <- model
    } else{
      if (isBetter(actmodelperf,bestmodelperf,maximization=maximize,withinSE=FALSE)){
        bestmodelperf <- actmodelperf
        bestmodelperfSE <- actmodelperfSE
        bestmodel <- model
      }
    }
    acc <- acc+1
    
    variablenames <- names(model$trainingData)[-length(names(model$trainingData))]
    perf_all[acc,1:length(variablenames)] <- variablenames
    perf_all[acc,(length(predictors)+1):ncol(perf_all)] <- c(actmodelperf,actmodelperfSE,length(variablenames))
    if(verbose){
      print(paste0("maximum number of models that still need to be trained: ",
                   round(choose(n, minVar)+(n-minVar)*(n-minVar+1)/2-acc,0)))
    }
  }
  #### increase the number of predictors by one (try all combinations)
  #and test if model performance increases
  selectedvars <- names(bestmodel$trainingData)[-which(
    names(bestmodel$trainingData)==".outcome")]
  if (maximize){
    selectedvars_perf <- max(bestmodel$results[,metric])
  } else{
    selectedvars_perf <- min(bestmodel$results[,metric])
  }
  selectedvars_SE <- bestmodelperfSE
  if(verbose){
    print(paste0(paste0("vars selected: ",paste(selectedvars, collapse = ',')),
                 " with ",metric," ",round(selectedvars_perf,3)))
  }
  for (k in 1:(length(names(predictors))-minVar)){
    startvars <- names(bestmodel$trainingData)[-which(
      names(bestmodel$trainingData)==".outcome")]
    nextvars <- names(predictors)[-which(
      names(predictors)%in%startvars)]
    if (length(startvars)<(k+(minVar-1))){
      message(paste0("Note: No increase in performance found using more than ",
                     length(startvars), " variables"))
      bestmodel$selectedvars <- selectedvars
      bestmodel$selectedvars_perf <- selectedvars_perf[-length(selectedvars_perf)]
      bestmodel$selectedvars_perf_SE <- selectedvars_SE[-length(selectedvars_SE)] #!!!
      bestmodel$perf_all <- perf_all
      bestmodel$perf_all <- bestmodel$perf_all[!apply(is.na(bestmodel$perf_all), 1, all),]
      bestmodel$perf_all <- bestmodel$perf_all[colSums(!is.na(bestmodel$perf_all)) > 0]
      bestmodel$minVar <- minVar
      bestmodel$type <- "ffs"
      return(bestmodel)
      break()
    }
    for (i in 1:length(nextvars)){
      if(verbose){
        print(paste0("model using additional variable ",nextvars[i], " will be trained now..." ))
      }
      set.seed(seed)
      
      #adaptation for pls:
      tuneGrid_orig <- tuneGrid
      if(method=="pls"&!is.null(tuneGrid)&any(tuneGrid$ncomp>ncol(predictors[,c(startvars,nextvars[i])]))){
        tuneGrid<- data.frame(ncomp=tuneGrid[tuneGrid$ncomp<=ncol(predictors[,c(startvars,nextvars[i])]),])
        if(verbose){
          print(paste0("note: maximum ncomp is ", ncol(predictors[,c(startvars,nextvars[i])])))
        }}
      #adaptation for ranger:
      if(method=="ranger"&!is.null(tuneGrid)&any(tuneGrid$mtry>ncol(predictors[,c(startvars,nextvars[i])]))){
        tuneGrid$mtry[tuneGrid$mtry>ncol(predictors[,c(startvars,nextvars[i])])] <- ncol(predictors[,c(startvars,nextvars[i])])
        if(verbose){
          print("invalid value for mtry. Reset to valid range.")
        }
      }
      
      
      
      model <- caret::train(predictors[,c(startvars,nextvars[i])],
                            response,
                            method = method,
                            metric=metric,
                            trControl = trControl,
                            tuneLength = tuneLength,
                            tuneGrid = tuneGrid,
                            ...)
      tuneGrid <- tuneGrid_orig
      actmodelperf <- evalfunc(model$results[,names(model$results)==metric])
      actmodelperfSE <- se(
        sapply(unique(model$resample$Resample),
               FUN=function(x){mean(model$resample[model$resample$Resample==x,
                                                   metric],na.rm=TRUE)}))
      if(isBetter(actmodelperf,bestmodelperf,
                  selectedvars_SE[length(selectedvars_SE)], #SE from model with nvar-1
                  maximization=maximize,withinSE=withinSE)){
        bestmodelperf <- actmodelperf
        bestmodelperfSE <- actmodelperfSE
        bestmodel <- model
      }
      acc <- acc+1
      
      variablenames <- names(model$trainingData)[-length(names(model$trainingData))]
      perf_all[acc,1:length(variablenames)] <- variablenames
      perf_all[acc,(length(predictors)+1):ncol(
        perf_all)] <- c(actmodelperf,actmodelperfSE,length(variablenames))
      if(verbose){
        print(paste0("maximum number of models that still need to be trained: ",
                     round(choose(n, minVar)+(n-minVar)*(n-minVar+1)/2-acc,0)))
      }
    }
    selectedvars <- c(selectedvars,names(bestmodel$trainingData)[-which(
      names(bestmodel$trainingData)%in%c(".outcome",selectedvars))])
    selectedvars_SE <- c(selectedvars_SE,bestmodelperfSE)
    if (maximize){
      selectedvars_perf <- c(selectedvars_perf,max(bestmodel$results[,metric]))
      if(verbose){
        print(paste0(paste0("vars selected: ",paste(selectedvars, collapse = ',')),
                     " with ", metric," ",round(max(bestmodel$results[,metric]),3)))
      }
    }
    if (!maximize){
      selectedvars_perf <- c(selectedvars_perf,min(bestmodel$results[,metric]))
      if(verbose){
        print(paste0(paste0("vars selected: ",paste(selectedvars, collapse = ',')),
                     " with ",metric," ",round(min(bestmodel$results[,metric]),3)))
      }
    }
  }
  bestmodel$selectedvars <- selectedvars
  bestmodel$selectedvars_perf <- selectedvars_perf
  bestmodel$selectedvars_perf_SE <- selectedvars_SE
  bestmodel$perf_all <- perf_all
  bestmodel$perf_all <- bestmodel$perf_all[!apply(is.na(bestmodel$perf_all), 1, all),]
  bestmodel$minVar <- minVar
  bestmodel$type <- "ffs"
  bestmodel$perf_all <- bestmodel$perf_all[colSums(!is.na(bestmodel$perf_all)) > 0]
  return(bestmodel)
}