levels(Clean_trainData[,"race"]) <- union(levels(test[,"race"]),levels(Clean_trainData[,"race"]))
levels(Clean_trainData[,"sex"]) <- union(levels(test[,"sex"]),levels(Clean_trainData[,"sex"]))
levels(Clean_trainData[,"nativecountry"]) <- union(levels(test[,"nativecountry"]),levels(Clean_trainData[,"nativecountry"]))
levels(Clean_trainData[,"occupation"]) <- union(levels(test[,"occupation"]),levels(Clean_trainData[,"occupation"]))
#Using test data to evaluate model
decision_tree = rpart(incomelevel ~ age + workclass + occupation + maritalstatus + relationship +
                        educationnum + capitalgain + capitalloss + hoursperweek + nativecountry + race, data = Clean_trainData)
fancyRpartPlot(decision_tree)

decision.model = rpart(incomelevel ~ age + workclass+ fnlwgt + education +
                       educationnum + maritalstatus + occupation +
                       relationship +  race + sex + capitalgain +
                       capitalloss + hoursperweek + nativecountry, data = Clean_trainData)

#predict on test data

pred_tree = predict(decision.model, test, type = "class")
combined_tree = cbind(test$incomelevel, pred_tree)
#combined_tree = combined_tree[,2:ncol(combined_tree)]
colnames(combined_tree) = c("Actual", "Predict")
df_combined_tree = data.frame(combined_tree)
confusion.table = table(df_combined_tree$Actual, df_combined_tree$Predict)
#View(df_combined_tree)
outcome <- levels(test$incomelevel)
colnames(confusion.table) <- c(paste("Actual",outcome[1]), outcome[2])
rownames(confusion.table) <- c(paste("Predicted",outcome[1]), outcome[2])
print(xtable(confusion.table, caption='Confusion Table for Decision Tree'), caption.placement = 'top', type="html", file = "dt.html", html.table.attributes='border=1 align="center" bgcolor="pink"')
preds_tree = prediction(as.numeric(prediction_tree), as.numeric(test$incomelevel))
perf_tree = performance(preds_tree, "tpr", "fpr")
plot(perf_tree)
N.test = length(pred_tree)
accuracy <- sum(diag(confusion.table)) / N.test
cat("accuracy of the model =", accuracy*100, "\nmissclassification =", 100-accuracy*100, "%\n")


#sensitivity
cat("Model sensitivity =", confusion.table[1,1]/sum(confusion.table[1,]) * 100)
#Specificity
cat("Model specificity =", confusion.table[2,2]/sum(confusion.table[2,]) * 100)

#precision
cat("Precision =", confusion.table[1,1]/sum(confusion.table[,1]))
#Recall
cat("Recall = ", confusion.table[1,1]/sum(confusion.table[1,]))
#F1 score
f1n = 2*(confusion.table[1,1]/sum(confusion.table[,1]))*(confusion.table[1,1]/sum(confusion.table[1,]))
f1d = (confusion.table[1,1]/sum(confusion.table[,1]))+(confusion.table[1,1]/sum(confusion.table[1,]))
f1 = f1n/f1d
cat("F1 score = ", f1)

levels(Clean_trainData[,"workclass"]) <- union(levels(test[,"workclass"]),levels(Clean_trainData[,"workclass"]))
#logistic regression

N.obs <- dim(Clean_trainData)[1]
y <- rep(0, N.obs)
y[Clean_trainData$incomelevel==levels(Clean_trainData$incomelevel)[2]] <- 1
lfit <- glm(y ~ age + educationnum + hoursperweek + fnlwgt + education+ workclass + capitalgain + capitalloss + maritalstatus + nativecountry + occupation + relationship + race + sex, data = Clean_trainData,family="binomial")


missingLevelsToNA<-function(object,data){
  
  #Obtain factor predictors in the model and their levels ------------------
  
  factors<-(gsub("[-^0-9]|as.factor|\\(|\\)", "",names(unlist(object$xlevels))))
  factorLevels<-unname(unlist(object$xlevels))
  modelFactors<-as.data.frame(cbind(factors,factorLevels))
  
  
  #Select column names in your data that are factor predictors in your model -----
  
  predictors<-names(data[names(data) %in% factors])
  
  
  #For each factor predictor in your data if the level is not in the model set the value to NA --------------
  
  for (i in 1:length(predictors)){
    found<-data[,predictors[i]] %in% modelFactors[modelFactors$factors==predictors[i],]$factorLevels
    if (any(!found)) data[!found,predictors[i]]<-NA
  }
  
  data
  
}
test_dat = missingLevelsToNA(lfit, test)
pred = predict(lfit, test_dat, type="response")
pred1 = round(pred,0)
combined = cbind(test_dat$incomelevel, pred1)
colnames(combined) = c("Actual", "Predicted")

df_combined= data.frame(combined)
confusion.table1 = table(df_combined$Actual, df_combined$Predicted)
#View(df_combined_tree)
outcome1 <- levels(test_dat$incomelevel)
colnames(confusion.table1) <- c(paste("Actual",outcome1[1]), outcome1[2])
rownames(confusion.table1) <- c(paste("Predicted",outcome1[1]), outcome1[2])
print(xtable(confusion.table1, caption='Confusion Table for Logistic regression'), caption.placement = 'top', type="html", file = "dt1.html", html.table.attributes='border=1 align="center" bgcolor="pink"')
preds = prediction(as.numeric(pred), as.numeric(test_dat$incomelevel))
perf = performance(preds, "tpr", "fpr")
plot(perf)

#Accuracy
N.test1 = length(pred)
accuracy1 <- sum(diag(confusion.table1)) / N.test1
print ("Performance metrics for Logistic regression")
cat("accuracy of logistic regression model =", accuracy1*100, "\nMissclassification rate =", 100-accuracy1*100, "%\n")
#sensitivity

cat("Model sensitivity =", confusion.table1[1,1]/sum(confusion.table1[1,]) * 100)
#Specificity
cat("Model specificity =", confusion.table1[2,2]/sum(confusion.table1[2,]) * 100)

#precision
cat("Precision =", confusion.table1[1,1]/sum(confusion.table1[,1]))
#Recall
cat("Recall = ", confusion.table1[1,1]/sum(confusion.table1[1,]))
#F1 score
f1n = 2*(confusion.table1[1,1]/sum(confusion.table1[,1]))*(confusion.table1[1,1]/sum(confusion.table1[1,]))
f1d = (confusion.table1[1,1]/sum(confusion.table1[,1]))+(confusion.table1[1,1]/sum(confusion.table1[1,]))
f1 = f1n/f1d
cat("F1 score = ", f1)

