
# Install and load required packages --------------------------------------

options(jupyter.plot_mimetypes = 'image/png')
if (!require(caTools)){
  install.packages('caTools', repos='http://cran.us.r-project.org')
}

if (!require(ROCR)){
  install.packages('ROCR', repos='http://cran.us.r-project.org')
}


if (!require(rpart)){
  install.packages('rpart', repos='http://cran.us.r-project.org')
}


if (!require(randomForest)){
  install.packages('randomForest', repos='http://cran.us.r-project.org')
}

if (!require(caret)){
  install.packages('caret', repos='http://cran.us.r-project.org')
}


if (!require(e1071)){
  install.packages('e1071', repos='http://cran.us.r-project.org')
}


if (!require(rpart.plot)){
  install.packages('rpart.plot', repos='http://cran.us.r-project.org')
}


if (!require(ggplot2)){
  install.packages('ggplot2', repos='http://cran.us.r-project.org')
}


# Logistic Regression -----------------------------------------------------

census<-read.csv("c:/Users/Bhavana/Desktop/Data science/R/census.csv")

set.seed(2000)

split<-sample.split(census$over50k,SplitRatio = 0.6)

train<-subset(census, split==TRUE)

test<-subset(census, split==FALSE)
logit1<-glm(over50k~., data=train, family='binomial')
summary(logit1)  # from this we can see which variables are signiificant

prediction<-predict(logit1, type="response",newdata=test)

# Contrction confusion matrix
combine = cbind(test$over50k, prediction>0.5)
colnames(combine) = c("Actual", "Predicted")
df_combine= data.frame(combine)
confusion_table = table(df_combine$Actual, df_combine$Predicted)
output <- levels(test$over50k)
colnames(confusion_table) <- c(paste("Actual",output[1]), output[2])
rownames(confusion_table) <- c(paste("Predicted",output[1]), output[2])
file = "C:/Users/Bhavana/Desktop/Data science/R/lg.html"
print(xtable(confusion_table, caption='Confusion Table for Logistic regression'), caption.placement = 'top', type="html", file = file, html.table.attributes='border=1 align="center" bgcolor="pink"')


table<-table(prediction>0.5,test$over50k)
accuracy<-sum(diag(table))/(sum(table))
accuracy
# Prediction function
ROCRpred = prediction(prediction, test$over50k)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve with colors and threshold labels

plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

as.numeric(performance(ROCRpred, "auc")@y.values)

print ("Performance metrics for Logistic regression")
cat("accuracy of logistic regression model =", accuracy*100, "\nMissclassification rate =", 100-accuracy*100, "%\n")
#sensitivity

cat("Model sensitivity =", confusion_table[1,1]/sum(confusion_table[1,]) * 100)
#Specificity
cat("Model specificity =", confusion_table[2,2]/sum(confusion_table[2,]) * 100)

#precision
cat("Precision =", confusion_table[1,1]/sum(confusion_table[,1]))
#Recall
cat("Recall = ", confusion_table[1,1]/sum(confusion_table[1,]))
#F1 score
f1n = 2*(confusion_table[1,1]/sum(confusion_table[,1]))*(confusion_table[1,1]/sum(confusion_table[1,]))
f1d = (confusion_table[1,1]/sum(confusion_table[,1]))+(confusion_table[1,1]/sum(confusion_table[1,]))
f1 = f1n/f1d
cat("F1 score = ", f1)


# Decision tree -----------------------------------------------------------
CARTmodel = rpart(over50k ~. , data=train, method="class")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
prp(CARTmodel, digits = 6)
fancyRpartPlot(CARTmodel)
summary(CARTmodel)


# Predicting accuracy of decision tree ------------------------------------

prediction<-predict(CARTmodel, type="class",newdata=test)

table<-table(prediction,test$over50k)
accuracy<-sum(diag(table))/(sum(table))
accuracy

# confusion matrix

combine = cbind(test$over50k, prediction)
colnames(combine) = c("Actual", "Predicted")
df_combine= data.frame(combine)
confusion_table = table(df_combine$Actual, df_combine$Predicted)
output <- levels(test$over50k)
colnames(confusion_table) <- c(paste("Actual",output[1]), output[2])
rownames(confusion_table) <- c(paste("Predicted",output[1]), output[2])
file = "C:/Users/Bhavana/Desktop/Data science/R/dt.html"
print(xtable(confusion_table, caption='Confusion Table for Decision tree'), caption.placement = 'top', type="html", file = file, html.table.attributes='border=1 align="center" bgcolor="pink"')

#Roc curve
prediction<-predict(CARTmodel,newdata=test)[ , 2]

# Prediction function
ROCRpred = prediction(prediction, test$over50k)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")


plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

as.numeric(performance(ROCRpred, "auc")@y.values)

### Precision, recall of decision tree

print ("Performance metrics for Decision tree")
cat("accuracy of decision tree model =", accuracy*100, "\nMissclassification rate =", 100-accuracy*100, "%\n")
#sensitivity

cat("Model sensitivity =", confusion_table[1,1]/sum(confusion_table[1,]) * 100)
#Specificity
cat("Model specificity =", confusion_table[2,2]/sum(confusion_table[2,]) * 100)

#precision
cat("Precision =", confusion_table[1,1]/sum(confusion_table[,1]))
#Recall
cat("Recall = ", confusion_table[1,1]/sum(confusion_table[1,]))
#F1 score
f1n = 2*(confusion_table[1,1]/sum(confusion_table[,1]))*(confusion_table[1,1]/sum(confusion_table[1,]))
f1d = (confusion_table[1,1]/sum(confusion_table[,1]))+(confusion_table[1,1]/sum(confusion_table[1,]))
f1 = f1n/f1d
cat("F1 score = ", f1)



# Random forest -----------------------------------------------------------
library(randomForest)
set.seed(1)
rfb = randomForest(over50k~. -nativecountry, data=train)
plot(rfb, log='y')
prediction<-predict(rfb,newdata=test)

table<-table(prediction,test$over50k)

accuracy<-(sum(diag(table)))/(sum(table))
accuracy
getTree(rfb, labelVar = TRUE)
combine = cbind(test$over50k, prediction)
colnames(combine) = c("Actual", "Predicted")
df_combine= data.frame(combine)
confusion_table = table(df_combine$Actual, df_combine$Predicted)
output <- levels(test$over50k)
colnames(confusion_table) <- c(paste("Actual",output[1]), output[2])
rownames(confusion_table) <- c(paste("Predicted",output[1]), output[2])
file = "C:/Users/Bhavana/Desktop/Data science/R/rf.html"
print(xtable(confusion_table, caption='Confusion Table for Random forest'), caption.placement = 'top', type="html", file = file, html.table.attributes='border=1 align="center" bgcolor="pink"')


varImpPlot(rfb)
vu = varUsed(rfb, count=TRUE)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(rfb$forest$xlevels[vusorted$ix]))

### using cforest to plot
cf = cforest(over50k~. -nativecountry, data=train)
varImpPlot(rfb)

## Roc curve
ROCRpred = prediction(prediction, test$over50k)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve with colors and threshold labels

plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

as.numeric(performance(ROCRpred, "auc")@y.values)

### Precision, recall of decision tree

print ("Performance metrics for Decision tree")
cat("accuracy of decision tree model =", accuracy*100, "\nMissclassification rate =", 100-accuracy*100, "%\n")
#sensitivity
print("Random Forest model Evaluation")
cat("Model sensitivity =", confusion_table[1,1]/sum(confusion_table[1,]) * 100)
#Specificity
cat("Model specificity =", confusion_table[2,2]/sum(confusion_table[2,]) * 100)

#precision
cat("Precision =", confusion_table[1,1]/sum(confusion_table[,1]))
#Recall
cat("Recall = ", confusion_table[1,1]/sum(confusion_table[1,]))
#F1 score
f1n = 2*(confusion_table[1,1]/sum(confusion_table[,1]))*(confusion_table[1,1]/sum(confusion_table[1,]))
f1d = (confusion_table[1,1]/sum(confusion_table[,1]))+(confusion_table[1,1]/sum(confusion_table[1,]))
f1 = f1n/f1d
cat("F1 score = ", f1)

cat("accuracy of random forest model =", accuracy*100, "\nMissclassification rate =", 100-accuracy*100, "%\n")
#sensitivity


#### cp parametr
set.seed(2)
library(caret)
library(e1071)


# Number of folds

tr.control = trainControl(method = "cv", number = 10)

# cp values
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))

# Cross-validation
tr = train(over50k ~., data = train, method = "rpart", trControl = tr.control, tuneGrid = cartGrid)

# Extract tree
best.tree = tr$finalModel



CARTmodel = rpart(over50k ~. , data=train, cp=0.002)

prp(CARTmodel, digits = 6)

prediction<-predict(CARTmodel, type="class",newdata=test)

table<-table(prediction,test$over50k)
accuracy<-sum(diag(table))/(sum(table))
accuracy
