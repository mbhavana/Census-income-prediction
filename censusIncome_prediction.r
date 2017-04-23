trainFileName = "adult.data"; testFileName = "adult.test"

if (!file.exists (trainFileName))
  download.file (url = "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",
                 destfile = trainFileName)

if (!file.exists (testFileName))
  download.file (url = "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test",
                 destfile = testFileName)
#trainFileName = "censusData.csv"

colNames = c ("age", "workclass", "fnlwgt", "education", 
              "educationnum", "maritalstatus", "occupation",
              "relationship", "race", "sex", "capitalgain",
              "capitalloss", "hoursperweek", "nativecountry",
              "incomelevel")

Read_data  <-  function (file = "adult.data") 
{
  train = read.table (file, header = FALSE, sep = ",",
                      strip.white = TRUE, col.names = colNames,
                      na.strings = "?", stringsAsFactors = TRUE)
}

train  <- Read_data()
View(train_file)

#summary of data
str (train)
# cleaning the data
table (complete.cases (train))
summary  (train [!complete.cases(train),])
# Distribution of the income level factor in the entire training data set.
table (train$incomelevel)

#removing missing values

Clean_trainData = train [!is.na (train$workclass) & !is.na (train$occupation), ]
Clean_trainData = Clean_trainData [!is.na (Clean_trainData$nativecountry), ]

#Explore the Data
#/*Create box plots to understand relationships between independent numerical variables
#and dependent categorical variable (Income)*/
bp1 = boxplot(Clean_trainData$age ~ Clean_trainData$incomelevel,
              main = "Age distribution for different income levels",
              xlab = "Income Levels", ylab = "Age", col = "rosybrown1")

# Removing outliers
bp1$stats
for(i in 1:nrow(Clean_trainData)){
  if(Clean_trainData[i,"age"] > 73)
    Clean_trainData[i,"age"] = 73
}

bp11 = boxplot(Clean_trainData$age ~ Clean_trainData$incomelevel,
              main = "Age distribution for different income levels after removing outliers
              ",
              xlab = "Income Levels", ylab = "Age", col = "pink2")

library(ggplot2)
library(gridExtra)
incomeBelow50K = (Clean_trainData$incomelevel == "<=50K")
xlimit = c (min (Clean_trainData$age), max (Clean_trainData$age))
ylimit = c (0, 1600)

hist1 = qplot (age, data = Clean_trainData[incomeBelow50K,], margins = TRUE, 
               binwidth = 2, xlim = xlimit, ylim = ylimit, colour = incomelevel)

hist2 = qplot (age, data = Clean_trainData[!incomeBelow50K,], margins = TRUE, 
               binwidth = 2, xlim = xlimit, ylim = ylimit, colour = incomelevel)

grid.arrange (hist1, hist2, nrow = 2)

#Education num vs income
bp2 <- boxplot(Clean_trainData$educationnum ~ Clean_trainData$incomelevel,
               main = "Years of education for different income levels",
               xlab = "Income Levels", ylab = "Edu years", col = "sandybrown")

#Remove outliers
mean <- mean(Clean_trainData$educationnum)
sd <- sd(Clean_trainData$educationnum)
minLimit <- mean-2*sd
maxLimit <- mean + 2*sd
for(i in nrow(Clean_trainData)){
  if(Clean_trainData[i,"educationnum"] >= maxLimit){
    Clean_trainData[i,"educationnum"] = maxLimit
  }
  if (Clean_trainData[i,"educationnum"] <= minLimit){
    Clean_trainData[i,"educationnum"] = minLimit
  }
}

#bp after removing outliers
bp2 <- boxplot(Clean_trainData$educationnum ~ Clean_trainData$incomelevel,
               main = "Years of education for different income levels after ",
               xlab = "Income Levels", ylab = "Edu years", col = "peachpuff3")

bp2 <- boxplot(Clean_trainData$fnlwgt ~ Clean_trainData$incomelevel,
               main = "final weight estimate for different income levels",
               xlab = "Income Levels", ylab = "Edu years", col = "peachpuff3")

bp2 <- boxplot(Clean_trainData$hoursperweek ~ Clean_trainData$incomelevel,
               main = "Hours per week for different income levels",
               xlab = "Income Levels", ylab = "Edu years", col = "blue")
