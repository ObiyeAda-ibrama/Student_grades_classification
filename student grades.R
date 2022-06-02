install.packages('tidyverse')
install.packages('dyplr')
install.packages('PerformanceAnalytics')
install.packages('ggplot2')
install.packages('forcats')
install.packages('factoextra')
install.packages('NbClust')
install.packages('rpart')
install.packages('rpart.plot')
install.packages('party')
install.packages('e1071')
install.packages('tibble')
install.packages('tidytext')
install.packages('textrank')
install.packages('mlbench')
install.packages("glmnet")
install.packages('rvest')
install.packages('naniar')
install.packages("arules")
install.packages("arulesViz")
install.packages('gridExtra')
install.packages('neuralnet')
install.packages('pROC')
install.packages('xgboost')
library(gridExtra)
library(grid)
library(arules)
library(arulesViz)
library(factoextra)
library(tidyverse)
library(mlbench)
library(glmnet)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)
library(forcats)
library(NbClust)
library(rpart)
library(rpart.plot)
library(party)
library(e1071)
library(naniar)
library(tibble)
library(tidytext)
library(textrank)
library(rvest)
library(neuralnet)
library(pROC)
library(xgboost)

grades <- read.csv(file = 'student_grades.csv', header = T, stringsAsFactors = T)
dim(grades) #dimensions of data (observations on features)
str(grades) #structure of data

#DATA CLEANING
missing_data <- colSums(is.na(grades)) #shows sum of missing data for all features
missing_data
grades <- grades %>% distinct() #checking for and removing Duplicate data
#fixing errors
grades$Medu <- grades$Medu %>% dplyr::na_if(0)
grades$Fedu <- grades$Fedu %>% dplyr::na_if(0)
vis_miss(grades)#to view missing data
mcar_test(grades) #checking missingness
ggplot(grades, aes(x = Medu)) +geom_bar()#plotting to find MoCT
ggplot(grades, aes(x = Fedu)) +geom_bar()#plotting to find MoCT
grades$Medu <- ifelse(is.na(grades$Medu) == TRUE, 4, grades$Medu)#replacing missing value with mode
grades$Fedu <- ifelse(is.na(grades$Fedu) == TRUE, 2, grades$Fedu)#replacing missing value with mode


#DATA PREPARATION
grades <- grades[-1]#as we are not predicting as regards to school, so it is taken out
grades$Total_ALc <- (grades$Dalc + grades$Walc)/2 #total alcohol intake is calculated
grades$Total_ALc <- round(grades$Total_ALc) #rounded up so it can stand as factor after transformation
#feature removal
grades <- grades[-27]#dalc taken out
grades <- grades[-26]#walc taken out


#DATA TRANSFORMATION
grades$Pass <- as.logical(grades$Pass) #making pass a boolean datatype
#making numbered categorical data,factors
grades$Medu <- as.ordered(grades$Medu) 
grades$Fedu <- as.ordered(grades$Fedu)
grades$traveltime <- as.ordered(grades$traveltime)
grades$studytime <- as.ordered(grades$studytime)
grades$famrel <- as.ordered(grades$famrel)
grades$freetime <- as.ordered(grades$freetime)
grades$goout <- as.ordered(grades$goout)
grades$health <- as.ordered(grades$health)
grades$Total_ALc <- as.ordered(grades$Total_ALc)


#DATA EXPLORATION
#simple categorical EDA
#pass rates
ggplot(aes(x = Pass), data = grades) + geom_bar()  + ggtitle("PASS RATES")#Barplot of pass and fail
prop.table(table(grades$Pass)) # Determine the percentage of passes and failures
table(grades$Pass)

#pass rates by gender
ggplot(aes(x = sex), data = grades) +geom_bar(stat = "count") + facet_wrap(~Pass) + xlab('Gender') + ggtitle("GENDER PASS RATES") #Bar chart plot of Male and Female according to pass rates
prop.table(table(grades$sex))
prop.table(table(grades$sex,by=grades$Pass)) #percentage of gender by pass rates
table(grades$sex,by=grades$Pass)

#pass rates by address
ggplot(aes(x = address), data = grades) +geom_bar(stat = "count") + facet_wrap(~Pass) + ggtitle("PASS RATES BASED ON ADDRESS") #Bar chart plot oF pass rates based on address
prop.table(table(grades$address))
prop.table(table(grades$address,by=grades$Pass)) #percentage of pass rates based on address

#pass rates by family size and pstatus
famsizeplot <- ggplot(aes(x = famsize), data = grades) +geom_bar(stat = "count") + facet_wrap(~Pass) + xlab('Family Size') 
prop.table(table(grades$famsize))
prop.table(table(grades$Pass,by=grades$famsize)) #percentage of pass rates based on famsize
pstatusplot <- ggplot(aes(x = Pstatus), data = grades) +geom_bar(stat = "count") + facet_wrap(~Pass) + xlab('Parental Status') 
prop.table(table(grades$Pstatus))
prop.table(table(grades$Pass,by=grades$Pstatus))
grid.arrange(famsizeplot, pstatusplot, ncol = 2, top = textGrob("PASS RATES BASED ON FAMILY SIZE AND PARENTAL STATUS"))

#pass rates by reason and guardian
reasonplot <- ggplot(aes(x = reason), data = grades) +geom_bar(stat = "count") + facet_wrap(~Pass) + xlab('Reason') 
prop.table(table(grades$reason))
prop.table(table(grades$Pass,by=grades$reason)) #percentage of pass rates based on reason
guardianplot <- ggplot(aes(x = guardian), data = grades) +geom_bar(stat = "count") + facet_wrap(~Pass) + xlab('Guardian') 
prop.table(table(grades$guardian))
prop.table(table(grades$Pass,by=grades$guardian))
grid.arrange(reasonplot, guardianplot, ncol = 2, top = textGrob("PASS RATES BASED ON REASON AND GUARDIAN"))

#pass rates by MEDU and FEDU
meduplot <- ggplot(aes(x = Medu), data = grades) + geom_histogram(stat = "count") + facet_wrap(~Pass) + xlab("Mother's Education")
prop.table(table(grades$Medu))
prop.table(table(grades$Pass,by=grades$Medu)) 
feduplot <- ggplot(aes(x = Fedu), data = grades) + geom_histogram(stat = "count") + facet_wrap(~Pass) + xlab("Father's Education")
prop.table(table(grades$Fedu))
prop.table(table(grades$Pass,by=grades$Fedu)) 
grid.arrange(meduplot, feduplot, ncol = 2, top = textGrob("PASS RATES BASED ON PARENT'S EDUCATION"))

#pass rates by Mjob and Fjob
mjobplot <- ggplot(aes(x = Mjob), data = grades1) + geom_bar(stat = "count") + facet_wrap(~Pass) + xlab("Mother's Job")
prop.table(table(grades1$Mjob))
prop.table(table(grades1$Pass,by=grades1$Mjob)) 
fjobplot <- ggplot(aes(x = Fjob), data = grades1) + geom_bar(stat = "count") + facet_wrap(~Pass) + xlab("Father's Job")
prop.table(table(grades1$Fjob))
prop.table(table(grades1$Pass,by=grades1$Fjob)) 
grid.arrange(mjobplot, fjobplot, nrow = 2, top = textGrob("PASS RATES BASED ON PARENT'S JOB"))

#pass rates by studytime and traveltime
traveltimeplot <- ggplot(aes(x = traveltime), data = grades1) + geom_bar(stat = "count") + facet_wrap(~Pass) + xlab("Travel Time")
prop.table(table(grades1$traveltime))
prop.table(table(grades1$Pass,by=grades1$traveltime)) 
studytimeplot <- ggplot(aes(x = studytime), data = grades1) + geom_bar(stat = "count") + facet_wrap(~Pass) + xlab("Study Time")
prop.table(table(grades1$studytime))
prop.table(table(grades1$Pass,by=grades1$studytime)) 
grid.arrange(traveltimeplot, studytimeplot, ncol = 2, top = textGrob("PASS RATES BASED ON TRAVEL TIME AND STUDY TIME"))

#pass rates by schoolsup, famsup, paid, activities, 
schoolsuplot <- ggplot(aes(x = schoolsup), data = grades1) + geom_histogram(stat = "count") + facet_wrap(~Pass) + xlab("School Support")
prop.table(table(grades1$schoolsup))
prop.table(table(grades1$Pass,by=grades1$schoolsup)) 
famsuplot <- ggplot(aes(x = famsup), data = grades1) + geom_histogram(stat = "count") + facet_wrap(~Pass) + xlab("Family Support")
prop.table(table(grades1$famsup))
prop.table(table(grades1$Pass,by=grades1$famsup)) 
paidplot <- ggplot(aes(x = paid), data = grades1) + geom_histogram(stat = "count") + facet_wrap(~Pass) + xlab("Paid Support")
prop.table(table(grades1$paid))
prop.table(table(grades1$Pass,by=grades1$paid)) 
activitiesplot <- ggplot(aes(x = activities), data = grades1) + geom_histogram(stat = "count") + facet_wrap(~Pass) + xlab("Activities")
prop.table(table(grades1$activities))
prop.table(table(grades1$Pass,by=grades1$activities)) 
grid.arrange(schoolsuplot, famsuplot, paidplot, activitiesplot, ncol = 2, top = textGrob("PASS RATES BASED ON SUPPORTS AND ACTIVITIES"))

#pass rates by nursery, higher, internet, romantic
nurseryplot <- ggplot(aes(x = nursery), data = grades1) + geom_bar(stat = "count") + facet_wrap(~Pass) + xlab("Nursery")
prop.table(table(grades1$nursery))
prop.table(table(grades1$Pass,by=grades1$nursery)) 
higherplot <- ggplot(aes(x = higher), data = grades1) + geom_bar(stat = "count") + facet_wrap(~Pass) + xlab("Higher")
prop.table(table(grades1$higher))
prop.table(table(grades1$Pass,by=grades1$higher)) 
internetplot <- ggplot(aes(x = internet), data = grades1) + geom_bar(stat = "count") + facet_wrap(~Pass) + xlab("Internet")
prop.table(table(grades1$internet))
prop.table(table(grades1$Pass,by=grades1$internet)) 
romanticplot <- ggplot(aes(x = romantic), data = grades1) + geom_bar(stat = "count") + facet_wrap(~Pass) + xlab("romantic")
prop.table(table(grades1$romantic))
prop.table(table(grades1$Pass,by=grades1$romantic)) 
grid.arrange(nurseryplot, higherplot, internetplot, romanticplot, ncol = 2)

#pass rates by famrel, freetime, goout, total_alc
famrelplot <- ggplot(aes(x = famrel), data = grades1) + geom_bar(stat = "count") + facet_wrap(~Pass) + xlab("Family relationships")
prop.table(table(grades1$famrel))
prop.table(table(grades1$Pass,by=grades1$famrel)) 
freetimeplot <- ggplot(aes(x = freetime), data = grades1) + geom_bar(stat = "count") + facet_wrap(~Pass) + xlab("Free time")
prop.table(table(grades1$freetime))
prop.table(table(grades1$Pass,by=grades1$freetime)) 
gooutplot <- ggplot(aes(x = goout), data = grades1) + geom_bar(stat = "count") + facet_wrap(~Pass) + xlab("Going out")
prop.table(table(grades1$goout))
prop.table(table(grades1$Pass,by=grades1$goout)) 
Total_Alcplot <- ggplot(aes(x = Total_ALc), data = grades1) + geom_bar(stat = "count") + facet_wrap(~Pass) + xlab("Total Alcohol Consumption")
prop.table(table(grades1$Total_ALc))
prop.table(table(grades1$Pass,by=grades1$Total_ALc)) 
grid.arrange(famrelplot, freetimeplot, gooutplot, Total_Alcplot, ncol = 2)

#health, absences and pass
data%>%
  group_by(Pass)%>%
  ggplot(aes(x=factor(health), y=absences, color=Pass))+ geom_smooth(aes(group=Pass), method="lm", se=FALSE) + ggtitle("RELATIONSHIP BETWEEN HEALTH, ABSENCES AND GRADES")


#continuous EDA
#Boxplot of Age BY PASS RATE
boxplot(grades$age~grades$Pass, xlab = 'Pass',ylab = 'Age',main = 'BOXPLOT OF AGE BY PASS RATE')
tapply(grades$age,grades$Pass, summary) #summary of the age by PASS
#Boxplot of FAILURES BY PASS RATE
boxplot(grades$failures~grades$Pass, xlab = 'Pass',ylab = 'Failures',main = 'BOXPLOT OF FAILURES BY PASS RATE')
tapply(grades$failures,grades$Pass, summary) #summary of the FAILURES by PASS


#CORRELATION MATRIX
chart.Correlation(grades1, histogram = TRUE)
grades1_st <- select(grades1, sex, age, address, famsize, Pstatus, Medu, Fedu, Mjob, Fjob, Pass) #for creating a subset using dylpr package
grades2_nd <- select(grades1, reason, guardian, traveltime, studytime, failures, schoolsup, famsup, paid, activities, Pass)
grades3_rd <- select(grades1, nursery, higher, internet, romantic, famrel, freetime, goout, health, absences, Total_ALc, Pass)
chart.Correlation(grades1_st, histogram = TRUE)
chart.Correlation(grades2_nd, histogram = TRUE)
chart.Correlation(grades3_rd, histogram = TRUE)


#DATA ANALYSIS
#standardisation
rs_function <- function(x){(x-min(x))/(max(x)-min(x))}#min-max scaling
grades$age <- rs_function(grades$age)#all other features are around 1-5, as they are levels.. and target variable is binary
grades$absences <- rs_function(grades$absences)
write.csv(grades, file = 'updated_grades2.csv')

#TESTSET & TRAINSET
set.seed(23)
grades[,"train"] <- ifelse(runif(nrow(grades))<0.7, 1, 0)#small dataset, split up to 70 & 30
trainset <- grades[grades$train == "1",]
testset <- grades[grades$train == "0",] 
trainset <- trainset[-30]
testset <- testset[-30] 

#RANDOM FOREST
rf_PASS <- cforest(Pass~., data = trainset, control = cforest_unbiased(mtry = 20, ntree = 500))
rf_prob <- predict(rf_PASS, newdata = test_data, type = "response") 
rf_pred <- ifelse(rf_prob>0.5, 1, 0)
#test
table(actual = testset$Pass, predicted = rf_pred) 
mean(rf_pred==testset$Pass)
#variable importance
ForestVarImp <- varimp(rf_PASS)
barplot(ForestVarImp) 
#evaluation (AUC)
roc(testset$Pass, rf_pred, plot = TRUE, col = "red",
    legacy.axes = TRUE, xlab = "False Positive Rate", ylab = "True
Positive Rate", lwd = 2, print.auc = TRUE)

#Suppoprt Vector Machine
grades1 <- read.csv(file = 'updated_grades2.csv', header = T, stringsAsFactors = TRUE)
grades1 <- grades1[-1]
#representing factors as numbers
grades1$sex <- as.numeric(grades1$sex)
grades1$address <- as.numeric(grades1$address)
grades1$famsize <- as.numeric(grades1$famsize)
grades1$Pstatus <- as.numeric(grades1$Pstatus)
grades1$Mjob <- as.numeric(grades1$Mjob)
grades1$Fjob <- as.numeric(grades1$Fjob)
grades1$reason <- as.numeric(grades1$reason)
grades1$guardian <- as.numeric(grades1$guardian)
grades1$schoolsup <- as.numeric(grades1$schoolsup)
grades1$famsup <- as.numeric(grades1$famsup)
grades1$paid <- as.numeric(grades1$paid)
grades1$activities <- as.numeric(grades1$activities)
grades1$nursery <- as.numeric(grades1$nursery)
grades1$higher <- as.numeric(grades1$higher)
grades1$internet <- as.numeric(grades1$internet)
grades1$romantic <- as.numeric(grades1$romantic)
str(grades1)
#SVM TEST & TRAIN SET
set.seed(13)
grades1[,"train"] <- ifelse(runif(nrow(grades1))<0.7, 1, 0)#small dataset, split up to 70 & 30
trainset1 <- grades1[grades1$train == "1",]
testset1 <- grades1[grades1$train == "0",] 
trainset1 <- trainset1[-30]
testset1 <- testset1[-30] 

svm_trainset <- trainset1
svm_pass <- svm(Pass~., data = svm_trainset)
summary(svm_pass)
#test
test_data1 <- testset1[-28] 
svm_prob <- predict(svm_pass, newdata = test_data1, type = "response")
svm_pred <- ifelse(svm_prob>0.5, 1, 0)
table(predicted = svm_pred, actual = testset1$Pass)
mean(svm_pred==testset1$Pass) 
#evaluation (AUC)
roc(testset1$Pass, svm_prob, plot = TRUE, col = "red",
    legacy.axes = TRUE, xlab = "False Positive Rate", ylab = "True
Positive Rate", lwd = 2, print.auc = TRUE)

#GRADIENT BOOSTING
grades_xg <- grades1
grades_xg$age <- grades$age
grades_xg$absences <- grades$absences
grades_xg <-grades_xg[-30]
pass_response <- grades_xg$Pass
grades_xg <-rs_function(grades_xg)
grades_xg$Pass <- pass_response
#xgboost test and train set
set.seed(53)
grades_xg[,"train"] <- ifelse(runif(nrow(grades_xg))<0.7, 1, 0)
trainset_xg <- grades_xg[grades_xg$train == "1",]
testset_xg <- grades_nn[grades_xg$train == "0",]
trainset_xg <- trainset_xg[-30]
testset_xg <- testset_xg[-30]
#matrix and labels
train_matrix <- model.matrix(Pass~., trainset_xg)
test_matrix <- model.matrix(Pass~., testset_xg) 
train_labels <- trainset$Pass 

xgb_pass <- xgboost(data = train_matrix, label = train_labels,
                    eta = 0.2, nrounds = 100, max_depth = 19, objective =
                      "binary:logistic", verbose=1) 
xgb_prob <- predict(xgb_pass, test_matrix, type = "response")
#test
xgb_pred <- ifelse(xgb_prob > 0.5, 1, 0)
table(actual = testset_xg$Pass, predicted = xgb_pred)
mean(xgb_pred==testset_xg$Pass)
#evaluation_AUC
roc(testset_xg$Pass, xgb_prob, plot = TRUE, col = "red",
    legacy.axes = TRUE, xlab = "False Positive Rate", ylab = "True
Positive Rate", lwd = 2, print.auc = TRUE)