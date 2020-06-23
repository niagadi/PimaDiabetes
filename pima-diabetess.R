## ---------------------------------------------------------------------------------------
#Check computer details#
version


## ---------------------------------------------------------------------------------------
#Download the dataset from an excel sheet on computer#
if(!require(readxl)) install.packages("readxl")
library(readxl)
file.exists("/Users/nerp/Desktop/diabetes.xlsx")
pima<- read_xlsx("/Users/nerp/Desktop/diabetes.xlsx")


## ---------------------------------------------------------------------------------------
#Check the first 6 rows of the provided dataset#
head(pima)


## ---------------------------------------------------------------------------------------
#See the overall structure of the dataset#
str(pima)


## ---------------------------------------------------------------------------------------
#Find the proportion of the dataset that is NA#
sum(is.na(pima))
sum(is.na(pima))/(ncol(pima)*nrow(pima))


## ---------------------------------------------------------------------------------------
#Download commonly needed packages for visualisation#
if (!require(plyr)) install.packages("plyr")
if (!require(ggrepel)) install.packages("ggrepel")
if (!require(gridExtra)) install.packages("gridExtra")
library(plyr)
library(dplyr)
library(gridExtra)


## ---------------------------------------------------------------------------------------
#Convert outcome to binary factor class#
pima$Outcome <- as.factor(pima$Outcome)

#Box and whisker plot for pregnancies stratified by T2DM status#
p1<- pima %>% group_by(Outcome) %>% ggplot(aes(y=Pregnancies)) + geom_boxplot(aes(fill=Outcome)) + labs(title="Pregnancies Across Presentation of T2DM", xlab= "Presentation of T2DM", ylab = "Pregnancies") + theme(axis.text.x=element_blank()) + theme(legend.position = "none")

#Box and whisker plot for blood glucose stratified by T2DM status#
p2<- pima %>% group_by(Outcome) %>% ggplot(aes(y=Glucose)) + geom_boxplot(aes(fill=Outcome)) + labs(title="Blood Glucose Across Presentation of T2DM", xlab= "Presentation of T2DM", ylab = "Blood Glucose") + theme(axis.text.x=element_blank()) + theme(legend.position = "none")

#Box and whisker plot for BP stratified by T2DM status#
p3<- pima %>% group_by(Outcome) %>% ggplot(aes(y=BloodPressure)) + geom_boxplot(aes(fill=Outcome)) + labs(title="Blood Pressure Across Presentation of T2DM", xlab= "Presentation of T2DM", ylab = "Blood Pressure") + theme(axis.text.x=element_blank()) + theme(legend.position = "none")

#Box and whisker plot for skin thickness stratified by T2DM status#
p4<- pima %>% group_by(Outcome) %>% ggplot(aes(y=SkinThickness)) + geom_boxplot(aes(fill=Outcome)) + labs(title="Skin Thickness Across Presentation of T2DM", xlab= "Presentation of T2DM", ylab = "Skin Thickness") + theme(axis.text.x=element_blank()) + theme(legend.position = "none")

#arrange the plots in a 2x2#
grid.arrange(p1,p2,p3,p4,ncol=2)


## ---------------------------------------------------------------------------------------
#Box and whisker plot for insulin levels stratified by T2DM status#
p5<- pima %>% group_by(Outcome) %>% ggplot(aes(y=Insulin)) + geom_boxplot(aes(fill=Outcome)) + labs(title="Insulin Levels Across Presentation of T2DM", xlab= "Presentation of T2DM", ylab = "Insulin Levels") + theme(axis.text.x=element_blank()) + theme(legend.position = "none")

#Box and whisker plot for BMI stratified by T2DM status#
p6<- pima %>% group_by(Outcome) %>% ggplot(aes(y=BMI)) + geom_boxplot(aes(fill=Outcome)) + labs(title="BMI Across Presentation of T2DM", xlab= "Presentation of T2DM", ylab = "BMI") + theme(axis.text.x=element_blank()) + theme(legend.position = "none")

#Box and whisker plot for pedigree levels stratified by T2DM status#
p7<- pima %>% group_by(Outcome) %>% ggplot(aes(y=DiabetesPedigreeFunction)) + geom_boxplot(aes(fill=Outcome)) + labs(title="Diabetes Pedigree Levels Across Presentation of T2DM", xlab= "Presentation of T2DM", ylab = "Pedigree Levels") + theme(axis.text.x=element_blank()) + theme(legend.position = "none")

#Box and whisker plot for ages stratified by T2DM status#
p8<- pima %>% group_by(Outcome) %>% ggplot(aes(y=Age)) + geom_boxplot(aes(fill=Outcome)) + labs(title="Age Across Presentation of T2DM", xlab= "Presentation of T2DM", ylab = "Age") + theme(axis.text.x=element_blank()) + theme(legend.position = "none")

#arrange the plots in a 2x2#
grid.arrange(p5,p6,p7,p8,ncol=2)


## ---------------------------------------------------------------------------------------
#scatterplot for Skin Thickness on BMI across T2DM status#
pima %>% group_by(Outcome) %>% ggplot() + geom_point(aes(x=BMI, y=SkinThickness, colour=Outcome), alpha=0.5) + theme(legend.position = "none") + ggtitle("Cutaneous Thickness on BMI Across T2DM Patients")

#correlation plot between all variables in `pima`"
if(!require(corrplot)) install.packages("corrplot")
library(corrplot)
par( mfrow = c(1,1) )
pima$Outcome<- as.numeric(pima$Outcome)
cor1 <- cor(pima, method = c("pearson"))
cor2 <- round(cor(cor1),2)
corrplot::corrplot(cor2, method = "circle")


## ---------------------------------------------------------------------------------------
if(!require(caret)) install.packages("caret")
library(caret)

#Attempt to convert the variables to class of factor#
pima[sapply(pima, is.character)] <- lapply(pima[sapply(pima, is.character)], as.factor)

#Split both datasets into an 80:20 split using a seed#
set.seed(1, sample.kind="Rounding")
sample_size<- floor(0.2*nrow(pima))
test_index <- sample(seq_len(nrow(pima)), size=sample_size)
pima_train<- pima[-test_index,]
pima_test<- pima[test_index,]


## ---------------------------------------------------------------------------------------
#Set the plots to a 2x2#
par(mfrow = c(2,2))
#Calculate the deviance residuals, coefficients, and significances#
pima_train$Outcome<- as.factor(pima_train$Outcome)
reg_glm <- glm(Outcome ~ ., data = pima_train, family = binomial(link = "logit"))
summary(reg_glm)
plot(reg_glm)


## ---------------------------------------------------------------------------------------
#Optimised logistic regression with significant parameters#
par(mfrow = c(2,2))
reg_glm2 <- glm(Outcome~ Pregnancies+Glucose+BloodPressure+BMI + DiabetesPedigreeFunction, data=pima_train, family=binomial(link= "logit"))
summary(reg_glm2)
plot(reg_glm2)


## ---------------------------------------------------------------------------------------
pima_test$Outcome<- as.factor(pima_test$Outcome)
pred_glm <- predict(reg_glm2,pima_test, type = "response")
pred_glm <- ifelse(pred_glm <= 0.5, 1, 2)
pred_glm<- as.factor(pred_glm)
cm_glm<- confusionMatrix(pred_glm,pima_test$Outcome)
cm_glm


## ---------------------------------------------------------------------------------------
#Develop a model to test the accuracy on the number of neighbours#
reg_knn <- train(Outcome ~ ., data= pima_test, method = "knn", tuneGrid = data.frame(k = seq(1,20,1)))
reg_knn %>% ggplot()+geom_line(aes(x=k, y=Accuracy)) + labs(title= "Change in Regression Accuracy with varying optimal kNN")


## ---------------------------------------------------------------------------------------
pred_knn <- predict(reg_knn, pima_test, type="raw")
cm_knn<- confusionMatrix(pred_knn,pima_test$Outcome)
cm_knn


## ---------------------------------------------------------------------------------------
#Develop a random forest regression model to produce a confusion matrix#
reg_rf <- train(Outcome ~ ., method = "rf", data = pima_train)
pred_rf <- predict(reg_rf, pima_test)
pima_test$Outcome<- as.factor(pima_test$Outcome)
cm_rf<- confusionMatrix(pred_rf,pima_test$Outcome)
cm_rf


## ---------------------------------------------------------------------------------------
# Develop an XGB regression model to produce a confusion matrix#
if(!require(xgboost)) install.packages("xgboost")
par(mfrow = c(2,1))
reg_xgb <- train(Outcome ~ ., method = "xgbTree", data = pima_test)
plot(reg_xgb)
pred_xgb <- predict(reg_xgb, pima_test)
cm_xgb<- confusionMatrix(pred_xgb,pima_test$Outcome)
cm_xgb


## ---------------------------------------------------------------------------------------
# Create a knitr table of the regression approaches and their values#
cm<- data.frame(c("General Logistic Model", "k Nearest Neighbours", "Random Forest", "XGBoost"), c(cm_glm$byClass[1], cm_knn$byClass[1], cm_rf$byClass[1], cm_xgb$byClass[1]), c(cm_glm$byClass[2], cm_knn$byClass[2], cm_rf$byClass[2], cm_xgb$byClass[2]), c(cm_glm$byClass[11], cm_knn$byClass[11], cm_rf$byClass[11], cm_xgb$byClass[11]))
cm<- as_tibble(cm)
colnames(cm) <- c("Regression", "Sensitivity", "Specificity", "Balanced Accuracy")
cm %>% knitr::kable()


## ---------------------------------------------------------------------------------------
knitr::purl()

