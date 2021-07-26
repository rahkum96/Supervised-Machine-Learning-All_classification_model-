#####################################################################
####################### Classificaion Case Study ###################
#####################################################################

#In case of a classification problem:  the target variable is a categorical in  nature.
#We'll be doing Logistic Regression and Classification decision tree. 


# Step-1
# Identify the Problem Statement, What are you trying to solve?
#here the ask is:
#"if the titanic ship is once again built, and people are boarded, and the
#journey starts again, then can you help us identify whether the person whom we
#are boarding will survive another crash or not?"


# Step-2
# Identify the Target variable in the data, What value will be predicted?
##spot this "Survived" in the data, might not be present with this name (e.g. Surv_x, sv_x)
#make sure you have the correct target variable

# Step-3
#Loading the raw Data

titanic<- read.csv(choose.files(),na.strings=c(""," ","NA","NULL"))
View(titanic)
head(titanic)
str(titanic)
summary(titanic)
class(titanic)


#####passenger ID####
#everytime a new passenger comes its ID will be something different. 
#A person having certain ID has no relation to whether that person will 
#survive the crash or not
##if you include this column then ML algo might find some patterns/relations and give
#importance to ID also- that will be a useless model from the business perspective

##ALWAYS THINK FROM THE BUSINESS PERSPECTIVE
#The input I am choosing is that making sense? If no, then reject. 

#####Pclass#####
#People travelling in 1st class might have access to life boats/jackets - they have 
#higher chance of survival.

#####Name#####
#same concept like PassengerID
##you can't say whether a person will survive or not, based on his/her name.

#####Sex#####
#In the movie also, we saw who was over the plank. Here, gender is a very 
#important variable and we'll see in our data exploration also why it's important. 

#####Age#####
#It is directly related to the swimming capacity or harsh condition tolerance

#####ticket#####
##ticket no. has not relation will survival


#####Fare#####
##people paying higher fares might have better access to different facilities in the ship
#and that can increase their chances of survival

#####cabin#####
##in which cabin the people are travelling
##lot of missing values
summary(titanic)

##529 missing values out of 714 variables- so we don't choose that column


#####Embarked#####
##docking area where a passenger boards the ship
#if the person didn't board the ship before it crashed, then he/she will
#survive

# Removing useless columns in the data, and explore the rest
UselessColumns=c('PassengerId', 'Name', 'Cabin', 'Ticket','Fare')
titanic[, UselessColumns]=NULL

head(titanic)

str(titanic)

# to see how many features we can move to factor
lapply(titanic,function(x)length(unique(x)))

#Let's move the features Survived, Pclass, Sex, Embarked to be factors
cols<- c("Survived", "Pclass", "Sex", "Embarked","SibSp","Parch")
for (i in cols){
  titanic[,i]<-as.factor(titanic[,i])
  
}
#now look on structure of titanic data
str(titanic)

# Step-5
# Whether it is a Regression problem or Classification?

#target variable - survive or not survive
##survival:Yes/1 or Survival:No/0


#step-06. Data Pre-processing
#To check missing values
colSums(is.na(titanic))
colSums(titanic=="") # to check where is missing data to see in charater or factor data

#now look how much % missing data is
colSums(is.na(titanic))/nrow(titanic) #19.86% we will calculate the missing value
colSums(titanic=="")/nrow(titanic) #77%- Cabin, 0.2 % Embarked

#Now look outliers in Age 
boxplot(titanic$Age)#yes found
m1<- median(titanic$Age, na.rm = T)
m1
#replace the missing value with m1 In Age 
titanic$Age[which(is.na(titanic$Age))]<- m1
colSums(is.na(titanic)) #done no missing values 
colSums(titanic=="")

#we will mode function to handle missing value in Embarked 
#table(titanic$Embarked)
getmode <- function(titanic) {
  uniqv <- unique(titanic)
  uniqv[which.max(tabulate(match(titanic, uniqv)))]
}
getmode(titanic$Embarked)
#replace with mode 
titanic$Embarked[which(is.na(titanic$Embarked))]<- getmode(titanic$Embarked)
colSums(titanic=="") #done with missing missing embarked categorise 


#########Missing values treatements done#######################################
###############################################################################

# Step-07-----------Data visualization

# Explore each "Potential" predictor for distribution and Quality
############################################################

# Exploring MULTIPLE CONTINUOUS features
ColsForHist=c("Age","Fare")

#Splitting the plot window into four parts
par(mfrow=c(2,3))

# library to generate professional colors
library(RColorBrewer) 

# looping to create the histograms for each column
for (ColumnName in ColsForHist){
  hist(titanic[,c(ColumnName)], main=paste('Histogram of:', ColumnName), 
       col=brewer.pal(8,"Paired"))
}


############################################################
# Exploring MULTIPLE CATEGORICAL features
ColsForBar=c("Survived","Pclass","Embarked")

#Splitting the plot window into four parts
par(mfrow=c(2,3))

# looping to create the Bar-Plots for each column
for (ColumnName in ColsForBar){
  barplot(table(titanic[,c(ColumnName)]), main=paste('Barplot of:', ColumnName), 
          col=brewer.pal(8,"Paired"))
}


############################################################

# Step-9
# Visual Relationship between target variable and predictors

##for classification- dependent:categorical and predictor: categorical/continuous
# Categorical Vs Continuous --- Box Plot
# Categorical Vs Categorical -- Grouped Bar chart

############################################################
# Categorical Vs Continuous Visual analysis: Boxplot

##Age: continuous, Survived: Categorical
par(mfrow=c(1,1))
boxplot(Age~Survived, data = titanic, col=brewer.pal(8,"Paired"))

boxplot(Fare~Survived, data = titanic, col=brewer.pal(8,"Paired"))

############################################################

# Categorical Vs Categorical Visual analysis: Grouped Bar chart

#lets look the the relationship between sex and survival
library(ggplot2)
ggplot(titanic,aes(x=Sex,fill=Survived))+ geom_bar() +labs(x="sex",
      y="No of people survived",
      title = "No of peopel survived vs sex")

#how many female didn't survive
#how many male didn't survive 
#how many female survived
#how many male survived 

#0: Not survived
#1:survived

##there is huge casualty for males, and for females it's the opposite
##based on the graph itself you can say that here gender actually has a
#relationship with survival
##there is a dependency

# Step-10
# Statistical Relationship between target variable (Categorical) and predictors

# Categorical Vs Continuous --- ANOVA
# Categorical Vs Categorical -- Chi-square test


# Continuous Vs Categorical relationship strength: ANOVA
# Analysis of Variance(ANOVA)
# H0: Variables are NOT correlated
# Small P-Value <5%--> Variables are correlated(H0 is rejected)
# Large P-Value--> Variables are NOT correlated (H0 is accepted)

summary(aov(Age~Survived, data = titanic))

summary(aov(Fare~Survived, data = titanic))


#### Categorical Vs Categorical relationship strength: Chi-Square test
# H0: Variables are NOT correlated
# Small P-Value--> Variables are correlated(H0 is rejected)
# Large P-Value--> Variables are NOT correlated (H0 is accepted)
Chisqcols=c("Pclass","Sex","Embarked")

  for(i in Chisqcols ){
    CrossTabResult=table(titanic[,c('Survived',i)])
    ChiResult=chisq.test(CrossTabResult)
    print(i)
    print(ChiResult)
  }
#H0:the two columns are not correlated

#p is very low, 
#so we reject the null and conclude that these two columns are correlated


###do the chi-sq test for all other variables - do an automation i.e write a for loop
#and see which of the categorical variables are correlated with Survived column
############################################################






#############################################################################################
# Sampling | Splitting data into 80% for training 20% for testing

# Split the data into training and testing 
library(caTools)
set.seed(101)
split<- sample.split(titanic$Survived,SplitRatio = 0.80)
split

#let looks how many have True and False 
table(split)

# Traning and testing the data 
training<- subset(titanic, split==T)
test<- subset(titanic,split==F)


#############################################################################################
#############################################################################################
# Creating Predictive models on training data to check the accuracy on test data
###### Logistic Regression #######

##we are predicting TV based on all other variables
##glm() is used for wide variety of modeling activities. Logistic regression
#is one of the models that you can create using glm()
##in order to tell glm() that you have to perform logistic regression,
#you have to say family= 'binomial"



lg_model<- glm(Survived~.,data = training,family = 'binomial')
summary(model)

# we use step() for better AIC:
step(lg_model)

lg_model1<-glm(formula = Survived ~ Pclass + Sex + Age + SibSp, family = "binomial", 
               data = training)
summary(lg_model1)

#Null deviance: 949.90  on 712  degrees of freedom
#Residual deviance: 622.04  on 702  degrees of freedom
#AIC: 644.04

#smaller the AIC: better the model

#Deviance is a measure of goodness of fit of a generalized linear model.
#Or rather, it's a measure of badness of fit: higher numbers indicate worse fit.
#The null deviance shows how well the response variable is predicted by a model 
#that includes only the intercept

#residual deviance has to be lesser than null deviance to have a good model


# Checking Accuracy of model on Testing data
pred<-predict(lg_model1,newdata = test,type = "response")
pred
#it will give us probability values.

##considering a threshold of 0.50
pred_thres_50<- ifelse(pred>0.5,1,0)

#Now we will make table of confusion matrix
cm<-table(test$Survived,pred_thres_50)
cm
#accurcay
accuracy= (95+45)/(95+15+23+45)
accuracy


# Creating the Confusion Matrix to calculate overall accuracy, precision and recall on TESTING data
library(caret)
confusionMatrix(cm) 

#Accuracy : 0.7921     


###############################################################################
##############################################################################
###################### Decision Tree ########################################

library(rpart)
str(titanic)

#creating the model by using "rpart"
titanic$Survived <- as.numeric(titanic$Survived)
titanic$Survived <- ifelse(titanic$Survived==1,0,1)
dtree<- rpart(Survived~.,data = training, method = 'class')

#In prediction of Dtree model all values should be numeric 
str(titanic)

pred_dtree<- as.numeric(predict(dtree, newdata = test,type = 'class'))

pred_dtree

pred_dtree<- ifelse(pred_dtree==1,0,1)

pred_dtree

#Now we have to make confusion matrix table 
cm1<- table(test$Survived,pred_dtree)

length(test$Survived)
length(pred_dtree)
length(training$Survived)

library(caret)
confusionMatrix(cm1)

#Accuracy : 0.8202 

###############################################################################
###############################################################################
###################### Random Forest ##########################################

library(randomForest)

#Now we have build the model
rf_<- randomForest(Survived~.,data = training)
rf_predict<-predict(rf_, newdata = test)


#now confusion matrix table
cm2<- table(test$Survived,rf_predict)
cm2
confusionMatrix(cm2)
#  Accuracy : 0.8034  

class(rf_predict)
class(titanic$Survived)

################################################################################
################################################################################
################### Naive Base Theorem ########################################

library(e1071)

#check dependent variable must be "Factor"
str(titanic)

#building the model
nbt<- naiveBayes(Survived~.,data = training)

#predict the model

nbt_predict<- predict(nbt, newdata = test)
nbt_predict

# now creating confusion matrix table

cm3<- table(test$Survived,nbt_predict)
cm3

confusionMatrix(cm3)

#Accuracy : 0.7528


###############################################################################
###############################################################################
##################### support vector machine ##################################

#svm - kernel = "linear"

#building the model by using svm();
sv_linear<- svm(Survived~.,data = training,kernel="linear")

#now we will predict the model

svm_predict<- predict(sv_linear, newdata = test)
svm_predict

cm4<- table(test$Survived, svm_predict)
cm4

confusionMatrix(cm4)

# Accuracy : 0.7809 

###############################################################################
#building the model by using svm();
sv_polynomial<- svm(Survived~.,data = training,kernel="polynomial")

#now we will predict the model

svm_predict<- predict(sv_polynomial, newdata = test)
svm_predict

cm5<- table(test$Survived, svm_predict)
cm5

confusionMatrix(cm5)

# Accuracy : 0.618

###############################################################################

#kernel- sigmoid
#building the model by using svm();
sv_sigmoid<- svm(Survived~.,data = training,kernel="sigmoid")

#now we will predict the model

svm_predict<- predict(sv_sigmoid, newdata = test)
svm_predict

cm6<- table(test$Survived, svm_predict)
cm6

confusionMatrix(cm6)

# Accuracy : 0.7753 

###############################################################################

#kernel- radial
#building the model by using svm();
svm_rbf <- svm(Survived ~ ., data=training, kernel='radial')
pred_svm_rbf <- as.numeric(predict(svm_rbf,newdata = test, type="class"))

pred_svm_rbf <- ifelse(pred_svm_rbf==1,0,1)

cm7 <- table(test$Survived, pred_svm_rbf)


confusionMatrix(cm7)


#Accuracy : 0.7753 


#conclusion:
# 1) Logistic Regression : Accuracy : 0.7921*****  
# 2) Decision Tree : Accuracy : 0.8202 ********
# 3) Random Forest : Accuracy : 0.8034 *******
# 4) Naive Bayes Theorem : Accuracy :0.7528
# 5) SVM - Kerel = "linear" : Accuracy : 0.7809 
# 6) SVM - Kerel = "sigmoid" : Accuracy : 0.7753 
# 7) SVM - Kerel = "polynomial" : Accuracy : 0.618
# 8) SVM - Kerel = "rbf" : Accuracy : 0.7753  

