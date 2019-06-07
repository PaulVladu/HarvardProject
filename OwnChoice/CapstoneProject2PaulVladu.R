
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")

if(!require(naivebayes)) install.packages("naivebayes", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(Boruta)) install.packages("Boruta", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

library(knitr)
library(ggplot2)
library(gridExtra)
library(naivebayes)
library(randomForest)

# the library Boruta is used later on in features selection
library(Boruta)

# Section 1 - get the data , downloading it from GitHub
#------------------------------------------------------------------------------------------------------------------
#
# Remark: though the dataset is provided on Kaggle, since Kaggle requests login credentials to get this dataset
#           the solution was to upload this dataset on GitHub in order to have it accessible for download


# download the file
download.file(url = "https://raw.githubusercontent.com/PaulVladu/HarvardProject/master/OwnChoice/heart.csv", destfile = "heart.csv")

dfheart <- read.csv("heart.csv", header = TRUE)

# clean missing values for [thal] variable
dfheart$thal[dfheart$thal==0]<-1

# define the column names for the dataset
colnames(dfheart)<-c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","target")

# transform to nominal (or factor) a group of numerical attributes

# age attribute is discretizez into 5 categories in order to have a more compact distribution of data, by category of age
dfheart<-dfheart %>% mutate(age_range = as.factor(ifelse(age<41,"*_40", ifelse(age<51,"41_50",ifelse(age<61,"51_60",ifelse(age<71,"61_70","71_*"))))))
# original age attribute is removed from the dataset
dfheart <- dfheart%>%select(-age)


#sex
dfheart <-dfheart%>%mutate(sex = as.factor(sex))
# cp = chest pain
dfheart <-dfheart%>%mutate(cp = as.factor(cp))

# fbs :fasting blood sugar ( > 120 mg/dl )
dfheart <-dfheart%>%mutate(fbs = as.factor(fbs))

# restecg :Resting electrocardiographic result
dfheart <-dfheart%>%mutate(restecg = as.factor(restecg))

# exang : exercise induced angina 
dfheart <-dfheart%>%mutate(exang = as.factor(exang))

# slope : the slope of the peak exercise ST segment  
dfheart <-dfheart%>%mutate(slope = as.factor(slope))

#ca : number of major vessels (0-3) colored by fluorosopy 
dfheart <-dfheart%>%mutate(ca = as.factor(ca))

#thal with 4 values : 0,1,2,3         
dfheart <-dfheart%>%mutate(thal = as.factor(thal))

# target : 0 = No heart disease ; 1 = Yes, indicates the presence of heart disease
dfheart <-dfheart%>%mutate(target = as.factor(target))



#Section 2- preparation of Train Dataset & Test dataset (circa 20% of the entire original dataset)
#------------------------------------------------------------------------------------------------------------------
#
set.seed(1)
test_index <- createDataPartition(y = dfheart$target, times = 1, p = 0.2, list = FALSE)
dftrain <- dfheart[-test_index,]
temp0 <- dfheart[test_index,]


# the following code insures that items in the test set can be found in the train set as well
temp1 <- temp0 %>% semi_join(dftrain, by = "age_range")
removed <- anti_join(temp0, temp1, by = "age_range")
dftrain <- rbind(dftrain, removed)

temp0 <- temp1 %>% semi_join(dftrain, by = "sex")
removed <- anti_join(temp1, temp0, by = "sex")
dftrain <- rbind(dftrain, removed)

temp1 <- temp0 %>% semi_join(dftrain, by = "cp")
removed <- anti_join(temp0, temp1, by = "cp")
dftrain <- rbind(dftrain, removed)


temp0 <- temp1 %>% semi_join(dftrain, by = "fbs")
removed <- anti_join(temp1, temp0, by = "fbs")
dftrain <- rbind(dftrain, removed)

temp1 <- temp0 %>% semi_join(dftrain, by = "exang")
removed <- anti_join(temp0, temp1, by = "exang")
dftrain <- rbind(dftrain, removed)

temp0 <- temp1 %>% semi_join(dftrain, by = "restecg")
removed <- anti_join(temp1, temp0, by = "restecg")
dftrain <- rbind(dftrain, removed)

temp1 <- temp0 %>% semi_join(dftrain, by = "ca")
removed <- anti_join(temp0, temp1, by = "ca")
dftrain <- rbind(dftrain, removed)

temp0 <- temp1 %>% semi_join(dftrain, by = "slope")
removed <- anti_join(temp1, temp0, by = "slope")
dftrain <- rbind(dftrain, removed)

temp1 <- temp0 %>% semi_join(dftrain, by = "thal")
removed <- anti_join(temp0, temp1, by = "thal")

dftrain <- rbind(dftrain, removed)
dftest <- temp1

rm(temp0, temp1, dfheart)

no_recs_train <-nrow(dftrain)
no_recs_test <-nrow(dftest)

paste("Train set - number of records :", no_recs_train)
paste("Test set - number of records :", no_recs_test)


#
# Section 3 - graphs
#------------------------------------------------------------------------------------------------------------------
#
#target
dftrain%>%ggplot(aes(target , fill=target))+geom_bar(stat="count") +ggtitle(label="Distribution of [target] variable in the dataset", subtitle = "[target] inidcates the presence of Heart Disease")+scale_fill_discrete(name = "Heart Disease", labels=c("0 - No", "1 - Yes"))

#age_range
dftrain%>%ggplot(aes(age_range , fill=target))+geom_bar(stat="count", position="fill")+ggtitle(label="Distribution of [target] variable by [age_range]", subtitle = "[target] inidcates the presence of Heart Disease")+scale_fill_discrete(name = "Heart Disease", labels=c("0 - No", "1 - Yes"))

# sex
dftrain%>%mutate(sex = ifelse(sex==0,'woman', 'man'))%>%ggplot(aes(sex , fill=target))+geom_bar(stat="count", position="fill")+ggtitle(label="Distribution of [target] variable by [sex]", subtitle = "[target] inidcates the presence of Heart Disease")+scale_fill_discrete(name = "Heart Disease", labels=c("0 - No", "1 - Yes"))

dftrain%>%mutate(sex = ifelse(sex==0,'woman', 'man'))%>%ggplot(aes(age_range , fill=target))+geom_bar(stat="count")+ggtitle(label="Distribution of [target] variable by [age_range] split by [sex]", subtitle = "[target] inidcates the presence of Heart Disease")+scale_fill_discrete(name = "Heart Disease", labels=c("0 - No", "1 - Yes")) + facet_grid(sex~.)

#cp
dftrain%>%mutate(chest_pain = cp)%>%ggplot(aes(chest_pain , fill=target))+geom_bar(stat="count", position="fill")+ggtitle(label="Distribution of [target] variable by [cp] (chest pain)", subtitle = "[target] inidcates the presence of Heart Disease")+scale_fill_discrete(name = "Heart Disease", labels=c("0 - No", "1 - Yes"))

dftrain%>%mutate(chest_pain = cp)%>%ggplot(aes(chest_pain , fill=target))+geom_bar(stat="count")+ggtitle(label="Distribution of [target] variable by [cp] (chest pain)", subtitle = "[target] inidcates the presence of Heart Disease")+scale_fill_discrete(name = "Heart Disease", labels=c("0 - No", "1 - Yes"))


#trestbps
dftrain%>%ggplot(aes(x=target, y=trestbps , fill=target))+ geom_boxplot()+ labs(y="resting blood pressure", x="Target") + ggtitle(label="Distribution of [target] variable by [trestbps]", subtitle = "[target] inidcates the presence of Heart Disease")+scale_fill_discrete(name = "Heart Disease", labels=c("0 - No", "1 - Yes"))

#chol

dftrain%>%ggplot(aes(x=target, y=chol , fill=target))+ geom_boxplot()+ labs(y="serum cholestoral in mg/dl", x="Target") + ggtitle(label="Distribution of [target] variable by [chol]", subtitle = "[target] inidcates the presence of Heart Disease")+scale_fill_discrete(name = "Heart Disease", labels=c("0 - No", "1 - Yes"))


dftrain%>%mutate(sex = ifelse(sex==0,'woman', 'man'))%>%ggplot() + geom_boxplot(aes(x=target, y=chol, fill = age_range))+facet_grid(sex ~ .) + ggtitle(label="Distribution of [target] variable by [chol], by [age_range], by [sex]", subtitle = "[target] inidcates the presence of Heart Disease")

#fbs
dftrain%>%mutate(fbs = ifelse(fbs==0,'no', 'yes'))%>%ggplot(aes(fbs , fill=target))+geom_bar(stat="count", position="fill")+ggtitle(label="Distribution of [target] variable by [fbs]", subtitle = "[target] inidcates the presence of Heart Disease")+scale_fill_discrete(name = "Heart Disease", labels=c("0 - No", "1 - Yes")) +labs(x="fasting blood sugar bigger than 120 mg/dl")

#restecg
dftrain%>%ggplot(aes(restecg , fill=target))+geom_bar(stat="count", position="fill")+ggtitle(label="Distribution of [target] variable by [restecg]", subtitle = "[target] inidcates the presence of Heart Disease")+scale_fill_discrete(name = "Heart Disease", labels=c("0 - No", "1 - Yes")) +labs(x="resting electrocardiographic")

#thalach
dftrain%>%ggplot(aes(x=target, y=thalach , fill=target))+ geom_boxplot()+ labs(y="maximum heart rate achieved", x="Target") + ggtitle(label="Distribution of [target] variable by [thalach]", subtitle = "[target] inidcates the presence of Heart Disease")+scale_fill_discrete(name = "Heart Disease", labels=c("0 - No", "1 - Yes"))

dftrain%>%mutate(talach =ifelse(thalach>150, "dataset for talach > 150", "dataset for talach <= 150") ) %>%ggplot(aes(x=talach, fill = target ))+geom_bar(stat="count", position="fill")+ggtitle(label="Distribution of [target] variable by [thalach]", subtitle = "[target] inidcates the presence of Heart Disease")+scale_fill_discrete(name = "Heart Disease", labels=c("0 - No", "1 - Yes"))


#exang
dftrain%>%mutate(exang = ifelse(fbs==0,'no', 'yes'))%>%ggplot(aes(exang , fill=target))+geom_bar(stat="count", position="fill")+ggtitle(label="Distribution of [target] variable by [exang]", subtitle = "[target] inidcates the presence of Heart Disease")+scale_fill_discrete(name = "Heart Disease", labels=c("0 - No", "1 - Yes")) +labs(x="exercise induced angina")

#oldpeak
dftrain%>%ggplot(aes(x=target, y=oldpeak , fill=target))+ geom_boxplot()+ labs(y="ST depression induced by exercise relative to rest", x="Target") + ggtitle(label="Distribution of [target] variable by [oldpeak]", subtitle = "[target] inidcates the presence of Heart Disease")+scale_fill_discrete(name = "Heart Disease", labels=c("0 - No", "1 - Yes"))

#slope
dftrain%>%ggplot(aes(slope , fill=target))+geom_bar(stat="count", position="fill")+ggtitle(label="Distribution of [target] variable by [slope]", subtitle = "[target] inidcates the presence of Heart Disease")+scale_fill_discrete(name = "Heart Disease", labels=c("0 - No", "1 - Yes")) +labs(x="the slope of the peak exercise ST segment")

#ca
dftrain%>%ggplot(aes(ca , fill=target))+geom_bar(stat="count", position="fill")+ggtitle(label="Distribution of [target] variable by [ca]", subtitle = "[target] inidcates the presence of Heart Disease")+scale_fill_discrete(name = "Heart Disease", labels=c("0 - No", "1 - Yes")) +labs(x="number of major vessels (0-3) colored by fluorosopy")

dftrain%>%ggplot(aes(ca , fill=target))+geom_bar(stat="count")+ggtitle(label="Distribution of [target] variable by [ca]", subtitle = "[target] inidcates the presence of Heart Disease")+scale_fill_discrete(name = "Heart Disease", labels=c("0 - No", "1 - Yes")) +labs(x="number of major vessels (0-3) colored by fluorosopy")

#thal
dftrain%>%ggplot(aes(thal , fill=target))+geom_bar(stat="count", position="fill")+ggtitle(label="Distribution of [target] variable by [thal]", subtitle = "[target] inidcates the presence of Heart Disease")+scale_fill_discrete(name = "Heart Disease", labels=c("0 - No", "1 - Yes")) +labs(x="a blood disorder called thalassemia")


dftrain%>%ggplot(aes(thal , fill=target))+geom_bar(stat="count")+ggtitle(label="Distribution of [target] variable by [thal]", subtitle = "[target] inidcates the presence of Heart Disease")+scale_fill_discrete(name = "Heart Disease", labels=c("0 - No", "1 - Yes")) +labs(x="a blood disorder called thalassemia")

#
# Section 4 - features selection with Boruta package
#------------------------------------------------------------------------------------------------------------------
#

set.seed(1)
boruta.train <- Boruta(target~., data = dftrain, doTrace = 2)

plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)


# new train dataset with only the featrues chosen through features selection with Boruta
dftrain<-dftrain%>%select(ca, cp, thal, oldpeak, thalach, sex, exang, slope, age_range, target)

#
# Section 5 - classification models and predictionresults
#------------------------------------------------------------------------------------------------------------------
#

#--------naive Bayes
nb <- naive_bayes(as.matrix(select(dftrain,-target)),dftrain$target)
pred_nb <-predict(nb, newdata = dftest)
cm_nb<-confusionMatrix(pred_nb, dftest$target)

nb_acc<-cm_nb$overall["Accuracy"]
nb_se<-cm_nb$byClass["Sensitivity"]
nb_sp<-cm_nb$byClass["Specificity"]
nb_pr<-cm_nb$byClass["Precision"]

print ("naive Bayes---------------------------------")
cm_nb$overall["Accuracy"]
cm_nb$byClass


#------Logistic regression
lg <- glm(target~.,data=dftrain,family=binomial(link="logit"))
lg_predict<-predict(lg,type="response",newdata=dftest)
lg_result<-as.factor(ifelse(lg_predict>0.5,1,0))
cm_lr<-confusionMatrix(lg_result, dftest$target)

lr_acc<-cm_lr$overall["Accuracy"]
lr_se<-cm_lr$byClass["Sensitivity"]
lr_sp<-cm_lr$byClass["Specificity"]
lr_pr<-cm_lr$byClass["Precision"]

print ("logistic regression---------------------------------")
cm_lr$overall["Accuracy"]
cm_lr$byClass


#------Random Forest

rf <- randomForest(dftrain%>%select(-target), dftrain$target)
rf_result <-predict(rf,newdata = dftest%>%select(-target))
cm_rf<-confusionMatrix(rf_result, dftest$target)

rf_acc<-cm_rf$overall["Accuracy"]
rf_se<-cm_rf$byClass["Sensitivity"]
rf_sp<-cm_rf$byClass["Specificity"]
rf_pr<-cm_rf$byClass["Precision"]

print ("Random Forest---------------------------------")
cm_rf$overall["Accuracy"]
cm_rf$byClass


#
# Section 6 - Final result and conclusion
#------------------------------------------------------------------------------------------------------------------
#

#"---------------------------------------------------------------"
#"                       Conclusion"
#"---------------------------------------------------------------"
#
#" random forest is chosen as the best solution for the model"
#
#" with the following features : target ~ ca + cp + thal + oldpeak + thalach + sex + exang + slope + age_range"
#
#" that yields the Accuracy of : "
cm_rf$overall["Accuracy"]
#
#"---------------------------------------------------------------"

