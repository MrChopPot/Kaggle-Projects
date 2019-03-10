### Survival Prediction for the Titanic ###
# Kind Reminds: Please check with line 26, and choose your own prefered work directory #

### 0. Install all the packages (You may not need to follow the steps here)
#install.packages("tidyverse")
#install.packages("tree")
#install.packages("rpart")
#install.packages("InformationValue")
#install.packages("randomForest")
#install.packages("party")
#install.packages("xgboost")

### 1. Library all the packages
library(tidyverse) # Including ggplot2 & dplyr
library(tree) # Decision Tree
library(rpart) # Recursive Partitioning
library(InformationValue) # IV / WOE calculation
library(randomForest) # Random Forest on the way
library(party) # Conditional Inference Trees
library(xgboost) # XGBoost on the way
options(dplyr.width = Inf) # Set the global environment


### 2. Prepare the work directory and the data 
#PS: the URLs provided could change at any time due to Kaggle's policy, you have to update them or download data manually
setwd("~/Desktop/Data Project/")
dir.create("data")
sample <- read_csv("https://storage.googleapis.com/kaggle-competitions-data/kaggle/3136/gender_submission.csv?GoogleAccessId=web-data@kaggle-161607.iam.gserviceaccount.com&Expires=1544282789&Signature=MqbT8astbhpHYZJ55%2FXIljQd%2FxM0A3%2BYSo4v5rSy6bd3gvYjepYDPW%2FZuCa%2Bnz9ceGBHwMAT1Nppo6SU22WRLdBGyM9RpI5QJP5qx2XF8TyCE%2FEISIVyFrE%2F947LfivX1HnbgUQKaqL5BhPTT2WGzPjV0dGBA5r0VP0uAqCE7nYLqNQxZwuoxmshxwyARssYW03XFQqsnO1uJBWXJ9mWRMGbAih100GfEmxpMTL1nyoli3%2B%2F72FJaUwpt7ylnt4hNEAIKR8ckBdOYTdlUY5cQstyV4zwWP5CsZa%2FdJwZRuyNVgmpSXpE2SMOD3nq1JgjDet7Kh9V25nPvbJSMfbHMg%3D%3D")
write_csv(sample,"./data/sample.csv")
train <- read_csv("https://storage.googleapis.com/kaggle-competitions-data/kaggle/3136/train.csv?GoogleAccessId=web-data@kaggle-161607.iam.gserviceaccount.com&Expires=1544282828&Signature=ZcZJdTGXTHfB65IKp%2FBEvk3Iso3dA8nxlyp7k0F5qVi2WKWB23R0Tu1kcoIpp3dqfQ3nP0ifbI3hGv9YREMNzF%2BU2iLVi%2BzYA85qa9ypHE3m%2BnbtTJXBf%2FzfTGuIjYv%2B0bp7%2BADGiAfKyzoJvjpAxaI2427tGLZD0iWz1Tsd4NsKLMpvvdBd1H1ebnFgKhB5s7uEBl8X0X5C8R%2FtksV3JKujajRHfz8YIyS0hL2jq06GXf4QQaknT29a28k5A9kmF%2BJujNkIrY6ZpXNEEt1DOn6wnUHHdymEif2aj3VoSfum5g6OrlomfJdmr68SVKV%2B1nGj93FRY72MXrk9wU8q3A%3D%3D")
write_csv(train,"./data/train.csv")
test <- read_csv("https://storage.googleapis.com/kaggle-competitions-data/kaggle/3136/test.csv?GoogleAccessId=web-data@kaggle-161607.iam.gserviceaccount.com&Expires=1544282845&Signature=qXBYAyUa8s24%2BFzfFkBKnpYAxLRD4jaODodN6EsICAr9L3VwHwlIzhSg5KY2rpDNO%2BYKlH7VrCELmkngjOLjsg%2Frg2EdEFcdcJap8lwFZTR7yOyeAdpbWfonpl9za9REWSM%2BnhB6COjQY9iYE03Jtejk58DhKwY3%2Br6Fz18AcQo7nSt5JHR1Q1iJjFcArAvLyo65b%2B0cjF%2BmX4G6rXETM50xdBOXc3Go659PYjcYvkw%2BfexIDRWpXjxhJ8O07pY%2BWsdVUi9G0htFQutAtHIZYIRdcgzNPHYfbbeIh01OE%2BHeKEEwIlks6Wrfak7LPoT5mqO%2BixPyZLEpk45u66OYuQ%3D%3D")
write_csv(test,"./data/test.csv")
titanic <- bind_rows(train,test) # Get the complete data
write_csv(titanic,"./data/titanic.csv")
train.row <- 1:nrow(train)
test.row <- (1+nrow(train)):nrow(titanic)


### 3. Feature Engineering
# Observe the dataset
View(titanic)
str(titanic)
summary(titanic)

# Survived: Change the class of Survived from integar to factor
titanic$Survived <- factor(titanic$Survived)
levels(titanic$Survived)

# Pclass: Change the class of Pclass from integar to factor
titanic$Pclass <- factor(titanic$Pclass)
levels(titanic$Pclass)

# Name: Extract the title from Name for future usage, create a new variable and make it a factor
titanic$Title <- sapply(titanic$Name, function(x) strsplit(x,"[,.]")[[1]][2])
titanic$Title <- sub(" ","",titanic$Title)
table(titanic$Sex,titanic$Title)
titanic$Title[titanic$Title %in% c("Capt","Col","Don","Jonkheer","Major","Master","Sir","Rev","Dr") & titanic$Sex=="male"] <- "Sir"
titanic$Title[titanic$Title %in% c("Dona","Lady","the Countess","Dr") & titanic$Sex==c("female")] <- "Lady"
titanic$Title[titanic$Title %in% c("Mr")] <- "Mr"
titanic$Title[titanic$Title %in% c("Mrs","Mme")] <- "Mrs"
titanic$Title[titanic$Title %in% c("Miss","Mlle","Ms")] <- "Miss"
titanic$Title <- factor(titanic$Title)
levels(titanic$Title)
table(titanic$Sex,titanic$Title)

# Sex: Change the class of Pclass from integar to factor
titanic$Sex <- factor(titanic$Sex)
levels(titanic$Sex)

# Sibsb & Parch: Aggregate them and create a new variable "Family"
titanic$Family <- titanic$SibSp + titanic$Parch + 1

# Ticket: Does sharing a ticket number matters?
TicketCount <- aggregate(titanic$Ticket, by = list(titanic$Ticket), function(x) sum(!is.na(x)))
View(TicketCount) 
titanic$TicketCount <- apply(titanic, 1, function(x) TicketCount[which(TicketCount[, 1] == x["Ticket"]), 2])
titanic$TicketCount <- factor(sapply(titanic$TicketCount, function(x) ifelse(x > 1, "Shared", "Unique")))
levels(titanic$TicketCount)

# Cabin: Create two new variable "Location" & "WithLoc"
# Location: Extract the deck information & treat NA as NA
titanic$Location <- sapply(titanic$Cabin, function(x) str_sub(x, start = 1, end = 1))
titanic$Location <- factor(titanic$Location)
levels(titanic$Location)

# Withloc: Take NA as "No", non-NA as "Yes"
titanic$WithLoc <- "Yes"
titanic$WithLoc[is.na(titanic$Location)] <- "No"
titanic$WithLoc <- factor(titanic$WithLoc)
levels(titanic$WithLoc)

# Embarked: Change the class of Embarked from charactor to factor
titanic$Embarked <- factor(titanic$Embarked)
levels(titanic$Embarked)


### 4. Exploratory Data Analysis (EDA)
# Pclass on Survived
ggplot(titanic[train.row,], aes(x = Pclass, y = ..count.., fill = Survived)) + 
  geom_bar(stat="count",position="dodge") + 
  xlab("Pclass") + 
  ylab("Count") + 
  ggtitle("How Pclass Impacts Survival") + 
  scale_fill_discrete(labels=c("0 (Decease)", "1 (Survival)")) +
  geom_text(stat="count",aes(label=..count..),position=position_dodge(width=1),vjust=-0.2) + 
  theme(plot.title=element_text(hjust=0.5)) # legend.position="right"

# Title on Survived
ggplot(titanic[train.row,], aes(x = Title, y = ..count.., fill = Survived)) + 
  geom_bar(stat="count",position="stack") + 
  xlab("Title") + 
  ylab("Count") + 
  ggtitle("How Title Impacts Survival") + 
  scale_fill_discrete(labels=c("0 (Decease)", "1 (Survival)")) + 
  geom_text(stat="count",aes(label=..count..),position=position_stack(vjust = 0.5)) +
  theme(plot.title=element_text(hjust = 0.5))

# Sex on Survived
ggplot(titanic[train.row,], aes(x = Sex, y = ..count.., fill = Survived))+
  geom_bar(stat="count",position="dodge") +
  xlab("Sex") +
  ylab("Count") +
  ggtitle("How Sex Impacts Survival") +
  scale_fill_discrete(labels=c("0 (Decease)", "1 (Survival)")) +
  geom_text(stat="count",aes(label=..count..),position = position_dodge(width = 1),vjust=-0.2) +
  theme(plot.title=element_text(hjust=0.5))

# Age on Survived
ggplot(titanic[(!is.na(titanic$Age)) & row(as.matrix(titanic[,"Age"]))<=train.row,],aes(x=Age,color=Survived)) +
  geom_line(aes(y=..count..),stat = "bin",binwidth=5,na.rm = TRUE) +
  labs(title="How Age Impacts Survival",x="Age",y="Count",fill="Survived") +
  scale_color_discrete(labels=c("0 (Decease)", "1 (Survival)")) +
  theme(plot.title=element_text(hjust=0.5))

# Family(Sibsp & Parch) on Survived
ggplot(titanic[train.row,], aes(x = Family, y = ..count.., fill = Survived)) +
  geom_bar(stat="count",position="dodge") +
  xlab("Family") +
  ylab("Count") +
  ggtitle("How Family Size Impacts Survival") +
  scale_fill_discrete(labels=c("0 (Decease)", "1 (Survival)")) +
  geom_text(stat="count",aes(label=..count..),position = position_dodge(width = 1),vjust=-0.2) +
  theme(plot.title=element_text(hjust=0.5))

# TicketCount (Ticket Number Sharing) on Survived
ggplot(titanic[train.row,], aes(x = TicketCount, y = ..count.., fill = Survived)) +
  geom_bar(stat="count",position="dodge") +
  xlab("TicketCount") +
  ylab("Count") +
  ggtitle("How Sharing Ticket Number Impacts Survival") +
  scale_fill_discrete(labels=c("0 (Decease)", "1 (Survival)")) +
  geom_text(stat="count",aes(label=..count..),position = position_dodge(width = 1),vjust=-0.2) +
  theme(plot.title=element_text(hjust=0.5))

# Fare on Survived
ggplot(titanic[(!is.na(titanic$Fare)) & row(as.matrix(titanic[,"Fare"]))<=train.row,],aes(x=Fare,color=Survived)) +
  geom_line(aes(y=..count..),stat = "bin",binwidth=5,na.rm = TRUE) +
  labs(title="How Fare Impacts Survival",x="Fare",y="Count",fill="Survived") +
  scale_color_discrete(labels=c("0 (Decease)", "1 (Survival)")) +
  theme(plot.title=element_text(hjust=0.5))

# Cabin on Survived
ggplot(titanic[train.row,], aes(x = Location, y = ..count.., fill = Survived)) +
  geom_bar(stat="count",position="dodge") + 
  xlab("Cabin") +
  ylab("Count") +
  ggtitle("How Cabin Impacts Survival (With Deck info)") +
  scale_fill_discrete(labels=c("0 (Decease)", "1 (Survival)")) +
  geom_text(stat="count",aes(label=..count..),position = position_dodge(width=1),vjust=-0.2) + 
  theme(plot.title=element_text(hjust = 0.5))

ggplot(titanic[train.row,], aes(x = WithLoc, y = ..count.., fill = Survived)) +
  geom_bar(stat="count",position="dodge") + 
  xlab("Cabin") +
  ylab("Count") +
  ggtitle("How Cabin Impacts Survival (With Info or Not)") +
  scale_fill_discrete(labels=c("0 (Decease)", "1 (Survival)")) +
  geom_text(stat="count",aes(label=..count..),position = position_dodge(width=1),vjust=-0.2) + 
  theme(plot.title=element_text(hjust = 0.5))

# Embarked on Survived
ggplot(titanic[train.row,], aes(x = Embarked, y = ..count.., fill = Survived)) +
  geom_bar(stat = "count",position = "dodge") +
  xlab("Embarked") +
  ylab("Count") +
  ggtitle("How Embarked Impacts Survival") +
  scale_fill_discrete(labels=c("0 (Decease)", "1 (Survival)")) +
  geom_text(stat = "count",aes(label=..count..),position = position_dodge(width = 1),vjust=-0.2) +
  theme(plot.title=element_text(hjust=0.5))


### 5. Impute the Missing Values
# Find the variables with missing values
sapply(titanic, function(x) sum(is.na(x)))

# Fare: Use the its correlation with other "solid" variables
titanic[is.na(titanic$Fare),]

fare.lm <- lm(Fare~Pclass + Embarked + Family + Age + Sex + Title, na.omit(titanic[train.row,]))
summary(fare.lm) # Found that only Pclass, Embarked and Family are significant
fare.lm <- lm(Fare~Pclass + Embarked + Family, na.omit(titanic[train.row,])) # Using the significant variables
fare.pred0 <- predict(fare.lm, titanic[test.row,])
mean((fare.pred0-titanic[test.row,]$Fare)^2, na.rm = T) # MSE == 2798.5
fare.lm <- lm(Fare~Pclass + Embarked, na.omit(titanic[train.row,])) # Using logic, drop Family
fare.pred0 <- predict(fare.lm, titanic[test.row,])
mean((fare.pred0-titanic[test.row,]$Fare)^2, na.rm = T) # MSE == 1959.6

fare.tree <- randomForest(Fare~Pclass + Embarked + Family, na.omit(titanic[train.row,]), ntree=2000,ntry=3,proximity=T,importance=T)
fare.pred <- predict(fare.tree, titanic[test.row,])
mean((fare.pred-titanic[test.row,]$Fare)^2, na.rm = T) # MSE == 2065.2

# No need to try anymore; just use the median of passengers in a specific group
summary(titanic$Fare[titanic$Pclass==3 & titanic$Embarked=="S"])
titanic$Fare[1044] <- 8.05

# Embarked: Use the correlation with other "solid" variables
titanic[is.na(titanic$Embarked),]

em.tree <- randomForest(Embarked~Pclass + Fare + Title + Family + Age + Sex, na.omit(titanic[train.row,]), ntree=2000,ntry=3,proximity=T,importance=T)
pred1 <- predict(em.tree, titanic[test.row,], type = "class")
mean(pred1==titanic[test.row,]$Embarked, na.rm = T) # 0.74

predict(em.tree, titanic[62,], type = "class")
titanic$Embarked[c(62,830)] <- "C"

# Age: Use the correlations with other variables
layout(matrix(c(1,2),2,1))
hist(titanic$Age[!is.na(titanic$Age)], freq = F, main = "Age Distribution", xlab = "Original Age Distribution")

age.model <- rpart(Age ~ Pclass + Sex + Fare + Embarked + Title + Family, titanic[!is.na(titanic$Age), ], method="anova")
yhat <- predict(age.model, newdata = titanic[!is.na(titanic$Age), ])
mean((yhat-titanic[!is.na(titanic$Age), ]$Age)^2) # 129.2

age.model1 <- randomForest(Age ~ Pclass + Sex + Fare + Embarked + Title + Family, titanic[!is.na(titanic$Age), ], ntree=2000,ntry=3,proximity=T,importance=T)
yhat1 <- predict(age.model1, newdata = titanic[!is.na(titanic$Age), ])
mean((yhat1-titanic[!is.na(titanic$Age), ]$Age)^2) # 89.1

age.model2 <- cforest(Age ~ Pclass + Sex + Fare + Embarked + Title + Family, titanic[!is.na(titanic$Age), ], controls=cforest_unbiased(ntree=2000, mtry=3))
yhat2 <- predict(age.model2, newdata = titanic[!is.na(titanic$Age), ], OOB=TRUE)
mean((yhat2-titanic[!is.na(titanic$Age), ]$Age)^2) # 107.7

age.model3 <- lm(Age ~ Pclass + Sex + Fare + Embarked + Title + Family, titanic[!is.na(titanic$Age), ])
yhat3 <- predict(age.model3, newdata = titanic[!is.na(titanic$Age), ])
mean((yhat3-titanic[!is.na(titanic$Age), ]$Age)^2) # 132.4

titanic$Age[is.na(titanic$Age)] <- round(predict(age.model, titanic[is.na(titanic$Age), ]),0)
hist(titanic$Age,freq = F,main = "Age Distribution", xlab = "Post-prediction Age Distribution")

# Cabin: Temporately no need to fill (too many missing & use Location or Withloc to replace)

### 5.5 Categorization (Optional)
# Age
titanic$AgeCat<-"Child"
titanic$AgeCat[titanic$Age>=18&titanic$Age<30]<-"Young"
titanic$AgeCat[titanic$Age>=30&titanic$Age<45]<-"Middle1"
titanic$AgeCat[titanic$Age>=45&titanic$Age<60]<-"Middle2"
titanic$AgeCat[titanic$Age>=60]<-"Elder"

# Fare
titanic$FareCat<- "Fare1"
titanic$FareCat[titanic$Fare>=quantile(titanic$Fare,0.1)&titanic$Fare<quantile(titanic$Fare,0.3)] <- "Fare2"
titanic$FareCat[titanic$Fare>=quantile(titanic$Fare,0.3)&titanic$Fare<quantile(titanic$Fare,0.5)] <- "Fare3"
titanic$FareCat[titanic$Fare>=quantile(titanic$Fare,0.5)&titanic$Fare<quantile(titanic$Fare,0.7)] <- "Fare4"
titanic$FareCat[titanic$Fare>=quantile(titanic$Fare,0.7)&titanic$Fare<quantile(titanic$Fare,0.9)] <- "Fare5"
titanic$FareCat[titanic$Fare>=quantile(titanic$Fare,0.9)] <- "Fare6"


### 6. Use Information Value / Logic to Choose the Appropriate Features
lapply(titanic[train.row,], function(x) IV(as.factor(x),titanic$Survived[train.row])) # See the IV rank
titanic.iv <- titanic[,c(2,3,5,6,10,12,13,14,15,17)]
str(titanic.iv)


### 7. Build the Training Model
set.seed(1201)
titanic.model <- randomForest(Survived ~ .-Survived, data = titanic.iv[train.row,], ntree=2000,ntry=3,proximity=T,importance=T)
#titanic.model <- cforest(Survived ~ .-Survived, data = titanic.iv[train.row,], controls=cforest_unbiased(ntree=2000, mtry=3))

### 7.5. Cross Validation (Optional)
cv.summarize <- function(true, predict) {
  print(paste('Recall:', Recall(true, predict)))
  print(paste('Precision:', Precision(true, predict)))
  print(paste('Accuracy:', Accuracy(predict, true)))
  print(paste('AUC:', AUC(predict, true)))
}
set.seed(1201)
cv.test.sample <- sample(train.row, 0.3*nrow(train), replace = TRUE)
cv.test <- titanic.iv[cv.test.sample,]
cv.prediction <- predict(titanic.model, cv.test)
cv.summarize(cv.test$Survived, cv.prediction)


### 8. Predict the test data
pred <- predict(titanic.model, titanic[test.row,])
#pred <- predict(titanic.model, newdata = titanic.iv[test.row,], OOB=TRUE, type = "response")
result <- data.frame(PassengerId = test$PassengerId, Survived = pred)
write.csv(result, file = "./data/Prediction.csv", row.names = FALSE)
