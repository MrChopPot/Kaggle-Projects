### Shelter Animal Outcomes

## 0.Install + Library & Set WD
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("magrittr")
#install.packages("naniar")
#install.packages("InformationValue")
#install.packages("caret")
#install.packages("tictoc")
library(tidyverse)
library(lubridate)
library(magrittr)
library(naniar)
library(InformationValue)
library(caret)
library(tictoc)

# Please download the data from 
# https://www.kaggle.com/c/shelter-animal-outcomes/overview
# and put that into one suitable working directory

# Please set your work directory

## 1. Read data & Take a glimpse
train <- read_csv("train.csv")
test <- read_csv("test.csv")
glimpse(train)
glimpse(test)

# Make sure do AnimalID & ID contain any valuable information
sum(is.na(train$AnimalID))
sum(str_detect(train$AnimalID, "A")) == nrow(train)
sum(sapply(train$AnimalID, nchar) == 7) == nrow(train)

sum(is.na(test$ID))

# Omit AnimalID & ID because we think they tell us nothing
shelter <- bind_rows(train[,-1], test[,-1])
glimpse(shelter)


## 2. Feature Engineering
# 2.1 Name
length(unique(shelter$Name)) # That's too many names!

shelter <- shelter %>% 
  mutate(Has_Name = factor(ifelse(is.na(shelter$Name), 0, 1)))

summary(shelter$Has_Name)

# 2.2 Datetime
shelter <- shelter %>% 
  mutate(Year = factor(year(shelter$DateTime)))

season <- function(x){
  s <- month(x)
  if(between(s, 3, 5)){
    return("Spring")
  } else if(between(s, 6, 8)) {
    return("Summer")
  } else if(between(s, 9, 11)){
    return("Autumn")
  } else {
    return("Winter")
  }
}

shelter <- shelter %>%
  mutate(Season = factor(sapply(shelter$DateTime, season)))

shelter <- shelter %>%
  mutate(Wday = factor(ifelse(wday(shelter$DateTime) %in% c(1, 7), 0, 1)))

shelter <- shelter %>%
  mutate(Daytime = factor(ifelse(between(hour(shelter$DateTime), 6, 18), 1, 0)))

# 2.3 AnimalType
shelter$AnimalType <- factor(shelter$AnimalType)

shelter %$%
  summary(AnimalType)

# 2.4 SexuponOutcome
shelter$SexuponOutcome <- factor(shelter$SexuponOutcome)
summary(shelter$SexuponOutcome)

shelter <- shelter %>%
  mutate(
    Sex = ifelse(str_detect(shelter$SexuponOutcome, "Male"), 1, 0),
    Intact = ifelse(str_detect(shelter$SexuponOutcome, "Intact"), 1, 0)
  )

To_NA <- (shelter$SexuponOutcome == "Unknown" | is.na(shelter$SexuponOutcome))
shelter$Sex <- factor(ifelse(To_NA, NA, shelter$Sex))
shelter$Intact <- factor(ifelse(To_NA, NA, shelter$Intact))

shelter %$%
  summary(Sex)

shelter %$%
  summary(Intact)

# 2.5 AgeuponOutcome
num_part <- shelter %$%
  sapply(AgeuponOutcome, function(x) str_split(x, ' ')[[1]][1]) %>%
  as.vector() %>%
  as.numeric()

unit_part <- shelter %$%
  sapply(AgeuponOutcome, function(x) str_split(x, ' ')[[1]][2]) %>%
  as.vector()

process <- function(x){
  round(as.numeric(duration(x)) / (60 * 60 * 24), 2)
}

shelter <- shelter %>%
  mutate(Age = map(list(num_part, unit_part), process)[[2]])

shelter %$%
  summary(Age)

# Anomaly Detection
ggplot(adoption, aes(Age)) +
  geom_freqpoly()

# 2.6 Breed
length(unique(shelter$Breed)) # That's too many!
shelter <- shelter %>%
  mutate(Mix = factor(ifelse(str_detect(shelter$Breed, "Mix"), 1, 0)))

shelter %$%
  summary(Mix)

# 2.7 Color
length(unique(shelter$Color)) # That's a lot!
shelter <- shelter %>%
  mutate(Hybrid = factor(ifelse(str_detect(shelter$Color, "/"), 1, 0)))

shelter %$%
  summary(Hybrid)

# 2.8 OutcomeType
length(unique(shelter$OutcomeType))
shelter <- shelter %>%
  mutate(Outcome = factor(ifelse(shelter$OutcomeType == "Adoption", 1, 0)))
shelter %$%
  summary(Outcome)

# 2.9 Put them together
adoption <- shelter %>%
  select(5, 10:20)
glimpse(adoption)


## 3. Exploratory Data Analysis
ggplot(adoption[1:nrow(train),], aes(x = AnimalType, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("AnimalType VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)"))

ggplot(adoption[1:nrow(train),], aes(x = Has_Name, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Has_Name VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)"))

ggplot(adoption[1:nrow(train),], aes(x = Year, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Year VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)"))

ggplot(adoption[1:nrow(train),], aes(x = Season, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Season VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)"))

ggplot(adoption[1:nrow(train),], aes(x = Wday, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Wday VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)"))

ggplot(adoption[1:nrow(train),], aes(x = Daytime, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Daytime VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)"))

ggplot(adoption[1:nrow(train),], aes(x = Sex, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Sex VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)"))

ggplot(adoption[1:nrow(train),], aes(x = Intact, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Intact VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)"))

ggplot(adoption[1:nrow(train),], aes(x = as.factor(Age), y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Age VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)"))

ggplot(adoption[1:nrow(train),], aes(x = Mix, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Mix VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)"))

ggplot(adoption[1:nrow(train),], aes(x = Hybrid, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Hybrid VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)"))


## 4. Feature Selection
lapply(adoption[1:nrow(train),], function(x) IV(as.factor(x), adoption$Outcome[1:nrow(train)]))


## 5. Missing Value Detection & Imputation
# Missing Report
miss_var_summary(adoption)

# Missing Randomness Detection
#vis_miss(adoption[,c(1:3, 7:11)], cluster = T) 
#aggr(adoption[,c(1:3, 7:11)])

# Missing Imputation(Median, KNN & ...)


## 6. Modeling (Building & CV & Hyper-tuning)
temp <- na.omit(adoption[1:nrow(train), ])

dmy <- dummyVars(~ ., data = temp[,-ncol(temp)])
dmy_data <- data.frame(predict(dmy, newdata = temp[,-ncol(temp)]))
dmy_data <- bind_cols(dmy_data, temp[, ncol(temp)])

index <- createDataPartition(dmy_data$Outcome, p = .70, list = FALSE)

fitControl <- trainControl(method = "repeatedcv", 
                           number = 3,
                           repeats = 5,
                           search = "random")
# XGBoost
tic()
mod1 <- train(Outcome ~ .,
              data = dmy_data[index,],
              method = "xgbTree",
              trControl = fitControl,
              verbose = FALSE,
              tunelength = 5)
toc()

pred1 <- predict(mod1, dmy_data[-index,], type = "raw")
caret::confusionMatrix(pred1, dmy_data[-index,]$Outcome)

# Random Forest
tic()
mod2 <- train(Outcome ~ .,
              data = dmy_data[index,],
              method = "rf",
              trControl = fitControl,
              verbose = FALSE,
              tunelength = 5)
toc()

pred2 <- predict(mod2, dmy_data[-index,], type = "raw")
caret::confusionMatrix(pred2, dmy_data[-index,]$Outcome)

# RBF SVM
tic()
mod3 <- train(Outcome ~ .,
              data = dmy_data[index,],
              method = "svmRadial",
              trControl = fitControl,
              verbose = FALSE,
              tunelength = 5)
toc()

pred3 <- predict(mod3, dmy_data[-index,], type = "raw")
caret::confusionMatrix(pred3, dmy_data[-index,]$Outcome)

# Neural Network
tic()
mod4 <- train(Outcome ~ .,
              data = dmy_data[index,],
              method = "nnet",
              trControl = fitControl,
              verbose = FALSE,
              tunelength = 5)
toc()

pred4 <- predict(mod4, dmy_data[-index,], type = "raw")
caret::confusionMatrix(pred4, dmy_data[-index,]$Outcome)
