### DSBI Final Project: Shelter Animal Outcomes

## 0.Install + Library & Set WD
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("magrittr")
#install.packages("naniar")
#install.packages("VIM")
#install.packages("InformationValue")
#install.packages("caret")
#install.packages("tictoc")
#install.packages("caretEnsemble")
#install.packages("gbm")
library(tidyverse)
library(lubridate)
library(magrittr)
library(naniar)
library(VIM)
library(InformationValue)
library(caret)
library(tictoc)
library(pROC)
library(caretEnsemble)
library(gbm)

# Please download the data from 
# https://www.kaggle.com/c/shelter-animal-outcomes/overview
# and put that into one suitable working directory

# Please set your work directory


### 1. Read data & Take a glimpse
train <- read_csv("train.csv")
test <- read_csv("test.csv")
glimpse(train)
glimpse(test)

dog_breed <- read_csv("dog_breed.csv") # outside source
glimpse(dog_breed)

tv_row <- 1:nrow(train)

# Make sure do AnimalID & ID contain any valuable information
sum(is.na(train$AnimalID))
mean(str_detect(train$AnimalID, "A"))
mean(sapply(train$AnimalID, nchar) == 7)

sum(is.na(test$ID))
# It seems like no information inside for those 2 features

# Omit AnimalID & ID because we think they tell us nothing
shelter <- bind_rows(train[,-1], test[,-1])
glimpse(shelter)


### 2. Feature Engineering
## 2.1 OutcomeType
length(unique(shelter$OutcomeType))

shelter <- shelter %>%
  mutate(Outcome = factor(ifelse(shelter$OutcomeType == "Adoption", 1, 0)))

shelter %$%
  summary(Outcome)

## 2.2 Name
length(unique(shelter$Name)) # That's too many names!

# 2.2.1 Has_Name
shelter <- shelter %>% 
  mutate(Has_Name = factor(ifelse(is.na(shelter$Name), 0, 1)))

shelter[tv_row,] %>%  
  group_by(Has_Name) %>%
  summarize(count = n(), prob = mean(Outcome == 1))

# 2.2.2 Len_Name
shelter <- shelter %>%
  mutate(Len_Name = ifelse(is.na(shelter$Name), 0, nchar(shelter$Name))) 

shelter[tv_row,] %>%  
  group_by(Len_Name) %>%
  summarize(count = n(), prob = mean(Outcome == 1)) # too similar with Has_Name

## 2.3 Datetime
# 2.3.1 Year
shelter <- shelter %>% 
  mutate(Year = factor(year(shelter$DateTime)))

shelter[tv_row,] %>%  
  group_by(Year) %>%
  summarize(count = n(), prob = mean(Outcome == 1)) # no help

# 2.3.2 Month & Season(Normal Seasons)
shelter <- shelter %>% 
  mutate(Month = factor(month(shelter$DateTime)))

shelter[tv_row,] %>%  
  group_by(Month) %>%
  summarize(count = n(), prob = mean(Outcome == 1))

shelter[tv_row,] %>%
  ggplot(aes(x = Month, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Month VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)")) +
  geom_hline(aes(yintercept = mean(shelter[tv_row,]$Outcome == 1))) # see the big pic

season <- function(x){
  s <- month(x)
  if(between(s, 4, 6)){
    return("Spring")
  } else if(between(s, 7, 9)) {
    return("Summer")
  } else if(between(s, 10, 12)){
    return("Autumn")
  } else {
    return("Winter")
  }
}

shelter <- shelter %>%
  mutate(Season = factor(sapply(shelter$DateTime, season)))

shelter[tv_row,] %>%  
  group_by(Season) %>%
  summarize(count = n(), prob = mean(Outcome == 1))

# 2.3.3 Day & Wday(Weekdays or Weekends)
shelter <- shelter %>%
  mutate(Day = factor(wday(shelter$DateTime)))

shelter[tv_row,] %>%  
  group_by(Day) %>%
  summarize(count = n(), prob = mean(Outcome == 1))

shelter[tv_row,] %>%
  ggplot(aes(x = Day, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Day VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)")) +
  geom_hline(aes(yintercept = mean(shelter[tv_row,]$Outcome == 1))) # see the big pic

shelter <- shelter %>%
  mutate(Wday = factor(ifelse(wday(shelter$DateTime) %in% c(1, 7), 0, 1)))

shelter[tv_row,] %>%  
  group_by(Wday) %>%
  summarize(count = n(), prob = mean(Outcome == 1))

# 2.3.4 Hour & Daytime(Mornings & Evenings or Others)
shelter <- shelter %>% 
  mutate(Hour = factor(hour(shelter$DateTime)))

shelter[tv_row,] %>%  
  group_by(Hour) %>%
  summarize(count = n(), prob = mean(Outcome == 1))

shelter[tv_row,] %>%
  ggplot(aes(x = Hour, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Hour VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)")) +
  geom_hline(aes(yintercept = mean(shelter[tv_row,]$Outcome == 1))) # see the big pic

shelter <- shelter %>%
  mutate(Daytime = factor(ifelse(hour(shelter$DateTime) %in% c(5:8, 16:22), 1, 0)))

shelter[tv_row,] %>%  
  group_by(Daytime) %>%
  summarize(count = n(), prob = mean(Outcome == 1))

## 2.4 AnimalType
shelter$Type <- factor(shelter$AnimalType)

shelter %$%
  summary(Type)

# 2.5 SexuponOutcome
shelter$SexuponOutcome <- factor(shelter$SexuponOutcome)

shelter %$%
  summary(SexuponOutcome)

shelter <- shelter %>%
  mutate(
    Sex = ifelse(str_detect(shelter$SexuponOutcome, "Male"), 1, 0),
    Intact = ifelse(str_detect(shelter$SexuponOutcome, "Intact"), 1, 0)
  )

To_NA <- (shelter$SexuponOutcome == "Unknown" | is.na(shelter$SexuponOutcome))

shelter$Sex <- ifelse(To_NA, NA, shelter$Sex) %>% 
  factor() %>%
  fct_explicit_na(na_level = "Unknown")
  
shelter$Intact <- shelter$Intact <- ifelse(To_NA, NA, shelter$Intact) %>% 
  factor() %>%
  fct_explicit_na(na_level = "Unknown")

shelter[tv_row,] %>%  
  group_by(Sex) %>%
  summarize(count = n(), prob = mean(Outcome == 1))

shelter[tv_row,] %>%  
  group_by(Intact) %>%
  summarize(count = n(), prob = mean(Outcome == 1))

# 2.6 AgeuponOutcome
num_part <- shelter %$%
  sapply(AgeuponOutcome, function(x) str_split(x, ' ')[[1]][1]) %>%
  as.vector() %>%
  as.numeric()

unit_part <- shelter %$%
  sapply(AgeuponOutcome, function(x) str_split(x, ' ')[[1]][2]) %>%
  as.vector() %>%
  str_replace("s$", "")

age <- function(x, y){
  if(is.na(x) | is.na(y)){
    return(0)
  } else {
    return(round(as.numeric(duration(x, y)) / (60 * 60 * 24), 2))
  }
}

temp <-  NULL
for (i in 1:nrow(shelter)){
  temp[i] <- age(num_part[i], unit_part[i])
}

shelter <- shelter %>%
  mutate(Age = round(temp, 1))

shelter %$%
  summary(Age)

# Anomaly Detection
ggplot(shelter[tv_row,], aes(Age)) +
  geom_freqpoly(bins = 30) +
  xlim(0, 8030) +
  ggtitle("Distribution of Age") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Age") +
  ylab("Count")

# Age VS. Outcome
shelter[tv_row,] %>%
  group_by(Age) %>%
  summarize(count = n(), mean = mean(Outcome == 1)) %>%
  ggplot(aes(Age, mean)) +
  geom_line() +
  xlim(0, 8030) +
  ggtitle("Age VS. Outcome") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Age") +
  ylab("Adoption(%)")
  
## 2.7 Breed
length(unique(shelter$Breed)) # That's too many!

# 2.7.1 Mix
shelter <- shelter %>%
  mutate(Mix = factor(ifelse((str_detect(shelter$Breed, "(Mix|/)")), 
                             1, 0)))

shelter[tv_row,] %>%  
  group_by(Mix) %>%
  summarize(count = n(), prob = mean(Outcome == 1)) # too unbalanced

# 2.7.2 Shorthair
shelter <- shelter %>%
  mutate(Shorthair = factor(ifelse(str_detect(shelter$Breed, "Shorthair"), 1, 0)))

shelter[tv_row,] %>%  
  group_by(Shorthair) %>%
  summarize(count = n(), prob = mean(Outcome == 1)) # little help

# 2.7.3 Take Type into consideration
shelter[tv_row,] %>%  
  group_by(Type, Mix) %>%
  summarize(count = n(), prob = mean(Outcome == 1))

shelter[tv_row,] %>%  
  group_by(Type, Shorthair) %>%
  summarize(count = n(), prob = mean(Outcome == 1))

shelter[tv_row,] %>%  
  group_by(Type, Mix, Shorthair) %>%
  summarize(count = n(), prob = mean(Outcome == 1))

## 2.8 Color
length(unique(shelter$Color)) # That's a lot!

# 2.8.1 Hybrid
shelter <- shelter %>%
  mutate(Hybrid = factor(ifelse(str_detect(shelter$Color, "/"), 1, 0)))

shelter[tv_row,] %>%  
  group_by(Hybrid) %>%
  summarize(count = n(), prob = mean(Outcome == 1)) # little help

# 2.8.2 Tabby
shelter <- shelter %>%
  mutate(Tabby = factor(ifelse(str_detect(shelter$Color, "(Tabby|/)"), 1, 0)))

shelter[tv_row,] %>%  
  group_by(Tabby) %>%
  summarize(count = n(), prob = mean(Outcome == 1)) # little help

# 2.8.3 Take Type into consideration
shelter[tv_row,] %>%  
  group_by(Type, Hybrid) %>%
  summarize(count = n(), prob = mean(Outcome == 1)) 

shelter[tv_row,] %>%  
  group_by(Type, Tabby) %>%
  summarize(count = n(), prob = mean(Outcome == 1))

## 2.9 DO some magic
# 2.9.0 Preperation
Cat <- shelter %>%
  filter(Type == "Cat")
Not_NA_cat <- which(!is.na(Cat$Outcome))

Dog <- shelter %>%
  filter(Type == "Dog")
Not_NA_dog <- which(!is.na(Dog$Outcome))

# 2.9.1 Breed-Cat
Cat <- Cat %>%
  mutate(Breed = str_remove(Cat$Breed, "Mix")) %>%
  mutate(
    Hair = ifelse(str_detect(Cat$Breed, "/") | !str_detect(Cat$Breed, "Domestic"), "Exotic", 
                     ifelse(str_detect(Cat$Breed, "Shorthair"), "Domestic Shorthair", 
                            ifelse(str_detect(Cat$Breed, "Longhair"), "Domestic Longhair",
                                              "Domestic Mediumhair")))
    )

Cat[Not_NA_cat,] %>%
  group_by(Hair) %>%
  summarize(count = n(), prob = mean(Outcome == 1))

# 2.9.2 Breed-Dog
dog_breed <- dog_breed %>%
  mutate(Mark1 = ifelse(dog_breed$Point1 == 0, median(dog_breed$Point), dog_breed$Point1),
         Mark2 = ifelse(dog_breed$Point2 == 0, median(dog_breed$Point), dog_breed$Point2),
         Mark = (Mark1 + Mark2)/2)

Dog <- Dog %>%
  mutate(Size = as.factor(dog_breed$Mark))

Dog[Not_NA_dog,] %>%
  group_by(Size) %>%
  summarize(count = n(), prob = mean(Outcome == 1))

Dog[Not_NA_dog,] %>%
  ggplot(aes(x = Size, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Size VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)")) +
  geom_hline(aes(yintercept = mean(Dog[Not_NA_dog,]$Outcome == 1)))# see the big pic
# coord_cartesian(ylim=c(5,15))

#lapply(Dog[Not_NA_dog,28], function(x) IV(as.factor(x), Dog$Outcome[Not_NA_dog]))

# 2.9.3 Color-Cat
Cat <- Cat %>%
  mutate(Col_cat = ifelse(str_detect(Cat$Color, "(Tabby)(/)"), "T&M",
                      ifelse(str_detect(Cat$Color, "/"), "Mix",
                             ifelse(str_detect(Cat$Color, "Tabby"), "Tabby",
                                    "Simple")))
         )

Cat[Not_NA_cat,] %>%
  group_by(Col_cat) %>%
  summarize(count = n(), prob = mean(Outcome == 1))

# 2.9.4 Color-Dog
temp <- c(as.vector(unlist(str_split(Dog[Dog$Hybrid==1,]$Color, "/"))), 
                    as.vector(Dog[Dog$Hybrid==0,]$Color))
table(temp)

Dog <- Dog %>%
  mutate(
    Col_dog = ifelse(str_detect(Dog$Color, "White"), "White",
                       ifelse(str_detect(Dog$Color, "Black"), "Black",
                              ifelse(str_detect(Dog$Color, "Tan"), "Tan",
                                     ifelse(str_detect(Dog$Color, "Brown"), "Brown",
                                            "Others"))))
    )

Dog[Not_NA_dog,] %>%
  group_by(Col_dog) %>%
  summarize(count = n(), prob = mean(Outcome == 1))

## 2.10 Put them together
adoption <- shelter %>%
  dplyr::select(11:27, 10)
glimpse(adoption)

cat_adoption <- Cat %>%
  dplyr::select(11:29, 10)
glimpse(cat_adoption)

dog_adoption <- Dog %>%
  dplyr::select(11:29, 10)
glimpse(dog_adoption)


### 3. Exploratory Data Analysis
ggplot(adoption[tv_row,], aes(x = Type, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("AnimalType VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)"))

ggplot(adoption[tv_row,], aes(x = Has_Name, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Has_Name VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)"))

ggplot(adoption[tv_row,], aes(x = Len_Name, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Len_Name VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)"))

ggplot(adoption[tv_row,], aes(x = Season, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Season VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)"))

ggplot(adoption[tv_row,], aes(x = Wday, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Wday VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)"))

ggplot(adoption[tv_row,], aes(x = Daytime, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Daytime VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)"))

# If Sex is Unknown, then Outcome is 0
ggplot(adoption[tv_row,], aes(x = Sex, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Sex VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)"))

# If Intact is Unknown, then Outcome is 0
ggplot(adoption[tv_row,], aes(x = Intact, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Intact VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)"))

# Some changes is needed for Age
# No ggplot here 

ggplot(adoption[tv_row,], aes(x = Mix, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Mix VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)"))

ggplot(adoption[tv_row,], aes(x = Shorthair, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Shorthair VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)")) +
  facet_wrap(~Type)

ggplot(adoption[tv_row,], aes(x = Hybrid, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Hybrid VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)")) +
  facet_wrap(~Type)

ggplot(adoption[tv_row,], aes(x = Tabby, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Tabby VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)")) +
  facet_wrap(~Type)

ggplot(cat_adoption[Not_NA_cat,], aes(x = Hair, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Hair(Cat) VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)"))

ggplot(cat_adoption[Not_NA_cat,], aes(x =Col_cat, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Col_cat(Cat) VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)"))

ggplot(dog_adoption[Not_NA_dog,], aes(x = Size, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Size(Dog) VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)"))

ggplot(dog_adoption[Not_NA_cat,], aes(x =Col_dog, y = ..count.., fill = Outcome)) +
  geom_bar(stat = "count", position = "fill") +
  ggtitle("Col_dog(Dog) VS. Outcome") +
  scale_fill_discrete(labels=c("0 (Not Adpoted)", "1 (Adopted)"))

### 4. Missing Value Detection & Imputation
## 4.1 Missing Report(including anomaly)
adoption %>%
  mutate(Age = ifelse(adoption$Age == 0, NA, adoption$Age),
         Sex = ifelse(adoption$Sex == "Unknown", NA, adoption$Sex),
         Intact = ifelse(adoption$Intact == "Unknown", NA, adoption$Intact)) %>%
  miss_var_summary()

## 4.2 Missing Randomness Detection
#vis_miss(adoption[,c(1,8:10)], cluster = T) 

## 4.3 Missing Imputation (Median/Mode/KNN)
# 4.3.1 Age
# caret preprocess later

# 4.3.2 Sex & Intact
# Because there seems like strong rules exists, so we just leave the NA's as "Unknown"


### 5. Feature Selection
lapply(adoption[tv_row,], function(x) IV(as.factor(x), adoption$Outcome[tv_row]))

lapply(cat_adoption[Not_NA_cat,], function(x) IV(as.factor(x), cat_adoption$Outcome[Not_NA_cat]))
lapply(dog_adoption[Not_NA_dog,], function(x) IV(as.factor(x), dog_adoption$Outcome[Not_NA_dog]))

adopt <- adoption %>%
  dplyr::select(c(1,5,7,9,10,12,13,18)) ###!!! according to what? Sense or Results?
  

### 6. Modeling (Building & CV & Hyper-tuning)
## 6.1 Fit Control for Hyper-Tuning
fitControl <- trainControl(method = "repeatedcv", 
                           number = 3,
                           repeats = 5,
                           search = "random")

## 6.2 rpart Missing Imputation
Age_NA <- which(adopt$Age == 0)

training <- adopt[-Age_NA, -8]
testing <- adopt[Age_NA, -8]

tic()
mod_imp <- train(Age ~., training, 
                 method = "rpart",
                 trControl = fitControl,
                 tuneLength = 5)
toc()

age_pred <- predict(mod_imp, testing)

adopt$Age[Age_NA] <- age_pred

## 6.3 Dummy Variables
temp <- adopt[tv_row, ]

dmy <- dummyVars(~ ., data = temp[,-ncol(temp)])
dmy_data <- data.frame(predict(dmy, newdata = temp[,-ncol(temp)]))
dmy_data <- bind_cols(dmy_data, temp[,ncol(temp)])

levels(dmy_data$Outcome) <- c("Not_Adopted", "Adopted")

## 6.4 Stratified Sampling
index <- createDataPartition(dmy_data$Outcome, p = .70, list = FALSE)

## 6.5 Modeling
# 6.5.1 XGBoost (250s; .82)
tic()
mod1 <- train(Outcome ~ .,
              data = dmy_data[index,],
              method = "xgbTree",
              trControl = fitControl,
              verbose = FALSE,
              tunelength = 5)
toc()

pred1 <- predict(mod1, dmy_data[-index,], type = "raw")
caret::confusionMatrix(pred1, dmy_data[-index,]$Outcome, mode = "prec_recall")

# 6.5.2 Random Forest (600s; .81)
tic()
mod2 <- train(Outcome ~ .,
              data = dmy_data[index,],
              method = "rf",
              trControl = fitControl,
              verbose = FALSE,
              tunelength = 5)
toc()

pred2 <- predict(mod2, dmy_data[-index,], type = "raw")
caret::confusionMatrix(pred2, dmy_data[-index,]$Outcome, mode = "prec_recall")

# 6.5.3 RBF SVM (800s; .80)
tic()
mod3 <- train(Outcome ~ .,
              data = dmy_data[index,],
              method = "svmRadial",
              trControl = fitControl,
              verbose = FALSE,
              tunelength = 5)
toc()

pred3 <- predict(mod3, dmy_data[-index,], type = "raw")
caret::confusionMatrix(pred3, dmy_data[-index,]$Outcome, mode = "prec_recall")

# 6.5.4 Neural Network(150s; .82)
tic()
mod4 <- train(Outcome ~ .,
              data = dmy_data[index,],
              method = "nnet",
              trControl = fitControl,
              verbose = FALSE,
              tunelength = 5)
toc()

pred4 <- predict(mod4, dmy_data[-index,], type = "raw")
caret::confusionMatrix(pred4, dmy_data[-index,]$Outcome, mode = "prec_recall")

## 6.6 Combining model
mothod_list <- list(
  xgb = mod1,
  rf = mod2,
  svm = mod3,
  nnet = mod4
)

# Together
resamps <- resamples(mothod_list)
summary(resamps, metric = "Accuracy")
bwplot(resamps, metric = "Accuracy")
# From the plot, it's reasonable to say that XGBoost performs best.

## 6.7 Advanced Modeling: Ensemble/Stacking
stackControl <- trainControl(
  method = "boot",
  number = 25,
  savePredictions = "final",
  classProbs=TRUE,
  index = createResample(dmy_data[index,]$Outcome, 25)
)

tic()
model_list <- caretList(
  Outcome ~ ., data = dmy_data[index,],
  trControl = stackControl,
  methodList = c("xgbTree", "rf", "svmRadial", "nnet")
)

toc()

gbm_ensemble <- caretStack(
  model_list,
  method="gbm",
  verbose=FALSE,
  tuneLength=10,
  trControl=trainControl(
    method="boot",
    number=10,
    savePredictions="final",
    classProbs=TRUE
  )
)

pred5 <- predict(gbm_ensemble, dmy_data[-index,], type = "raw")
caret::confusionMatrix(pred5, dmy_data[-index,]$Outcome, mode = "prec_recall")

model_preds <- 
  lapply(model_list, predict, newdata = dmy_data[-index,], type = "raw")
conf_matrix <- 
  lapply(model_preds, caret::confusionMatrix, dmy_data[-index,]$Outcome, mode = "prec_recall")

#as.numeric(tidy(conf_matrix[1]$xgbTree)[9, 3])

## 6.8 Model choosing: F1
# This plan is used to help determine which animals we want to keep. 
# (we'll first keep those animals that are more likely to be adopted)
# In this way, precision is important, because our budget is limited;
# however, recall is also critical, because we don't want to miss a lot.
# So for which beta we'll use, it depends on our budget size.
# For now, we use F1.
