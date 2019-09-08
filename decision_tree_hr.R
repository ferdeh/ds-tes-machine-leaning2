# Make Employee turn over using Decision Tree Model

## remove all variables (start from scratch)
rm(list=ls())

# Load library
library(tidyverse)
library(DataExplorer)
library(rpart)
library(rpart.plot)

# Load dataset 
df_people <- read.csv("https://raw.githubusercontent.com/arikunco/machinelearning/master/dataset/HR_comma_sep.csv",stringsAsFactors = T)
levels(df_people$sales)
#membuat urutan kategori pada gaji
levels(df_people$salary) <- c("low","medium","high")
levels(df_people$salary)

# Preview first six rows
head(df_people)

# Preview last six rows 
tail(df_people) 

# Column Names 
names(df_people)

# Data type per columns 
str(df_people)

# Statistic Description
summary(df_people)

# check missing value 
plot_missing(df_people)

# Visualize data 

# histogram of satisfaction level by Left 
df_people$left <- as.factor(df_people$left)
ggplot(df_people) + geom_histogram(aes(x=satisfaction_level,color=left), fill="White",alpha=0.5, bins = 30,position="identity")

# histogram of last evluation by Left 
ggplot(df_people) + geom_histogram(aes(x=last_evaluation,color=left), fill="White",alpha=0.5, bins = 30,position="identity")

# barchart of Number Project Vs Left
counts <- table(df_people[,c('left','number_project')])
counts
barplot(counts, main="Number of Project vs Left",
        xlab="Number of Project", col=c("darkblue","red"),legend = rownames(counts), beside=TRUE)

# stackbarchart of Number Project by Left
propcount <- prop.table(counts)
propcount
barplot(propcount, main="Number Project vs Left",
        xlab="Number of Project", col=c("darkblue","red"),legend = rownames(counts))


# histogram of Monthly Hour by Left 
ggplot(df_people) + geom_histogram(aes(x=average_montly_hours,color=left), fill="White",alpha=0.5, bins = 30,position="identity")

# barchart of Work Accident Vs Left
counts <- table(df_people[,c('left','Work_accident')])
counts
barplot(counts, main="Work Accident vs Left",
        xlab="Work Accident", col=c("darkblue","red"),legend = rownames(counts), beside=TRUE)

# stackbarchart of Work Accident by Left
propcount <- prop.table(counts)
propcount
barplot(propcount, main=" Work Accident vs Left",
        xlab="Work Accident", col=c("darkblue","red"),legend = rownames(counts))


# Table crosstab of promotion by left 
counts <- table(df_people$promotion_last_5years, df_people$left)
counts
barplot(counts, main="Promotion last 5 years vs Left",
        xlab="Promotion last 5 years",col=c("darkblue","red"), legend = rownames(counts),beside=TRUE)

propcount <- prop.table(counts)
propcount
barplot(propcount, main="promotion vs Left",
        xlab="Promotion", col=c("darkblue","red"),legend = rownames(counts))


# barchart of Salary by Left 
counts <- table(df_people[,c('left','salary')])
counts
barplot(counts, main="Salary Distribution vs Left",
        xlab="Salary", col=c("darkblue","red"),legend = rownames(counts), beside=TRUE)

# stackbarchart of Salary by Left
propcount <- prop.table(counts)
propcount
barplot(propcount, main="Salary Distribution vs Left",
        xlab="Salary", col=c("darkblue","red"),legend = rownames(counts))


# Membuat Model

# membuat random sampling dengan index yang sama dalam setiap program di run
set.seed(123)

## 80% of the sample size
smp_size <- floor(0.8 * nrow(df_people))

## membuat index dari data training
train_ind <- sample(seq_len(nrow(df_people)), size = smp_size)

## Membuat data training
train <- df_people[train_ind, ]
dim(train)
head(train)

## Membuat data test
test <- df_people[-train_ind, ]
dim(test)
head(test)

## meihat proporsi left pada data test dan data training
prop.table(table(train$left))
prop.table(table(test$left))

fit <- rpart(left ~.,data = train,
             method="class"
)

## Decision Tree Model
fit
summary(fit)
rpart.plot(fit)

## Validation Models
predict_unseen <-predict(fit, test, type = 'class')
table_mat <- table(test$left, predict_unseen)

## Confusion Matrix
table_mat

## Acuracy Test
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

library(randomForest) # package yang memuat fungsi randomForest
randomFor <- randomForest(left ~ ., data = train, ntree=500, importance = TRUE)
predict(randomFor, test,type="class")
table_mat2<-table(test$left,predict(randomFor, test, type="class"))
prop.table(table(test$left,predict(randomFor, test, type="class")))

## Confusion Matrix
table_mat2

## Acuracy Test
accuracy_Test <- sum(diag(table_mat2)) / sum(table_mat2)
print(paste('Accuracy for test', accuracy_Test))


