library(glmnet)
library(caret)
library(randomForest)
library(magrittr)
library(caret)



getwd()
setwd("C:/Users/User/Documents/kaggle/titanic")
train_data = read.csv("train.csv")
test_data = read.csv("test.csv")

dim(train_data)
head(train_data)


set.seed(825)
sum(is.na(train_data$Survived))


train_data$Survived = as.factor(train_data$Survived)



age_pred = lm(Age ~ . -Name -Ticket - Cabin , data = train_data[, -which(names(train_data) == "Survived")])

new_age = predict(age_pred, newdata = test_data)


age_pred$xlevels[['Name']] = union(age_pred$xlevels[['Name']], 
                           levels(test_data$Name))
age_pred$xlevels[['Ticket']] = union(age_pred$xlevels[['Ticket']], 
                                  levels(test_data$Ticket))
age_pred$xlevels[['Cabin']] = union(age_pred$xlevels[['Cabin']], 
                                    levels(test_data$Cabin))



replace_fn = function(x,y){
  
  for(i in 1 : nrow(x)){
  if(is.na(x[i,"Age"]))
    x[i,"Age"] <- y[i,"new_age"]
  }
  return(x)
} 

test_data2 <- replace_fn(test_data, new_age)

log_mod = glm(Survived ~ . - Name - Ticket - Cabin , data = train_data , family = 'binomial')
summary(log_mod)

#rf_grid =  expand.grid(mtry = 1:12)

oob = trainControl(method = "oob")

rf_tune = train(Survived ~ Pclass + Sex + Age + SibSp, data = train_data,
                method = "rf",
                trControl = oob,
                verbose = FALSE,
                na.action=na.exclude)

test_data$Survived = predict(rf_tune, newdata = test_data2)

x = test_data %>% select(PassengerId, Survived)

write.csv(as.data.frame(x), file = "result.csv")



featurePlot(train_data$Survived, train_data[,-2])

pairs(~ Survived + Pclass + Sex + Age + SibSp,data=train_data, 
      main="Simple Scatterplot Matrix")



######### Analysis ###################

plot(train_data$Sex , train_data$Survived)

plot(train_data$Age , train_data$Survived)

            
