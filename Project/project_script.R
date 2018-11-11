rm(list=ls())

library("data.table")
library("ggplot2")
library("gridExtra")
library("corrplot")
library("caret")
library("e1071")
library("ROSE") # Randomly Over Sampling Examples # Not preferred
library("DMwR")
library("rpart")
library("rpart.plot")
library("randomForest")
library("ROCR")
hr.dt <- fread("WA_Fn-UseC_-HR-Employee-Attrition.csv")
# To rename the first column
colnames(hr.dt)[1] <- "Age"
summary(hr.dt)

hr.dt$Attrition <- factor(hr.dt$Attrition)
hr.dt$BusinessTravel <- factor(hr.dt$BusinessTravel)
hr.dt$Department <- factor(hr.dt$Department)
hr.dt$Education <- factor(hr.dt$Education, levels = c(1, 2, 3, 4, 5), labels = c("Below College", "College", "Bachelor", "Master", "Doctor"))
hr.dt$EducationField <- factor(hr.dt$EducationField)
hr.dt$EnvironmentSatisfaction <- factor(hr.dt$EnvironmentSatisfaction, levels = c(1, 2, 3, 4), labels = c("Low", "Medium", "High", "Very High"))
hr.dt$Gender <- factor(hr.dt$Gender)
hr.dt$JobInvolvement <- factor(hr.dt$JobInvolvement, levels = c(1, 2, 3, 4), labels = c("Low", "Medium", "High", "Very High"))
hr.dt$JobLevel <- factor(hr.dt$JobLevel)
hr.dt$JobRole <- factor(hr.dt$JobRole)
hr.dt$JobSatisfaction <- factor(hr.dt$JobSatisfaction, levels = c(1, 2, 3, 4), labels = c("Low", "Medium", "High", "Very High"))
hr.dt$MaritalStatus <- factor(hr.dt$MaritalStatus)
hr.dt$Over18 <- factor(hr.dt$Over18)
hr.dt$OverTime <- factor(hr.dt$OverTime)
hr.dt$PerformanceRating <- factor(hr.dt$PerformanceRating, levels = c(1, 2, 3, 4), labels = c('Low', 'Good', 'Excellent', 'Outstanding'))
hr.dt$RelationshipSatisfaction <- factor(hr.dt$RelationshipSatisfaction, levels = c(1, 2, 3, 4), labels = c("Low", "Medium", "High", "Very High"))
hr.dt$StockOptionLevel <- factor(hr.dt$StockOptionLevel)
hr.dt$WorkLifeBalance <- factor(hr.dt$WorkLifeBalance, levels = c(1, 2, 3, 4), labels = c("Bad", "Good", "Better", "Best"))

str(hr.dt) 
summary(hr.dt)

# Check to see whether there is NA values
sum(is.na(hr.dt)) # there is no NA value

hr.dt[EmployeeCount != 1,] # All the rows are 1
# Reomve EmployeeCount column since all the values are the same (i.e. 1)
hr.dt$EmployeeCount <- NULL

# Remove EmployeeNumber column since it is just an index, hence not relevant for building model
hr.dt$EmployeeNumber <- NULL

hr.dt[Over18 == "N",] # Check if anybody is under 18, but there is nobody
# Remove Over18 column since everyone is above 18
hr.dt$Over18 <- NULL

hr.dt[StandardHours != 80,] # Check if everyone is working 80 hours: yes.
# Remove StandardHours column since everyone has to work 80 hours
hr.dt$StandardHours <- NULL

summary(hr.dt)

# Explorartory Data Analysis
# Univariate Analysis
# Attrition - DailyRate. Higher daily rate lower attrition rate
p1 <- ggplot(hr.dt, aes(x= Age, fill = Attrition)) + geom_density(alpha = 0.4) #The younger the employee, the more likely they will leave, vice versa for older employees.
p2 <- ggplot(hr.dt, aes(x= DailyRate, fill = Attrition)) + geom_density(alpha = 0.4) #Employees with low daily rates are more likely to leave, vice versa for employees with high daily rates
p3 <- ggplot(hr.dt, aes(x= HourlyRate, fill = Attrition)) + geom_density(alpha = 0.4) #Employees with low hourly rates are more likely to leave, vice versa for empployees with high hour rate
p4 <- ggplot(hr.dt, aes(x= MonthlyRate, fill = Attrition)) + geom_density(alpha = 0.4) #Data is unclear
p5 <- ggplot(hr.dt, aes(x= MonthlyIncome, fill = Attrition)) + geom_density(alpha = 0.4) #Employees with low monthly income are more likely to leave, vice versa for employees with high monthly income

p6 <- ggplot(hr.dt, aes(x= DistanceFromHome, fill = Attrition)) + geom_density(alpha = 0.4) # Longer travelling distance to work from home has a greater likelihood of attrition
p7 <- ggplot(hr.dt, aes(x= NumCompaniesWorked, fill = Attrition)) + geom_density(alpha = 0.4) # The greater the number of companies worked, the more likely is the employee going to leave
p8 <- ggplot(hr.dt, aes(x= PercentSalaryHike, fill = Attrition)) + geom_density(alpha = 0.4) # Correlation is not obvious
p9 <- ggplot(hr.dt, aes(x= TotalWorkingYears, fill = Attrition)) + geom_density(alpha = 0.4) #The longer the total working years of an employee, the less likely that are to leave.

p10 <- ggplot(hr.dt, aes(x= TrainingTimesLastYear, fill = Attrition)) + geom_density(alpha = 0.4) # Correlation is not obvious
p11 <- ggplot(hr.dt, aes(x= YearsAtCompany, fill = Attrition)) + geom_density(alpha = 0.4) #The longer the employee works in at the company, the less likely they are going to leave.
p12 <- ggplot(hr.dt, aes(x= YearsInCurrentRole, fill = Attrition)) + geom_density(alpha = 0.4)#The longer the employee is in their current role, the less likely they are going to leave.
p13 <- ggplot(hr.dt, aes(x= YearsSinceLastPromotion, fill = Attrition)) + geom_density(alpha = 0.4) # Correlation is not obvious

p14 <- ggplot(hr.dt, aes(x= YearsWithCurrManager, fill = Attrition)) + geom_density(alpha = 0.4) #The longer the employee works with their current manager, the less likely they are going to leave.
combined.plot1 <- grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, ncol = 5)
plot(combined.plot1)

p15 <- ggplot(hr.dt, aes(x = BusinessTravel, fill = Attrition)) + geom_bar(position = 'fill', alpha = 0.4) # Those who travel frequently are more likely to leave
p16 <- ggplot(hr.dt, aes(x = Department, fill = Attrition)) + geom_bar(position = 'fill', alpha = 0.4) # Employees from R&D are th least likely to leave, while those from Sales are more likley to leave.
p17 <- ggplot(hr.dt, aes(x = Education, fill = Attrition)) + geom_bar(position = 'fill', alpha = 0.4) # Higher Education, less likely to leave
p18 <- ggplot(hr.dt, aes(x = EducationField, fill = Attrition)) + geom_bar(position = 'fill', alpha = 0.4) # Little correlation, a weak predictor
p19 <- ggplot(hr.dt, aes(x = EnvironmentSatisfaction, fill = Attrition)) + geom_bar(position = 'fill', alpha = 0.4) # Higher Satisfaction, less likely to leave
p20 <- ggplot(hr.dt, aes(x = Gender, fill = Attrition)) + geom_bar(position = 'fill', alpha = 0.4) # Males are more likely to leave. However, difference is small, so relatively weak predictor
p21 <- ggplot(hr.dt, aes(x = JobInvolvement, fill = Attrition)) + geom_bar(position = 'fill', alpha = 0.4) # The more involved an employee is, the less likely to leaves
p22 <- ggplot(hr.dt, aes(x = JobLevel, fill = Attrition)) + geom_bar(position = 'fill', alpha = 0.4) # Higher job level, less likely to leave
p23 <- ggplot(hr.dt, aes(x = JobRole, fill = Attrition)) + geom_bar(position = 'fill', alpha = 0.4) #Sales representatives are the most likely to leave the company.
p24 <- ggplot(hr.dt, aes(x = JobSatisfaction, fill = Attrition)) + geom_bar(position = 'fill', alpha = 0.4) # Higher job satisfaction, less likely to leave
p25 <- ggplot(hr.dt, aes(x = MaritalStatus, fill = Attrition)) + geom_bar(position = 'fill', alpha = 0.4) # Single > Married > Divorced
p26 <- ggplot(hr.dt, aes(x = OverTime, fill = Attrition)) + geom_bar(position = 'fill', alpha = 0.4) # Move overtime -> more likely to leave
p27 <- ggplot(hr.dt, aes(x = PerformanceRating, fill = Attrition)) + geom_bar(position = 'fill', alpha = 0.4) # Performance rating has no impact
p28 <- ggplot(hr.dt, aes(x = RelationshipSatisfaction, fill = Attrition)) + geom_bar(position = 'fill', alpha = 0.4) # Higher relationship satisfaction -> less likely to leave. However, correlation is weak
p29 <- ggplot(hr.dt, aes(x = StockOptionLevel, fill = Attrition)) + geom_bar(position = 'fill', alpha = 0.4) # Meaning unclear
p30 <- ggplot(hr.dt, aes(x = WorkLifeBalance, fill = Attrition)) + geom_bar(position = 'fill', alpha = 0.4) # Better work life balance means lower chance of leaving 
combined.plot2 <- grid.arrange(p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, ncol = 4)
plot(combined.plot2)
rm(list = ls(pattern = "^p"))

# From the above analysis, we decide to not consider Department, EducationField, Gender, PerformanceRating, TrainingTimeLastYear, YearsSinceLastPromotion.

# Find correlations between numeric data types of the data table.
corrplot(cor(hr.dt[, c(1, 4, 6, 11, 17, 18, 19, 21, 25, 26, 28, 29, 30, 31)]), type = "upper", method = 'ellipse')
# Decide to remove TotalWorkingYears, since it is highly correlated with age and monthly income. This makes logical sence because a higher age is required for one to work longer.
# And as one works more, his monthly income will tend to be higher.
corrplot(cor(hr.dt[, c(1, 4, 6, 11, 17, 18, 19, 21, 26, 28, 29, 30, 31)]), type = "upper", method = 'ellipse')
# We found out that there is high correlation between YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion and YearsWithCurrManager.
# Thus we decide to remove YearsAtCompany since the more years since last promotion, with current manager, in current role, the more years one works at the company
corrplot(cor(hr.dt[, c(1, 4, 6, 11, 17, 18, 19, 21, 26, 29, 30, 31)]), type = "upper", method = 'ellipse')
# Lastly, we decide to remove YearsWithCurrManager, since it has a high correlation with YearsInCurrentRole and YearsSinceLastPromotion
corrplot(cor(hr.dt[, c(1, 4, 6, 11, 17, 18, 19, 21, 26, 29, 30)]), type = "upper", method = 'ellipse')

# To find out the correlation between categorical variables and numeric variables, we perform a one-way ANOVA F-test for:
get.summary <- function(i, j){
  col1 <- colnames(hr.dt[,..i])
  col2 <- colnames(hr.dt[,..j])
  print(col1)
  print(col2)
  summary(aov(hr.dt[[col1]]~ hr.dt[[col2]]))
}

for (i in c(1, 4, 6, 11, 17, 18, 19, 21, 25, 26, 28, 29, 30, 31)){
  for (j in c(3, 7, 9, 12, 13, 14, 15, 16, 20, 22, 23, 24, 25, 28)){
    print(get.summary(i, j))
  }
}

# In addition, we discovered that there is a very strong correlation between JobLevel, Age, MonthlyIncome, TotalWorkingYears
# This makes logical sense since normally people with higher age will occupy a higher position/higher job role.
# Also, higher job levelars are sometimes needed to attain a higher job levl.

# Also, there exists a strong correlation between PerformanceRating and PercentSalaryHike
# Since p-value is 2e-16, are oftentimes associated with higher monthly income.
# Also, total working ye way less than 0.05, so the null hypothesis of independence is rejected.
# This also makes logical sense as salary hike normally depends the performance of individual

# In conclusion, we are going to remove Department, EducationField, Gender, PerformanceRating, TrainingTimeLastYear, YearsSinceLastPromotion, JobLevel, YeasInCurrRole,PercentSalaryHike from our consideration.
new.hr.dt <- hr.dt[, - c(5, 8, 10, 13, 21, 22, 25, 26, 28, 29, 30, 31)]

ggplot(new.hr.dt, aes(x = Attrition)) + geom_bar(fill = "steelblue")
summary(new.hr.dt$Attrition)
# We observe that there is a severe class imbalance problem in our response variable 'Attrition' (i.e. 1233 cases of 'No', 237 cases of 'Yes')
# So we use oversampling of minority class/undersampling of majority class to solve the problem. For this, we will use both SMOTE and ROSE technique
set.seed(11112018)
ind <- sample(2, nrow(new.hr.dt), replace = T, prob = c(0.7, 0.3))
hr.dt.train <- new.hr.dt[ind == 1,]
hr.dt.test <- new.hr.dt[ind == 2,]

set.seed(11112018)
summary(hr.dt.train$Attrition)
hr.dt.train.SMOTE <- SMOTE(Attrition ~ ., hr.dt.train)
hr.dt.train.over.ROSE <- ovun.sample(Attrition ~., data = hr.dt.train, method = "over", N = 1676)$data
hr.dt.train.under.ROSE <- ovun.sample(Attrition ~., data = hr.dt.train, method = "under", N = 336)$data
summary(hr.dt.train.SMOTE$Attrition)
summary(hr.dt.train.over.ROSE$Attrition)
summary(hr.dt.train.under.ROSE$Attrition)


################################################
# Utility Functions
################################################
plot.roc <- function(built.model, hr.datatable.test){
  # Evaluation of the model using ROC & AUC (Area Under the Curve)
  roc <- performance(prediction(as.numeric(predict(built.model, hr.datatable.test)), as.numeric(hr.datatable.test$Attrition)), "tpr", "fpr")
  plot(roc, colorize = T, main = "ROC Curve", ylab = "Sensitivity", xlab = "1-Specificity")
  abline(a = 0, b = 1)
  auc <- performance(prediction(as.numeric(predict(built.model, hr.datatable.test)), as.numeric(hr.datatable.test$Attrition)), "auc")
  auc <- unlist(slot(auc, "y.values"))
  legend(0.618, 0.2, round(auc, 4), title = "AUC")
}

get.confusion.matrix <- function(built.model, hr.datatable.test){
  print(confusionMatrix(predict(built.model, hr.datatable.test), hr.datatable.test$Attrition, positive = "Yes"))
}

show.coefficients <- function(built.model){
  print(coef(built.model$finalModel, s = built.model$bestTune$lambda)) # Shows the coefficients
}

####################################################################################################################################
# Model Definitions
####################################################################################################################################

# Set custom control parameter first (i.e. 10-fold Cross Validation):
custom.control.param <- trainControl(method = "repeatedcv",
                                     number = 10,
                                     repeats = 5,
                                     verboseIter = T)

# Stepwise Forward Logistic Regression
log.reg.stepwise.forward <- function(hr.datatable.train, hr.datatable.test){
  logreg.null <- glm(Attrition ~ 1, family = binomial, data = hr.datatable.train)
  logreg.full <- glm(Attrition ~ ., family = binomial, data = hr.datatable.train)
  logreg.forward <- step(logreg.null, scope = formula(logreg.full), direction = 'forward')
  summary(logreg.forward)
  plot.roc(logreg.forward, hr.datatable.test)
  # Select a threshold:
  threshold <- sum(hr.datatable.train$Attrition == 'Yes') / length(hr.datatable.train$Attrition)
  pred <- predict(logreg.forward, type = 'response', newdata = hr.datatable.test)
  pred.hat <- ifelse(pred > threshold, 2, 1)
  print(confusionMatrix(data = factor(pred.hat, labels = c("No", "Yes")), reference = hr.datatable.test$Attrition))
  
  roc <- performance(prediction(as.numeric(pred), as.numeric(hr.datatable.test$Attrition)), "tpr", "fpr")
  plot(roc, colorize = T, main = "ROC Curve", ylab = "Sensitivity", xlab = "1-Specificity")
  abline(a = 0, b = 1)
  auc <- performance(prediction(as.numeric(pred), as.numeric(hr.datatable.test$Attrition)), "auc")
  auc <- unlist(slot(auc, "y.values"))
  legend(0.618, 0.2, round(auc, 4), title = "AUC")
  invisible(logreg.forward)
}

# Stepwise Backward Logistic Regression
log.reg.stepwise.backward <- function(hr.datatable.train, hr.datatable.test){
  logreg.null <- glm(Attrition ~ 1, family = binomial, data = hr.datatable.train)
  logreg.full <- glm(Attrition ~ ., family = binomial, data = hr.datatable.train)
  logreg.backward <- step(logreg.full, direction = 'backward')
  summary(logreg.backward)
  plot.roc(logreg.backward, hr.datatable.test)
  # Select a threshold:
  threshold <- sum(hr.datatable.train$Attrition == 'Yes') / length(hr.datatable.train$Attrition)
  pred <- predict(logreg.backward, type = 'response', newdata = hr.datatable.test)
  pred.hat <- ifelse(pred > threshold, 2, 1)
  print(confusionMatrix(data = factor(pred.hat, labels = c("No", "Yes")), reference = hr.datatable.test$Attrition))
  
  roc <- performance(prediction(as.numeric(pred), as.numeric(hr.datatable.test$Attrition)), "tpr", "fpr")
  plot(roc, colorize = T, main = "ROC Curve", ylab = "Sensitivity", xlab = "1-Specificity")
  abline(a = 0, b = 1)
  auc <- performance(prediction(as.numeric(pred), as.numeric(hr.datatable.test$Attrition)), "auc")
  auc <- unlist(slot(auc, "y.values"))
  legend(0.618, 0.2, round(auc, 4), title = "AUC")
  invisible(logreg.backward)
}

# Logistic Regression with Regularization

# Ridge Regression (L2 Parameterized Logistic Regression)
# Ridge Regression tries to shrink the coefficients, and keeps ALL variables in the model
log.reg.ridge <- function(hr.datatable.train, hr.datatable.test){
  set.seed(11112018)
  ridge <- train(Attrition ~ .,
                 hr.datatable.train,
                 family = "binomial",
                 method = "glmnet",
                 tuneGrid = expand.grid(alpha = 0, lambda = seq(0.0001, 1, length = 10)),
                 trControl = custom.control.param) # Alpha is zero for ridge regression
  
  print(plot(ridge))
  plot(ridge$finalModel, xvar = "lambda", label = T)
  plot(ridge$finalModel, xvar = "dev", label = T) # Percentage of variability explained at a point
  print(plot(varImp(ridge, scale = T)))
  plot.roc(ridge, hr.datatable.test)
  get.confusion.matrix(ridge, hr.datatable.test)
  invisible(ridge)
}

# Lasso Regression (L1 Parameterized Logistic Regression) - Least Absolute Shrinkage & Selection Operator
# Lasso Regression does both shrinkage of coefficents and feature selection.
# If there is a group of highly correlated variables that cause multicolinearity, Lasso Regression will select one variable from the group while ignoring the rest
log.reg.lasso <- function(hr.datatable.train, hr.datatable.test){
  set.seed(11112018)
  lasso <- train(Attrition ~ .,
                 hr.datatable.train,
                 family = "binomial",
                 method = "glmnet",
                 tuneGrid = expand.grid(alpha = 1, lambda = seq(0.0001, 1, length = 10)),
                 trControl = custom.control.param)
  print(plot(lasso))
  plot(lasso$finalModel, xvar = "lambda", label = T)
  plot(lasso$finalModel, xvar = "dev", label = T)
  print(plot(varImp(lasso, scale = T)))
  plot.roc(lasso, hr.datatable.test)
  get.confusion.matrix(lasso, hr.datatable.test)
  invisible(lasso)
}

# Elastic Net Regression (Linear Combination of Lasso & Ridge Regression)
log.reg.enet <- function(hr.datatable.train, hr.datatable.test){
  set.seed(11112018)
  enet <- train(Attrition ~ .,
                hr.datatable.train,
                family = "binomial",
                method = "glmnet",
                tuneGrid = expand.grid(alpha = seq(0, 1, length = 10), lambda = seq(0.0001, 1, length = 10)),
                trControl = custom.control.param)
  print(plot(enet))
  plot(enet$finalModel, xvar = "lambda", label = T)
  plot(enet$finalModel, xvar = "dev", label = T)
  print(plot(varImp(enet, scale = T)))
  plot.roc(enet, hr.datatable.test)
  get.confusion.matrix(enet, hr.datatable.test)
  invisible(enet)
}

# Random Forest model
rand.forest <- function(hr.datatable.train, hr.datatable.test){
  set.seed(11112018)
  rforest <- randomForest(Attrition ~ ., data = hr.datatable.train)
  get.confusion.matrix(rforest, hr.datatable.test)
  plot.roc(rforest, hr.datatable.test)
  invisible(rforest)
}

# Support Vector Machine Classifier
support.vector.classifier <- function(hr.datatable.train, hr.datatable.test, Kernel = "radial"){
  # Default Kernel is radial-based. However, can be changed to "linear", "polynomial", "sigmoid"
  supportvm <- svm(Attrition ~ ., data = hr.datatable.train, kernel = Kernel)
  print(summary(supportvm))
  pred.svm <- predict(supportvm, hr.datatable.test)
  cm.svm <- table(Predicted = pred.svm, Actual = hr.datatable.test$Attrition)
  sum(diag(cm.svm)) / sum(cm.svm) # To find the accuracy
  print(cm.svm)
  
  # Evaluation of the model using ROC & AUC (Area Under the Curve)
  roc <- performance(prediction(as.numeric(pred.svm), as.numeric(hr.datatable.test$Attrition)), "tpr", "fpr")
  plot(roc, colorize = T, main = "ROC Curve", ylab = "Sensitivity", xlab = "1-Specificity")
  abline(a = 0, b = 1)
  auc <- performance(prediction(as.numeric(pred.svm), as.numeric(hr.datatable.test$Attrition)), "auc")
  auc <- unlist(slot(auc, "y.values"))
  legend(0.618, 0.2, round(auc, 4), title = "AUC")
  invisible(supportvm)
}

# Hyper Parameter Optimization / Tuning for Support Vector Machine Classifier
best.svm <- function(hr.datatable.train, hr.datatable.test){
  set.seed(11112018)
  svm.tuned <- tune(svm, Attrition ~ ., data = hr.datatable.train, ranges = list(epsilon = seq(0, 1, 0.25), cost = 2 ^ (2:6))) 
  # Cost captures contraint violation. If cost is too high -> overfitting, otherwise underfitting
  plot(svm.tuned)
  # Dark Region means lower misclassification error
  svm.best <- svm.tuned$best.model
  print(summary(svm.best))
  pred.svm.best <- predict(svm.best, hr.datatable.test)
  cm.svm.best <- table(Predicted = pred.svm.best, Actual = hr.datatable.test$Attrition)
  print(sum(diag(cm.svm.best)) / sum(cm.svm.best)) # To find the accuracy
  print(cm.svm.best)
  
  roc <- performance(prediction(as.numeric(pred.svm.best), as.numeric(hr.datatable.test$Attrition)), "tpr", "fpr")
  plot(roc, colorize = T, main = "ROC Curve", ylab = "Sensitivity", xlab = "1-Specificity")
  abline(a = 0, b = 1)
  auc <- performance(prediction(as.numeric(pred.svm.best), as.numeric(hr.datatable.test$Attrition)), "auc")
  auc <- unlist(slot(auc, "y.values"))
  legend(0.618, 0.2, round(auc, 4), title = "AUC")
  invisible(svm.tuned)
}

# Classification and Regression Tree
max.tree <- function(hr.datatable.train, hr.datatable.test){
  set.seed(201811)
  cart.max <- rpart(Attrition ~., data = hr.datatable.train, method = "class", control = rpart.control(minsplit = 2, cp = 0))
  print(cart.max)
  printcp(cart.max, digits = 3)
  plotcp(cart.max)
  return(cart.max)
}

pruned.tree <- function(max.tree, cp.opt){
  cart.pruned <- prune(max.tree, cp = cp.opt)
  print(cart.pruned)
  printcp(cart.pruned, digits = 3)
  plotcp(cart.pruned)
  return(cart.pruned)
}

eval.cart <- function(built.model, hr.datatable.test){
  prp(built.model, type=2, nn=T, fallen.leaves=T, branch.lty=3, nn.box.col = 'light blue', min.auto.cex = 0.7, nn.cex = 0.6, split.cex = 1.1, shadow.col="grey")
  print(built.model$variable.importance)
  pred.cart <- predict(built.model, newdata = hr.datatable.test, type='class')
  table(hr.datatable.test$Attrition, pred.cart)
  
  print(confusionMatrix(data = factor(pred.cart, labels = c("No", "Yes")), reference = hr.datatable.test$Attrition))
  
  # Evaluation of the model using ROC & AUC (Area Under the Curve)
  roc <- performance(prediction(as.numeric(pred.cart), as.numeric(hr.datatable.test$Attrition)), "tpr", "fpr")
  plot(roc, colorize = T, main = "ROC Curve", ylab = "Sensitivity", xlab = "1-Specificity")
  abline(a = 0, b = 1)
  auc <- performance(prediction(as.numeric(pred.cart), as.numeric(hr.datatable.test$Attrition)), "auc")
  auc <- unlist(slot(auc, "y.values"))
  legend(0.618, 0.2, round(auc, 4), title = "AUC")
}


####################################################################################################################################
# End of Model function definitions
####################################################################################################################################

####################################################################################################################################
# Hiring Analysis
####################################################################################################################################

####################################################################################################################################
# Company Analysis
####################################################################################################################################
## SMOTE:
### Logistic Regressions:
hr.company.logreg.forward.SMOTE <- log.reg.stepwise.forward(hr.dt.train.SMOTE, hr.dt.test)
# Accuracy : 0.7047
# Sensitivity : 0.6937
# Specificity : 0.7681
# AUC : 0.8075
hr.company.logreg.backward.SMOTE <- log.reg.stepwise.backward(hr.dt.train.SMOTE, hr.dt.test)
# Accuracy : 0.7047
# Sensitivity : 0.6937
# Specificity : 0.7681
# AUC : 0.8075 (Same as forward)
hr.company.logreg.lasso.SMOTE <- log.reg.lasso(hr.dt.train.SMOTE, hr.dt.test)
# Accuracy : 0.7371
# Sensitivity : 0.7101
# Specificity : 0.7481
# AUC : 0.726
hr.company.logreg.ridge.SMOTE <- log.reg.ridge(hr.dt.train.SMOTE, hr.dt.test)
# Accuracy : 0.7414
# Sensitivity : 0.7246
# Specificity : 0.7443
# AUC : 0.7345
hr.company.logreg.enet.SMOTE <- log.reg.enet(hr.dt.train.SMOTE, hr.dt.test)
# Accuracy : 0.7414
# Sensitivity : 0.7246
# Specificity : 0.7443
# AUC : 0.7345 (Same as ridge)

### Classification and Regression Tree (CART)
hr.company.cart.max.SMOTE <- max.tree(hr.dt.train.SMOTE, hr.dt.test)
hr.company.cart.pruned.SMOTE <- pruned.tree(hr.company.cart.max.SMOTE, 0.001324)
hr.company.cart.pruned.SMOTE <- pruned.tree(hr.company.cart.pruned.SMOTE, 0.00135)
eval.cart(hr.company.cart.pruned.SMOTE, hr.dt.test)
# Accuracy : 0.6595
# Sensitivity : 0.6608
# Specificity : 0.6522
# AUC : 0.6565

### Random Forest
hr.company.random.forest.SMOTE <- rand.forest(hr.dt.train.SMOTE, hr.dt.test)
# Accuracy : 0.7629
# Sensitivity : 0.60870
# Specificity : 0.78987
# AUC : 0.6993

### Support Vector Machine
hr.company.svm.radial.SMOTE <- support.vector.classifier(hr.dt.train.SMOTE, hr.dt.test)
#           Actual
# Predicted  No Yes
#        No 325  28
#       Yes  70  41
# Accuracy : 0.7887931
# Sensitivity : 0.5942029
# Specificity : 0.8227848
# AUC : 0.7085
hr.company.svm.linear.SMOTE <- support.vector.classifier(hr.dt.train.SMOTE, hr.dt.test, Kernel = "linear")
#           Actual
# Predicted  No Yes
#        No 325  28
#       Yes  70  41
# Accuracy : 0.7521552
# Sensitivity : 0.6811594
# Specificity : 0.764557
# AUC : 0.7229
hr.company.svm.poly.SMOTE <- support.vector.classifier(hr.dt.train.SMOTE, hr.dt.test, Kernel = "polynomial")
#           Actual
# Predicted  No Yes
#        No 395  68
#       Yes   0   1
# Accuracy : 0.8534483
# Sensitivity : 0.01449275
# Specificity : 1
# AUC : 0.5072
hr.company.svm.sigmoid.SMOTE <- support.vector.classifier(hr.dt.train.SMOTE, hr.dt.test, Kernel = "sigmoid")
#           Actual
# Predicted  No Yes
#        No 312  25
#       Yes  83  44
# Accuracy : 0.7672414
# Sensitivity : 0.63768123
# Specificity : 0.7898734
# AUC : 0.7138

## ROSE - Undersampling:
### Logistic Regressions:
hr.company.logreg.forward.ROSE.under <- log.reg.stepwise.forward(hr.dt.train.under.ROSE, hr.dt.test)
# Accuracy : 0.7177
# Sensitivity : 0.7241
# Specificity : 0.6812
# AUC : 0.7862
hr.company.logreg.backward.ROSE.under <- log.reg.stepwise.backward(hr.dt.train.under.ROSE, hr.dt.test)
# Accuracy : 0.7091
# Sensitivity : 0.7089
# Specificity : 0.7101
# AUC : 0.787
hr.company.logreg.lasso.ROSE.under <- log.reg.lasso(hr.dt.train.under.ROSE, hr.dt.test)
# Accuracy : 0.7328
# Sensitivity : 0.7246
# Specificity : 0.7342
# AUC : 0.7294
hr.company.logreg.ridge.ROSE.under <- log.reg.ridge(hr.dt.train.under.ROSE, hr.dt.test)
# Accuracy : 0.7284
# Sensitivity : 0.7246
# Specificity : 0.7291
# AUC : 0.7269
hr.company.logreg.enet.ROSE.under <- log.reg.enet(hr.dt.train.under.ROSE, hr.dt.test)
# Accuracy : 0.7284
# Sensitivity : 0.7246
# Specificity : 0.7291
# AUC : 0.7269 (Same as Ridge)

### Classification and Regression Tree (CART)
hr.company.cart.max.ROSE.under <- max.tree(hr.dt.train.under.ROSE, hr.dt.test)
hr.company.cart.pruned.ROSE.under <- pruned.tree(hr.company.cart.max.ROSE.under, 0.01191)
eval.cart(hr.company.cart.pruned.ROSE.under, hr.dt.test)
# Accuracy : 0.7241
# Sensitivity : 0.7443
# Specificity : 0.6087
# AUC : 0.6765

### Random Forest
hr.company.random.forest.ROSE.under <- rand.forest(hr.dt.train.under.ROSE, hr.dt.test)
# Accuracy : 0.7349
# Sensitivity : 0.66667
# Specificity : 0.74684
# AUC : 0.7042

### Support Vector Machine
hr.company.svm.radial.ROSE.under <- support.vector.classifier(hr.dt.train.under.ROSE, hr.dt.test)
#           Actual
# Predicted  No Yes
#        No 289  21
#       Yes 106  48
# Accuracy : 0.7262931
# Sensitivity : 0.6956522
# Specificity : 0.7316456
# AUC : 0.7136
hr.company.svm.linear.ROSE.under <- support.vector.classifier(hr.dt.train.under.ROSE, hr.dt.test, Kernel = "linear")
#           Actual
# Predicted  No Yes
#        No 289  21
#       Yes 106  48
# Accuracy : 0.7262931
# Sensitivity : 0.6956522
# Specificity : 0.7316456
# AUC : 0.7136 (Same as above)
hr.company.svm.poly.ROSE.under <- support.vector.classifier(hr.dt.train.under.ROSE, hr.dt.test, Kernel = "polynomial")
#           Actual
# Predicted  No Yes
#        No  27   1
#       Yes 368  68
# Accuracy : 0.8534483
# Sensitivity : 0.9855072
# Specificity : 0.06835443
# AUC : 0.5269
hr.company.svm.sigmoid.ROSE.under <- support.vector.classifier(hr.dt.train.under.ROSE, hr.dt.test, Kernel = "sigmoid")
#           Actual
# Predicted  No Yes
#        No 271  19
#       Yes 124  50
# Accuracy : 0.6918103
# Sensitivity : 0.7246377
# Specificity : 0.6860759
# AUC : 0.7054

## ROSE - Oversampling:
### Logistic Regressions:
hr.company.logreg.forward.ROSE.over <- log.reg.stepwise.forward(hr.dt.train.over.ROSE, hr.dt.test)
# Accuracy : 0.7457
# Sensitivity : 0.7443
# Specificity : 0.7536
# AUC : 0.8174
hr.company.logreg.backward.ROSE.over <- log.reg.stepwise.backward(hr.dt.train.over.ROSE, hr.dt.test)
# Accuracy : 0.7457
# Sensitivity : 0.7443
# Specificity : 0.7536
# AUC : 0.8174 (Same as forward)
hr.company.logreg.lasso.ROSE.over <- log.reg.lasso(hr.dt.train.over.ROSE, hr.dt.test)
# Accuracy : 0.7478
# Sensitivity : 0.7681
# Specificity : 0.7443
# AUC : 0.7562
hr.company.logreg.ridge.ROSE.over <- log.reg.ridge(hr.dt.train.over.ROSE, hr.dt.test)
# Accuracy : 0.7435
# Sensitivity : 0.7536
# Specificity : 0.7418
# AUC : 0.7477
hr.company.logreg.enet.ROSE.over <- log.reg.enet(hr.dt.train.over.ROSE, hr.dt.test)
# Accuracy : 0.7478
# Sensitivity : 0.7681
# Specificity : 0.7443
# AUC : 0.7562 (Same as Lasso)

### Classification and Regression Tree (CART)
hr.company.cart.max.ROSE.over <- max.tree(hr.dt.train.over.ROSE, hr.dt.test)
hr.company.cart.pruned.ROSE.over <- pruned.tree(hr.company.cart.max.ROSE.over, 0.001195)
eval.cart(hr.company.cart.pruned.ROSE.over, hr.dt.test)
# Accuracy : 0.75
# Sensitivity : 0.8000
# Specificity : 0.4638
# AUC : 0.6319

### Random Forest
hr.company.random.forest.ROSE.over <- rand.forest(hr.dt.train.over.ROSE, hr.dt.test)
# Accuracy : 0.8513
# Sensitivity : 0.24638
# Specificity : 0.95696
# AUC : 0.6017

### Support Vector Machine
hr.company.svm.radial.ROSE.over <- support.vector.classifier(hr.dt.train.over.ROSE, hr.dt.test)
#           Actual
# Predicted  No Yes
#        No 317  25
#       Yes  78  44
# Accuracy : 0.7780172
# Sensitivity : 0.6376812
# Specificity : 0.8025316
# AUC : 0.7201
hr.company.svm.linear.ROSE.over <- support.vector.classifier(hr.dt.train.over.ROSE, hr.dt.test, Kernel = "linear")
#           Actual
# Predicted  No Yes
#        No 299  19
#       Yes  96  50
# Accuracy : 0.7521552
# Sensitivity : 0.7246377
# Specificity : 0.756962
# AUC : 0.7408
hr.company.svm.poly.ROSE.over <- support.vector.classifier(hr.dt.train.over.ROSE, hr.dt.test, Kernel = "polynomial")
#           Actual
# Predicted  No Yes
#        No 219  13
#       Yes 176  56
# Accuracy : 0.5926724
# Sensitivity : 0.8115942
# Specificity : 0.5544304
# AUC : 0.683
hr.company.svm.sigmoid.ROSE.over <- support.vector.classifier(hr.dt.train.over.ROSE, hr.dt.test, Kernel = "sigmoid")
#           Actual
# Predicted  No Yes
#        No 296  20
#       Yes  99  49
# Accuracy : 0.7435345
# Sensitivity : 0.7101449
# Specificity : 0.637931
# AUC : 0.7298

# All the data has been recorded in a file
performace.dt <- fread("ModelPerformance.csv")
head(performace.dt[order(Accuracy, decreasing = TRUE)])
#                      V1  Accuracy Sensitivity Specificity AUC
# 1:       svm.poly.SMOTE 0.8534483 0.01449275 1.00000000 0.5072
# 2:  svm.poly.ROSE.under 0.8534483 0.98550720 0.06835443 0.5269
# 3:    rforest.ROSE.over 0.8513000 0.24638000 0.95696000 0.6017
# 4:     svm.radial.SMOTE 0.7887931 0.59420290 0.82278480 0.7085
# 5: svm.radial.ROSE.over 0.7780172 0.63768120 0.80253160 0.7201
# 6:    svm.sigmoid.SMOTE 0.7672414 0.63768123 0.78987340 0.7138
head(performace.dt[order(Sensitivity, decreasing = TRUE)])
#                        V1 Accuracy Sensitivity Specificity AUC
# 1:    svm.poly.ROSE.under 0.8534483 0.9855072 0.06835443 0.5269
# 2:     svm.poly.ROSE.over 0.5926724 0.8115942 0.55443040 0.6830
# 3:         cart.ROSE.over 0.7500000 0.8000000 0.46380000 0.6319
# 4: logreg.lasso.ROSE.over 0.7478000 0.7681000 0.74430000 0.7562
# 5:  logreg.enet.ROSE.over 0.7478000 0.7681000 0.74430000 0.7562
# 6: logreg.ridge.ROSE.over 0.7435000 0.7536000 0.74180000 0.7477
head(performace.dt[order(Specificity, decreasing = TRUE)])
#                      V1 Accuracy Sensitivity Specificity AUC
# 1:       svm.poly.SMOTE 0.8534483 0.01449275 1.0000000 0.5072
# 2:    rforest.ROSE.over 0.8513000 0.24638000 0.9569600 0.6017
# 3:     svm.radial.SMOTE 0.7887931 0.59420290 0.8227848 0.7085
# 4: svm.radial.ROSE.over 0.7780172 0.63768120 0.8025316 0.7201
# 5:    svm.sigmoid.SMOTE 0.7672414 0.63768123 0.7898734 0.7138
# 6:        rforest.SMOTE 0.7629000 0.60870000 0.7898700 0.6993
head(performace.dt[order(AUC, decreasing = TRUE)])
#                   V1 Accuracy Sensitivity Specificity AUC
# 1:   logreg.forward.ROSE.over 0.7457 0.7443 0.7536 0.8174
# 2:  logreg.backward.ROSE.over 0.7457 0.7443 0.7536 0.8174
# 3:       logreg.forward.SMOTE 0.7047 0.6937 0.7681 0.8075
# 4:      logreg.backward.SMOTE 0.7047 0.6937 0.7681 0.8075
# 5: logreg.backward.ROSE.under 0.7091 0.7089 0.7101 0.7870
# 6:  logreg.forward.ROSE.under 0.7177 0.7241 0.6812 0.7862

## In conclusion:
# Overall, oversampling with ROSE(Randomly Over Sampling Examples) tend to give us better result.
# Modeling with SVM give the highest accuracy.
# To predict Attrition( == "Yes"), oversampling with ROSE should be used and SVM can give the best result.
# However, this can come at the cost of very poor specificty (i.e. Attrition == "No")
# On the other hand, to predict Attrition ( == "No"), oversampling with SMOTE should be used, both SVM and RandomForest yield satisfactory results.
# However, in terms of AUC, stepwise logistic regression yield the best result.
# AUC of a classifier is equal to the probability that the classifier will rank a randomly chosen positive example higher than a randomly chosen negative example

####################################################################################################################################
# End Of Company Analysis
####################################################################################################################################


####################################################################################################################################
# Departmental Analysis
####################################################################################################################################

# From the above analysis, we choose to oversample our departmental sub-datast using both ROSE (to predict Attrition (== "Yes")) and SMOTE (to predict Attrition (== "No"))
# SVM is used as it tends to give the best sensitivity and specificity
# For the following analysis, we are going to use Hyper Parameter Optimization technique (aka. tuning) to find the best SVM model.

# We split the data into 3 departments
hr.dt.sales <- hr.dt[Department == "Sales"]
hr.dt.rd <- hr.dt[Department == "Research & Development"]
hr.dt.hr <- hr.dt[Department == "Human Resources"]

# Split the Sales department data
set.seed(11112018)
ind <- sample(2, nrow(hr.dt.sales), replace = T, prob = c(0.7, 0.3))
hr.dt.sales.train <- hr.dt.sales[ind == 1,]
hr.dt.sales.test <- hr.dt.sales[ind == 2,]
# SMOTE Sales department train data
hr.dt.sales.train.SMOTE <- SMOTE(Attrition ~ ., hr.dt.sales.train)
# Oversample Sales department train data with ROSE
hr.dt.sales.train.over.ROSE <- ovun.sample(Attrition ~ ., data = hr.dt.sales.train, method = "over", N = 474)$data

# Split the R&D department data
set.seed(11112018)
ind <- sample(2, nrow(hr.dt.rd), replace = T, prob = c(0.7, 0.3))
hr.dt.rd.train <- hr.dt.rd[ind == 1,]
hr.dt.rd.test <- hr.dt.rd[ind == 2,]
# SMOTE R&D department train data
hr.dt.rd.train.SMOTE <- SMOTE(Attrition ~ ., hr.dt.rd.train)
# Oversample R&D department train data with ROSE
hr.dt.rd.train.over.ROSE <- ovun.sample(Attrition ~ ., data = hr.dt.rd.train, method = "over", N = 1110)$data

# Split the HR department data
set.seed(11112018)
ind <- sample(2, nrow(hr.dt.hr), replace = T, prob = c(0.7, 0.3))
hr.dt.hr.train <- hr.dt.hr[ind == 1,]
hr.dt.hr.test <- hr.dt.hr[ind == 2,]
# SMOTE HR department train data
hr.dt.hr.train.SMOTE <- SMOTE(Attrition ~ ., hr.dt.hr.train)
# Oversample HR department train data with ROSE
hr.dt.hr.train.over.ROSE <- ovun.sample(Attrition ~ ., data = hr.dt.hr.train, method = "over", N = 66)$data

# Modelling:

# Sales:
hr.department.sales.svm.SMOTE <- best.svm(hr.dt.sales.train.SMOTE, hr.dt.sales.test) # Out put a graph to show best cost and epsilon
#           Actual
# Predicted  No Yes
#        No  96  10
#       Yes  21  22
# Accuracy : 0.7919463
# Sensitivity : 0.6875
# Specificity : 0.8205128
# AUC : 0.754
hr.department.sales.over.ROSE <- best.svm(hr.dt.sales.train.over.ROSE, hr.dt.sales.test)
#           Actual
# Predicted  No Yes
#        No 106  15
#       Yes  11  17
# Accuracy : 0.8255034
# Sensitivity : 0.53125
# Specificity : 0.9059829
# AUC : 0.7186

# Research & Development
hr.department.rd.svm.SMOTE <- best.svm(hr.dt.rd.train.SMOTE, hr.dt.rd.test) # Out put a graph to show best cost and epsilon
#           Actual
# Predicted  No Yes
#        No 236  26
#       Yes  37  21
# Accuracy : 0.803125
# Sensitivity : 0.4468085
# Specificity : 0.8644689
# AUC : 0.6556
hr.department.rd.over.ROSE <- best.svm(hr.dt.rd.train.over.ROSE, hr.dt.rd.test)
#           Actual
# Predicted  No Yes
#        No 106  15
#       Yes  11  17
# Accuracy : 0.853125
# Sensitivity : 0.3404255
# Specificity : 0.9413919
# AUC : 0.6409

# Human Resources:
hr.department.hr.svm.SMOTE <- best.svm(hr.dt.hr.train.SMOTE, hr.dt.hr.test) # Out put a graph to show best cost and epsilon
#           Actual
# Predicted  No Yes
#        No  96  10
#       Yes  21  22
# Accuracy : 0.7916667
# Sensitivity : 0.5
# Specificity : 0.8888889
# AUC : 0.6944
hr.department.hr.over.ROSE <- best.svm(hr.dt.hr.train.over.ROSE, hr.dt.hr.test)
#           Actual
# Predicted  No Yes
#        No 106  15
#       Yes  11  17
# Accuracy : 0.7916667
# Sensitivity : 0.1666667
# Specificity : 1
# AUC : 0.5833

### Conclusion
# Overall model accuracy is good for both Sales and R & D department
# HR cannot predicted with very good accuracy primarily due to a large number of variables in comparison to sample size
# This means increasing sample size can probably help
# Also, all models are pretty bad at predicting Attrition(== "Yes"), there is quite a significant imblance even after oversampling with SMOTE and ROSE
# Implication: Companys should probably develop other models, or use the model for the whole company