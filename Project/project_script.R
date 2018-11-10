library("data.table")
library("ggplot2")
library("gridExtra")
library("corrplot")
library("caret")
library("e1071")
# library("ROSE") # Randomly Over Sampling Examples # Not preferred
library("DMwR")
library("randomForest")
library("ROCR")
setwd("D:/OneDrive/OneDrive - Nanyang Technological University/Course Materials/Year 2 Semester 1/BC2406 Analytics I - Visual & Predictive Techniques/Project")
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
sum(is.na(hr.dt))

# Reomve EmployeeCount column since all the values are the same (i.e. 1)
hr.dt$EmployeeCount <- NULL
# Remove EmployeeNumber column since it is just an index, hence not relevant for building model
hr.dt$EmployeeNumber <- NULL
# Remove Over18 column since everyone is above 18
hr.dt$Over18 <- NULL
# Remove StandardHours column since everyone has to work 80 hours
hr.dt$StandardHours <- NULL

summary(hr.dt)

# Explorartory Data Analysis
# Univariate Analysis
# Attrition - DailyRate. Higher daily rate lower attrition rate
p1 <- ggplot(hr.dt, aes(x= Age, fill = Attrition)) + geom_density(alpha = 0.4)
p2 <- ggplot(hr.dt, aes(x= DailyRate, fill = Attrition)) + geom_density(alpha = 0.4)
p3 <- ggplot(hr.dt, aes(x= HourlyRate, fill = Attrition)) + geom_density(alpha = 0.4)
p4 <- ggplot(hr.dt, aes(x= MonthlyRate, fill = Attrition)) + geom_density(alpha = 0.4)
p5 <- ggplot(hr.dt, aes(x= MonthlyIncome, fill = Attrition)) + geom_density(alpha = 0.4)

p6 <- ggplot(hr.dt, aes(x= DistanceFromHome, fill = Attrition)) + geom_density(alpha = 0.4) # Travel long distance to work means more likely go for attrition
p7 <- ggplot(hr.dt, aes(x= NumCompaniesWorked, fill = Attrition)) + geom_density(alpha = 0.4)
p8 <- ggplot(hr.dt, aes(x= PercentSalaryHike, fill = Attrition)) + geom_density(alpha = 0.4)
p9 <- ggplot(hr.dt, aes(x= TotalWorkingYears, fill = Attrition)) + geom_density(alpha = 0.4)

p10 <- ggplot(hr.dt, aes(x= TrainingTimesLastYear, fill = Attrition)) + geom_density(alpha = 0.4) # Correlation not obvious
p11 <- ggplot(hr.dt, aes(x= YearsAtCompany, fill = Attrition)) + geom_density(alpha = 0.4)
p12 <- ggplot(hr.dt, aes(x= YearsInCurrentRole, fill = Attrition)) + geom_density(alpha = 0.4)
p13 <- ggplot(hr.dt, aes(x= YearsSinceLastPromotion, fill = Attrition)) + geom_density(alpha = 0.4) # Correlation not obvious

p14 <- ggplot(hr.dt, aes(x= YearsWithCurrManager, fill = Attrition)) + geom_density(alpha = 0.4)

combined.plot1 <- grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, ncol = 5)
plot(combined.plot1)

p15 <- ggplot(hr.dt, aes(x = BusinessTravel, fill = Attrition)) + geom_bar(position = 'fill', alpha = 0.4) # Those who travel frequently are more likely to leave
p16 <- ggplot(hr.dt, aes(x = Department, fill = Attrition)) + geom_bar(position = 'fill', alpha = 0.4) # Little correlation, a weak predictor
p17 <- ggplot(hr.dt, aes(x = Education, fill = Attrition)) + geom_bar(position = 'fill', alpha = 0.4) # Higher Education, less likely to leave
p18 <- ggplot(hr.dt, aes(x = EducationField, fill = Attrition)) + geom_bar(position = 'fill', alpha = 0.4) # Little correlation, a weak predictor
p19 <- ggplot(hr.dt, aes(x = EnvironmentSatisfaction, fill = Attrition)) + geom_bar(position = 'fill', alpha = 0.4) # Higher Satisfaction, less likely to leave
p20 <- ggplot(hr.dt, aes(x = Gender, fill = Attrition)) + geom_bar(position = 'fill', alpha = 0.4) # Males are more likely to leave. However, difference is small, so relatively weak predictor
p21 <- ggplot(hr.dt, aes(x = JobInvolvement, fill = Attrition)) + geom_bar(position = 'fill', alpha = 0.4) # The more involved an employee is, the less likely to leaves
p22 <- ggplot(hr.dt, aes(x = JobLevel, fill = Attrition)) + geom_bar(position = 'fill', alpha = 0.4) # Higher job leve, less likely to leave
p23 <- ggplot(hr.dt, aes(x = JobRole, fill = Attrition)) + geom_bar(position = 'fill', alpha = 0.4)
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

# In addition, we discovered that there is a very strong correlation between JobLevel, Age, MonthlyIncome, TotalWorkingYears
# This is done by performing a one-way ANOVA F-test:
summary(aov(hr.dt$Age ~ hr.dt$JobLevel))
summary(aov(hr.dt$MonthlyIncome ~ hr.dt$JobLevel))
summary(aov(hr.dt$TotalWorkingYears ~ hr.dt$JobLevel))
# This makes logical sense since normally people with higher age will occupy a higher position/higher job role.
# Also, higher job level are oftentimes associated with higher monthly income.
# Also, total working years are sometimes needed to attain a higher job levl.

# Also, there exists a strong correlation between PerformanceRating and PercentSalaryHike
summary(aov(hr.dt$PercentSalaryHike ~ hr.dt$PerformanceRating))
# Since p-value is 2e-16, way less than 0.05, so the null hypothesis of independence is rejected.
# This also makes logical sense as salary hike normally depends the performance of individual

# In conclusion, we are going to remove Department, EducationField, Gender, PerformanceRating, TrainingTimeLastYear, YearsSinceLastPromotion, JobLevel, PercentSalaryHike from our consideration.
new.hr.dt <- hr.dt[, - c(5, 8, 10, 13, 21, 22, 26, 30)]

ggplot(new.hr.dt, aes(x = Attrition)) + geom_bar(fill = "steelblue")
summary(new.hr.dt$Attrition)
# We observe that there is a severe class imbalance problem in our response variable 'Attrition' (i.e. 1233 cases of 'No', 237 cases of 'Yes')
# So we use oversampling to solve the problem.
set.seed(11112018)
ind <- sample(2, nrow(new.hr.dt), replace = T, prob = c(0.7, 0.3))
hr.dt.train <- new.hr.dt[ind == 1,]
hr.dt.test <- new.hr.dt[ind == 2,]

summary(hr.dt.train$Attrition)
hr.dt.train.SMOTE <- SMOTE(Attrition ~ ., hr.dt.train)
summary(hr.dt.train.SMOTE$Attrition)

####################################################################################################################################
# Logistic Regression
####################################################################################################################################
# Stepwise Logistic Regression
logreg.null <- glm(Attrition ~ 1, family = binomial, data = hr.dt.train.SMOTE)
logreg.full <- glm(Attrition ~ ., family = binomial, data = hr.dt.train.SMOTE)
logreg.forward <- step(logreg.null, scope = formula(logreg.full), direction = 'forward')
logreg.backward <- step(logreg.full, direction = 'backward')
summary(logreg.forward)
summary(logreg.backward)
# Both forward and backward stepwise logistic regression model give the same result
# Select a threshold:
threshold <- sum(hr.dt.train.SMOTE$Attrition == 'Yes') / length(hr.dt.train.SMOTE$Attrition)
pred <- predict(logreg.forward, type = 'response', newdata = hr.dt.test)
pred.hat <- ifelse(pred > threshold, 2, 1)
confusionMatrix(data = factor(pred.hat, labels = c("No", "Yes")), reference = hr.dt.test$Attrition)
# Accuracy:0.7882

# Evaluation of the model using ROC & AUC (Area Under the Curve)
roc <- performance(prediction(as.numeric(pred), as.numeric(hr.dt.test$Attrition)), "tpr", "fpr")
plot(roc, colorize = T, main = "ROC Curve", ylab = "Sensitivity", xlab = "1-Specificity")
abline(a = 0, b = 1)
auc <- performance(prediction(as.numeric(pred), as.numeric(hr.dt.test$Attrition)), "auc")
auc <- unlist(slot(auc, "y.values"))
legend(0.618, 0.2, round(auc, 4), title = "AUC")

# The AUC is 76.9%


# Logistic Regression with Regularization
# Set custom control parameter first (i.e. 10-fold Cross Validation):
custom.control.param <- trainControl(method = "repeatedcv",
                                     number = 10,
                                     repeats = 5,
                                     verboseIter = T)

# Ridge Regression (L2 Parameterized Logistic Regression)
# Ridge Regression tries to shrink the coefficients, and keeps ALL variables in the model
set.seed(11112018)
ridge <- train(Attrition ~ .,
               hr.dt.train.SMOTE,
               family = "binomial",
               method = "glmnet",
               tuneGrid = expand.grid(alpha = 0, lambda = seq(0.0001, 1, length = 10)),
               trControl = custom.control.param) # Alpha is zero for ridge regression
plot(ridge) # Accuray highest when Regularization Parameter is 0.0001
plot(ridge$finalModel, xvar = "lambda", label = T)
plot(ridge$finalModel, xvar = "dev", label = T) # Percentage of variability explained at a point
plot(varImp(ridge, scale = T)) # Being sales representative or Manager tend to have high attrition

# Lasso Regression (L1 Parameterized Logistic Regression) - Least Absolute Shrinkage & Selection Operator
# Lasso Regression does both shrinkage of coefficents and feature selection.
# If there is a group of highly correlated variables that cause multicolinearity, Lasso Regression will select one variable from the group while ignoring the rest
set.seed(11112018)
lasso <- train(Attrition ~ .,
               hr.dt.train.SMOTE,
               family = "binomial",
               method = "glmnet",
               tuneGrid = expand.grid(alpha = 1, lambda = seq(0.0001, 1, length = 10)),
               trControl = custom.control.param)
plot(lasso) # Accuray highest when Regularization Parameter is 0.0001
plot(lasso$finalModel, xvar = "lambda", label = T)
plot(lasso$finalModel, xvar = "dev", label = T)
plot(varImp(lasso, scale = T))

# Elastic Net Regression (Linear Combination of Lasso & Ridge Regression)
set.seed(11112018)
enet <- train(Attrition ~ .,
              hr.dt.train.SMOTE,
              family = "binomial",
              method = "glmnet",
              tuneGrid = expand.grid(alpha = seq(0, 1, length = 10), lambda = seq(0.0001, 1, length = 10)),
              trControl = custom.control.param)
# We get alpha = 1, lambda = 1e-04, which means this is the same as a Lasso Regression Model
plot(enet)
plot(enet$finalModel, xvar = "lambda", label = T)
plot(enet$finalModel, xvar = "dev", label = T)
plot(varImp(enet, scale = T)) # Being Sales Representative/Human Resource more likely to leave

#### Comparing Models ####
model.list <- list(Ridge = ridge, Lasso = lasso, ElasticNet = enet)
res <- resamples(model.list)
summary(res)

# Accuracy
#                 Min.  1 st Qu.    Median      Mean  3 rd Qu.      Max. NA's
# Ridge      0.7142857 0.7881356 0.8050847 0.8018745 0.8216536 0.8813559    0
# Lasso      0.7288136 0.7831684 0.8127626 0.8054211 0.8305085 0.8813559    0
# ElasticNet 0.7288136 0.7831684 0.8093443 0.8054211 0.8305085 0.8813559    0

xyplot(res, metric = 'Accuracy') # Clearly shows that Lasso Regression model is better

coef(lasso$finalModel, s = lasso$bestTune$lambda) # Shows the coefficients

# Prediction using Lasso Regression Model
confusionMatrix(predict(lasso, hr.dt.test), hr.dt.test$Attrition, positive = "Yes")
# Accuracy : 0.8223

# Evaluation of the model using ROC & AUC (Area Under the Curve)
roc <- performance(prediction(as.numeric(predict(lasso, hr.dt.test)), as.numeric(hr.dt.test$Attrition)), "tpr", "fpr")
plot(roc, colorize = T, main = "ROC Curve", ylab = "Sensitivity", xlab = "1-Specificity")
abline(a = 0, b = 1)
auc <- performance(prediction(as.numeric(predict(lasso, hr.dt.test)), as.numeric(hr.dt.test$Attrition)), "auc")
auc <- unlist(slot(auc, "y.values"))
legend(0.618, 0.2, round(auc, 4), title = "AUC")

# The AUC is 71.5%

####################################################################################################################################
# Random Forest
####################################################################################################################################
set.seed(11112018)
rforest <- randomForest(Attrition ~ ., data = hr.dt.train.SMOTE)
confusionMatrix(predict(rforest, hr.dt.test), hr.dt.test$Attrition, positive = "Yes")
# Accuracy : 0.8292

# Evaluation of the model using ROC & AUC (Area Under the Curve)
roc <- performance(prediction(as.numeric(predict(rforest, hr.dt.test)), as.numeric(hr.dt.test$Attrition)), "tpr", "fpr")
plot(roc, colorize = T, main = "ROC Curve", ylab = "Sensitivity", xlab = "1-Specificity")
abline(a = 0, b = 1)
auc <- performance(prediction(as.numeric(predict(rforest, hr.dt.test)), as.numeric(hr.dt.test$Attrition)), "auc")
auc <- unlist(slot(auc, "y.values"))
legend(0.618, 0.2, round(auc, 4), title = "AUC")

# The AUC is 67.7%
#           Actual
# Predicted  No Yes
#        No 333  37
#       Yes  38  31



####################################################################################################################################
# Support Vector Machine Classification
####################################################################################################################################
supportvm1 <- svm(Attrition ~ ., data = hr.dt.train.SMOTE) # Using radial-based kernel
summary(supportvm1)
pred.svm1 <- predict(supportvm1, hr.dt.test)
cm.svm1 <- table(Predicted = pred.svm1, Actual = hr.dt.test$Attrition)
sum(diag(cm.svm1)) / sum(cm.svm1) # To find the accuracy
cm.svm1
# Accuracy : 0.8223
#           Actual
# Predicted  No Yes
#        No 331  38
#       Yes  40  30

supportvm2 <- svm(Attrition ~ ., data = hr.dt.train.SMOTE, kernel = "linear") # Using linear-based kernel
summary(supportvm2)
pred.svm2 <- predict(supportvm2, hr.dt.test)
cm.svm2 <- table(Predicted = pred.svm2, Actual = hr.dt.test$Attrition)
sum(diag(cm.svm2)) / sum(cm.svm2) # To find the accuracy
cm.svm2
# Accuracy : 0.8269
#           Actual
# Predicted  No Yes
#        No 327  32
#       Yes  44  36

supportvm3 <- svm(Attrition ~ ., data = hr.dt.train.SMOTE, kernel = "polynomial") # Using polynomial-based kernel
summary(supportvm3)
pred.svm3 <- predict(supportvm3, hr.dt.test)
cm.svm3 <- table(Predicted = pred.svm3, Actual = hr.dt.test$Attrition)
sum(diag(cm.svm3)) / sum(cm.svm3) # To find the accuracy
cm.svm3
# Accuracy : 0.8633
#           Actual
# Predicted  No Yes
#        No 370  59
#       Yes   1   9

# Hyper Parameter Optimization / Tuning
set.seed(11112018)
svm.tuned <- tune(svm, Attrition ~ ., data = hr.dt.train.SMOTE, ranges = list(kernel = c("polynomial"), epsilon = seq(0, 1, 0.1), cost = 2 ^ (1 : 5)))
# Cost captures contraint violation. If cost is too high -> overfitting, otherwise underfitting
plot(svm.tuned)
# Dark Region means lower misclassification error
summary(svm.tuned)
# The best parameter is:
# epsilon cost
#       0   32
svm.best <- svm.tuned$best.model
summary(svm.best)
pred.svm.best <- predict(svm.best, hr.dt.test)
cm.svm.best <- table(Predicted = pred.svm.best, Actual = hr.dt.test$Attrition)
sum(diag(cm.svm.best)) / sum(cm.svm.best) # To find the accuracy
# Accuracy : 0.7517
#           Actual
# Predicted  No Yes
#        No 301  39
#       Yes  70  29

# In conclusion, we choose supportvm2
# Evaluation of the model using ROC & AUC (Area Under the Curve)
roc <- performance(prediction(as.numeric(pred.svm2), as.numeric(hr.dt.test$Attrition)), "tpr", "fpr")
plot(roc, colorize = T, main = "ROC Curve", ylab = "Sensitivity", xlab = "1-Specificity")
abline(a = 0, b = 1)
auc <- performance(prediction(as.numeric(pred.svm2), as.numeric(hr.dt.test$Attrition)), "auc")
auc <- unlist(slot(auc, "y.values"))
legend(0.618, 0.2, round(auc, 4), title = "AUC")

# The AUC is 70.4%