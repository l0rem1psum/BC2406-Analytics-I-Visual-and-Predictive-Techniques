remove(list = ls())
install.packages(c("data.table", "caTools", "car"))
library(data.table)
library(caTools)
library(car)

lawsuit.dt <- fread("Lawsuit.csv")
lawsuit.dt$Dept <- factor(lawsuit.dt$Dept)
lawsuit.dt$Gender <- factor(lawsuit.dt$Gender)
lawsuit.dt$Clin <- factor(lawsuit.dt$Clin)
lawsuit.dt$Cert <- factor(lawsuit.dt$Cert)
lawsuit.dt$Rank <- factor(lawsuit.dt$Rank)

set.seed(2018)
train <- sample.split(Y = lawsuit.dt$Sal94, SplitRatio = 0.7)
lawsuit.train.dt <- subset(lawsuit.dt, train == T)
lawsuit.test.dt <- subset(lawsuit.dt, train == F)
summary(lawsuit.train.dt)
summary(lawsuit.test.dt) # To check whether the distribution is similar in trainset vs testset

# Split the data by Gender
lawsuit.train.male.dt <- lawsuit.train.dt[Gender == 1]
lawsuit.train.male.dt[,Gender:=NULL] # Remove Gender column as it will cause error (only contains 1 factor)
lawsuit.train.female.dt <- lawsuit.train.dt[Gender == 0,]
lawsuit.train.female.dt[, Gender:=NULL]


# Full model that includes both male and female
model.full <- lm(Sal94 ~ . -Sal95 -ID, data = lawsuit.train.dt)
model.null <- lm(Sal94 ~ 1, data = lawsuit.train.dt)
model94.forward <- step(model.null, scope = formula(model.full), direction = "forward") 
# Sal94 ~ Dept + Exper + Prate + Rank + Cert + Clin, AIC=3695.3
model94.backward <- step(model.full, direction = "backward") 
# Sal94 ~ Dept + Clin + Cert + Exper + Rank, AIC = 3694.11

# Differ by a single variable (i.e. Prate)
# So, need to test whether the presence of variable Prate helps the model to be better
model.prate <- lm(Prate ~ Dept + Clin + Cert + Exper + Rank, data = lawsuit.train.dt)
summary(model.prate)
###########################Result###########################
# Residual standard error: 0.4834 on 171 degrees of freedom
# Multiple R-squared:  0.944,	Adjusted R-squared:  0.9408 
# F-statistic: 288.4 on 10 and 171 DF,  p-value: < 2.2e-16
############################################################

vif(model94.forward)
###############Result###############
# GVIF Df GVIF^(1/(2*Df))
# Dept   9.921464  5        1.257933
# Exper  1.874060  1        1.368963
# Prate 17.867818  1        4.227034
# Rank   1.920256  2        1.177172
# Cert   1.414978  1        1.189528
# Clin   6.108601  1        2.471558
####################################
vif(model94.backward)
###############Result###############
# GVIF Df GVIF^(1/(2*Df))
# Dept  2.085407  5        1.076265
# Clin  1.655586  1        1.286696
# Cert  1.414947  1        1.189515
# Exper 1.830235  1        1.352862
# Rank  1.872368  2        1.169763
####################################

###############Conclusion###############
# Gvif of Prate > 4 (i.e. 4.227), so Prate discarded from model as it contributes to the collinearity problem of the model.
# Also, the collinearity problem of Prate can be seen from the high muliple r-squred value of model.prate (i.e. 0.944). Meaning 94.4% of the Prate value can be explained by other vairables (i.e. Dept, Clin, Cert, Exper, Rank). So the solution is to discard it.
# Also, addition of Prate to the model does not improve the model significantly as can be seen from the AIC of both models. (3695.3 to 3694.11)
########################################

RSME.train <- sqrt(mean(residuals(model94.backward) ^ 2)) # 24057.48
summary(abs(residuals(model94.backward)))
#######################Result#######################
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 322.6   5036.4  11988.5  17143.7  21931.5 113592.8
####################################################
model94.predicted <- predict(model94.backward, newdata = lawsuit.test.dt)
testset.error <- lawsuit.test.dt$Sal94 - model94.predicted
RSME.test <- sqrt(mean(testset.error ^ 2)) # 27271.18
summary(abs(testset.error))
#######################Result#######################
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 359.4   9657.9  15664.8  19223.0  22771.8 147117.9
####################################################

###############Conclusion###############
# The RSME of both the train set and test do not differ by too much, showing our model is not bad.
# Also the distribution of errors are pretty similar, again showing a valid model.
########################################


# Seperate models for male and female
### Male:
model94.male.full <- lm(Sal94 ~ . -Sal95 -ID, data = lawsuit.train.male.dt)
model94.male.null <- lm(Sal94 ~ 1, data = lawsuit.train.male.dt)
model94.male.forward <- step(model94.male.null, scope = formula(model94.male.full), direction = "forward") # Sal94 ~ Dept + Exper + Cert + Rank + Clin
model94.male.backward <- step(model94.male.full, direction = "backward") # Sal94 ~ Dept + Exper + Cert + Rank + Clin (same as forward)
summary(model94.male.forward)
summary(model94.male.backward)

##########Backward summary() result:##########
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)     9036       9632   0.938 0.350420    
#   Dept2         -17761       9464  -1.877 0.063503 .  
#   Dept3          29064      14973   1.941 0.055086 .  
#   Dept4          25802      12631   2.043 0.043744 *  
#   Dept5          77453       9988   7.755 8.01e-12 ***
#   Dept6         181214      10624  17.057  < 2e-16 ***
#   Exper           3174        540   5.878 5.62e-08 ***
#   Cert1          23554       7515   3.134 0.002266 ** 
#   Rank2          25507       8064   3.163 0.002075 ** 
#   Rank3          43491       8448   5.148 1.34e-06 ***
#   Clin1          29077       8435   3.447 0.000833 ***
##############################################

### Female:

model94.female.full <- lm(Sal94 ~ . -Sal95 -ID, data = lawsuit.train.female.dt)
model94.female.null <- lm(Sal94 ~ 1, data = lawsuit.train.female.dt)
model94.female.forward <- step(model94.female.null, scope = formula(model94.female.full), direction = "forward") # Sal94 ~ Dept + Rank + Clin + Cert + Exper
model94.female.backward <- step(model94.female.full, direction = "backward") # Sal94 ~ Dept + Rank + Clin + Cert + Exper (same as forward)
summary(model94.female.forward)
summary(model94.female.backward)

##########Backward summary() result:##########
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  41618.2     7183.8   5.793 2.59e-07 ***
#   Dept2       -17871.2     6845.2  -2.611 0.011353 *  
#   Dept3        13441.7     7790.4   1.725 0.089515 .  
#   Dept4        13065.3     8482.7   1.540 0.128677    
#   Dept5        63251.5     7530.7   8.399 9.07e-12 ***
#   Dept6       123779.4    12710.8   9.738 4.87e-14 ***
#   Clin1        26030.1     5026.4   5.179 2.65e-06 ***
#   Cert1        20238.5     5844.0   3.463 0.000982 ***
#   Exper         2108.1      691.8   3.047 0.003409 ** 
#   Rank2        19000.9     6260.9   3.035 0.003535 ** 
#   Rank3        30898.8     8591.8   3.596 0.000647 ***
##############################################

# Creating a prediction dataframe
prediction.datatable = as.data.table(expand.grid(Dept = unique(lawsuit.dt$Dept), Clin = unique(lawsuit.dt$Clin), Cert = unique(lawsuit.dt$Cert), Rank = unique(lawsuit.dt$Rank)))
prediction.male.dataframe = expand.grid(Dept = unique(lawsuit.dt$Dept), Clin = unique(lawsuit.dt$Clin), Cert = unique(lawsuit.dt$Cert), Rank = unique(lawsuit.dt$Rank), Exper = 1, KEEP.OUT.ATTRS = TRUE, stringsAsFactors = TRUE)
prediction.female.dataframe = expand.grid(Dept = unique(lawsuit.dt$Dept), Clin = unique(lawsuit.dt$Clin), Cert = unique(lawsuit.dt$Cert), Rank = unique(lawsuit.dt$Rank), Exper = 1, KEEP.OUT.ATTRS = TRUE, stringsAsFactors = TRUE)

# Predicting both male and female models using prediction dataframe.
sal94.male.expected <- predict(model94.male.backward, newdata = prediction.male.dataframe)
sal94.female.expected <- predict(model94.female.backward, newdata = prediction.female.dataframe)

# Combine the prediction result to the same dataframe
prediction.datatable[,Male.predicted := sal94.male.expected]
prediction.datatable[,Female.predicted := sal94.female.expected]

remove(sal94.male.expected, sal94.female.expected) # Remove temporary variables which are no longer needed.

print(prediction.datatable[Female.predicted > Male.predicted, .N]) # There are 56 cases (out of 72) where predicted female salary is higher than their male conterparts.

###############Conclusion###############
# Majority of cases are where female salary is higher than male counterparts.
# This is using the model built for both genders seperately, while keeping all significant vairables constant.
########################################


####################Relationship between Sal94 and Sal95####################
model.sal94.sal95 <- lm(Sal95 ~ Sal94, data = lawsuit.dt)
summary(model.sal94.sal95)

###########################Result###########################
# Residual standard error: 3432 on 259 degrees of freedom
# Multiple R-squared:  0.9985,	Adjusted R-squared:  0.9985 
# F-statistic: 1.737e+05 on 1 and 259 DF,  p-value: < 2.2e-16
############################################################

###############Conclusion###############
# Sal95 and Sal94 are highlt correlate with a multiple R-square value of 0.9985
# So Sal95 are not tested again for brevity.
########################################