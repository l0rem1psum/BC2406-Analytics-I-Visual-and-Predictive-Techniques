library(data.table)
library(rpart)
library(rpart.plot)

cust.dt <- fread("health_ins_cust.csv")

## Data Preparing
cust.dt$sex <- factor(cust.dt$sex)
cust.dt$marital.stat <- factor(cust.dt$marital.stat)
cust.dt$housing.type <- factor(cust.dt$housing.type)
cust.dt$state.of.res <- factor(cust.dt$state.of.res)


####################################################################################################################################################


## Data Cleaning 
# Create a new column named employment, and record NA values as missing. If not, removing those rows will significanly reduce the sample size since there are 328 missing values in the is.employed column.
cust.dt[is.na(is.employed), employment:='missing']
cust.dt[is.employed == T, employment:='employed']
cust.dt[is.employed == F, employment:='unemployed']
cust.dt$employment <- factor(cust.dt$employment, levels = c('unemployed', 'employed', 'missing'))
summary(cust.dt$employment)
# unemployed   employed    missing 
# 73           599         328

# Examine negative values in income column.
cust.dt[income < 0, .N] # There is one case of nagative income. Possibility of incorrect recording of data.
cust.dt[income < 0] # The negative income is -8700. It is possible that there is a typo and the correct income is 8700. We shall examine the distribution.
# custid sex is.employed income marital.stat health.ins                 housing.type recent.move num.vehicles age state.of.res employment
# 971703   M       FALSE  -8700      Married       TRUE Homeowner with mortgage/loan       FALSE            3  60     Oklahoma unemployed
percentile <- ecdf(cust.dt[is.employed == F & marital.stat == 'Married' & age > 50]$income) # Examine the distribution of income for the particular group that is characteristic of customer with custid 971703.
percentile(8700) # We want to see the percentile of income 8700 is at in this group of people.
# [1] 0.2631579
# Meaning an income of 8700 situates at 26th percentile of this group of people.
# Thus it is likely that the actual value of -8700 is just 8700. So we replace -8700 with 8700
cust.dt[income == -8700, income:= 8700]

# Examine zero values in income column
cust.dt[income == 0, .N]
# [1] 78
# There are 78 cases of zero income. And our decision to clean it is by letting those with zero income and whose employment status is missing be NA. These are the group of peole who may have retired already or who have income but do not want to disclose their employment status.
cust.dt[,income.fix := income] # Create a new column to store fixed income data.
cust.dt[income == 0 & employment == 'missing', income.fix := NA]

# Examine ages
cust.dt[age <= 0] # There are 3 people with recorded age of zero. However, all of them have either married or divorced. Hence it is not likely that their actual age is 0. So we will remove these anomalies.
#     custid sex is.employed income       marital.stat health.ins                 housing.type recent.move num.vehicles age state.of.res employment income.fix
# 1:  931449   M          NA  42400            Married       TRUE     Homeowner free and clear       FALSE            2   0     Missouri    missing      42400
# 2: 1229417   F        TRUE  48000            Married       TRUE Homeowner with mortgage/loan       FALSE            3   0   Washington   employed      48000
# 3: 1375846   F        TRUE 113950 Divorced/Separated       TRUE                       Rented       FALSE            2   0    Louisiana   employed     113950
# Examine the distribution of ages
plot(density(cust.dt$age))
# There are people with age of 148+. This is highly unlikely. Also, there are a few whose age is above 100. We suspect that these is a recording error as the distribution of ages is supposed to follow a normal distribution. However, there is a slight skew/anomaly in the density plot after about age 75. We cannot determine conclusively whether people above 75 all have their data recorded wrongly, so we will proceed on only removing those with age above 100.
cust.dt[, age.fix := age] 
cust.dt[age <= 0 | age >= 100, age.fix := NA]

# Remove unused columns
cust.dt[, is.employed := NULL]
cust.dt[, income := NULL]
cust.dt[, age := NULL]

# Remove all rows with NA values to get the cleaned data. If not, there will be error from logistic regression model.
cust.dt <- na.omit(cust.dt) 


####################################################################################################################################################


## Building Logistic Regression model
lg.m.full <- glm(health.ins ~ . -custid, data = cust.dt, family = binomial)
lg.m.null <- glm(health.ins ~ 1, data = cust.dt, family = binomial)

cust.lg.m.b = step(lg.m.full) # Stepwise backward model.
# health.ins ~ sex + marital.stat + employment + income.fix + age.fix
cust.lg.m.f = step(lg.m.null, scope = formula(lg.m.full), direction = "forward")
# health.ins ~ income.fix + age.fix + employment + sex + marital.stat

# Since both forward and backward stepwise produce the same model. We will stick to it.
cust.lg.m <- cust.lg.m.f
summary(cust.lg.m)

# Set the threshold for predicting health.ins = T
# We run a for-loop from 1% to 100% and find the treshold which yields the highest accuracy.
prob <- predict(cust.lg.m, type = 'response')

highest_tres = 0
highest_acc <- 0
for (tres in seq(1:100)){
  ins.hat <- ifelse(prob > tres/100, 1, 0)
  acc <- mean(ins.hat == cust.dt$health.ins)
  if (acc > highest_acc){
    highest_acc <- acc
    highest_tres <- tres
  }
}

# highest_tres == 53, so we set the treshold as 53%
ins.hat <- ifelse(prob > highest_tres / 100, 1, 0)
# Create a confusion matrix with actuals on rows and predictions on columns.
log.model.table <- table(cust.dt$health.ins, ins.hat)

#     ins.hat
#         0   1
# FALSE  28  91
# TRUE   16 753

# Overall accuracy if same misclassification costs
log.model.overall <- mean(ins.hat == cust.dt$health.ins)
# [1] 0.8795045


####################################################################################################################################################


# Building CART model
set.seed(20181016)
# Growing the tree the the maximum extent by setting complexity penalty as 0.
cust.cart <- rpart(health.ins ~ . -custid, data = cust.dt, method = "class", control = rpart.control(minsplit = 2, cp = 0))
print(cust.cart)
prp(cust.cart, type=2, extra=104, nn=T, nn.box.col = 'light blue')
printcp(cust.cart, digits = 3)
#         CP nsplit rel error xerror   xstd
# 1  0.01471      0    1.0000   1.00 0.0853
# 2  0.01345     16    0.6387   1.17 0.0910
# 3  0.01261     21    0.5714   1.25 0.0936
# 4  0.01120     27    0.4958   1.25 0.0936
# 5  0.00840     30    0.4622   1.24 0.0931
# 6  0.00700     54    0.2605   1.32 0.0955
# 7  0.00672     61    0.2101   1.33 0.0958
# 8  0.00560     66    0.1765   1.33 0.0958
# 9  0.00420     70    0.1513   1.35 0.0965
# 10 0.00336     98    0.0252   1.45 0.0992
# 11 0.00280    103    0.0084   1.46 0.0994
# 12 0.00000    106    0.0000   1.46 0.0994
plotcp(cust.cart)
# As we can see, the cross validation error (xerror) increases as the tree grows. The reason for this can be due to overfitting (source: https://stats.stackexchange.com/questions/37784/is-it-possible-to-have-xerror-increased-in-a-tree-using-rpart) In fact, we may indeed face an overfitting model since we use all the columns as our explanatory variables and some of them are proven to be useless in the building of our logistic regression model. So, we try to drop a few variables such that we are using the same variables as that of logistic regression model in the hope of solving this issue.

cust.cart <- rpart(health.ins ~ sex + marital.stat + employment + income.fix + age.fix, data = cust.dt, method = "class", control = rpart.control(minsplit = 2, cp = 0))
print(cust.cart)
prp(cust.cart, type=2, extra=104, nn=T, nn.box.col = 'light blue')
printcp(cust.cart, digits = 3)

# The printcp will yield different result, the following is just one of the result, since the algorithm will choose anyone when there is a tie

#       CP nsplit rel error xerror   xstd
# 1 0.01513      0    1.0000   1.00 0.0853
# 2 0.01120     11    0.7899   1.07 0.0877
# 3 0.00840     23    0.6303   1.05 0.0871
# 4 0.00560     47    0.3950   1.03 0.0862
# 5 0.00525     53    0.3613   1.09 0.0885
# 6 0.00420     64    0.3025   1.09 0.0885
# 7 0.00336     96    0.1681   1.17 0.0910
# 8 0.00280    116    0.0924   1.30 0.0951
# 9 0.00000    141    0.0168   1.31 0.0953

# The following is another possible output:

#       CP nsplit rel error xerror   xstd
# 1 0.01513      0    1.0000   1.00 0.0853
# 2 0.01120     11    0.7899   1.04 0.0868
# 3 0.00840     23    0.6303   1.03 0.0865
# 4 0.00560     47    0.3950   1.04 0.0868
# 5 0.00525     53    0.3613   1.11 0.0891
# 6 0.00420     64    0.3025   1.11 0.0891
# 7 0.00336     96    0.1681   1.22 0.0926
# 8 0.00280    116    0.0924   1.29 0.0948
# 9 0.00000    141    0.0168   1.31 0.0953

### HENCE, WE CHOOSE THE OPTIMAL CP TO BE 0.01120, WHICH CORRESPONDS TO THE CROSS VALIDATION ERROR (XERROR) WHICH MINIMIZEs THE NUMBER OF SPLITS
plotcp(cust.cart)

cp.opt <- 0.01120

# Prune the maximum tree cust.cart using a the optimal CP value. In this case, we use 0.0084
cust.cart.pruned <- prune(cust.cart, cp = cp.opt)
print(cust.cart.pruned)
printcp(cust.cart.pruned, digits = 3)
# Plot the final decision tree chosen
prp(cust.cart.pruned, type=2, extra=104, nn=T, fallen.leaves=T, branch.lty=3, nn.box.col = 'light blue', min.auto.cex = 0.7, nn.cex = 0.6, split.cex = 1.1, shadow.col="grey")

cust.cart.pruned$variable.importance
# Both age and income contribute the most to node purity.

predicted <- predict(cust.cart.pruned, newdata = cust.dt, type = "class")

cart.model.table <- table(cust.dt$health.ins, predicted)
cart.model.overall <- mean(cust.dt$health.ins == predicted)

####################################################################################################################################################
print("Logistic Regression Model Confusion Matrix:")
print(log.model.table)
print("CART Model Confusion Matrix:")
print(cart.model.table)
print("Logistic Regression Model Overall Accuracy:")
print(log.model.overall)
print("CART Model Overall Accuracy:")
print(cart.model.overall)