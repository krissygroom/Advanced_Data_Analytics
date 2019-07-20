# Kristen Groom
# Final Project code

# import libraries
library(corrplot)
library(car)
library(QuantPsyc)
library(leaps)
library(RColorBrewer)
library(Hmisc)
library(psych)
library(dummies)

library(glmnet)
library(MASS)
# Hierarchical Cluster analysis:
setwd("/Users/kristengroom/Desktop/DSC 424/HW4/")

# load in dataset
df = read.csv('economic_freedom_new.csv')
head(df)
# remove index col
df = df[, 2:22]
head(df)

# I need to get the countries column back into my dataset so:
# read in edited dataset
df_countries = read.csv('economic_freedom_edited.csv')
df$countries <- df_countries$countries
head(df)

# I don't think I need to scale the data but I could do it just in case:
std <- apply(df[, 1:21], 2, sd) # finding standard deviations of variables
df.std <- sweep(df[, 1:21],2,std,FUN="/") 
head(df.std)

# quick regression analysis and check of studentized residuals:
fit = lm(df.std$ECONOMIC.FREEDOM ~ ., data = df.std)
summary(fit)

library(MASS)
qqPlot(fit, main="QQ Plot")
# didn't really change anything - looks ok



####### K-means #########
# Note that the stability of the result can be improved by increasing the maximum number 
# of iterations and using multiple random starts:

countries.k5 <- kmeans(df.std, centers=5, iter.max=100, nstart=25)
countries.k5
# withing cluster sum of squares by cluster


# try 4:
countries.k4 <- kmeans(df.std, centers=4, iter.max=100, nstart=25)
countries.k4

# Printing the clustering vector for the 4-cluster solution:

countries.k4$cluster
head(df)
countries.k4.clust <- lapply(1:4, function(nc) df$countries[countries.k4$cluster==nc])  
countries.k4.clust   # printing the clusters in terms of the country labels


############# Visualization of Clusters:

### Via the scatterplot matrix:
dev.new(width=15, height=10)
pairs(df[,1:21], panel=function(x,y) text(x,y,countries.k4$cluster))
# not helpful b/c so many variables so difficult to interpret


### Via a plot of the scores on the first 2 principal components, 
### with the clusters separated by color:

countries.pc <- princomp(df[, 1:21],cor=T)

# Setting up the colors for the 4 clusters on the plot:
my.color.vector <- rep("green", times=nrow(df))
my.color.vector[countries.k4$cluster==2] <- "blue"
my.color.vector[countries.k4$cluster==3] <- "red"
my.color.vector[countries.k4$cluster==4] <- "orange"

# Plotting the PC scores:


par(pty="s")
plot(countries.pc$scores[,1], countries.pc$scores[,2], ylim=range(countries.pc$scores[,1]), 
     xlab="PC 1", ylab="PC 2", type ='n', lwd=2)
text(countries.pc$scores[,1], countries.pc$scores[,2], labels=df$countries, cex=0.7, lwd=2,
     col=my.color.vector)

# so much overlap - k-means does not look like a good clustering technique
# for this data - limitations are probably the repeated countries
# too much similarities in the results




################LINEAR REGRESSION#################


# set working directory
setwd("/Users/kristengroom/Desktop/DSC 424/Week3_4_5")

# check working directory:
getwd() 

# load in dataset
dataset = read.csv('economic_freedom_edited.csv')

# view the dataframe
str(dataset)
head(dataset)
describe(dataset)
# looking at means vs medians - I can see the the data
# is fairly normal except for our categorical variable
# X3d_freedom_own_foreign_currency which is strongly skewed right


# remove label columns and categorical column for regression analysis
df = subset(dataset, select = -c(year, ISO_code, countries) )
head(df)


# creating dummy variables for categorical variable
df <- cbind(df, dummy(df$X3d_freedom_own_foreign_currency, sep = 'X3d_own_currency_'))
head(df)

# drop the categorical variable from the dataframe for regression and one dummy var
df = subset(df, select = -c(X3d_freedom_own_foreign_currency, dfX3d_own_currency_0) )
head(df)

# rename dummies:
colnames(df)
names(df)[names(df) == "dfX3d_own_currency_10"] <- "X3d_own_currency_10"
names(df)[names(df) == "dfX3d_own_currency_5"] <- "X3d_own_currency_5"
colnames(df)

# recheck missing values:
describe(df)
sum(is.na(df))  # no missing values

# Economic Freedom (Y variable) is already at the begining of our dataframe
# checking to make sure all variables are numeric:
str(df)

# In order to check for multicollinearity - check correlation matrix
cor.df = cor(df)
cor.df

# I do see multicollinearity which I assumed I would see
# with values in >.7

# plot correlation matrix
dev.new(width=15, height=10)
corrplot(cor.df, method="ellipse")
corrplot(cor.df, method="number") # difficult to see r values

# I do see multicollinearity and will check again with VIF

# Linear Regression:
fullFit = lm(df$ECONOMIC.FREEDOM ~ ., data = df)
summary(fullFit)

# p-value in F-statistic is significant at < 2.2e-16
# R2 and ADJR2 at 1 - problematic and indicates overfitting and multicollinearity

# removing variables with NA vals b/c they are basically averages of their group of variables:
df_new = subset(df, select = -c(X5_regulation, X4_trade) )
fullFit = lm(df_new$ECONOMIC.FREEDOM ~ ., data = df_new)
summary(fullFit)

# still seeing R2 and ADJR2 issues at 1 each
# checking VIF
vif(fullFit)

# seeing large vifs!!  Start with removing largest 1 at a time to see how this affects model:
# even though some are highly significant:
# sound money is another variable that averages the others in its group
# so it makes sense that this would cause high vif
df_new = subset(df_new, select = -c(X3_sound_money) )
fullFit = lm(df_new$ECONOMIC.FREEDOM ~ ., data = df_new)
summary(fullFit)
vif(fullFit)

# just removing this variable brought down vifs of others in that category:
# X2_property_rights vif is >23 so will remove as well even though significant:
# again, this is another average of the variables in its group so it makes sense to remove it
df_new = subset(df_new, select = -c(X2_property_rights) )
fullFit = lm(df_new$ECONOMIC.FREEDOM ~ ., data = df_new)
summary(fullFit)
vif(fullFit)

# X1_size_government is an 8 as for vif
# this is also an averages variable so I will remove this
df_new = subset(df_new, select = -c(X1_size_government) )
fullFit = lm(df_new$ECONOMIC.FREEDOM ~ ., data = df_new)
summary(fullFit)
vif(fullFit)
# p-value in F-statistic is significant at < 2.2e-16
# Multiple R2 = 0.986 ADJR2 = 0.9858  - very close
# so 98.6% of ECONOMIC FREEDOM is explained by this model.
# Residual standard error for this model: 0.08953 
# All variables are highly significant

# write the new dataset as csv file to use in 
# cluster analysis:
write.csv(df_new, "economic_freedom_new.csv")

# view correlation plot again
str(df_new)
cor.df = cor(df_new)
cor.df

dev.new(width=15, height=10)
corrplot(cor.df, method="ellipse")
corrplot(cor.df, method="number")
# still seeing  some possible multicollinearity
# but definitely less of it

# checking for normality:
qqPlot(fullFit, main="QQ Plot")
# I can see that the fullFit model does appear 
# to be basically normal with a few potential outliers
# I also see that removing the redundant variables was helpful
# to the useability of Linear Regression
# distribution of studentized residuals 
# code found at: https://www.statmethods.net/stats/rdiagnostics.html

library(MASS)
sresid <- studres(fullFit) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)


# using stepwise
# I am going to start stepwise with the fullFit variables from the above
# manual regression because these variables are redundant and it will allow me
# to see if stepwise removes other variables that are less significant

# so create variables:

null = lm(df_new$ECONOMIC.FREEDOM ~ 1, data=df_new)
null

full = lm(df_new$ECONOMIC.FREEDOM ~ ., data=df_new)
full

# stepwise
dfStep = step(null, scope = list(upper=full), direction="both")
summary(dfStep)
# pretty much the same results
# seeing R2 = 0.986 and ADJR2 = 0.9858
# p-value in global F-statistic is significant p-value: < 2.2e-16

# trying Lasso regression using df_new dataset with redundant variables removed
library(caTools)

# put in ratio for training
split = sample.split(df_new$ECONOMIC.FREEDOM, SplitRatio = 0.8)
training_set = subset(df_new, split == TRUE)
test_set = subset(df_new, split == FALSE)

xTrain = subset(training_set, select = -c(ECONOMIC.FREEDOM) )
yTrain = training_set$ECONOMIC.FREEDOM

xTest = subset(test_set, select = -c(ECONOMIC.FREEDOM) )
yTest = test_set$ECONOMIC.FREEDOM

head(xTrain)
head(xTest)
head(yTrain)
head(yTest)
# getting into proper form for model building
# yTrain
yTrain = data.matrix(yTrain)
head(yTrain)
str(yTrain)

#yTest
yTest = data.matrix(yTest)
head(yTest)
str(yTest)

# xTest
xTest = data.matrix(xTest)
head(xTest)
str(xTest)

# xTrain
xTrain = data.matrix(xTrain)
head(xTrain)
str(xTrain)

# quick regression on this
ds = data.frame(yTrain, xTrain)
names(ds)[1] = "Y"
head(ds)
fitOld = lm(Y ~ ., data=ds)
summary(fitOld)

yHat = predict(fitOld, data.frame(xTest))
c = fitOld$coefficients
dof = length(yTest) - length(c)
rsePredict = sqrt(sum((yTest - yHat)^2) / dof)
rsePredict   
# the error is pretty low at 0.1128


# The glmnet function 
# Defaults to lasso, but can also do a version of ridge
fit = glmnet(xTrain, yTrain)  # note there is no ~ here and y comes after the x's.
plot(fit, label=T)

summary(fit)  
names(fit)


coef(fit, s=.004416)
c = as.matrix(coef(fit, s=.004416))
yHat = predict(fit, xTrain, s=.003154)
dof = length(yTrain) - length(c)
rse = sqrt(sum((yTrain - yHat)^2) / dof)  
rse
head(yHat)
head(yTrain)

yPredict = predict(fit, xTest, s=.003154)
dof = length(yTest) - length(c)
rsePredict = sqrt(sum((yTest - yPredict)^2) / dof)
rsePredict   

head(yPredict)
head(yTest)


# I can see that the predicted values are very close
# for both the training set and the test set

