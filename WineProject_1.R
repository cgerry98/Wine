
library(car)
library(alr4)
library(leaps)
library(mice)
library(dplyr)
library(corrplot)
library(ggplot2)
library(visreg)
library(ggcorrplot)
library(corrr)
library(VIM)
library(moments)
library(tidyverse)
library(caret)
library(MASS)
library(leaps)
library(Simpsons)
library(memisc)
library(caret)
library(ggplot2)

df <- read.csv("~/Undergrad/Computer Science/Comp 442/Project/Data/winequality-red_1.csv")

str(df)
summary(df)
as.double(df$quality)
df=na.omit(df)
str(df)

head(df)
dfNoRed<-df[ ,-c(13)]
head(dfNoRed)
mahal1<-mahalanobis(dfNoRed,
                    colMeans(dfNoRed,na.rm=TRUE),
                    cov(dfNoRed, use='pairwise.complete.obs'))
summary(mahal1)
cutoff=qchisq(.999,ncol(dfNoRed))
summary(mahal1<cutoff)
NoOut<-df[mahal1<cutoff, ]
summary(NoOut)
str(NoOut)
View(NoOut)
dfnew<-NoOut[ ,-c(13)]
View(dfnew)

corelation<-cor(dfNoRed,use='pairwise.complete.obs' )
symnum(corelation)

fa=NoOut$fixed.acidity
va=NoOut$volatile.acidity
ca=NoOut$citric.acid
rs=NoOut$residual.sugar
ch=NoOut$chlorides
fsd=NoOut$free.sulfur.dioxide
tsd=NoOut$total.sulfur.dioxide
d=NoOut$density
ph=NoOut$pH
s=NoOut$sulphates
a=NoOut$alcohol
q=NoOut$quality
hist(fa)
hist(va)
hist(ca)
hist(rs)
hist(ch)
hist(fsd)
hist(tsd)
hist(d)
hist(ph)
hist(s)
hist(a)
hist(q)

m1<-lm(q~fa+va+ca+rs+ch+fsd+tsd+d+ph+s+a)
summary(m1)
#checking again to see what is important to the model or not
regfit.full=regsubsets(quality~.,dfnew)
summary(regfit.full)
regfit.full=regsubsets(quality~., data=dfnew, nvmax=11)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$cp
vif(m1)
par(mfrow=c(1,2))
#boxcox(m1,lambda=seq(0,1,length=20))
#boxcox(m1,lambda=seq(0,1,length=20))
ppois(q= 4, lambda = 13, lower.tail = TRUE)
dpois(x= 0:4, lambda = 13)

ggplot(data = dfnew, aes(x = quality, y = fixed.acidity)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
#we can see that fixed acidity has almost no effect on Quality

#2
ggplot(data=dfnew, aes(x = quality, y = volatile.acidity)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
#volatile acidity has a negative affect on the quality where if VA goes up quality goes down

#3
ggplot(data=dfnew, aes(x=quality, y=citric.acid)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
#citric acid seems to have a positive affect higher quality higher citric acid

#4
ggplot(data=dfnew, aes(x=quality, y=residual.sugar)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  scale_y_continuous(lim = c(0,5)) +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
#residual sugar seems to have little to no affect on quality (sweetness)

#5
ggplot(data=dfnew, aes(x=quality, y=chlorides)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  scale_y_continuous(lim = c(0,0.2)) +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
#chlorides seems to have very little affect on quality but the less the chlorides a little better quality (salt)
#```
#```{r echo=FALSE, message=FALSE, warning=FALSE}
#6
ggplot(data=dfnew, aes(x=quality, y=free.sulfur.dioxide)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  scale_y_continuous(lim = c(0,40)) +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
#interesting... low FSD low quality however higher FSD average quality
#```
#```{r echo=FALSE, message=FALSE, warning=FALSE}
#7
ggplot(data=dfnew, aes(x=quality, y=total.sulfur.dioxide)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  scale_y_continuous(lim = c(0,150)) +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
#same thing here with total sulfur dioxide
#```
#```{r echo=FALSE, message=FALSE, warning=FALSE}
#8
ggplot(data=dfnew, aes(x=quality, y=density)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
#better wines seem to have lower densities however could be from alcohol since low density means higher alcohol which is an important factor for wine.
#```
#```{r echo=FALSE, message=FALSE, warning=FALSE}
#9
ggplot(data=dfnew, aes(x=quality, y=pH)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
#lower pH better wine (more acidic) (FURTHER ANALYSIS)
ggplot(data = dfnew, aes(x = fixed.acidity, y = pH)) +
  geom_point(alpha = 0.3) +
  scale_x_log10(breaks=seq(5,15,1)) +
  xlab("Fixed Acidity in Log Scale") +
  geom_smooth(method="lm")
ggplot(data = dfnew, aes(x = volatile.acidity, y = pH)) +
  geom_point(alpha = 0.3) +
  scale_x_log10(breaks=seq(.1,1,.1)) +
  xlab("Volatile Acidity in Log Scale") +
  geom_smooth(method="lm")
ggplot(data = subset(dfnew, citric.acid > 0), aes(x = citric.acid, y = pH)) +
  geom_point(alpha = 0.3) +
  scale_x_log10() +
  xlab("Citric Acid in Log Scale") +
  geom_smooth(method="lm")

simpsons <- Simpsons(volatile.acidity, pH, data=dfnew)
plot(simpsons)

#```

#```{r, echo=FALSE}
knitr::opts_chunk$set(error = TRUE)

#10
ggplot (data = dfnew, aes(x=quality, y=sulphates)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
#generally seems better quality have more sulphates
#```
#```{r echo=FALSE, message=FALSE, warning=FALSE}
#11

ggplot(data=dfnew, aes(x=quality, y=alcohol)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
#higher alcohol seems to have an affect on quality but we can do a simple linear regression to see if big impact is made
ma<-lm(quality~alcohol, data=dfnew)
summary(ma)
#with my cleaned data we see that alcohol alone 25% of the wines quality so there has to be more to it 
#```
#```{r echo=FALSE, message=FALSE, warning=FALSE}
simple_cor_test <- function(x, y) {
  return(cor.test(x, as.numeric(y))$estimate)
}
cor(dfnew)

correlations <- c(
  simple_cor_test(dfnew$fixed.acidity, dfnew$quality),
  simple_cor_test(dfnew$volatile.acidity, dfnew$quality),
  simple_cor_test(dfnew$citric.acid, dfnew$quality),
  simple_cor_test(log10(dfnew$residual.sugar), dfnew$quality),
  simple_cor_test(log10(dfnew$chlorides), dfnew$quality),
  simple_cor_test(dfnew$free.sulfur.dioxide, dfnew$quality),
  simple_cor_test(dfnew$total.sulfur.dioxide, dfnew$quality),
  simple_cor_test(dfnew$density, dfnew$quality),
  simple_cor_test(dfnew$pH, dfnew$quality),
  simple_cor_test(log10(dfnew$sulphates), dfnew$quality),
  simple_cor_test(dfnew$alcohol, dfnew$quality))
names(correlations) <- c('fixed.acidity', 'volatile.acidity', 'citric.acid',
                         'log10.residual.sugar',
                         'log10.chlordies', 'free.sulfur.dioxide',
                         'total.sulfur.dioxide', 'density', 'pH',
                         'log10.sulphates', 'alcohol')

correlations
#```

#```{r echo=FALSE, message=FALSE, warning=FALSE}
fitAll <- lm(quality ~ ., data = dfnew)
fitStart <- lm(quality ~ 1, data = dfnew)
head(dfnew)
summary(fitStart)
step(fitStart, 
     direction = "forward", 
     scope = formula(fitAll))
#```
#```{r}
#after cross validation lets do partial F testing on reduced model and full model. 
#havent decided how im going to do the f-test yet 
#```
#```{r}
#cross validation

predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

k=5

set.seed(1)
folds=sample(1:k,nrow(dfnew),replace=TRUE)
cv.errors=matrix(NA,k,11, dimnames=list(NULL, paste(1:11)))
for(j in 1:k){
  best.fit=regsubsets(quality~.,data=dfnew[folds!=j,],nvmax=11)
  for(i in 1:11){
    pred=predict(best.fit,dfnew[folds==j,],id=i)
    cv.errors[j,i]=mean( (dfnew$quality[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
reg.best=regsubsets(quality~.,data=dfnew, nvmax=11)
coef(reg.best,11)
#```




#```{r}
#linear model with variables from forward stepwise selection
#just for the sake to see if there is a difference
#and with the ideas of how alcohol correlates with the other predictors
m4<-lm(quality~log(alcohol)+log(volatile.acidity)+log(sulphates)+log(total.sulfur.dioxide)+pH+log(free.sulfur.dioxide)+citric.acid, data=dfnew)
summary(m4)
plot(m4)
#generalized linear model we have not covered in another course (learn by yourself)
#poisson?
#```


#```{r}
#after stepwise selection forward and also taking into consideration my skewed data
#so i can use a log function for a transformation of my data
m3<-lm(quality~density*alcohol+alcohol*sulphates+(alcohol)+(volatile.acidity)+(sulphates)+(total.sulfur.dioxide)+pH+(free.sulfur.dioxide)+(citric.acid), data=dfnew)
summary(m3)
#```
#```{r}
#attempt number 3 of a linear model creating a training and testing set
set.seed(1534)
training_data <- sample_frac(dfnew, .6)
test_data <- dfnew[ !dfnew$X %in% training_data$X, ]
m1 <- lm(as.numeric(quality) ~ alcohol, data = training_data)
m2 <- update(m1, ~ . + log(sulphates))
m3 <- update(m2, ~ . + volatile.acidity)
m4 <- update(m3, ~ . + citric.acid)
m5 <- update(m4, ~ . + fixed.acidity)
m6 <- update(m2, ~ . + pH)
mtable(m1,m2,m3,m4,m5,m6)




#PCA
library(pls)


#PCR
#make this example reproducible
set.seed(1)
#fit PCR model
model <- pcr(quality~., data=dfnew, scale=TRUE, validation="CV")
# summary of model 
summary(model)
#We want to chooose the component with the lowest RMSE



validationplot(model, val.type="MSEP")
#As we look into the graph, we notice that the mean squared error looks the smallest at PCA 9, but the second smallest 
#MSEP looks is between PCA 4-8.




# Data: 	X dimension: 1534 11 
# Y dimension: 1534 1
# Fit method: svdpc
# Number of components considered: 11
# 
# VALIDATION: RMSEP
# Cross-validated using 10 random segments.
# (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps  9 comps  10 comps
# CV          0.7996    0.793   0.6774   0.6502   0.6439   0.6423   0.6417   0.6382   0.6386   0.6339    0.6318
# adjCV       0.7996    0.793   0.6773   0.6501   0.6437   0.6421   0.6415   0.6380   0.6384   0.6337    0.6315
# 11 comps
# CV       0.6321
# adjCV    0.6318
# 
# TRAINING: % variance explained
# 1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps  9 comps  10 comps  11 comps
# X         28.173    46.14    60.89    70.78    79.12    85.88    91.17    94.89    97.89     99.49    100.00
# quality    1.834    28.30    34.15    35.56    35.93    36.12    36.90    36.93    37.95     38.43     38.45


#We choose PC 7, since it is the smallest RSME
# We will be able to explain more variance by using more principal components, but by adding more, doen't actually 
# increase the amount of information by much.

#define training and testing sets

library(caTools)  # loading caTools library
set.seed(123)   #  set seed to ensure you always have same random numbers generated

train <- dfnew[1:25,] 
y_test <- dfnew[26:nrow(dfnew), c("quality")]
test <- dfnew[26:nrow(dfnew), ]


#use model to make predictions on a test set
model <- pcr(quality~., data=train, scale=TRUE, validation="CV")
pcr_pred <- predict(model, test, ncomp=7)

sqrt(mean((pcr_pred - y_test)^2))


#Our RMSE is, 1.042368, which is the average distance from the predicted value of quality and 
#the observation of the testing value. 




train <- dfnew[1:25,] 
y_test <- dfnew[26:nrow(dfnew), c("quality")]
test <- dfnew[26:nrow(dfnew), ]



#Lets try with a bigger PCA
#use model to make predictions on a test set
model <- pcr(quality~., data=train, scale=TRUE, validation="CV")
pcr_pred <- predict(model, test, ncomp=8)

sqrt(mean((pcr_pred - y_test)^2))
#RMSE:1.426864
#When we increased the PCA and got a higher RMSE. Decreased accuracy. Still usable to model




