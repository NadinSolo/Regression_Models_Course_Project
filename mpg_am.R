data(mtcars)
str(mtcars)
library(car)
library(dplur)

#Exploratory analysis
#creating a multiple linear regression with multiple independent variables,
#dependent variable mpg (Miles/(US) gallon)
#checking the summary on it
fitmpg<-lm(mpg~.,mtcars)
summary(fitmpg)
#the model has good indicators Adjusted R-squared:  0.8066, p-value: 3.793e-07 
#and shows the dependence of mpg on various factors.

#we construct a diagram reflecting a linear approximation of the dependence 
#for each pair of variables

scatterplotMatrix(~cyl+ disp + hp + drat + vs + wt + qsec + am + gear + carb, data=mtcars, diag="boxplot") 

#we select the most significant variables and use the function Step

recuce_fitmpg<-step(fitmpg, direction = "backward")

#the most significant variables are defined by
# am (Transmission (0 = automatic, 1 = manual)), qsec(1/4 mile time),
#wt(Weight (1000 lbs))
#the improved model will look like :

recuce_fitmpg<-lm(mpg~wt + qsec + am, data=mtcars)

#diagnostic charts:

par(mfrow=(c(2,2)))
plot(recuce_fitmpg)

#conclusions: the points on the graph (1) are scattered randomly
# without a pattern, (2) the points are on the line - the residuals 
# have a normal distribution

summary(recuce_fitmpg)
#The resulting model showed that mpg depends not only on am (p-value=0.046716),
#but also on wt (p-value= 6.95e-06) and qsec (p-value=0.000216)

#Quantify the MPG difference between automatic and manual transmissions  

am0<- mtcars$am==0
recuce_fitmpg0<-lm(mpg~am0+qsec+wt, mtcars)


am1<- mtcars$am==1
recuce_fitmpg1<-lm(mpg~am1+qsec+wt, mtcars)


Confint(recuce_fitmpg0)
Confint(recuce_fitmpg1)

# Transmission (1 = manual), coef:
(qsec=9.617781+1.225886)
(wt=9.617781-3.9165)
(am=9.617781+2.935837)

#Transmission 0 = automatic, coef:
(am=12.553618-2.935837)
(qsec=12.553618+1.2259)
(wt=12.553618-3.9165)

# am=1 manual am=12.5536 qsec=10.8437, wt=5.7013
# am=0 automatic  am =9.617781 qsec=13.7795, wt=8.6371

#Conclusions: the resulting model does not give an unambiguous answer
# to the question: Is an automatic or manual transmission better for MPG? 
#p-value=0.046716 does not give grounds to refute 
#the null hypothesis (that there is no difference between manual transmission 
#and automatic transmission).
#MPG Transmission 0 = automatic one mile 9.6 gallon, less than
#Transmission 1 = manual 12.6 gallon, but qsec automatic Transmission = 13.77952
# more than manual Transmission =10.8



