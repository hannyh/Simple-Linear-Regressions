#Galton's Peas

#Research Task: Test if there is a statistically significant inheritance effect between parent pea 
#diameters and offspring pea diameters. 

#Data
#Data Features: The data is linear, has no outliers or influential 
#observations, and it has a constant variance (no megaphone shapes). Therefore, it is well suited for a
#Simple Linear Regression Model. 

#from 1877 experiment by Galton
peas <- read.csv(header=TRUE, sep=",", text="
         Parent, Offspring
         0.21, 0.1726
         0.20, 0.1707
         0.19, 0.1637
         0.18, 0.1640
         0.17, 0.1613
         0.16, 0.1617
         0.15, 0.1598")
peas

#EDA
#Correlation Coefficient of Parent and Offspring
cor(peas$Parent, peas$Offspring)
#There is a very strong positive correlation that diameter is a trait inherited from Parents. 
#Because r=0.9248759, the variables have a strong positive relationship. 

#Create a scatterplot to confirm characteristics suggested by r
library(ggplot2)
ggplot(peas, aes(Parent, Offspring))+geom_point(xlab="Diameter of Parent", ylab="Diameter of Offspring")
#or use qplot, quick plot
qplot(Parent, Offspring, data=peas, geom="point", xlab="Diameter of Parent Pea", ylab="Diameter of offspring Pea")
#The data is linear, has no outliers or influential observations, and it has a constant variance
#(no megaphone shapes). It does have the characteristics suggested by r.

#Analysis

# The Response Variable is the Offspring Diameter
# The Explanatory Variable is the Parent Diameter

# MODEL:  Offspring = beta0 + beta1*parent + epsilon, epsilon~N(0,sigma2)

#Fit Model/ Estimate Parameters
peas.out <- lm(Offspring~Parent, data=peas)
#for table of estimates and std errors
summary(peas.out)

#(Intercept)=beta0
#Parent = Slope = beta1

#Our estimated beta1 (slope) is 0.21
#Interpretation: For a one unit increase in parent sweet pea diameter, we expect an estimated increase
#of 0.21 inch in the offspring sweet pea diameter. 

#Is there a statistically significant inheritance effect?

####T-test
#H0: beta1 = 0   AKA there is no change in offspring given different parent diameters
#T-test statistic value = 5.438
#p-value = 0.00285
#Formal Conclusion: There is a statistically significant inheritance effect of parent pea diameters on 
    #offspring pea diameters. There is an estimated effect with a slope of 0.21.
#To a Non-statistician: There is a statistically significant effect of parent pea diameters on offspring
    #pea diameters. For every 1 unit increase in parent sweet pea diameter, we expect an estimated 
    #increase of 0.21 inch in the offspring sweet pea diameter.


####ANOVA
#H0: beta1 = 0
#ANOVA F-test statistic value: 29.58 on 1 and 5 DF
#p-value: 0.002852
#Formal Conclusion:  Because the pvalue is less that .05, there is a statistically significant   
    #inheritance effect of parent pea diameters on offspring pea diameters. There is an estimated effect 
    #with a slope of 0.21.
#To a Non-statistician: There is a statistically significant effect of parent pea diameters on offspring
    #pea diameters. For every 1 unit increase in parent sweet pea diameter, we expect an estimated 
    #increase of 0.21 inch in the offspring sweet pea diameter.


#####95% Confidence Interval on the beta1
confint(peas.out)
#Formal Conclusion: Because 0 is not in our 95% confidence interval for beta1, we believe that there is
    #a significant inheritance effect between parent pea diameters and offspring pea diameters. The 
    #estimated effect is beta1=0.21
#To a Non-Statistician: We are 95% confident that 0 is not a possible value for the slope, which means
    # that there is a statistically significant effect between parent pea diameters and offspring pea 
    #diameters.

#Graphic showing line and uncertainty associated with line
qplot(Parent, Offspring, data=peas, geom="smooth", formula=y~x, method="lm", se=TRUE,
      xlab="Diameter of Parent Pea", 
      ylab="Diameter of Offspring Pea")

#95% Confidence Interval for E(Offspring | Parent = 0.2)
predict(peas.out, newdata=data.frame(Parent = 0.2), interval="confidence")

#Model for Offspring.hat
#Offspring.hat = 0.127 + 0.210*Parent

#We were asked to predict a value of a future offspring diameter from parents with diameter=0.18
#95% Prediction Interval
predict(peas.out, newdata=data.frame(Parent = 0.18), interval="prediction")

#Create a publication quality graphic demonstrating the uncertainty associated with predicting Offspring
#using the estimated model

#graphic showing prediction performance
plot.df <- cbind(peas, predict(peas.out, interval="prediction"))
ggplot(plot.df, aes(Parent, Offspring)) + 
  xlab("Diameter of Parent Pea") +
  ylab("Diameter of Offspring Pea") +
  geom_point() +
  geom_line(aes(y=fit), color="blue") + 
  geom_line(aes(y=lwr), color="red", linetype="dashed")+
  geom_line(aes(y=upr), color="red", linetype="dashed")

#Compute R squared ( in summary function - "Multiple R-squared")
summary(peas.out)
# Our r squared value is  0.8554. This means that 85.5% of the variation in Offspring Pea Diameter can
# be explained by the model Offspring = beta0 + beta1*parent + epsilon, epsilon~N(0,sigma2)

#Analysis Weakness: This model is not necessarily meant for predicting future observations of Offspring
#Diameter. It is meant more for estimating the correlation between the data that we currently have. 



