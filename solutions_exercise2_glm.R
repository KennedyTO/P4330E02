# Exercise 2: General Linear Model

# Open the dataset (us_gss_1991.csv) 
d <- read.csv(file.choose())
names(d)
# 1) Run the GLM
d$RELIG <- factor(d$RELIG)
levels(d$RELIG)
m <- lm(REALINC ~ SEX + PRESTG80 + RELIG + HRS1, data=d)
summary(m)

# 2) Interpret the Coefficients
coef(m)
# Intercept: catholic females who don't work and 
# have a job with zero prestige are predicted to 
# make $3159

# SEXMale: males are predicted to make $5426 per year more 
# than females

# PRESTG80: every 1 unit increase in prestige 
# increase annual income by $528

# RELIGJewish: jewish people make on average 
# $16848 more than Catholic people

# RELIGNone: non religious people are expected to 
# make $3147 less than catholic people per year

# RELIGOther: people of 'other' religions are 
# expected to make $3686 more than catholic 
# people per year

# RELIGProtestant: protestants are expected to make 
# $2263 less than catholic people per year

# HRS1: every extra hour worked per week results 
# is expected to lead to $92 more per year 
# (~ $2 per hour)

confint(m) # There is a fair amount of variability 
# in the estimation of these coefficients, even 
# though the N is fairly large


# 3) Statistical significance

# Model: R-squared = .16/Adj R-squared = .15 (p < .0001)
# SEX: p < .001
# PRESTG80: p < .001
# RELIGJewish: p < .001
# RELIGNone: p = .26
# RELIGOther: p = .46
# RELIGProtestant: p = .17
# HRS1: p = .06

# 4) Calculate the predicted income for a protestant male 
# who works 30 hours/week at a job with an occupation 
# prestige of 55.
m$coefficients["(Intercept)"] + m$coefficients["SEXMale"]*1 +
  m$coefficients["PRESTG80"]*55 + m$coefficients["RELIGJewish"]*0 +
  m$coefficients["RELIGNone"]*0 + m$coefficients["RELIGOther"]*0 +
  m$coefficients["RELIGProtestant"]*1 + m$coefficients["HRS1"]*30

# 5) Effect Size Measures
library(rockchalk)
getDeltaRsquare(m)
library(lm.beta)
lm.beta(m)
aa<-lm.beta(m)
aa$standardized.coefficients^2

# 6) Assumptions
#Homoscedasticity
plot(factor(d$SEX), residuals(m)) 
plot(factor(d$RELIG), residuals(m)) 
plot(d$HRS1, residuals(m)) 
plot(d$PRESTG80, residuals(m))
plot(predict(m),residuals(m))
#Normality
hist(residuals(m))
#Linearity
plot(d$PRESTG80, d$REALINC)
plot(d$HRS1, d$REALINC)
plot(predict(m),residuals(m))
