# linear models and linear mixed effects models

# http://www.bodowinter.com/tutorial/bw_LME_tutorial1.pdf
# http://www.bodowinter.com/tutorial/bw_LME_tutorial2.pdf

# linear with interaction terms and comparison with mixed effects
# https://cran.r-project.org/web/packages/phia/vignettes/phia.pdf

subject<-1:6
sex <- c("F", "F", "F", "M", "M", "M")
voice <- c(233, 204, 242, 130, 112, 142)

df <- data.frame(subject, sex, voice)
df$sex <- as.factor(df$sex)
str(df)

# error term already included in model

xmdl <- lm(voice ~ sex, data = df)

summary(xmdl)

#plot(x = df$sex, y = df$voice)
#abline( xmdl)

mean(df[df$sex == "F",]$voice)   # intercept



subject<-1:6
age <- c(14,23,35,48,52,67)
voice <- c(252, 244, 240, 233, 212, 204)

df2 <- data.frame(age, voice)

xmdl <- lm(voice ~ age, df2)
summary(xmdl)

names(xmdl)
plot(fitted(xmdl), residuals(xmdl))

plot(xmdl)

#normality of residuals assumption:
hist(residuals(xmdl))
qqnorm(residuals(xmdl))

#plot(x = df2$age, y = df2$voice)
#abline( xmdl)


# Absence of influential data points
dfbeta(xmdl)
# gives value of which coeficients need to be adjusted without given point 


#### MIXED MODELS
library(lme4)

?lmer

# Random-effects terms are distinguished by vertical bars (|) separating expressions for design matrices from grouping factors. Two vertical bars (||) can be used to 
# specify multiple uncorrelated random effects for the same grouping variable. (Because of the way it is implemented, the ||-syntax works only for design matrices 
# containing numeric (continuous) predictors; to fit models with independent categorical effects, see dummy or the lmer_alt function from the afex package.)

politeness <- read.csv(file = "~/rScripts_learning/politeness_data_mixedLM.csv")
head(politeness)


which( !complete.cases(politeness) )  # 39

boxplot(frequency ~ attitude*gender, col=c("white","lightgray"),politeness)


lmer(frequency ~ attitude, data=politeness)   # no random effect

# add  random  intercepts  for  subjects  and  items  (remember  that  items  are  call ed “scenarios” here)

polModel <- lmer(frequency  ~  attitude  + (1|subject) + (1|scenario), data=politeness)
summary(polModel)

# Random effects
# more variability between subjects than between scenario
# "Residual" == epsilon; stands for the variability that’s not due to either scenario or subject 

polModel <- lmer(frequency  ~  attitude + gender  + (1|subject) + (1|scenario), data=politeness)
summary(polModel)

#variability of the subject shifted to fixed effect (gender). Intercept of fixed effect correspond to mean of inf female

# checking p-value : by removing factor and comparing likelihood

polNull <- lmer(frequency  ~  gender  + (1|subject) + (1|scenario), data=politeness, REML = F)  
# REML - restricted maximum likelihood
# REML logical scalar - Should the estimates be chosen to optimize the REML criterion (as opposed to the log-likelihood)? - as default = TRUE

polModel <- lmer(frequency  ~  attitude + gender  + (1|subject) + (1|scenario), data=politeness, REML = F)

#models tested by ANOVA

anova(polNull, polModel)

# to check interaction, we can obtain p-value by compare with anova:
#full model:   frequency ~ attitude*gender
#reduced model:   frequency ~ attitude + gender


#### Random Slope vs Random intercept
coef(polModel)

#random slope:
polModel_rs <- lmer(frequency ~ attitude + 
                          gender + (1+attitude|subject) + 
                     # means that you tell the model to expect differing baseline-levels of frequency (the intercept, represented by 1) as well as differing 
                     # responses to the main factor in question, which is “attitude” in this case
                          (1+attitude|scenario),
                        data=politeness, REML=FALSE)

coef(polModel_rs)


#  p-value
polNull_rs <- lmer(frequency ~ gender +
                         (1+attitude|subject)  +  (1+attitude|scenario), 
                       data=politeness, REML=FALSE)

anova(polNull_rs, polModel_rs)

plot(polModel_rs)


#dfbeta() does not work for mixed models
# check package influence.ME


####  nested random effect (1 | r1/r2)

# https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf

library(MASS)
data("oats")
names(oats) = c('block', 'variety', 'nitrogen', 'yield')
oats$mainplot = oats$variety
oats$subplot = oats$nitrogen

oats
summary(oats)


m1_lme4 = lmer(yield ~ variety*nitrogen + (1|block/mainplot),
               data = oats)

VarCorr(m1_lme4)   # estimated variances, standard deviations, and correlations between the random-effects terms in a mixed-effects model

# 1|block/mainplot is fitting block and mainplot within block

m2_lme4 = lmer(yield ~ variety*nitrogen + (1|block:mainplot),   # feature block not taken into account
               data = oats)

VarCorr(m1_lme4)

summary(m1_lme4)
summary(m2_lme4)

# gdy nie chcemy miec fixed intercept to stosujemy zapis: 0 + (1 | g) 

############
# # more levels - 
# A <- c(rep("a", 100), rep("b", 100))
# B <- c(rep("e", 50), rep("f", 50 ), rep("g",50), rep("h", 50))
# C <- c(rep("k", 25), rep("l", 25 ), rep("m",25), rep("n", 25), rep("o", 25), rep("p", 25 ), rep("q",25), rep("r", 25) )
# 
# sd1 <- 20
# m1 <- 100
# sd2 <- 10
# m2 <- 50
# 
# sd3 <- 1
# sd4 <- 3
# sd5 <- 5 
# sd5 <- 7
# sd6 <- 9
# 
# no <- 1
# 
# func <- function(x){
#   if(x[1] == "a"){
#     
#     if(x[2] == "e"){
#       return(rnorm(mean = m1, sd= (sd1 + sd3), n = no))
#       
#     } else if(x[2] == "f"){
#       return(rnorm(mean = m1, sd= (sd1+ sd4), n = no))
#       
#     } else if(x[2] == "g"){
#       return(rnorm(mean = m1, sd= (sd1+ sd5), n = no))
#              
#     }else{
#       return(rnorm(mean = m1, sd= (sd1+ sd6), n = no))
#     }
#       
#     
#     
#   }else{
#     
#     if(x[2] == "e"){
#       return(rnorm(mean = m2, sd= (sd2 + sd3), n = no))
#              
#     } else if(x[2] == "f"){
#       return(rnorm(mean = m2, sd= (sd2+ sd4), n = no))
#              
#     } else if(x[2] == "g"){
#       return(rnorm(mean = m2, sd= (sd2+ sd5), n = no))
#              
#     }else{
#       return(rnorm(mean = m2, sd= (sd2+ sd6), n = no))
#     }
#     
#   }
# }
# 
# df<-data.frame(A,B,C)
# y <- sapply(df, func)

