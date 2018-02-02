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

plot(x = df2$age, y = df2$voice)
abline(xmdl)

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


#### MIXED MODELS  ####
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

polModelLL <- lmer(frequency  ~  attitude  + (1|subject) + (1|scenario), data=politeness, REML = F)
summary(polModelLL)  # negative LL
 
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


#### Random Slope vs Random intercept  ####
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


####  nested random effect (1 | r1/r2)  ####

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


#### publication ####

path <- "~/rScripts_learning"
setwd(path)

data <- read.table(file = "linear.txt")

splitted <- split(data, rep(1:4 , each = 18))

data2 <- rep(NA,18)

for(name in names(splitted)){
  data2 <- cbind(data2, splitted[[name]])
}

data2 <- data2[,2:5]
colnames(data2)<-c("Subj", "Item", "SOA", "RT")

#head(data2)


data2$Subj <- factor(data2$Subj)
data2$Item <- factor(data2$Item)
data2$SOA <- factor(data2$SOA)
data2$RT <- as.numeric(data2$RT)

str(data2)

library(lme4)

m1 <- lmer( RT ~ SOA  + (1|Item), data = data2 )
m2 <- lmer( RT ~ SOA + (1+SOA|Subj), data = data2 )
m3 <- lmer( RT ~ SOA + (1|Item) + (1+SOA|Subj), data = data2 )

var1 <- as.data.frame(VarCorr(m1))
coef(m1)

var2 <- as.data.frame(VarCorr(m2))
coef(m2)

summary(m3)
var3 <- as.data.frame(VarCorr(m3))
coef(m3)

# (1+SOA|Subj)
coef(m3)[["Subj"]][["(Intercept)"]]
coef(m3)[["Subj"]][["(Intercept)"]] - summary(m3)$coefficients[1] 

coef(m3)


m4 <-  lmer(RT ~ SOA + (1|Item) + (1|Subj) , data = data2)
print(m4)

# source("~/projekty/healthtell/Oleks/rcode/Scripts_Olek/anova.R")
# 
# anova(data2, c("Subj", "Item", "SOA"))



#### fit to matrix  ####

matr <- matrix(c(0,0,0,1, 0,0,1,0, 0,1,0,0, 1,0,0,0), nrow = 4, byrow = T)
matr
df <- as.data.frame(matr)
df

lin <- lm(matr ~ matr[1,] + matr[2,] + matr[3,], matr[4,])
