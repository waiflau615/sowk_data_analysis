# --------------------------------------------------------------------------- #
# Programmer:   Yu-Chih Chen, PhD                                             #
# Department:   Social Work & Social Administration                           # 
# Course:       SOWK2144 Introduction to Social Data Analysis                 #
# Lab 6:        LOGISTIC BINARY AND MULTINOMIAL LOGISTIC REGRESSION           #
# --------------------------------------------------------------------------- #

# Note: data source: Class 4 data

# R demonstration -------------------------------------------------------------

###############################################################################

# Set working directory (Note: path may vary based on your working directory) 

setwd("C:/Users/Chen Yu-chih/Dropbox/HKU/Syllabus/SOWK2144-Intro to Social Data Analysis/Data")
setwd("C:/Users/ycche/Dropbox/HKU/Syllabus/SOWK2144-Intro to Social Data Analysis/Data")
getwd()

# Load haven to read external data sets 
install.packages("haven")
library(haven)

# Read all the datasets from the working directory
# Demographic data set from SAS
demo1 <-read_sas("Demographic11_short_sas.sas7bdat")

# Health data sets from 2011
hlth1_long <-read_dta("HealthStatus11.dta")

###############################################################################

# 1. EXAMINE DATA SETS -------------------------------------------------------
#
# 1.1. Variable names in data
names(demo1)
names(hlth1_long)

# 1.2. First 6 observations in data
head(demo1)
head(hlth1_long)

# 1.3. Variable description in data
str(demo1)
str(hlth1_long)

###############################################################################

# 1. EXAMINE DATA SETS -------------------------------------------------------
#
# 1.1. Variable names in data
names(demo1)
names(hlth1_long)

# 1.2. First 6 observations in data
head(demo1)
head(hlth1_long)

# 1.3. Variable description in data
str(demo1)
str(hlth1_long)

# 2. VARIABLES SELECTION, RENAME, AND MERGE -----------------------------------
#
# For demo1, keep variables: ID, byr_w1, rgender, bd001
# For hlth1, select the following variables:
#
# BELOW IS THE VARIABLE LISTS % % % % % % % % % % % % % % % % % % % % % % % % % 
#
# ID, da002, dc011      
#
# END OF THE VARIABLE LISTS % % % % % % % % % % % % % % % % % % % % % % % % % %

# 2.1. To select variables, we use a package called dplyr

install.packages("dplyr")
library(dplyr)

# 2.2. Select variables
demo1 <- select(demo1,
                ID,        ba002_1,   rgender,  bd001)

hlth1 <- select(hlth1_long, 
                ID,        da002,    dc011)

# If you want to remove some data sets, remove the # sign below
# rm(hlth1_long, hlth2_long)

# 2.3. Rename variables to make them become meaningful (you will need delyr package to do so!)

# For demo1 data
demo1 <- demo1 %>% rename(edu_w1 = bd001     , gender = rgender,
                          byr_w1 = ba002_1)

# For hlth1 data
hlth1 <- hlth1 %>% rename(srh_w1 = da002     , dp3_w1 = dc011)

# 2.3. Merge data 
#
# You will need an identifier to merge data sets, and this identifier should be 
# the same across the data sets. 
#
#
total <- merge(demo1, hlth1, all.x = TRUE)

# Remove unnecessary packages and view the targeted data 
rm(demo1, hlth1, hlth1_long)
View(total)

# 3. EXAMINE VARIABLES AND RECODE ---------------------------------------------

# 3.1. Examine multiple variables using apply command

# If you want to know more about apply function
?apply

# Demographics
apply(total[c("byr_w1" , "edu_w1" , "gender")], 2, table)

# Health for 2011 wave
apply(total[c("srh_w1" , "dp3_w1")], 2, table)

# 3.2. Select variables for use: age, gender, edu_w1, srh_w1, srh_w2, and dp1_w1:dp10_w1 
#                                to do data management 
# Age
#
# Gender
#
# Education: Keep original matric and recode the variable 
#
# Self-rated health 2011: Reverse code and recode it into three levels: poor, average, good health
#
# Depression 2011: Dummy code so that 1 = depressed and 0 = not depressed 

# 3.2.0. Age
# Create age variable by using 2011 - birth year
total$age_w1 <- 2011-total$byr_w1

# 3.2.1. Gender
# Force gender with a value 0 become missing (NA) 
total$gender[total$gender == "0"]<-NA

# Examine the distribution of gender
table(total$gender)

# 3.2.2.1. Education (ordinal): doing nothing; keep its original matric 
table(total$edu_w1, useNA = "always")

# 3.2.2.2. Education (recoded): low, medium, high
total$edu3_w1 <- cut(total$edu_w1,
                     breaks = c(0, 4, 7, 11),
                     labels = c("low", "medium", "high")
                     )

# 3.2.3. Self-rated health in 2011
#
# 3.2.3.1. Examine data distribution 
table(total$srh_w1)
str(total$srh_w1)

# 3.2.3.2. Reverse code and create a new variable called srhr_w1 so that higher
#          score indicate better self-rated health
total$srhr_w1 <- 6-total$srh_w1

# 3.3. Subset the data and select only age 60+
total_60 <- subset(total, age_w1>=60, )
summary(total_60$age_w1)
table(total_60$age_w1, useNA = "always")


# 4. DESCRIPTIVE STATISTICS ----------------------------------------------------
#
# 4.1. Basic mean and median

summary(total_60$age_w1)
summary(total_60$srhr_w1)

table(total_60$gender, useNA = "always")
table(total_60$edu_w1, useNA = "always")
table(total_60$edu3_w1, useNA = "always")
table(total_60$dp3_w1, useNA = "always")

# 4.1.1. More basic info (mean, SD, skewness) using "psych" packages

install.packages("psych")
library(psych)

describe(total_60$age_w1)
describe(total_60$srhr_w1)
describe(total_60$edu_w1)

# 4.2. Assessing normality for continuous variables using Q-Q plot

install.packages("ggplot2")
library(ggplot2)

# Q-Q plot for age
qqplot.age <- qplot(sample = total_60$age_w1, stat="qq")
qqplot.age

# Q-Q plot for self-rated health in 2011
qqplot.srh_w1 <- qplot(sample = total_60$srhr_w1, stat="qq")
qqplot.srh_w1

# Q-Q plot for education
qqplot.edu <- qplot(sample = total_60$edu_w1, stat="qq")
qqplot.edu

# 4.3 Histogram 
hist(total_60$age_w1)
hist(total_60$srhr_w1)
hist(total_60$edu_w1)

# 4.4. Skewness and kurtosis (we use psych package we loaded before)
describe(cbind(total_60$age_w1, total_60$srhr_w1, total_60$edu_w1))

# 4.5. Kolmogorov-Smirnov test (K-S test)
install.packages("nortest")
library(nortest)

# K-S test. 
lillie.test(total_60$age_w1)
lillie.test(total_60$srhr_w1)
lillie.test(total_60$edu_w1)

# 5. LOGISTIC REGRESSION ----------------------------------------------------------------------

# IV: gender    (1 = male, 2 = female). Need dummy code
#     edu3_w1   (low, medium, high). Need dummy code
#     age_w1    Continuous variable of age
#     srhr_w1   Ordinal variable of self-rated health in 2011

#
# DV: dp3_w1    (1 = rarely, 2 = sometimes, 3 = occasionally, 4 = mostly). Need dummy code so
#               that 1 = depressed (3+4) and 0 = not depressed (1+2). We create a new binary 
#               variable called dep_bi
#
#     srhr_w1   (1 = very poor, 2 = poor, 3 = average, 4 = good, 5 = very good). We will recode 
#               it as a 3-level variable (1 = poor, 2 = average, 3 = good) and create a new 
#               categorical variable called srh_cat 
#
# 5.1 Recode/dummy code for education, gender, depression, and self-rated health

# 5.1.1. Examine all variables 
table(total_60$edu3_w1, useNA = "always")
table(total_60$gender, useNA = "always")
table(total_60$dp3_w1, useNA = "always")
table(total_60$srhr_w1, useNA = "always")

# 5.1.2. Dummy code for gender
total_60$male   <- ifelse(total_60$gender == '1', 1, 0)
total_60$female <- ifelse(total_60$gender == '2', 1, 0)

# 5.1.3. Dummy code for education 
total_60$edu_l <- ifelse(total_60$edu3_w1 == 'low', 1, 0)
total_60$edu_m <- ifelse(total_60$edu3_w1 == 'medium', 1, 0)
total_60$edu_h <- ifelse(total_60$edu3_w1 == 'high', 1, 0)

# 5.1.4. Dummy code for depression
total_60$dep_bi <- ifelse(total_60$dp3_w1 >= 3 & total_60$dp3_w1 <= 4, 1, 0)

# Examine the recode accuracy
table(total_60$dp3_w1, useNA = "always")
table(total_60$dep_bi, useNA = "always")

# 5.1.5. Categorize self-rated health into 3 levels
total_60$srh_ca[total_60$srhr_w1 >=1 & total_60$srhr_w1 <= 2] <- 1
total_60$srh_ca[total_60$srhr_w1 ==3] <- 2
total_60$srh_ca[total_60$srhr_w1 >=4 & total_60$srhr_w1 <= 5] <- 3

table(total_60$srhr_w1, useNA = "always")
table(total_60$srh_ca, useNA = "always")

# 5.2. Multiple logistic binary regression
#
# 5.2.1. Generate logistic model
m1_log <- glm(dep_bi ~ age_w1 + female + edu_m + edu_h + srhr_w1, 
              data = total_60, 
              family = "binomial")
summary(m1_log)

# 5.2.2. Generate odds ratio (OR)
exp(cbind(OR = coef(m1_log), confint(m1_log)))


# 5.3. Multiple multinomial logistic regression
#
# 5.3.1. Generate multinomial model
# Note: Given that the multinom function does not include p-value calculation #
#       for the regression coefficients, we calculate p-values using          #
#       Wald tests (here z-tests)                                             #

# covert to categorical variables
total_60$srh_ca <- as.factor(total_60$srh_ca)

# set the reference level (ref = the lowest self-rated health level)
total_60$srh_ca <- relevel(total_60$srh_ca, ref = 1)

# Generate logistic model
install.packages("nnet")
library(nnet)
multinom_model <- multinom(srh_ca ~ age_w1 + female + edu_m + edu_h + dep_bi, 
                           data = total_60)
summary(multinom_model)

# Check the Z-score for the model (wald Z /Tests of Significance)
z <- summary(multinom_model)$coefficients/summary(multinom_model)$standard.errors
z

# 2-tailed z test (Tests of Significance /P value)
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# 5.3.2. Generate IRR
exp(coef(multinom_model))

# Summary of the results
round(exp(coef(multinom_model)), 3) # IRR # Three decimal places
round(p, 3) # P-value # Three decimal places

# 5.4. Regression assumptions
#
# 5.4.1. Examine multicollinearity (VIF > 10 indicated multicollinearity)
install.packages("car")
library(car)
vif(m1_log)

# 5.4.2. Examine multicollinearity (ToL < 0.1 indicated multicollinearity)
1/vif(m1_log)

# 5.4.3. Independence assumption: Durbin-Watson test. The value should be close to 2
dwt(m1_log)

# End of the lab -------------------------------------------------------------