# --------------------------------------------------------------------------- #
# Programmer:   Yu-Chih Chen, PhD                                             #
# Department:   Social Work & Social Administration                           # 
# Course:       SOWK2144 Introduction to Social Data Analysis                 #
# Lab 5:        LINEAR REGRESSION AND NON-PARAMETRIC ANALYSIS                 #
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

# Health data sets from Stata, two waves (2011 and 2013)
hlth1_long <-read_dta("HealthStatus11.dta")
hlth2_long <-read_dta("HealthStatus13.dta")  

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
names(hlth2_long)

# If you run into print maximum when you execute code for names(hlth2_long), use: 
options(max.print=999999)

# 1.2. First 6 observations in data
head(demo1)
head(hlth1_long)
head(hlth2_long)

# 1.3. Variable description in data
str(demo1)
str(hlth1_long)
str(hlth2_long)

# 2. VARIABLES SELECTION, RENAME, AND MERGE -----------------------------------
#
# For demo1, keep all variables 
# For hlth1 and hlth2, select the following variables:
#
# BELOW IS THE VARIABLE LISTS % % % % % % % % % % % % % % % % % % % % % % % % % 
#
# ID        householdID communityID da002     da005_1_  da005_3_  da005_4_  
# dc009     dc010       dc011       dc012     dc013     dc014     dc015       
# dc016     dc017       dc018
#
# END OF THE VARIABLE LISTS % % % % % % % % % % % % % % % % % % % % % % % % % %

# 2.1. To select variables, we use a package called dplyr

install.packages("dplyr")
library(dplyr)

# 2.2. Select variables
demo1 <- select(demo1,
                ID,        ba002_1,  rgender,  bd001)

hlth1 <- select(hlth1_long, 
                ID,        da002,        
                dc009,     dc010,    dc011,          dc012,    dc013,       
                dc014,     dc015,    dc016,          dc017,    dc018)

# You can use : to connect variables if the variables are constructed with consecutive numbers
hlth2 <- select(hlth2_long, 
                ID,        da002,    dc009:dc018)

# If you want to remove some data sets, remove the # sign below
# rm(hlth1_long, hlth2_long)

# 2.3. Rename variables to make them become meaningful (you will need delyr package to do so!)

# For demo1 data
demo1 <- demo1 %>% rename(edu_w1 = bd001     , gender = rgender,
                          byr_w1 = ba002_1)

# For hlth1 data
hlth1 <- hlth1 %>% rename(srh_w1 = da002     , 
                          dp1_w1 = dc009     , dp2_w1 = dc010,
                          dp3_w1 = dc011     , dp4_w1 = dc012,
                          dp5_w1 = dc013     , dp6_w1 = dc014,
                          dp7_w1 = dc015     , dp8_w1 = dc016,
                          dp9_w1 = dc017     , dp10_w1 = dc018)

# For hlth2 data
hlth2 <- hlth2 %>% rename(srh_w2 = da002     , 
                          dp1_w2 = dc009     , dp2_w2 = dc010,
                          dp3_w2 = dc011     , dp4_w2 = dc012,
                          dp5_w2 = dc013     , dp6_w2 = dc014,
                          dp7_w2 = dc015     , dp8_w2 = dc016,
                          dp9_w2 = dc017     , dp10_w2 = dc018)

# 2.3. Merge data 
#
# You will need an identifier to merge data sets, and this identifier should be 
# the same across the data sets. 
#
# all.x indicates the 1st data set (hlth1); all.y indicates the 2nd data set (hlth2)
# all.x = TRUE tells R to merge the 2nd data to the 1st data, based on the sample of 1st data
# all.y = TRUE tells R to merge the 1st data to the 2nd data, based on the sample of 2nd data  
#
total_w1 <- merge(hlth1,hlth2,by="ID", all.x = TRUE)
total_w2 <- merge(hlth1,hlth2,by="ID", all.y = TRUE)

total <- merge(demo1, total_w1, all.x = TRUE)

# Remove unnecessary packages and view the targeted data 
rm(demo1, hlth1, hlth1_long, hlth2, hlth2_long, total_w1, total_w2)
View(total)

# 3. EXAMINE VARIABLES AND RECODE ---------------------------------------------

# 3.1. Examine multiple variables using apply command

# If you want to know more about apply function
?apply

# Demographics
apply(total[c("byr_w1" , "edu_w1" , "gender")], 2, table)

# Health for 2011 wave
apply(total[c("srh_w1" ,  
              "dp1_w1" , "dp2_w1" , "dp3_w1" , "dp4_w1"  , "dp5_w1"  , 
              "dp6_w1" , "dp7_w1" , "dp8_w1" , "dp9_w1"  , "dp10_w1")], 2, table)

# Health for 2013 wave
apply(total[c("srh_w2" , 
              "dp1_w2" , "dp2_w2" , "dp3_w2" , "dp4_w2"  , "dp5_w2"  , 
              "dp6_w2" , "dp7_w2" , "dp8_w2" , "dp9_w2"  , "dp10_w2")], 2, table)

# 3.2. Select variables for use: age, gender, edu_w1, srh_w1, srh_w2, and dp1_w1:dp10_w1 
#                                to do data management 
# Age
#
# Gender
#
# Education: Keep original matric and recode the variable 
#
# Self-rated health 2011 & 2013: Reverse code it so that higher score indicate better health
#
# Depression 2013: reverse code dp5_w1 and dp8_w1 as they are positive-wording variables, 
#                  then combine all 10 variables

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

# 3.2.3. Self-rated health in 2011 and 2013
#
# 3.2.3.1. Examine data distribution 
table(total$srh_w1)
table(total$srh_w2)

str(total$srh_w1)
str(total$srh_w2)

# 3.2.3.2. Reverse code and create a new variable called srhr_w1 so that higher
#          score indicate better self-rated health
total$srhr_w1 <- 6-total$srh_w1
total$srhr_w2 <- 6-total$srh_w2

# 3.2.4. Depressive symptoms in 2013
#
# 3.2.4.1. Recode positive-wording variables 
total$dp5_w2 <- 5-total$dp5_w2
total$dp8_w2 <- 5-total$dp8_w2

# 3.2.4.2. Combine all variables 
total$dep_w2 = total$dp1_w2 + total$dp2_w2 + total$dp3_w2 + total$dp4_w2 +
               total$dp5_w2 + total$dp6_w2 + total$dp7_w2 + total$dp8_w2 +
               total$dp9_w2 + total$dp10_w2

# 3.3. Subset the data and select only age 60+

total_60 <- subset(total, age_w1>=60, )
summary(total_60$age_w1)
table(total_60$age_w1, useNA = "always")


# 4. DESCRIPTIVE STATISTICS ----------------------------------------------------
#
# 4.1. Basic mean and median

summary(total_60$age_w1)
summary(total_60$srhr_w1)
summary(total_60$srhr_w2)
summary(total_60$dep_w2)

table(total_60$gender, useNA = "always")
table(total_60$edu_w1, useNA = "always")
table(total_60$edu3_w1, useNA = "always")

# 4.1.1. More basic info (mean, SD, skewness) using "psych" packages

install.packages("psych")
library(psych)

describe(total_60$age_w1)
describe(total_60$srhr_w1)
describe(total_60$srhr_w2)
describe(total_60$dep_w2)

-- care about skewness 
if data like self rated health only have 5 values, not continuous (revise skewness not meaningful)

# 4.2. Assessing normality for continuous variables using Q-Q plot

install.packages("ggplot2")
library(ggplot2)

# Q-Q plot for age
qqplot.age <- qplot(sample = total_60$age_w1, stat="qq")
qqplot.age
--not normal data 


# Q-Q plot for self-rated health in 2011
qqplot.srh_w1 <- qplot(sample = total_60$srhr_w1, stat="qq")
qqplot.srh_w1
-- not normal data (consider using non-parmatic anaylsis, wrong if trated it as continuous data)

# Q-Q plot for self-rated health in 2013
qqplot.srh_w2 <- qplot(sample = total_60$srhr_w2, stat="qq")
qqplot.srh_w2

# Q-Q plot for depressive symptoms in 2013
qqplot.dep <- qplot(sample = total_60$dep_w2, stat="qq")
qqplot.dep

# 4.3 Histogram 
hist(total_60$age_w1)
hist(total_60$srhr_w1)
hist(total_60$srhr_w2)
hist(total_60$dep_w2)

# 4.4. Skewness and kurtosis (we use psych package we loaded before)
describe(cbind(total_60$age_w1, total_60$srhr_w1, total_60$srhr_w2, 
               total_60$edu_w1, total_60$dep_w2))

# 4.5. Kolmogorov-Smirnov test (K-S test)
install.packages("nortest")
library(nortest)

# K-S test. 
lillie.test(total_60$age_w1)
lillie.test(total_60$srhr_w1)
lillie.test(total_60$srhr_w2)
lillie.test(total_60$dep_w2)

--> perceive parmatic (give rationales) / non-parmatic (why you do this test decision)

# 5. LINEAR REGRESSION ----------------------------------------------------------------------

# IV: gender    (1 = male, 2 = female). Need dummy code
#     edu3_w1   (low, medium, high). Need dummy code
#     age_w1    Continuous variable of age
#     srhr_w1   Ordinal variable of self-rated health in 2011
#     srhr_w2   Ordinal variable of self-rated health in 2013
#
# DV: dep_w2    Continuous variable of depression in 2013
#
# 5.1 Dummy code for education and gender

# 5.1.1. Examine gender and education variables 
table(total_60$edu3_w1, useNA = "always")
table(total_60$gender, useNA = "always")

# 5.1.2. Dummy code for gender
total_60$male   <- ifelse(total_60$gender == '1', 1, 0)
total_60$female <- ifelse(total_60$gender == '2', 1, 0)

# 5.1.3. Dummy code for education 
total_60$edu_l <- ifelse(total_60$edu3_w1 == 'low', 1, 0)
total_60$edu_m <- ifelse(total_60$edu3_w1 == 'medium', 1, 0)
total_60$edu_h <- ifelse(total_60$edu3_w1 == 'high', 1, 0)

# 5.1.4. Examine data
head(cbind(total_60$gender, total_60$male, total_60$female))
--show the dummy code variable
table(total_60$gender, useNA = "always")
table(total_60$male, useNA = "always")
table(total_60$female, useNA = "always")

head(cbind(total_60$edu3_w1, total_60$edu_l, total_60$edu_m, total_60$edu_h))
as 327
table(total_60$edu3_w1, useNA = "always")
table(total_60$edu_l, useNA = "always")
table(total_60$edu_m, useNA = "always")
table(total_60$edu_h, useNA = "always")

# 5.2. Correlation (to test for linearity)

# Variables for testing: age_w1, srhr_w1, srhr_w2, dep_w2
#
# 5.2.1. Create a dataframe only includes continuous data
corr.num = total_60[c("age_w1", "srhr_w1", "srhr_w2" , "dep_w2")]

# 5.2.2. Produce Pearson correlation matrix
corr.test(corr.num,
          use    = "pairwise",
          method = "pearson",
          adjust = "none")
-- age and depression are correlated , but not significance, linear 

# 5.2.3. Show significance using package "PerformanceAnalytics"
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

chart.Correlation(corr.num,
                  method="pearson",
                  histogram=TRUE,
                  pch=16)

# 5.3. Multiple linear regression (Question: which category of education serves as reference?)
m1 <- lm(dep_w2 ~ age_w1 + female + edu_m + edu_h + srhr_w1 + srhr_w2, data = total_60)
summary(m1)

# 5.4. Regression assumptions
#
# 5.4.1. Examine multicollinearity (VIF > 10 indicated multicollinearity)
install.packages("car")
library(car)
vif(m1)

# 5.4.2. Examine multicollinearity (ToL < 0.1 indicated multicollinearity)
1/vif(m1)


# 5.4.3. Independence assumption: Durbin-Watson test. The value should be close to 2 and not significant
# The test is significant, which means we may violate the independence issues. Why? It is because of the
# data set structure. The CHARLS data collect older adults using multi-stage sampling
# (individuals<-household<-village<-city<-province). Therefore, older adults' health will be affected by
# their partner living in the same household and by those who living in the same village
dwt(m1)
just report the p value in the paper 

# 5.4.4. Equal variance 
hist(rstudent(m1)) 

# 6. NON-PARAMETRIC ANALYSIS ----------------------------------------------------------------
#
# 6.1. Chi-square analysis
#
# IV: gender
# DV: edu3_w1 
#
#
install.packages("gmodels")
library(gmodels)
CrossTable(total_60$gender, total_60$edu3_w1, chisq = TRUE)

# 6.2. Wilcoxon rank-sum test
#
# IV: gender
# DV: srhr_w1
#

# 6.2.1. Compare mean and median by gender
by(total_60$srhr_w1, total_60$gender, describe)

# 6.2.2. Wilcoxon rank sum test
wrs <- wilcox.test(srhr_w1 ~ gender, data = total_60, paired = FALSE)
wrs

# 6.3. Wilcoxon signed-rank test
#
# Time1: srhr_w1
# Time2: srhr_w1
#
# 6.3.1. Obtain median for time1 and time2
summary(total_60$srhr_w1)
summary(total_60$srhr_w2)

# 6.3.2. Wilcoxon signed-rank test
wsr <- wilcox.test(total_60$srhr_w1, total_60$srhr_w2, paired = TRUE)
wsr

# 6.4. Kruskal-Wallis test
#
# IV: edu3_w1
# DV: dep_w2
#
# 6.4.1. Obtain mean rank for each category in education
total_60$dep_rank <- rank(total_60$dep_w2)
by(total_60$dep_rank, total_60$edu3_w1, mean)

# 6.4.2. Kruskal-Wallis test
kw <- kruskal.test(dep_w2 ~ edu3_w1, data = total_60)
kw

# 6.4.3. Post-hoc test
install.packages("FSA")
library(FSA)
dunnTest(dep_w2 ~ edu3_w1, data = total_60, method = "holm")
#dunnTest(dep_w2 ~ edu3_w1, data = total_60, method = "bh")

# 6.5. Spearman correlation
#
# IV: edu_w1, age_w1
# DV: srhr_w2
#

# 6.5.1. Create a dataframe include ordinal data
corr.ord = total_60[c("age_w1", "srhr_w2", "edu_w1")]

# 6.2. Produce Spearman correlation matrix
corr.test(corr.ord,
          use    = "pairwise",
          method = "spearman",
          adjust = "none")

# 6.3. Show signifiance using package "PerformanceAnalytics"
chart.Correlation(corr.ord,
                  method="spearman",
                  histogram=TRUE,
                  pch=16)

# End of the lab -------------------------------------------------------------