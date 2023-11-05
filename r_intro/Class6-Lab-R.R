# --------------------------------------------------------------------------- #
# Programmer:   Yu-Chih Chen, PhD                                             #
# Department:   Social Work & Social Administration                           # 
# Course:       SOWK2144 Introduction to Social Data Analysis                 #
# Lab 4:        CORRELATION (PEARSON, SPEARMAN, NON-LINEAR)                   #
# --------------------------------------------------------------------------- #

# Note: data source: Class 4 data

# R demonstration -------------------------------------------------------------

###############################################################################

# In case you run into issues with package ellipsis execute the following two lines 
# remove.packages("ellipsis")
# install.packages("ellipsis")

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

# Health data sets from Stata, 2011 wave
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

# 2. VARIABLES SELECTION, RENAME, AND MERGE -----------------------------------
#
# For demo1, keep all variables 
# For hlth1, select the following variables:
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
hlth1 <- select(hlth1_long, 
                ID,        da002,    da005_1_,       da005_3_, da005_4_,    
                dc009:dc018)

# If you want to remove some data sets, remove the # sign below
# rm(hlth1_long)

# 2.3. Rename variables to make them become meaningful (you will need delyr package to do so!)

# For demo1 data
demo1 <- demo1 %>% rename(byr_w1 = ba002_1   , edu_w1 = bd001,
                          mar_w1 = be001     , mat_w1 = be003,
                          gender = rgender)

# For hlth1 data
hlth1 <- hlth1 %>% rename(srh_w1 = da002     , phy_w1 = da005_1_, 
                          vis_w1 = da005_3_  , ear_w1 = da005_4_,
                          dp1_w1 = dc009     , dp2_w1 = dc010,
                          dp3_w1 = dc011     , dp4_w1 = dc012,
                          dp5_w1 = dc013     , dp6_w1 = dc014,
                          dp7_w1 = dc015     , dp8_w1 = dc016,
                          dp9_w1 = dc017     , dp10_w1 = dc018)

# 2.4. Merge data 
#
# You will need an identifier to merge data sets, and this identifier should be 
# the same across the data sets. 
#
# all.x indicates the 1st data set (demo1); all.y indicates the 2nd data set (hlth1)
# all.x = TRUE tells R to merge the 2nd data to the 1st data, based on the sample of 1st data
# all.y = TRUE tells R to merge the 1st data to the 2nd data, based on the sample of 2nd data  
#

total <- merge(demo1, hlth1, by="ID", all.x = TRUE)

# Remove unnecessary data frame and view the targeted data 
rm(demo1, hlth1, hlth1_long)
View(total)
str(total)


# 3. EXAMINE VARIABLES AND RECODE ---------------------------------------------

# 3.1. Examine multiple variables using apply command

# If you want to know more about apply function
?apply

# Demographics
apply(total[c("byr_w1" , "edu_w1")], 2, table)


# Health for 2011 wave
apply(total[c("srh_w1" , 
              "dp1_w1" , "dp2_w1" , "dp3_w1" , "dp4_w1"  , "dp5_w1"  , 
              "dp6_w1" , "dp7_w1" , "dp8_w1" , "dp9_w1"  , "dp10_w1")], 2, table)

# 3.2. Select variables for use: byr_w1, edu_w1 & srh_w1 and dp1_w1:dp10_w1 to do data management 
#
# Age: Created from 2011 - birth year (byr_w1) as the data is collected in 2011
#
# Education: Keep original matric
#
# Self-rated health: Reverse code it so that higher score indicate better health
#
# Depression: reverse code dp5_w1 and dp8_w1 as they are positive-wording variables, 
#             then combine all 10 variables

# 3.2.1. Age
# Create age variable by using 2011 - birth year
total$age_w1 <- 2011-total$byr_w1

# Show distribution for birth year and age
table(total$byr_w1, useNA = "always")
table(total$age_w1, useNA = "always")

# 3.2.2. Education: doing nothing; keep its original matric 
table(total$edu_w1, useNA = "always")

# 3.2.3. Self-rated health
#
# 3.2.3.1. Examine data distribution 
table(total$srh_w1)
str(total$srh_w1)

# 3.2.3.2. Reverse code and create a new variable called srhr_w1 so that higher
#          score indicate better self-rated health
total$srhr_w1 <- 6-total$srh_w1

# 3.2.3.3. Examine these two variables 
table(total$srh_w1)
table(total$srhr_w1)

# 3.2.4. Depressive symptoms
#
# 3.2.4.1. Recode positive-wording variables 
total$dp5_w1 <- 5-total$dp5_w1
total$dp8_w1 <- 5-total$dp8_w1

# 3.2.4.2. Combine all variables 
total$dep_w1 = total$dp1_w1 + total$dp2_w1 + total$dp3_w1 + total$dp4_w1 +
               total$dp5_w1 + total$dp6_w1 + total$dp7_w1 + total$dp8_w1 +
               total$dp9_w1 + total$dp10_w1

# 3.3. Subset the data and select only age 60+

total_60 <- subset(total, age_w1>=60, )
summary(total_60$age_w1)
table(total_60$age_w1, useNA = "always")


# 4. DESCRIPTIVE STATISTICS ----------------------------------------------------
#
# 4.1. Basic mean and median

summary(total_60$age_w1)
summary(total_60$srhr_w1)
summary(total_60$dep_w1)
summary(total_60$edu_w1)
table(total_60$edu_w1, useNA = "always")

# 4.1.1. More basic info (mean, SD, skewness) using "psych" packages

install.packages("psych")
library(psych)

describe(total_60$age_w1)
describe(total_60$srhr_w1)
describe(total_60$dep_w1)
describe(total_60$edu_w1)

# 4.2. Assessing normality for continuous variables using Q-Q plot

install.packages("ggplot2")
library(ggplot2)

# Q-Q plot for age
qqplot.age <- qplot(sample = total_60$age_w1, stat="qq")
qqplot.age

# Q-Q plot for self-rated health
qqplot.srh <- qplot(sample = total_60$srhr_w1, stat="qq")
qqplot.srh

# Q-Q plot for depressive symptoms
qqplot.dep <- qplot(sample = total_60$dep_w1, stat="qq")
qqplot.dep

# Q-Q plot for education
qqplot.edu <- qplot(sample = total_60$edu_w1, stat="qq")
qqplot.edu


# 4.3 Histogram 
hist(total_60$age_w1)
hist(total_60$srhr_w1)
hist(total_60$dep_w1)
hist(total_60$edu_w1)

# 4.4. Skewness and kurtosis (we use psych package we loaded before)
describe(cbind(total_60$age_w1, total_60$srhr_w1, total_60$dep_w1, total_60$edu_w1))

# 4.5. Kolmogorov-Smirnov test (K-S test)
install.packages("nortest")
library(nortest)

# K-S test. What did you find?
lillie.test(total_60$age_w1)
lillie.test(total_60$srhr_w1)
lillie.test(total_60$dep_w1)
lillie.test(total_60$edu_w1)

# 5. PEARSON CORRELATION --------------------------------------------------------------------
#
# VAR1: Education levels
# VAR2: Age
# VAR3: Depressive symptoms
#
# 5.1 Scatter plot

# 5.1.1. Scatter plot, pair 
#
# 5.1.1.1. Age and depressive symptoms 
qplot(age_w1, dep_w1, data=total_60,
      ylab="Depressive symptoms", xlab="Age",
      main="Relationship between age and depressive symptoms") +
      geom_smooth(method="lm")

# 5.1.1.2. Age and self-rated health 
qplot(age_w1, srhr_w1, data=total_60,
      ylab="Self-rated health", xlab="Age",
      main="Relationship between age and self-rated health") +
      geom_smooth(method="lm")

# 5.1.1.3. Self-rated health and depressive symptoms 
qplot(srhr_w1, dep_w1, data=total_60,
      ylab="Depressive symptoms", xlab="Self-rated health",
      main="Relationship between elf-rated health and depressive symptoms") +
      geom_smooth(method="lm")

# 5.1.2. Another way to produce scatter plot with multiple variables 
pairs(data=total_60, ~ age_w1 + srhr_w1 + dep_w1)

# 5.2 Correlation matrix 

# 5.2.1. Create a dataframe only includes continuous data
corr.num = total_60[c("age_w1", "srhr_w1", "dep_w1")]

# 5.2.2. Produce Pearson correlation matrix
corr.test(corr.num,
          use    = "pairwise",
          method = "pearson",
          adjust = "none")

# 5.2.3. Show signifiance using package "PerformanceAnalytics"
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

chart.Correlation(corr.num,
                  method="pearson",
                  histogram=TRUE)

# 6. SPEARMAN CORRELATION --------------------------------------------------------------------
#
# 6.1. Create a dataframe include ordinal data
corr.ord = total_60[c("age_w1", "srhr_w1", "dep_w1" , "edu_w1")]

# 6.2. Produce Spearman correlation matrix
corr.test(corr.ord,
          use    = "pairwise",
          method = "spearman",
          adjust = "none")

# 6.3. Show signifiance using package "PerformanceAnalytics"
chart.Correlation(corr.ord,
                  method="spearman",
                  histogram=TRUE)

# 7. Linear and non-linear correlation 
#
# 7.1. Create non-linear terms for age 
total_60$age2_w1 <- total_60$age_w1^2
total_60$age3_w1 <- total_60$age_w1^3

# 7.2. Test both linear and non-linear relationship, step by step
# 7.2.1. Linear
m1 <- lm(srhr_w1 ~ age_w1, data=total_60)
summary(m1)

# 7.2.2. Quadratic
m2 <- lm(srhr_w1 ~ age_w1 + age2_w1, data=total_60)
summary(m2)

# 7.2.3. Cubic
m3 <- lm(srhr_w1 ~ age_w1 + age2_w1 + age3_w1, data=total_60)
summary(m3)

# End of the lab -------------------------------------------------------------