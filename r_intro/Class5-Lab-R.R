# --------------------------------------------------------------------------- #
# Programmer:   Yu-Chih Chen, PhD                                             #
# Department:   Social Work & Social Administration                           # 
# Course:       SOWK2144 Introduction to Social Data Analysis                 #
# Lab 3:        RECODE, DATA TRANSFORMATION, AND ANOVA (F-TEST)               #
# --------------------------------------------------------------------------- #

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

# 2.3. Merge data 
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
apply(total[c("byr_w1" , "edu_w1" , "mar_w1" , "mat_w1"  , "gender")], 2, table)


# Health for 2011 wave
apply(total[c("srh_w1" , "phy_w1" , "vis_w1" , "ear_w1"  , 
              "dp1_w1" , "dp2_w1" , "dp3_w1" , "dp4_w1"  , "dp5_w1"  , 
              "dp6_w1" , "dp7_w1" , "dp8_w1" , "dp9_w1"  , "dp10_w1")], 2, table)

# 3.2. Select variables for use: byr_w1, edu_w1 & srh_w1 and do data management 
#
# Age: Created from 2011 - birth year (byr_w1) as the data is collected in 2011
#
# Education: Recode it into three levels (low = 1,2,3,4; Medium = 5,6,7; High = 8,9,10)
#
# Self-rated health: Reverse code it so that higher score indicate better health
#

# 3.2.1. Age
# Create age variable by using 2011 - birth year
total$age_w1 <- 2011-total$byr_w1

# Show distribution for birth year and age
table(total$byr_w1, useNA = "always")
table(total$age_w1, useNA = "always")

# 3.2.2. Education 

# 3.2.2.1. Examine the distribution and types of variable
table(total$edu_w1, useNA = "always")
str(total$edu_w1)
is.character(total$edu_w1)

# 3.2.2.2. Recode it into three levels (low = 1,2,3,4; Medium = 5,6,7; High = 8,9,10)
#
# 3.2.2.2.1. Method 1 (the easiest) for recoding this variable I set floor (0) 
#            and ceiling (10) as I only want to recode values fall between
#            in this range, that is because for some data set they will set 
#            some impossible values as missing (e.g., 9999 or -9999). So it is always
#            safe by setting the boundary. For example, if missing = 99999, and 
#            you only put total$edu_w1 > 7, you will group people with values  
#            > 7 and missing together. In this data set, the missing is set as
#            an empty cell or a dot (.) so we don't need to worry about this

total$edu3a_w1[total$edu_w1 >=0 & total$edu_w1 <= 4] <- "low"
total$edu3a_w1[total$edu_w1 > 4 & total$edu_w1 <= 7] <- "medium"
total$edu3a_w1[total$edu_w1 > 7 & total$edu_w1 <= 10] <- "high"

table(total$edu3a_w1, useNA = "always")
str(total$edu3a_w1)

# 3.2.2.2.2. Method 2 (recommended!! Make it become a factor variable)
total$edu3_w1 <- cut(total$edu_w1,
                     breaks = c(0, 4, 7, 10),
                     labels = c("low", "medium", "high")
                     )

table(total$edu3a_w1, useNA = "always")
table(total$edu3_w1, useNA = "always")

# Remove edu3a_w1 from the data to avoid confusion
total$edu3a_w1 <- NULL

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

# 3.3. Subset the data and select only age 60+

column empty - want to include all data, like missing value
total_60 <- subset(total, age_w1>=60, )
total_60
summary(total_60$age_w1)
table(total_60$age_w1, useNA = "always")

# 3.3.1. Subset using row & column approach. Q: Any differences between the two?
total_60a <- total[total$age_w1>=60, ]
summary(total_60a$age_w1)
table(total_60a$age_w1, useNA = "always")

# 4. DESCRIPTIVE STATISTICS ----------------------------------------------------
#
# 4.1. Basic mean and median

summary(total_60$age_w1)
summary(total_60$srhr_w1)
table(total_60$edu3_w1, useNA = "always")

# 4.1.1. More basic info (mean, SD, skewness) using "psych" packages

install.packages("psych")
library(psych)

describe(total_60$age_w1)
describe(total_60$srhr_w1)

# 4.2. Assessing normality for continuous variables using Q-Q plot

install.packages("ggplot2")
library(ggplot2)

# Q-Q plot for age
qqplot.age <- qplot(sample = total_60$age_w1, stat="qq")
qqplot.age

# Q-Q plot for self-rated health
qqplot.srh <- qplot(sample = total_60$srhr_w1, stat="qq")
qqplot.srh

# 4.3 Histogram 
hist(total_60$age_w1)
hist(total_60$srhr_w1)

# 4.4. Skewness and kurtosis (we use psych package we loaded before)
describe(cbind(total_60$age_w1, total_60$srhr_w1))

# 4.5. Kolmogorov-Smirnov test (K-S test)
install.packages("nortest")
library(nortest)

# K-S test. What did you find?
lillie.test(total_60$age_w1)
lillie.test(total_60$srhr_w1)

# 5. ANOVA --------------------------------------------------------------------
# the mean difference between each others

# IV: Education levels (low, meduim, high)
# DV: 1. Self-rated health (reverse coded)
#     2. Age
#
# 5.1 Descriptive (mean and SD) and normality test (Q-Q plot) using "ggpubr" package
#
# 5.1.1. Descriptive analysis

# Get mean by group
by(total_60$age_w1, total_60$edu3_w1, describe)

It assume equal variable
#VR= largest Var / smallest Var
VR_age = 7.26^2/5.81^2
VR_age

by(total_60$srhr_w1, total_60$edu3_w1, describe)
VR_srh = 0.92^2/0.88^2
VR_srh


# 5.1.2. QQ plot "BY each category"
install.packages("ggpubr")
library(ggpubr)

ggqqplot(total_60, "age_w1", facet.by = "edu3_w1")
ggqqplot(total_60, "srhr_w1", facet.by = "edu3_w1")

# 5.2. Equal variance test using Lavene's test; you need to "car" package
install.packages("car")
library(car)

# 5.2.1. Equal variance in age, by education
leveneTest(total_60$age_w1, total_60$edu3_w1, center = median)

# 5.2.2. Equal variance in self-rated health, by education
leveneTest(total_60$srhr_w1, total_60$edu3_w1, center = median)

# 5.3. Conduct F test
#
# 5.3.1. Age (DV) by education (IV)
# Assuming equal variance (which is not true based on prior tests)
F_age <- aov(age_w1 ~ edu3_w1, data = total_60)
summary(F_age)

# Welch Test(correct unequal variance)
oneway.test(age_w1 ~ edu3_w1, data = total_60)

# 5.3.2. Self-rated health (DV) by education (IV)
# Assuming equal variance (which is not true based on prior tests)
F_srh <- aov(srhr_w1 ~ edu3_w1, data = total_60)
summary(F_srh)

# Welch Test
oneway.test(srhr_w1 ~ edu3_w1, data = total_60)

# What do f-tests tell you about the comparisons across mean?
# Descriptive stats of age by 3 education levels
by(total_60$age_w1, total_60$edu3_w1, describe)

# Descriptive stats of self-rated health by 3 education levels
by(total_60$srhr_w1, total_60$edu3_w1, describe)

# 5.4. Post-hoc test 
#
# 5.4.1. Post-hoc test for age (DV) by education (IV)
install.packages("multcomp")
library(multcomp)

# The multicomp uses data created by "aov" command (assuming equal variance)

post_age <- glht(F_age, linfct = mcp(edu3_w1 = "Tukey"))
summary(post_age)
by(total_60$age_w1, total_60$edu3_w1, describe)

# 5.4.2. Post-hoc test for self-rated health (DV) by education (IV)
post_srh <- glht(F_srh, linfct = mcp(edu3_w1 = "Tukey"))
summary(post_srh)
by(total_60$srhr_w1, total_60$edu3_w1, describe)

# End of the lab -------------------------------------------------------------