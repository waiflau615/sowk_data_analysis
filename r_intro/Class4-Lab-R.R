# --------------------------------------------------------------------------- #
# Programmer:   Yu-Chih Chen, PhD                                             #
# Department:   Social Work & Social Administration                           # 
# Course:       SOWK2144 Introduction to Social Data Analysis                 #
# Lab 2:        DESCRIPTIVE ANALYSIS, NORMALITY, AND T-TEST                   #
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

# Health data sets from Stata, two waves (2011 and 2013)
hlth1_long <-read_dta("HealthStatus11.dta")
hlth2_long <-read_dta("HealthStatus13.dta")

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
hlth1 <- select(hlth1_long, 
                ID,        da002,    da005_1_,       da005_3_, da005_4_,    
                dc009,     dc010,    dc011,          dc012,    dc013,       
                dc014,     dc015,    dc016,          dc017,    dc018)

# You can use : to connect variables if the variables are constructed with consecutive numbers
hlth2 <- select(hlth2_long, 
                ID,        da002,    da005_1_,       da005_3_, da005_4_,    
                dc009:dc018)

# If you want to remove some data sets, remove the # sign below
rm(hlth1_long, hlth2_long)

# 2.3. Rename variables to make them become meaningful (you will need dplyr package to do so!)

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

# For hlth2 data
hlth2 <- hlth2 %>% rename(srh_w2 = da002     , phy_w2 = da005_1_, 
                          vis_w2 = da005_3_  , ear_w2 = da005_4_,
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
apply(total[c("byr_w1" , "edu_w1" , "mar_w1" , "mat_w1"  , "gender")], 2, table)


# Health for 2011 wave
apply(total[c("srh_w1" , "phy_w1" , "vis_w1" , "ear_w1"  , 
              "dp1_w1" , "dp2_w1" , "dp3_w1" , "dp4_w1"  , "dp5_w1"  , 
              "dp6_w1" , "dp7_w1" , "dp8_w1" , "dp9_w1"  , "dp10_w1")], 2, table)

# Health for 2013 wave
apply(total[c("srh_w2" , "phy_w2" , "vis_w2" , "ear_w2"  , 
              "dp1_w2" , "dp2_w2" , "dp3_w2" , "dp4_w2"  , "dp5_w2"  , 
              "dp6_w2" , "dp7_w2" , "dp8_w2" , "dp9_w2"  , "dp10_w2")
              ], 2, table)


# 3.2. Select variables for use: gender, dp1_w1:dp10_w1, & dp1_w2:dp10_w2 and do data management 
#
# Gender: should only have values of 1 and 2, but there is a 0 --> need to treat it as missing
#
# Depression score: in both wave 1 and wave 2. Need to construct them by summing them up
#

# 3.2.1. Gender 
# Force gender with a value 0 become missing (NA) 
total$gender[total$gender == "0"]<-NA

# Another way to examine variable distribution using table function.
# Note. table function can only be used for single (one) measure, not multiple measures
# Note. prop.table function gives you the proportion
table(total$gender)
prop.table(table(total$gender))

m.prop = 8471/(8471+9221)
m.prop

f.prop = 9221/(8471+9221)
f.prop

# 3.2.2. Depression score 

# 3.2.2.1. Recode two measures from depression scale. In the original scale, there
#          are two positive-wording measures which are different than others 
#          (i.e., negative wording). We have to reverse code hopeful (dp5) and  
#          happy (dp8)

# Examine the distribution
apply(total[c("dp5_w1" , "dp5_w2" , "dp8_w1" , "dp8_w2")], 2, table)

# Now we will recode this variable. 
total$dp5_w1 <- 5-total$dp5_w1
total$dp5_w2 <- 5-total$dp5_w2
total$dp8_w1 <- 5-total$dp8_w1
total$dp8_w1 <- 5-total$dp8_w2

# Examine the distribution again
apply(total[c("dp5_w1" , "dp5_w2" , "dp8_w1" , "dp8_w2")], 2, table)

# 3.3. Data management for gender and depression 
#
# 3.3.1. Classify gender into a categorical variable 
#
# Now gender is a numeric variable, not a categorical variable. How do we know? 
str(total$gender)
is.character(total$gender)

# Classfy it into categorical/factor variable using recode command
total$gender <- recode(total$gender, "1" = "M", "2" = "F")
str(total$gender)
is.character(total$gender)

# 3.3.2. Combine depression score for 2011 (w1) and 2013 (w2). I show you two ways:
#
# Method A for 2011 (w1)

total$dep_w1a = total$dp1_w1 + total$dp2_w1 + total$dp3_w1 + total$dp4_w1 +
                total$dp5_w1 + total$dp6_w1 + total$dp7_w1 + total$dp8_w1 +
                total$dp9_w1 + total$dp10_w1

# Method B for 2011 (w1)
total$dep_w1 <- rowSums(total[ ,c( "dp1_w1" , "dp2_w1" , "dp3_w1" , "dp4_w1" , 
                                   "dp5_w1" , "dp6_w1" , "dp7_w1" , "dp8_w1" , 
                                   "dp9_w1" , "dp10_w1")])

# Compare A & B for 2011 (w1). They produce exact the same results 
summary(total$dep_w1a)
summary(total$dep_w1)

# Remove dep_w1a from the data to avoid confusion
total$dep_w1a <- NULL

# Why not use rm() to remove? rm() removes objects from the environment, but in
# here we want to remove a variable from the data frame (i.e., total), therefore
# we have to use NULL to exclude that variable from the data

# Use method B for 2013 (w2)
total$dep_w2 <- rowSums(total[ ,c("dp1_w2" , "dp2_w2" , "dp3_w2" , "dp4_w2" , 
                                  "dp5_w2" , "dp6_w2" , "dp7_w2" , "dp8_w2" , 
                                  "dp9_w2" , "dp10_w2")])

# 4. DATA ANALYSIS ------------------------------------------------------------
#
# 4.1. Central tendency: mean and median
#
# You can use summary() function to get basic mean, median
summary(total$dep_w1)
summary(total$dep_w2)

# 4.1.1. You can use some packages to get descriptive stats for multiple variables
# One of useful packages called "summarytools"

install.packages("summarytools")
library(summarytools)

# descr
descr(total, stats = "common")

# dfsummary
dfSummary(total)

# 4.1.2. psych package
install.packages("psych")
library(psych)
describe(total$dep_w1)

# 4.2. Central tendency: Mode 
mode_dep_w1 <- table(total$dep_w1)        # number of occurrences for each unique value
sort(mode_dep_w1, decreasing = TRUE)      # sort highest to lowest. Mode = 13

# 4.3. Variability: range and standard deviation
descr(total$dep_w1, stats = "common")
descr(total$dep_w2, stats = "common")

# 4.4. Assessing normality for continuous variables 
#
# 4.4.1. Q-Q plot
install.packages("ggplot2")
library(ggplot2)

# 4.4.1.1. Depression at w1
qqplot.dep_w1 <- qplot(sample = total$dep_w1, stat="qq")
qqplot.dep_w1

# If you want to save your plot, remove # from below
# ggsave(file = paste(imageDirectory,"Depression in 2011.png",sep="/"))

# 4.4.1.2. Depression at w2
qqplot.dep_w2 <- qplot(sample = total$dep_w2, stat="qq")
qqplot.dep_w2

# 4.4.2. Histogram 
hist(total$dep_w1)
hist(total$dep_w2)

# 4.4.3. Skewness and kurtosis (we use psych package we loaded before)
describe(cbind(total$dep_w1, total$dep_w2))

# 4.4.4. Kolmogorov-Smirnov test (K-S test)
install.packages("nortest")
library(nortest)

# K-S test. What did you find?
lillie.test(total$dep_w1)
lillie.test(total$dep_w2)

# 4.4.5. Shapiro-Wilk test (S-W test) Doesn't work. We got large N
shapiro.test(total$dep_w1)
shapiro.test(total$dep_w2)

# 5. INDEPENDENT T TEST AND PAIRED T TEST ------------------------------------
#
# 5.1 Indenpendent t test: one categorical IV and one continuous DV
#     IV: gender; DV: depression (w1)

# 5.1.1. Lavene's test: make sure the distribution of depression between males
#        and females is the same. To do levene's test, you need to car package
install.packages("car")
library(car)

# Lavene's test. It is significant. What does it mean? Distribution is different!
leveneTest(total$dep_w1, total$gender, center = median)

# If we examine their SD. We see the difference. However, we know that independent 
# t test is a robust test, so we can still proceed!
by(total$dep_w1, total$gender, describe)

# Variance = SD*SD. VR = 1.29
F_var = 6.38*6.38
M_var = 5.61^2
VR = F_var/M_var
VR

# 5.1.2. Independent t test
t.test(dep_w1 ~ gender, var.equal = FALSE, data = total)     # Why we don't need to use total$dep_w1?
                                                             # why we need to use var.equal = FALSE?

t.test(dep_w1 ~ gender, var.equal = TRUE, data = total)      # what's the difference between the two?

# 5.1.3. Paired t test
describe(cbind(total$dep_w1, total$dep_w2))
t.test(total$dep_w1, total$dep_w2, paired = TRUE)

# End of the lab -------------------------------------------------------------