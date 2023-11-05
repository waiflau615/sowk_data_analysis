# --------------------------------------------------------------------------- #
# Programmer:   Yu-Chih Chen, PhD                                             #
# Department:   Social Work & Social Administration                           # 
# Course:       SOWK2144 Introduction to Social Data Analysis                 #
# Lab 1:        Introduction to R environment                                 #
# --------------------------------------------------------------------------- #

# R demonstration -------------------------------------------------------------

###############################################################################

# 1. Vector 
#
# 1.A. Create a numeric vector (i.e., object) for age, pre.weight, and post.weight

age <- c(25,33,30,40,28) 
pre.weight <- c(65, 68, 54, 70, 89)
post.weight <- c(63, 60, 60, 71, 86)

# 1.B. Create string variables for hobby and animal

hobby <- c("Reading","Sports","Games","Reading","Games") 
animal <- c("Elephant", "Giraffe", NA, "Monkey", "Cat") 

###############################################################################

# 2. List 
#
# 2.A. Let's combine age and hobby together and make a list called list1

list1 <- list(age, hobby)
list1

# 2.B. Let's combine age and hobby together and make a list called list2, but 
#      assign x1 for age and x2 for hobby

list2 <- list(x1=age, x2=hobby)
list2

# Question 1: Any differences between list1 and list2?

###############################################################################

# 3. Matrices (creating from numeric variables) using cbind and rbind
#
# 3.A. use "cbind" (vector as column)
mxc <- cbind(age, pre.weight, post.weight)
mxc

# 3.B. use "rbind" (vector as row)
mxr <- rbind(age, pre.weight, post.weight)
mxr

###############################################################################

# 4. Remove unwanted objects using remove function (rm)

rm(mxc, mxr, list1, list2)

###############################################################################

# 5. Data frame
#
# Let's create a data frame called dat1 that include age, pre.weight, post.weight
# hobby, and animal

dat1 <- data.frame(age, pre.weight, post.weight, hobby, animal)
dat1

# Question 2: Any differences between dat1 and mxc?

###############################################################################

# 6. Selection
#
# 6.A. Row and column approach. Select age>=30 & keep all variables

dat2a <- dat1[age>=30, ]
dat2a

# 6.B. Subset approach. Select age>=30 & keep all variables 

dat2b <- subset(dat1, age>=30, )
dat2b

# 6.C. Row and column approach. Select age>=30. Keep age and hobby variables

dat2c <- dat1[age>=30, c("age", "hobby")]
dat2c

# 6.D. Subset approach. Select age>=40 "AND" pre.weight>=65. Keep age, pre.weight, and animal 

dat2d <- subset(dat1, age>=40 & pre.weight>=65, select = c("age", "pre.weight" , "animal"))
dat2d

# 6.E. Row and column approach. Select age>=40 "OR" pre.weight >=65). keep age, pre.weight, & hobby 
dat2e <- dat1[age>=40 | pre.weight>=65, c("age", "pre.weight" , "hobby")]
dat2e

# Question 3: what makes the differences between dat2d and dat2e?

###############################################################################

# 7. Index

# Indicating/showing data with specific variable (vector)
dat1
dat1$age

# Indicating/showing data with specific criterion
dat1[dat1$age>=30, ]

# Create a new data using index approach
ind1 <- dat1[dat1$age>=30, ]
ind1

###############################################################################

# 8. Read outside data sets 
#
# To do: (1) Download data for lab1 from Moodle, and then unzipped it
#        (2) put everything in your designated folder, and copy the folder path.
#            Folder path may be varied by either Windows or Mac system!
#
# 8.1. Install related R packages: 
#      For SPSS, Stata, SAS: haven 
#      For Excel: readxl
#      For text and csv: readr

install.packages("haven")
install.packages("readxl")
install.packages("readr")

# 8.2. Load packages. You will need to load packages every time when you open R

library(haven)
library(readxl)
library(readr)

# 8.3. Set working directory (Note. path in this demo is from Windows system)
#      Mac user: setwd("~Dropbox/HKU/Syllabus/SOWK2144-Intro to Social Data Analysis/Data")
#   
#      ALWAYS use forward slash (/), NOT backslash(\) for folder path!
#
#      getwd() helps you to check the folder path

setwd("C:/Users/Chen Yu-chih/Dropbox/HKU/Syllabus/SOWK2144-Intro to Social Data Analysis/Data")
getwd()

# 8.4. Use package function to search haven package, and then browse the package (click earth sign) to learn codes

# 8.5. Read SPSS file

dat.spss <- read_sav("Demographic11_short_spss.sav")
View(dat.spss)

# If you don't set working directory, you will need to type:
# dat.spss <- read_sav("C:/Users/Chen Yu-chih/Dropbox/HKU/Syllabus/SOWK2144-Intro to Social Data Analysis/Data/Demographic11_short_spss.sav")

# 8.6. Read SAS file

dat.sas <- read_sas("Demographic11_short_sas.sas7bdat")
View(dat.sas)  

# 8.7. Read Stata file (Note. haven only support Stata version 15 or lower!)

dat.stata <- read_dta("Demographic11_short_stata.dta")
View(dat.stata)

# 8.8. Read Excel file

dat.excel <- read_excel("Demographic11_short_excel.xlsx")
View(dat.excel)

# 8.9. Read dat file (header = variable name. If you have this row, put header = TRUE
#      the default is header = FLASE)

dat.dat <- read.delim("Demographic11_short_dat.dat", header = TRUE)
View(dat.dat)

# 8.10. Read csv file 

dat.csv <- read.csv("Demographic11_short_csv.csv", header = TRUE)
View(dat.csv)

###############################################################################

# 9. Examine the data structure 

str(dat.spss)
head(dat.stata)

###############################################################################

# 10. Remove everything (working environment, ls) we just created 

rm(list=ls())
