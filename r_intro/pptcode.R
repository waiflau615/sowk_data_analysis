getwd()
library(haven)
sample <- read_dta("Sample_Infor.dta")
sample_infor <-read_dta("Sample_Infor.dta")
weight <- read_dta("Weights.dta")
Demograph <- read_dta("Demographic_Background.dta")
informats <- read_dta("Insider.dta")
Cognition <- read_dta ("Cognition.dta")
Health_status <- read_dta ("Health_Status_and_Functioning.dta")
Family_Transfer <- read_dta ("Family_Transfer.dta")
str(Cognition)
head(Cognition)
library(dplyr)
Cognition_final <- select(Cognition, ID, householdID,   communityID, dc009,
                           dc010, dc011, dc012, dc013, dc014, dc015, dc016,
                          dc017, dc018)
Healthstatus_final <- select (Health_status, ID, householdID, 
                              communityID, db001, db002,  
                              db003, 
                              db004, db005, db006, db007, 
                              db008, db009, db010,
                              db010_w2, db011, db011_w2, db012, db012_w2, 
                              db013, db013_w2, db014, db014_w2, db015,
                              db016, db016_w2, db017, 
                              db017_w2, db018, db018_w2, db035, db035_w2, 
                              db020, db020_w2, db019, db019_w2)
Health_days <- select(Health_status, ID, householdID, community ID,
                      
                 
                      