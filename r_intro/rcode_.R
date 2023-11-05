library(dplyr)
library(car)
Cognition_final <- select(Cognition, ID, householdID,   communityID,
                          dc009: dc018)
Demograph_final <- select (Demograph, ID, householdID, communityID,
                           ba000_w2_3, ba004_w3_1, bd001_w2_4, bd001_w3_1)
informats_final <- select (informats, ID, householdID,inf1_rtr, 
                           inf2_rtr, inf3_rtr, dd001_w4: dd011_w4)
Cognition_final <- Cognition_final %>% rename(dp1 = dc009, dp2 = dc010,
                                              dp3 = dc011, dp4 = dc012,
                                              dp5 = dc013, dp6 = dc014,
                                              dp7 = dc015, dp8 = dc016,
                                              dp9 = dc017, dp10 = dc018)
CESD <- na.omit(Cognition_final)
CESD_final <- filter(CESD, dp1 <= 4 & dp2 <= 4 
                     & dp3 <= 4 & dp4 <= 4 & dp5 <= 4 
                     & dp6 <= 4 & dp7 <= 4 & dp8 <= 4
                     & dp9 <= 4 & dp10 <= 4)
CESD_final$dp5 <- 5-CESD_final$dp5
CESD_final$dp8 <- 5-CESD_final$dp8
CESD_f$Depression_Score <-rowSums(CESD_final[, c ("dp1", "dp2", "dp3", 
                                                  "dp4", "dp5", "dp6", 
                                                  "dp7","dp8", "dp9",
                                                  "dp10")])
informats_final <-informats_final %>% rename (gender = dd002_w4)
RQ_2 <- merge(informats_final, CESD_final, all.x = TRUE)
library(nortest)
lillie.test(RQ_2$Depression_Score)
wrss <- wilcox.test(Depression_Score ~ gender, data = RQ_2, paired = FALSE)
wrss
hours_care <- select(Health_status, ID, householdID,   communityID,
                     db024_1_ : db024_23_)
hours_care_1 <- hours_care[!is.na(hours_care$db024_1_), ]
hours_care_1[is.na(hours_care_1)] <- 0
hours_care_1$care_score <- rowSums(hours_care_1[, c("db024_1_", "db024_2_", 
                                                    "db024_3_",
                                                    "db024_4_", "db024_5_", "db024_6_",
                                                    "db024_7_", "db024_8_", "db024_9_",
                                                    "db024_10_", "db024_11_", 
                                                    "db024_12_", "db024_13_", 
                                                    "db024_14_", "db024_15_", 
                                                    "db024_16_", "db024_17_", 
                                                    "db024_17_", "db024_19_",
                                                    "db024_20_", "db024_21_",
                                                    "db024_22_", "db024_23_")])
total_hourscare <- filter(hours_care_1, care_score<= 24)
RQ_3 <- merge (total_hourscare, CESD_final, all.x =  TRUE)
corr.num = RQ_3[c("care_score", "Depression_Score")]
corr.test(corr.num, use = "pairwise", method = "spearman", adjust = "none")
chart.Correlation(corr.num, method = "spearman", histogram = TRUE)
informats_ult <- select(informats, ID, householdID, dd002_w4, dd003_w4, 
                        dd004_w4, dd007_w4)
informats_final2 <- na.omit(informats_ult)
informats_final2$male <- ifelse(informats_final2$ == 1, 1, 0)
informats_final2$female <- ifelse(informats_final2$gender == 2, 1, 0)
reg1 <- merge (informats_final2, CESD_final, all.x = TRUE)
reg2 <- merge(reg1, total_hourscare, all.x =TRUE)
reg_final <- merge (reg2, Health_Status_formal1, all.x = TRUE)
m1 <- lm(Depression_Score ~ care_score + Informal + female, data = reg_final)
summary(m1)