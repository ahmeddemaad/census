library(tidyverse)
library(dplyr)
DS <- read.csv("../census/inputs/census_income_original_2.csv")
###################################(1st)

DS %>% group_by(ID) %>% summarise(as.numeric(income_c)) -> Sum_income
colnames(Sum_income)[2] <- c("income_c")
Sum_income %>% summarise(sum(income_c)) -> Sum_income

DS %>% select(ID,age,income) -> D_join
DS_Sum_income_age <- left_join(Sum_income,D_join)
DS_Sum_income_age %>% distinct() %>% drop_na() -> DS_Sum_income_age

remove(D_join,Sum_income)

DS_Sum_income_age %>% filter(age <25) ->DS_under_25
Avg_under_25 <- mean(DS_under_25$`sum(income_c)`)
DS_Sum_income_age %>% filter(age >=25 & age <=50) ->DS_from_25_to_50
Avg_from_25_to_50 <- mean(DS_from_25_to_50$`sum(income_c)`)
DS_Sum_income_age %>% filter(age> 50 & age<=75) ->DS_above_50_to_75
Avg_above_50_to_75 <- mean(DS_above_50_to_75$`sum(income_c)`)
DS_Sum_income_age %>% filter(age >75) ->DS_above_75
Avg_above_75 <- mean(DS_above_75$`sum(income_c)`)

cat("Avg income under 25 =",Avg_under_25)
cat("Avg income 25:50    =",Avg_from_25_to_50)
cat("Avg income 50:75    =",Avg_above_50_to_75)
cat("Avg income above 75 =",Avg_above_75)

###(2nd)
############################## (in view of education level)
DS %>% group_by(ID) %>% select(Gender,education.num) %>% distinct() ->DS_1
DS_1 %>% filter(grepl('M',Gender)) -> DS_male_1
DS_1 %>% filter(grepl('^m',Gender)) -> DS_male_2
DS_male_total <- full_join(DS_male_1,DS_male_2)
DS_male_total %>% group_by(ID) %>% filter(row_number()==1) ->DS_male_total
Avg_edu_lvl_m <- mean(DS_male_total$education.num)

DS_1 %>% filter(grepl('F',Gender)) -> DS_female_1
DS_1 %>% filter(grepl('^f',Gender)) -> DS_female_2
DS_female_total <- full_join(DS_female_1,DS_female_2)
DS_female_total %>% group_by(ID) %>% filter(row_number()==1) ->DS_female_total
Avg_edu_lvl_f <- mean(DS_female_total$education.num)
remove(DS_male_1,DS_male_2,DS_female_1,DS_female_2,DS_1)

cat("Avg.Edu.lvl of   male=",Avg_edu_lvl_m)
cat("Avg.Edu.lvl of female=",Avg_edu_lvl_f)


############################ (in view of the income)

male_income <-full_join(DS_Sum_income_age,DS_male_total)
male_income %>% drop_na() %>% select(ID,Gender,`sum(income_c)`) ->male_income
avg_male_income <- mean(male_income$`sum(income_c)`)

female_income <-full_join(DS_Sum_income_age,DS_female_total)
female_income %>% drop_na() %>% select(ID,Gender,`sum(income_c)`) ->female_income
avg_female_income <- mean(female_income$`sum(income_c)`)

cat("Avg.male income=",avg_male_income)
cat("Avg.female income=",avg_female_income)

##########################(in view of the working class)
DS %>% select(ID,workclass) %>% distinct() ->DS_join7
male_Wclass <-left_join(DS_male_total,DS_join7)
male_Wclass %>% group_by(workclass) %>% select(workclass) -> male_Wclass
male_Wclass %>% count(workclass) ->male_Wclass

female_Wclass <-left_join(DS_female_total,DS_join7)
female_Wclass %>% group_by(workclass) %>%select(workclass) -> female_Wclass
female_Wclass %>% count(workclass) ->female_Wclass

perc_male_Federal_gov <- 645/(sum(male_Wclass$n)+sum(female_Wclass$n))*100
Perc_male_local_gov <- 1258/(sum(male_Wclass$n)+sum(female_Wclass$n))*100
Perc_male_Never_worked <- 5/(sum(male_Wclass$n)+sum(female_Wclass$n))*100
perc_male_Private <-14944/(sum(male_Wclass$n)+sum(female_Wclass$n))*100
perc_male_State_gov <-809/(sum(male_Wclass$n)+sum(female_Wclass$n))*100

perc_female_Federal_gov <- 315/(sum(male_Wclass$n)+sum(female_Wclass$n))*100
Perc_female_local_gov_male <- 835/(sum(male_Wclass$n)+sum(female_Wclass$n))*100
Perc_female_Never_worked <- 2/(sum(male_Wclass$n)+sum(female_Wclass$n))*100
perc_female_Private <- 7752/(sum(male_Wclass$n)+sum(female_Wclass$n))*100
perc_female_State_gov <- 489/(sum(male_Wclass$n)+sum(female_Wclass$n))*100
################################
###(3rd)
###############(income&race)
DS_Sum_income_age %>% select(ID,`sum(income_c)`) ->DS_join1
DS %>% select(ID,race) %>% distinct() ->DS_join2
Income_Race<-left_join(DS_join1,DS_join2)
Income_Race %>% group_by(race) %>% summarise(mean(`sum(income_c)`)) ->Avg_income_race
print.data.frame(Avg_income_race)
cat("no huge differences in values so race not considered to be a an income predictor")
##########(income&education,num)
DS %>% select(ID,education.num) %>% distinct() ->DS_join3
income_edu_num <-left_join(DS_join1,DS_join3)
income_edu_num %>% group_by(education.num) %>% summarise(mean(`sum(income_c)`)) ->Avg_income_edu_num
cat("no huge differences in values so education number not considered to be a an income predictor")
##########(income&education)
DS %>% select(ID,education) %>% distinct() ->DS_join4
income_edu <-left_join(DS_join1,DS_join4)
income_edu %>% group_by(education) %>% summarise(mean(`sum(income_c)`)) ->Avg_income_edu
cat("no huge differences in values so education not considered to be a an income predictor")
##########(income&occupation)
DS %>% select(ID,occupation) %>% distinct() ->DS_join5
income_occ <-left_join(DS_join1,DS_join5)
income_occ %>% group_by(occupation) %>% summarise(mean(`sum(income_c)`)) ->Avg_income_occ
cat("Noticable difference in income so occupation is considered to be income predictor")
##########(income&workinghours)
DS %>% select(ID,hours.per.week) %>% distinct() ->DS_join5
income_hours <-left_join(DS_join1,DS_join5)
income_hours %>% group_by(hours.per.week) %>% summarise(mean(`sum(income_c)`)) ->Avg_income_hours
cat("working hours only are not considered to be a predictor for income")
############(native counrty)
DS %>% select(ID,native.country) %>% distinct() ->DS_join6
income_country <-left_join(DS_join1,DS_join6)
income_country %>% group_by(native.country) %>% summarise(mean(`sum(income_c)`)) ->Avg_income_country
cat("Noticable difference in income so native country is considered to be income predictor")
############(icome&workclass)
DS %>% select(ID,workclass) %>% distinct() ->DS_join7
income_Wclass <-left_join(DS_join1,DS_join7)
income_Wclass %>% group_by(workclass) %>% summarise(mean(`sum(income_c)`)) ->Avg_income_Wclass
cat(" No Noticable difference in income so working class isnot considered to be income predictor")
