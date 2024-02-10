
library(dplyr)
library(knitr)
library(qualtRics) #for easier importing of qualtrics csv data.
#qualtrics csv contains 3 header rows which makes it difficult to import into R. 
#exporting qualtrics data in SPSS format (.sav) then reading in using import() from rio or read_sav() from haven may be easier

#load in baseline qualtrics data
d <- read_survey("G:/My Drive/INCH Projects/[2023] Apple Watch Study/2023-2024 Apple Watch Physical Activity Behavior (Week 0 - Baseline)_November 10, 2023_10.57.csv")

#look at variable names
names(d)
glimpse(d)

#retain variables of interest
# d1 <- d %>%
#   select(email.2, `Duration (in seconds)`, RecordedDate, age, sex,
#          `k10  _1`, `k10  _2`, `k10  _3`, `k10  _4`, `k10  _5`,
#          `k10  _6`, `k10  _7`, `k10  _8`, `k10  _9`, `k10  _10`,
# 
#          wemwbs_1, wemwbs_2, wemwbs_3, wemwbs_4, wemwbs_5, wemwbs_6, wemwbs_7,
#          wemwbs_8, wemwbs_9, wemwbs_10, wemwbs_11, wemwbs_12, wemwbs_13, wemwbs_14,  #this looks like its an incline slope lol
# 
#          `PHQ-9_1`, `PHQ-9_2`, `PHQ-9_3`, `PHQ-9_4`, `PHQ-9_5`,
#          `PHQ-9_6`, `PHQ-9_7`, `PHQ-9_8`, `PHQ-9_9`, `PHQ-9.1`
#   )

#the variable names for k10 are a mess to deal with (blame denver probably). chatgpt to the rescue
d1 <- d %>%
  select(
    email.2, `Duration (in seconds)`, RecordedDate, age, sex,
    matches("^attitude*"),
    matches("^intent*"),
    matches("^capability*"),
    matches("^pars_*"),
    matches("^habit"),
    matches("^ex_ident*"),
    matches("^k10.*"),  # Matches columns starting with "k10"
    matches("^wemwbs.*"),  # Matches columns starting with "wemwbs"
    matches("^PHQ-9.*")  # Matches columns starting with "PHQ-9"
    
  )

names(d1)

#rename the variables

d1 <- setNames(d1, c("email", "duration", "record_date", "age", "sex",
                     paste0("attitude_", 1:6),
                     paste0("intent_", 1:2),
                     paste0("capability_", 1:3),
                     paste0("pars_", 1:14),
                     paste0("habit_", 1:4),
                     paste0("ex_identity_", 1:9),
                     paste0("k10.", 1:10),
                     paste0("wemwbs.", 1:14),
                     paste0("phq9.", 1:9), "phq9.91"))



#remove participants with less than half of median response duration (saw someone else do this, may be standard practice?)
summary(d1$duration) #median = 874.0

d1 <- d1 %>%
  filter(duration > median(d1$duration)/2)

#code sex as factor
d1$sex <- as.factor(d1$sex)
summary(d1$sex) #hella females



# attitude ----------------------------------------------------------------
table(d1$attitude_3) %>% kable()

sapply(select(d1, attitude_1:attitude_6), function(x) table(x)) %>% kable


d1 <- d1 %>%
  mutate(attitude_1 = case_when(attitude_1 == 'Harmful (1)' ~ 1,
                              attitude_1 == '2' ~ 2,
                              attitude_1 == '3' ~ 3,
                              attitude_1 == '4' ~ 4,
                              attitude_1 == '5' ~ 5,
                              attitude_1 == '6' ~ 6,
                              attitude_1 == 'Benefical (7)' ~ 7)) %>%
  mutate(attitude_2 = case_when(attitude_2 == 'Useless (1)' ~ 1,
                                attitude_2 == '2' ~ 2,
                                attitude_2 == '3' ~ 3,
                                attitude_2 == '4' ~ 4,
                                attitude_2 == '5' ~ 5,
                                attitude_2 == '6' ~ 6,
                                attitude_2 == 'Useful  (7)' ~ 7)) %>%
  mutate(attitude_3 = case_when(attitude_3 == 'Foolish (1)' ~ 1,
                                attitude_3 == '2' ~ 2,
                                attitude_3 == '3' ~ 3,
                                attitude_3 == '4' ~ 4,
                                attitude_3 == '5' ~ 5,
                                attitude_3 == '6' ~ 6,
                                attitude_3 == 'Wise (7)' ~ 7)) %>%
  mutate(attitude_4 = case_when(attitude_4 == 'Unpleasant  (1)' ~ 1,
                                attitude_4 == '2' ~ 2,
                                attitude_4 == '3' ~ 3,
                                attitude_4 == '4' ~ 4,
                                attitude_4 == '5' ~ 5,
                                attitude_4 == '6' ~ 6,
                                attitude_4 == 'Pleasant  (7)' ~ 7)) %>%
  mutate(attitude_5 = case_when(attitude_5 == 'Boring  (1)' ~ 1,
                                attitude_5 == '2' ~ 2,
                                attitude_5 == '3' ~ 3,
                                attitude_5 == '4' ~ 4,
                                attitude_5 == '5' ~ 5,
                                attitude_5 == '6' ~ 6,
                                attitude_5 == 'Fun  (7)' ~ 7)) %>%
  mutate(attitude_6 = case_when(attitude_6 == 'Unenjoyable   (1)' ~ 1,
                                attitude_6 == '2' ~ 2,
                                attitude_6 == '3' ~ 3,
                                attitude_6 == '4' ~ 4,
                                attitude_6 == '5' ~ 5,
                                attitude_6 == '6' ~ 6,
                                attitude_6 == 'Enjoyable  (7)' ~ 7))




# intent --------------------------------------------------------------
sapply(select(d1, intent_1:intent_2), function(x) table(x)) %>% kable

d1 <- d1 %>%
  mutate(intent_1 = case_when(intent_1 == "No" ~ 0,
                              intent_1 == "Yes" ~ 1),
         
         intent_2 = case_when(intent_2 == "Strongly Disagree (1)" ~ 1,
                              intent_2 == "2" ~ 2,
                              intent_2 == "3" ~ 3,
                              intent_2 == "4" ~ 4,
                              intent_2 == "5" ~ 5,
                              intent_2 == "6" ~ 6,
                              intent_2 == "Strongly Agree (7)" ~ 6))


# capability --------------------------------------------------------------
sapply(select(d1, capability_1:capability_3), function(x) table(x)) %>% kable

d1 <- d1 %>%
  mutate(across(starts_with("capability_"), ~ case_when(
    is.na(.) ~ NA_integer_,
    . == "Strongly Disagree" ~ 1,
    . == "Disagree" ~ 2,
    . == "Slightly Disagree" ~ 3,
    . == "Neutral" ~ 4,
    . == "Slightly Agree" ~ 5,
    . == "Agree" ~ 6,
    . == "Strongly Agree" ~ 7
  )))

# dont know if I did this right



# pars --------------------------------------------------------------------
sapply(select(d1, pars_1:pars_14), function(x) table(x)) %>% kable





#recode k10 variables to 1-5 and get a sum score
table(d1$k10.1)

d1 <- d1 %>%
  mutate(across(starts_with("k10."), ~ case_when(
    is.na(.) ~ NA_integer_,
    . == "None of the time" ~ 1,
    . == "A little of the time" ~ 2,
    . == "Some of the time" ~ 3,
    . == "Most of the time" ~ 4,
    . == "All of the time" ~ 5
  )))


d1 <- d1 %>%
  mutate(k10_total = rowMeans(across(k10.1:k10.10), na.rm=T))

summary(d1$k10_total)



#recode wemwbs variables to  1-5
table(d1$wemwbs.1)
d1 <- d1 %>%
  mutate(across(starts_with("wemwbs."), ~ case_when(
    is.na(.) ~ NA_integer_,
    . == "None of the time" ~ 1,
    . == "Rarely" ~ 2,
    . == "Some of the time" ~ 3,
    . == "Often" ~ 4,
    . == "All of the time" ~ 5
  )))

d1 <- d1 %>%
  mutate(wemwbs_sum = rowSums(select(., wemwbs.1:wemwbs.14)))




#recode phq9 variables to 0-3
table(d1$phq9.1)

d1 <- d1 %>%
  mutate(across(starts_with("phq9."), ~ case_when(
    is.na(.) ~ NA_integer_,
    . == "Not at all" ~ 0,
    . == "Several days" ~ 1,
    . == "More than half the days" ~ 2,
    . == "Nearly every day" ~ 3
  )))
#phq9.91 is coded different, just ignore for now
d1 <- d1 %>%
  mutate(phq9_sum = rowSums(select(., phq9.1:phq9.9)))

#check missingness for sum scores
sum(is.na(d1$k10_sum))
sum(is.na(d1$wemwbs_sum))
sum(is.na(d1$phq9_sum))
#around 10% for all

#convert emails to lowercase for easy merging with qualtrics data later
d1 <- d1 %>%
  mutate(email = tolower(email))

#final inspection
glimpse(d1)

write_csv(d1, file = "G:/My Drive/INCH Projects/[2023] Apple Watch Study/clean_baseline_qualtrics.csv")



# redo everything for week4 qualtrics -------------------------------------

rm(list=ls()) #clear environment


d <- read_survey("G:/My Drive/INCH Projects/[2023] Apple Watch Study/2023-2024 Apple Watch Physical Activity Behavior (Week 4)_November 10, 2023_10.59.csv")

#look at variable names
names(d)
glimpse(d)

#retain variables of interest
d1 <- d %>%
  select(
    email, `Duration (in seconds)`, RecordedDate,
    matches("^k10.*"),  # Matches columns starting with "k10"
    matches("^wemwbs.*"),  # Matches columns starting with "wemwbs"
    matches("^PHQ-9.*")  # Matches columns starting with "PHQ-9"
  )

names(d1)

#rename the variables

names(d1) <- c("email", "duration", "record_date",
               "k10.1", "k10.2", "k10.3", "k10.4", "k10.5",
               "k10.6", "k10.7", "k10.8", "k10.9", "k10.10",
               "wemwbs.1", "wemwbs.2", "wemwbs.3", "wemwbs.4",
               "wemwbs.5", "wemwbs.6", "wemwbs.7", "wemwbs.8",
               "wemwbs.9", "wemwbs.10", "wemwbs.11", "wemwbs.12",
               "wemwbs.13", "wemwbs.14",
               "phq9.1", "phq9.2", "phq9.3", "phq9.4", "phq9.5",
               "phq9.6", "phq9.7", "phq9.8", "phq9.9", "phq9.91")



summary(d1$duration) #median=610

d1 <- d1 %>%
  filter(duration > (610/2))



#recode k10 variables to 1-5 and get a sum score
table(d1$k10.1)

d1 <- d1 %>%
  mutate(across(starts_with("k10."), ~ case_when(
    is.na(.) ~ NA_integer_,
    . == "None of the time" ~ 1,
    . == "A little of the time" ~ 2,
    . == "Some of the time" ~ 3,
    . == "Most of the time" ~ 4,
    . == "All of the time" ~ 5
  )))

d1 <- d1 %>%
  mutate(k10_sum = rowSums(select(., k10.1:k10.10)))  #if one response is NA, then the sum score is NA



#recode wemwbs variables to  1-5
table(d1$wemwbs.1)
d1 <- d1 %>%
  mutate(across(starts_with("wemwbs."), ~ case_when(
    is.na(.) ~ NA_integer_,
    . == "None of the time" ~ 1,
    . == "Rarely" ~ 2,
    . == "Some of the time" ~ 3,
    . == "Often" ~ 4,
    . == "All of the time" ~ 5
  )))

d1 <- d1 %>%
  mutate(wemwbs_sum = rowSums(select(., wemwbs.1:wemwbs.14)))


#recode phq9 variables to 0-3
table(d1$phq9.1)

d1 <- d1 %>%
  mutate(across(starts_with("phq9."), ~ case_when(
    is.na(.) ~ NA_integer_,
    . == "Not at all" ~ 0,
    . == "Several days" ~ 1,
    . == "More than half the days" ~ 2,
    . == "Nearly every day" ~ 3
  )))
#phq9.91 is coded different, just ignore for now
d1 <- d1 %>%
  mutate(phq9_sum = rowSums(select(., phq9.1:phq9.9)))

#check missingness for sum scores
sum(is.na(d1$k10_sum))
sum(is.na(d1$wemwbs_sum))
sum(is.na(d1$phq9_sum))
#nice

#convert emails to lowercase for easy merging with qualtrics data later
d1 <- d1 %>%
  mutate(email = tolower(email))

#final inspection
glimpse(d1)

write_csv(d1, file = "G:/My Drive/INCH Projects/[2023] Apple Watch Study/clean_week4_qualtrics.csv")





















