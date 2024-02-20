
library(dplyr) #always load this
library(lubridate)
library(knitr)


setwd("G:/My Drive/INCH Projects/[2023] Apple Watch Study")

#load in step count data
# d <- read.csv("G:/My Drive/INCH Projects/[2023] Apple Watch Study/UTSA_Steps_Data_Export2023Nov14.csv")
# 
# d <- read.csv("G:/My Drive/INCH Projects/[2023] Apple Watch Study/synced_steps_20231208-163306.csv")
# 
# d1 <- read.csv("G:/My Drive/INCH Projects/[2023] Apple Watch Study/synced_exerc_mins_20231217-125231.csv")

d <- read.csv("G:/My Drive/INCH Projects/[2023] Apple Watch Study/synced_steps_20231217-124928.csv")

kable(table(d$email))

d %>% select(email, participant) %>% unique() %>% View()

# #remove our testing data
d <- d %>%
  filter(participant != "a4347608-ad4a-4265-857e-2d81b5a04b95") %>%
  filter(participant != "d31f724d-2f79-45c3-b1a4-4356707c7f0a") %>%
  filter(participant != "9ee0b114-11cf-4d13-b06d-7cbe6dab3ef2") %>%
  filter(participant != "0f4831cf-5838-48ff-ae8f-91b28ea9fe61")
# 
# #rewrite csv without our test data
# write_csv(d, "G:/My Drive/INCH Projects/Random/[2023] Apple Watch Study/Exercise Minutes (Apple Health)_2023-11-11T07_24_38.640Z_.csv")

#check data for anything weird
glimpse(d)
table(d$current_goal) #idk
table(d$program) #all are 418
table(d$timezone) #various time zones other than CDT, inspect further



d %>% filter(timezone == "CST") %>% View() # apparently CST is the same as CDT. who made these rules??
d %>% filter(timezone == "EDT") %>% View()
d %>% filter(timezone == "MDT") %>% View()
d %>% filter(timezone == "MST") %>% View()
d %>% filter(timezone == "PDT") %>% View()

#note that some emails don't have an associated participant ID

#remove all 0 step counts
d <- d %>%
  filter(steps !=0)


#see if any participant has multiple time zones in their data
d %>%
  group_by(email) %>%
  summarize(unique_timezones = n_distinct(timezone),
            distinct_timezones = toString(unique(timezone))) %>% View()
#note there are 232 total participants
#some have 2+ time zones. see if we can get away without dealing with time zones


#date is in character strings, need to convert to date-time (dttm)
d <- d %>%
  mutate(date = ymd_hms(date))  # convert to a date-time object, times are already CDT

#when a participant syncs, their anteceding full-day data will appear with the timestamp at 00:00:00, 
#along with the timestamp at the moment they synced, which we don't neeed. so remove those

d1 <- d %>%
  mutate(time = format(date, "%H:%M:%S")) %>% #retrieve hour-minute-second timestamps from dttm
  filter(time == "00:00:00") %>% #filter out times that aren't 00:00:00
  mutate(date = as.Date(date)) #don't need time info in date variable anymore, so convert dttm to date



d1 <- d1 %>%
  group_by(email, date) %>%
  mutate(duplicate_dates = n()) %>%
  arrange(email, date)

#take a look at duplicate dates
duplicates <- d1 %>% filter(duplicate_dates > 1)
#seems most duplicate dates are duplicate "minute" values too, but there are some weird 
#lets replace duplicates with the mean value

d2 <- d1 %>%
  group_by(email, date) %>%
  mutate(steps = round(mean(steps), digits=2)) %>% #replace identical dates+email with average steps
  ungroup() %>%
  distinct(participant, date, .keep_all = TRUE) #keep distinct rows only


#count rows per participant
participation <- d2 %>%
  group_by(email) %>%
  summarize(rows = n())  #nonamericans can use summarise() if more comfortable


#full participation = 4 weeks = 28 days. remove anyone with less than 4 weeks

participation1 <- participation %>%
  filter(rows > 28)
#143 participants left

d3 <- d2 %>%
  filter(email %in% participation1$email)

#convert emails to lowercase for easy merging with qualtrics data later
d3 <- d3 %>%
  mutate(email = tolower(email))


write.csv(d3, file = "clean_step_count_2-13-24.csv", row.names = F)



