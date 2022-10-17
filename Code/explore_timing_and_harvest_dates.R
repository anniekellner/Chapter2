#######################################################################################
#####     IS THERE SOCIAL STRATIFICAION SURROUNDING THE HARVEST   #####################
#######################################################################################

# About Fisher's Exact test: need 2 x 2 contingency tables. 


library(lubridate)
library(dplyr)
library(tidyr)
library(sf)
library(rstatix)

rm(list = ls())

# --------------------  LOAD AND PREP DATA ------------------------------------------------ #

## Repro and age data

r <- readRDS('./Data/Derived-data/DFs/bears_ch2_093022.Rds') # for repro and age data

r <- st_drop_geometry(r)

r <- r %>%
  group_by(id) %>%
  select(id, age, repro) %>%
  slice_head() %>%
  mutate(age_class = 
         case_when(
           age < 5 ~ "Subadult",
           TRUE ~ "Adult"
         ))

## Bonepile

bpt <- readRDS('./Data/Derived-data/DFs/Space_Use_Summaries/time_at_bonepile.Rds')


bpt$Year <- year(bpt$start)
years <- unique(bpt$Year)

bpt <- bpt %>%
  select(id, start, end, Year) %>%
  mutate(Bonepile = if_else(
    id == "pb_20492.2008" | id == "pb_20520.2012" | id == "pb_20735.2009" | id == "pb_20966.2008" | id == "pb_20735.2009.2" |
      id == "pb_20982.2008" | id == "pb_32282.2008" | id == "pb_32366.2011" | id == "pb_32608.2008",
    "Kaktovik", "Cross")) %>%
  mutate(t_interval = interval(start = start, end = end, tzone = 'US/Alaska'))

## Harvest

harvest <- read.csv('./Data/Bonepile_Dates.csv')

harvest$Dates <- mdy(harvest$Dates)
harvest$Year <- year(harvest$Dates)

h <- harvest %>%
  filter(Year %in% years) %>%
  filter(!(Bonepile == "Barrow")) %>%
  mutate(Bonepile = recode(Bonepile, "Cross " = "Cross")) # Because there was a space in csv file and wouldn't join correctly

tz(h$Dates) <- 'US/Alaska'

# Harvest summary stats: 2008-2015 (ALL whales landed)

h$ordinal <- yday(h$Dates)

h %>% # mean
  group_by(Bonepile) %>%
  summarise(mean = mean(ordinal))

h %>% # check for leap years when translating to calendar dates
  group_by(Bonepile) %>%
  arrange(ordinal) %>%
  print(n = 41)


summary(h$ordinal)


h.cross <- subset(h, Bonepile == "Cross")
h.kak <- subset(h, Bonepile == "Kaktovik")

t.test(h.cross$ordinal, h.kak$ordinal) # kaktovik mean ordinal date is 4 days later (p = 0.08) but probably easier to say no difference

# ------------------   JOIN -------------------------------------------------- #

bpt2 <- bpt %>%
  full_join(h, by = "Bonepile") %>% 
  dplyr::rename(c(harvest_date = Dates, harvest_year = Year.y, Bear_year = Year.x)) %>%
  mutate(t_interval = interval(start, end)) %>%
  glimpse()

sameDay <- bpt2 %>%
  mutate(within_t = harvest_date %within% t_interval) 

overlap <- sameDay %>% # 13 bears overlap; 5 do not
  filter(within_t == TRUE) %>%
  group_by(id) %>%
  slice_head()

overlap[6,1] <- "pb_20735.2009" # 20735.2009 goes to the bonepile twice. Second time overlaps the harvest.

dayOf <- unique(overlap$id)

# ------------- ANALYSES  --------------------- #

###### Which bears were not present on any harvest date? (including non-bp bears)

r2 <- r %>%
  mutate(dayOf = if_else(id %in% dayOf, "TRUE", "FALSE")) # bring back non-bp bears

# 13/18 present at harvest; 5/18 bp bears not present at harvest; 
# 8/21 land bears not present; 13/21 present at harvest


###### Does age class influence arrival date?

table(r2$dayOf, r2$age_class)
 
# 2/3 subadults not present at harvest. 1 waited but later visited, 1 never went to the BP. 1 present same day. 

sub <- subset(r2, age_class == "Subadult")
fisher.test(sub$dayOf == TRUE, sub$dayOf == FALSE) # not signif

######  Does repro status influence arrival date?

table(r2$repro, r2$dayOf) # only interesting result is 5/6 bears with coys visit the bonepile the day of harvest

# add bp status to r2 df

bpid <- unique(b$id)
r2$bpBear <- ifelse(r2$id %in% bpid, "TRUE", "FALSE")

table(r2$coy, r2$bpBear)

fisher.test(r2$coy, r2$bpBear) # not significant. Subadults = FALSE (no coy) - but I don't understand how 5/6 arrived on same day p = 0.16


# How long did 20735 wait at the bonepile with her COY (prior to the harvest) before leaving?

ex <- b3 %>% filter(id == "pb_20735.2009")
difftime(ex$bear_arrival, ex$bear_departure) # 20.0 days

table(r2$repro, r2$overlap) # adults with coys looks to be interesting - 6/6 visit bonepile and 5/6 arrive the day of

r2 <- r2 %>% replace_na(list(coy = "FALSE"))


fisher.test(r2$age_class, r2$overlap)  # result is not significant but worth mentioning


###### Last investigation: how many days bears arrived after first harvest

# First harvest

first <- h %>%
  group_by(Year, Bonepile) %>%
  arrange(Year, Bonepile, Dates) %>%
  slice_head()

# Last harvest

last <-h %>%
  group_by(Year, Bonepile) %>%
  arrange(Year, Bonepile, Dates) %>%
  slice_tail()

# Bears

b <- bpt %>% # start = date bear arrived at bonepile
  select(id, start, end, Year, Bonepile) %>%
  rename(bear_arrival = start) %>%
  rename(bear_departure = end)

#x <- r2 %>% # getting ids for bears that were not present at harvest
  #filter(dayOf == "FALSE")

#no.ids <- x$id

b2 <- b %>% # join with first harvest date
  left_join(first, by = c('Year', 'Bonepile')) %>%
  rename(first_harvest = Dates) %>%
  select(-c("ordinal", "month"))

#b3 <- b2 %>% # join with last harvest
  #left_join(last, by = c('Year', 'Bonepile')) %>%
  #rename(last_harvest = Dates)

b2$time_to_first_harvest <- difftime(b2$bear_arrival, b2$first_harvest)
#b3$time_to_last_harvest <- difftime(b3$last_harvest, b3$bear_arrival)

mean(b2$time_to_first_harvest) # -0.44 days: mean arrival preceded the landing by a half day. DISCUSSION: bears maybe smelled the kill. Idea: movement rate 1-2 days prior to first landing?

# Join with age/repro data and see if means are different between groups

br <- b2 %>%
  left_join(r) 

# Add in age/repro info for 20735.2009.2. Remember to deal with this when calculating

br[18,8] <- 7
br[18,9] <- "coy"
br[18,10] <- "Adult"

br$time_to_first_harvest <- as.numeric(br$time_to_first_harvest)
br <- br %>% replace_na(list(repro = "unknown"))
br$repro <- as.factor(br$repro)

## Summaries
# pb_20735.2009 is an outlier, having arrived 35 days prior to harvest

# T-test for difference between subadult and adult

br %>% 
  #filter(!(id == "pb_20735.2009")) %>% # decide whether or not to include outlier
  group_by(age_class) %>%
  summarize(mean = mean(time_to_first_harvest)) 
# see table for results. Subadult mean ~ 4.6 while Adult mean -1.07. But not significant because n = 2 subadults. 
  # worth mentioning that 1 subadult never visited the bonepile

br %>%
  #filter(!(id == "pb_20735.2009")) %>%
  {t.test(.$time_to_first_harvest ~ .$age_class, var.equal = TRUE)} # not significant with or without extreme value

# 1-way ANOVA for difference among repro status - not significant

m <- aov(formula = time_to_first_harvest ~ repro, data = br)
summary(m)

br %>%
  filter(!(id == "pb_20735.2009")) %>% 
  group_by(repro) %>%
  summarize(mean = mean(time_to_first_harvest)) 

# with coys: -5 days with outlier; -0.2 days without outlier. All other groups positive. yearling = 1 day; entering den = 4 days later
# Subadults (status unknown) arrive last at 4.6 days

br2 <- br %>%
  mutate(coy = if_else(
    repro == "coy", "coy", "no coy"
  ))

br2$coy <- as.factor(br2$coy)

br2 %>% {t.test(.$time_to_first_harvest ~ .$coy, var.equal = TRUE)} # still not significant when divided into coy and not-coy

########## SCRATCH  ##############################################################


h2 <- h %>%
  dplyr::mutate(h, twoDays = interval(Dates + days(2)))

i <- interval(test, test + days(2)) # sweet - this works







