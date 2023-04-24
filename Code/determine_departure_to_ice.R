#################################################
#####   Determine Departure to Ice  ###########
#################################################

# 04/23/23: Revisiting script with chapter 2 bears only
# 10/17/22: Departure to ice = first point after which bear is at sea at least 1x for 7 days (DEM only; no buffer)


library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(ggplot2)
library(sf)

rm(list = ls())

# ---- LOAD AND JOIN DATA  ----------------- #

ch2 <- readRDS('./Data/Derived-data/DFs/bears_ch2_110622.Rds') # study data
all <- readRDS('./Data/Derived-data/DFs/all_11_06_2022.Rds') # all data - has denning start dates

all$ymd <- ymd(all$ymd)

ch2all <- ch2 %>%
  left_join(all)

# ----  DEPARTURE TO ICE  ------------- #

# First day after which bear does not use land for 7 consecutive days

ice <- ch2all %>%
  group_by(id, ymd) %>%
  summarise(on_ice = any(land == 0)) %>% 
  mutate(consec_seven = rollapply(on_ice, 7, all, align = 'left', fill = NA)) %>%
  ungroup() #%>%
  #arrange(desc(on_ice)) 
  #left_join(ch2all) # status for on_ice is by day. DO NOT USE AS INDICATOR FOR WHETHER BEAR IS ON ICE. Use 'land' column instead.

#land <- ch2all %>%
  #group_by(id, ymd) %>%
  #summarise(on_land = any(land == 1)) %>%
  #ungroup()

#depart <- full_join(ice, land)

depart <- ice %>% 
  group_by(id, consec_seven) %>% 
  arrange(desc(on_ice)) %>%
  slice_head()
  
  
ice <- land %>% # 8 observations
  group_by(id, consec_seven) %>%
  filter(consec_seven == TRUE & land == 0) %>%
  slice_head() %>%
  select(1:13) %>%
  ungroup()

ice$ymd <- ymd(ice$ymd)
ice$ordinal <- yday(ice$ymd)

# --------------- RESULTS --------------------- #

## Add study end dates to dataframe

ice$departure_to_ice <- 1

ice2 <- select(ice, id, year, month, day, hour, minute, second, departure_to_ice)

all2 <- all %>%
  left_join(ice2) %>%
  replace_na(list(departure_to_ice = 0))

all2 %>% # check
  filter(departure_to_ice == 1)

#saveRDS(all2, file = './Data/Derived-data/DFs/all.Rds')


## Descriptive stats

summary(ice$ordinal)

ice %>% 
  arrange(ordinal)

# Trend over time (linear model)

shapiro.test(ice$ordinal) # assumptions of normality

m <- lm(ordinal ~ year, data = ice)
summary(m) # 8 days later per year (p = 0.06) - disregarding this result because small sample size; not significant; irrelevant
            # Contrary to climate predictions, trend is later departure dates over time

# Plot

ice2 <- ice %>%
  dplyr::select(animal, year, ordinal) %>%
  mutate(diff = year - 2008)

ice2$year<- as.factor(ice2$year)

# Regression line

new <- data.frame(year =  c(2008, 2009, 2010, 2011, 2012, 2013, 2014))
new$ordinal <- predict(m, newdata = new)
new$year <- as.factor(new$year)

ggplot(data = ice2, aes(x = year, y = ordinal)) + 
  geom_point(size = 2) + 
  xlab("\nYear") +
  ylab("Ordinal Date of Departure\n") + 
  geom_line(data = new, aes(year, ordinal), group = 1, color = "black", linetype = "dashed") +
  theme_classic()

# --------------------------------- #

# How many bears are denning?

ind <- lb %>% 
  group_by(id) %>%
  slice_head()

table(ind$repro) # 4 bears are denning
  
# What's up with the remaining 9 bears?

x <- land %>%
  group_by(id) %>%
  slice_tail() %>%
  select(id, ymd, on_ice, consec_seven, land) %>%
  print(n = 21)

repro <- lb %>% select(id, repro)

x <- x %>% left_join(repro) 

x %>% 
  filter(!(repro == "enter_den")) %>%
  anti_join(ice, by = "id") %>%
  group_by(id) %>%
  slice_tail() 

# Looks like collars stopped functioning in September for 1 and October in 4, while bears were still on land. 
# 20418.2005 and 32366.2014 were still on land at least once per day until December (12/31 for 32366.2014). 
  
  
  
  
  group_by(id, ymd) %>%
  summarise(on_land = any(land == 1)) %>%
  mutate(consec_seven = rollapply(on_land, width = 7, all, align = 'left', fill = NA)) %>%
  ungroup() %>%
  left_join(pb)
