#################################################
#####   Determine Departure to Ice  ###########
#################################################

# 10/17/22: Departure to ice = first point after which bear is at sea at least 1x for 7 days (DEM only; no buffer)

library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(ggplot2)

rm(list = ls())

# ---- LOAD AND FILTER DATA  ----------------- #

all <- readRDS('./Data/Derived-data/DFs/all_v2.Rds') 
which(is.na(all$land)) # no NA values for land

lb <- readRDS('./Data/Derived-data/DFs/bears_ch2_093022.Rds')
lbs <- unique(lb$id)

land <- all %>%
  filter(id %in% lbs & month > 8)

# ----  DEPARTURE TO ICE  ------------- #

land <- land %>%
  group_by(id, ymd) %>%
  summarise(on_ice = any(land == 0)) %>%
  mutate(consec_seven = rollapply(on_ice, 7, all, align = 'left', fill = NA)) %>%
  ungroup() %>%
  left_join(land)

ice <- land %>% # 8 observations
  group_by(id, consec_seven) %>%
  filter(consec_seven == TRUE & land == 0) %>%
  slice_head() %>%
  select(1:13)

ice$ymd <- ymd(ice$ymd)
ice$ordinal <- yday(ice$ymd)

summary(ice$ordinal)

ice %>% # Results
  arrange(ordinal)

# linear model - g
m <- lm(ordinal ~ year, data = ice)
summary(m) # 8 days later per year (p = 0.06)


# ------------  PLOT DEPARTURE DATE OVER TIME  ------------------- #

# Contrary to climate predictions, trend is later departure dates over time

shapiro.test(ice$ordinal) # data does not deviate from a normal distribution

ice2 <- ice %>%
  dplyr::select(animal, year, ordinal) %>%
  mutate(diff = year - 2008)

ice2$year<- as.factor(ice2$year)

# df to predict over for regression line

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
