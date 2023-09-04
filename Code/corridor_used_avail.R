########################################################################
#####   DETERMINE USED AND AVAILABLE FOR CORRIDOR PTS - OG ANALYSIS ####
########################################################################

library(tidyverse)
library(amt)
library(sf)
#library(tmap)
#library(tmaptools)
library(adehabitatLT)
library(here)
library(conflicted)

conflicts_prefer(
  dplyr::filter(),
  dplyr::select()
)

rm(list = ls())


# ----------  LOAD AND PREP DATA  --------------- #

b <- readRDS(here("Data", "Derived-data", "DFs", "OG", "OG_090323.Rds"))
b <- as.data.frame(b)

# Corridor points

u <- b %>% filter(at_bonepile == 0)  # load all bonepile points
u <- u %>%
  filter(!(id == "pb_32608.2008")) # AT SOME POINT, ADJUST BONEPILE DATES TO REFLECT BONEPILE-ONLY BEAR

# ------  SUMMARY STATS ----------------- #

# Create track in adehabitatLT

ltraj <- as.ltraj(xy=u[,c("Xaa","Yaa")], date=u$datetime, id=as.character(u$id))

summary <- summary(ltraj)

summary$DaysTrack <-round(difftime(summary$date.end, summary$date.begin, units="days"),digits=1)
summary <- dplyr::select(summary, id, date.begin, date.end, DaysTrack)
summary

summary$DaysTrack <- as.numeric(summary$DaysTrack)

summary(summary)

sd(summary$DaysTrack)

# ------------  USED AND AVAILABLE PTS  ------------- #

# Make track (amt package)

track <- make_track(u, .x = Xaa, .y = Yaa, .t = datetime, id = id, crs = 3338)
tr <- track %>% nest(data = c(-"id")) # create individual dataframes

tr2 <- tr %>% # downsample to 2-hr fix rate
  mutate(steps = map(data, function(x)
    x %>% track_resample(rate = hours(2), tolerance = minutes(20)))) #%>% 
      #steps_by_burst()))

# Dataframe

df <- tr2 %>% dplyr::select(id, steps) %>% unnest(cols = steps) 

# ------  PLOTS ------------------  #

# Histogram showing step length

slHist <- ggplot(data = df, aes(sl_, fill =factor(id))) + 
  geom_histogram(alpha = 0.5) + 
  xlab("step length (m)") + 
  ylab("Number of steps") + 
  theme_classic() +
  theme(legend.position = "bottom") 


ggsave(slHist, filename = "Step Length Histogram for Corr Bears.pdf", 
       path = here("Plots", "OG"),
       dpi = 300,
       width = 7,
       height = 5,
       units = "in")

# Density distribution showing turning angle

taDensity <- ggplot(data = steps, aes(ta_, fill = factor(id))) + 
  geom_density(alpha = 0.2) + 
  xlab("Turning Angle") + 
  ylab("Density") + 
  theme_classic() +
  theme(legend.position = "bottom") 

ggsave(taDensity, filename = "Turning Angle Density plot for Corr Bears.pdf", 
       path = here("Plots", "OG"),
       dpi = 300,
       width = 7,
       height = 5, 
       units = "in")

# ----- GET USED AND AVAILABLE POINTS --------- #

tr3 <- tr2 %>% filter_min_n_burst(3)

ua <- steps %>% group_by(id) %>% random_steps(n_control = 20) # add random steps (gamma/von mises = default distributions)

df %>% filter(df,)

ua <- df %>% random_steps(n_control = 20) # add random steps (gamma/von mises = default distributions)

# Plot random v matched points

ggplot(ua, aes(x2_, y2_, color=case_))+
  geom_point()+
  facet_wrap(~id, scales="free")

saveRDS(ua, './non_bp_pts_used_avail.Rds')

# --------- USED AND AVAILABLE POINTS --------------  #

tr2 <- 

ua <- tr2 %>% steps_by_burst(n_control = 20) # add random steps (gamma/von mises = default distributions)

# Plot random v matched points

ggplot(ua, aes(x2_, y2_, color=case_))+
  geom_point()+
  facet_wrap(~id, scales="free")

saveRDS(ua, './non_bp_pts_used_avail.Rds')