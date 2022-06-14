########################################################
###   USE PKG GOOGLEWAY TO RETRIEVE COORDINATES   ######
########################################################

library(sf)
library(ggplot2)
library(googleway)


sf_use_s2(FALSE) # Otherwise get errors; https://github.com/r-spatial/sf/issues/1856

world <- ne_countries(scale = "medium", returnclass = "sf") # rnaturalearth package
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

# Get city coords from Google

key <- "AIzaSyDSOPDGRNBNE-ZoLj4PM608-dpDrQ0VNgg" # Google API key

akcities <- data.frame(state= rep("Alaska", 3), city = c("Niuqsut", "Deadhorse", "Kaktovik"))

coords <- apply(akcities, 1, function(x){
  google_geocode(address = paste(x["city"], x["state"], sep = ", "), 
                 key = key)
})

akcities <- cbind(akcities, do.call(rbind, lapply(coords, geocode_coordinates)))

akcities <- st_as_sf(akcities, coords = c("lng", "lat"), remove = FALSE, # Convert to SF format
                     crs = 4326, agr = "constant")

## ggplot

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = akcities) +
  geom_text_repel(data= akcities, aes(x = lng, y = lat, label = city),
                  size = 2.5, fontface = "bold", nudge_y = c(-0.5, -0.5, 0.5)) + # package ggrepel = flexible approach to label placement
  coord_sf(xlim = c(-155, -141), ylim = c(69.5,70.8), expand = FALSE)