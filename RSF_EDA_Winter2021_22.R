library(raster)
library(amt)
library(dplyr)
library(tibble)
library(purrr)
library(tidyr)
library(lme4)
library(lmerTest)
library(janitor)
#install.packages("amt")
#install.packages("lmerTest")
library(amt)
library(lmerTest)
library(rio)

#Importing shapefile of study site.
habitat <- raster::shapefile("/Users/jeffgrayum/Downloads/Simplified_LandCover/Simplified_LandCover.shp")

#Checking the extent of the bounding box.
extent(habitat@bbox)

#Creating a column that treats land cover (habitat) type as a numeric factor.
habitat$hab.factor <- as.numeric(as.factor(habitat$HABITAT))

#Viewing dataframe now.
habitat %>%
  view()

#viewing levels
levels(as.factor(habitat$HABITAT))

#Quick look
head(habitat)

#Creating template raster. Here, we specify crs, extent, and resolution of our raster.
template.raster <- raster(crs = habitat@proj4string,
       ext = extent(habitat@bbox),
       res = 5)

#Plotting template raster (This doesn't work because there is nothing to plot!)
plot(template.raster)


#Rasterizing the map of study site, filling with land cover type (as numeric factor)
habitat.raster <- rasterize(x = habitat,
                            y = template.raster,
                            field = habitat$hab.factor)
#Viewing habitat raster
plot(habitat.raster)
#windows()

#Viewing habitat raster, but colored by habitat type (using simplified lcov)
plot(habitat, col = as.factor(habitat$HABITAT))

#assigning lcov types to "habitats" as levels
habitats <- levels(as.factor(habitat$HABITAT))

#Creating a raster list. This will be used in our for loop.
rasterList <- list()

#Creating our for loop. This creates a distance-based raster layer for each of our ten lcov types.
#This will show the distance from each land cover type, rather that the boundry.
for(i in 1:length(habitats)){
  # i <- 1
  
  hab <- subset(habitat, HABITAT == habitats[i])
  hab$field <- 1
  # plot(hab)
  rasterList[[i]] <- distance(rasterize(hab, template.raster, field = hab$field, background = NA))
  names(rasterList)[i] <- habitats[i]
}

#I think this applies the same extent to each raster layer in our list.
lapply(rasterList, extent)

#Now we stack our raster layers.
distanceStack <- stack(rasterList)


#Plotting our ten distance-based raster layers.
plot(distanceStack)


winter_2021_22_locs <- rio::import("/Users/jeffgrayum/Downloads/All_Winter_Locs_Clean.xlsx", setclass = "tibble") %>%
  clean_names() %>%
  mutate(date = lubridate::as_date(date))

winter_2021_22_locs %>%
  view()

rsfData <- winter_2021_22_locs %>% 
  as_tibble() %>%
  mutate(DT.chr = gsub("-05:00","",gsub("T"," ",date_created)),
         DT.GMT = as.POSIXct(DT.chr, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"),
         DT = DT.GMT-lubridate::hours(5)) %>% 
  nest(indData = !trap_locat) %>% 
  mutate(n.locs = map_dbl(indData, ~nrow(.))) %>% 
  filter(n.locs > 3) %>%
  mutate(indData = map(indData, function(x){
    # x <- rsfData$indData[[1]]
    x %>%
      make_track(.x = easting,
                 .y = northing,
                 .t = DT,
                 crs = 26916) %>%
      random_points(., hr = "mcp", n = nrow(x) * 5, level = 1) %>%
      extract_covariates(distanceStack)
  })) %>%
  unnest(indData)



pop.rsf <- glmer(case ~ season*(Agricultural + Wetland + Hardwood + Natural Pine) + Agriculture*(Natural.Pine + Wetland) + (1|anid),
      data = rsfData %>% mutate(case = ifelse(case_ == T, 1, 0)),
      family = "binomial")

summary(pop.rsf)

library(MuMIn)

pop.dre <- dredge(pop.rsf, fixed = ~(1|anid))

competing <- model.sel(pop.dre, subset = "delta" < 2)

  
