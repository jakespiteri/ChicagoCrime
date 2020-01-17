setwd('~/Documents/PhD/chicago_crime_project/ChicagoCrime/')

data_file_path <- '../data/crimes2001topresent.csv'

library(tidyverse)
library(lubridate)

# read csv
coltypes = list("_","_","c","_","_","c","c","c","l","l","d","d","d","d","_","_","_","d","_","d","d","_")
all_crime = read_csv(data_file_path, quote = '"', col_types = coltypes)

# convert date and time to date and time objects
all_crime$Hour = substr(all_crime$Date,12,22)
all_crime$Hour[substr(all_crime$Hour,10,11)=="PM"] = as.numeric(substr(all_crime$Hour[substr(all_crime$Hour,10,11)=="PM"],1,2))+12
all_crime$Hour[substr(all_crime$Hour,10,11)=="AM"] = substr(all_crime$Hour[substr(all_crime$Hour,10,11)=="AM"],1,2)
all_crime$Hour = as.numeric(all_crime$Hour)
all_crime$Date = as.Date(all_crime$Date, format = "%m/%d/%Y")

#crimes <- read_csv('../data/crimes2001topresent.csv')
#crimes_subsample <- sample_n(crimes, 100000)
#rm(crimes)

set.seed(123)
crimes_subsample <- sample_n(all_crime, 50000)
crimes_subsample <- drop_na(crimes_subsample)
#rm(all_crime)
crimes_subsample <- crimes_subsample %>% filter(Longitude > quantile(Longitude, 0.01) - IQR(Longitude)) %>%
                     filter(Longitude < quantile(Longitude, 0.99) + IQR(Longitude)) %>%
                     filter(Latitude > quantile(Latitude, 0.01) - IQR(Latitude)) %>%
                     filter(Latitude < quantile(Latitude, 0.99) + IQR(Latitude))


## KDE for raw crimes
library(KernSmooth)
library(leaflet)
library("sp")
library("rgdal")

crime_loc <- select(crimes_subsample, Longitude, Latitude)

gs <- 50L

h=0.01
crime_kde <- bkde2D(as.matrix(crime_loc),
                     bandwidth = c(h,h),
                     gridsize = c(gs,gs))
contour_lines <- contourLines(crime_kde$x1 , crime_kde$x2 , crime_kde$fhat)
levels <- as.factor(sapply(contour_lines, `[[`, "level"))
n_levels <- length(levels(levels))
## CONVERT CONTOUR LINES TO POLYGONS
polygons <- lapply(1:length(contour_lines), function(i)
  Polygons(list(Polygon(cbind(contour_lines[[i]]$x, contour_lines[[i]]$y))), ID=i))
s_polygons = SpatialPolygons(polygons)
## Leaflet map with polygons
leaflet(s_polygons) %>% addTiles() %>%
  addPolygons(color = heat.colors(n_levels, NULL)[levels])




## POPULATION DATA

census_blocks_file_path <- '../data/census_blocks_2010.csv'
block_population_file_path <- '../data/population_by_census_block_2010.csv'
census_blocks <- read_csv(census_blocks_file_path)
block_populations <- read_csv(block_population_file_path)

process_long_lat <- function(x, long = TRUE){
  x %<>%
    str_remove('^.{16}') %>%
    str_remove('.{3}$') %>%
    str_remove_all(',') %>%
    str_split(' ') %>%
    unlist() %>%
    as.numeric() %>%
    matrix(ncol=2, byrow = TRUE) %>%
    colMeans()
  if (long == TRUE){
    return(as.double(x[1]))
  } else return(as.double(x[2]))
}

census_blocks_centers <- census_blocks %>%
  rowwise() %>%
  mutate(Longitude = sapply(the_geom, process_long_lat, long=TRUE)) %>%
  mutate(Latitude = sapply(the_geom, process_long_lat, long=FALSE)) %>%
  select(CENSUS_BLOCK = TRACT_BLOC, Longitude, Latitude) %>%
  drop_na()

block_populations <- block_populations %>%
  select(CENSUS_BLOCK = `CENSUS BLOCK`, TOTAL_POPULATION = `TOTAL POPULATION`) %>%
  right_join(census_blocks_centers, by = 'CENSUS_BLOCK') %>%
  transform(Longitude = as.numeric(Longitude)) %>%
  transform(Latitude = as.numeric(Latitude)) %>%
  arrange(Longitude, Latitude)



census_blocks_centers_long_lat <- select(census_blocks_centers, Longitude, Latitude) %>% drop_na()
census_blocks_centers_long_lat <- unnest(census_blocks_centers_long_lat, cols = c(Longitude, Latitude)) %>% drop_na()

block_populations <- drop_na(block_populations)
block_populations_expanded <- block_populations[rep(seq_len(nrow(block_populations)), floor(block_populations$TOTAL_POPULATION)),]

census_block_centers_long_lat_expanded <- select(block_populations_expanded, Longitude, Latitude) %>% drop_na()

## Population KDE

h=0.01
census_kde <- bkde2D(as.matrix(census_block_centers_long_lat_expanded),
                    bandwidth = c(h,h),
                    gridsize = c(gs,gs))
contour_lines <- contourLines(census_kde$x1 , census_kde$x2, census_kde$fhat)
levels <- as.factor(sapply(contour_lines, `[[`, "level"))
n_levels <- length(levels(levels))
## CONVERT CONTOUR LINES TO POLYGONS
polygons <- lapply(1:length(contour_lines), function(i)
  Polygons(list(Polygon(cbind(contour_lines[[i]]$x, contour_lines[[i]]$y))), ID=i))
s_polygons = SpatialPolygons(polygons)
## Leaflet map with polygons
leaflet(s_polygons) %>% addTiles() %>%
  addPolygons(color = heat.colors(n_levels, NULL)[levels])

gs <- length(crime_kde$x1)
crime_kde_tbl <- tibble(Longitude = rep(crime_kde$x1, each=gs),
                        Latitude = rep(crime_kde$x2, gs),
                        crime_fhat = as.vector(crime_kde$fhat))
census_kde_tbl <- tibble(Longitude = rep(census_kde$x1, each=gs),
                         Latitude = rep(census_kde$x2, gs),
                         census_fhat = as.vector(census_kde$fhat))
crime_kde_tbl_drop0 <- crime_kde_tbl %>% filter(crime_fhat > 1e-8)
census_kde_tbl_drop0 <- census_kde_tbl %>% filter(census_fhat > 1e-8)


library(geosphere)
crimes_w_crime_density <- joinLonLat(crimes_subsample, crime_kde_tbl, function(z1, z2) z2$crime_fhat, outname="Crime Density", inc_lonlat = FALSE)
crimes_w_crime_density <- joinLonLat(crimes_w_crime_density, census_kde_tbl, function(z1, z2) z2$census_fhat, outname="Population Density", inc_lonlat = FALSE)
crimes_w_crime_density <- mutate(crimes_w_crime_density,
                                 Ratio = `Crime Density`/`Population Density`)

crime_and_census_density_tbl <- joinLonLat(crime_kde_tbl, census_kde_tbl, function(z1,z2) z2$census_fhat, outname="census_fhat", inc_lonlat = FALSE)
ratio_densities_tbl <- crime_and_census_density_tbl %>%
  mutate(`Ratio Density` = crime_fhat / census_fhat)

ratio_densities_tbl <- mutate(ratio_densities_tbl, `Ratio Density` = replace(ratio_densities_tbl$`Ratio Density`, which(!is.finite(ratio_densities_tbl$`Ratio Density`)), 0)) # replace -Inf and NaN with 0

upper_limit <- quantile(ratio_densities_tbl$`Ratio Density`, 0.95, na.rm = TRUE)
ratio_densities_tbl <- mutate(ratio_densities_tbl, `Ratio Density` = replace(ratio_densities_tbl$`Ratio Density`, which(ratio_densities_tbl$`Ratio Density` > upper_limit), upper_limit)) # set upper limit on values


ratio_matrix <- matrix(ratio_densities_tbl$`Ratio Density`, nrow = gs, ncol = gs)

# plot ratio
contour_lines <- contourLines(crime_kde$x1 , crime_kde$x2, ratio_matrix)
levels <- as.factor(sapply(contour_lines, `[[`, "level"))
n_levels <- length(levels(levels))
## CONVERT CONTOUR LINES TO POLYGONS
polygons <- lapply(1:length(contour_lines), function(i)
  Polygons(list(Polygon(cbind(contour_lines[[i]]$x, contour_lines[[i]]$y))), ID=i))
s_polygons = SpatialPolygons(polygons)
## Leaflet map with polygons
leaflet(s_polygons) %>% addTiles() %>%
  addPolygons(color = heat.colors(n_levels, NULL)[levels])





## ----------------- IGNORE EVERYTHING FROM HERE DOWN -----------------------

round_down <- function(x, digits = 0) floor(x * 10^digits) / 10^digits

n <- length(crime_kde$x1)
## LINE UP GRIDS (not going to work)
crime_kde_tbl <- tibble(x1 = rep(round_down(crime_kde$x1, 2), each = n),
                        x2 = rep(round_down(crime_kde$x2, 2), n),
                        crime_fhat = as.vector(crime_kde$fhat))
census_kde_tbl <- tibble(x1 = rep(round_down(census_kde$x1, 2), each = n),
                         x2 = rep(round_down(census_kde$x2, 2), n),
                         census_fhat = as.vector(census_kde$fhat))
joined_kde_tbl <- full_join(crime_kde_tbl, census_kde_tbl)
joined_kde_tbl <-  mutate(joined_kde_tbl, fhat_ratio = crime_fhat / census_fhat)
joined_kde_tbl$fhat_ratio[joined_kde_tbl$fhat_ratio == -Inf] <- 0
joined_kde_tbl$fhat_ratio[which(is.na(joined_kde_tbl$fhat_ratio))] <- 0
contour_lines <- contourLines(joined_kde_tbl$x1[((0:(n-1))*n)+1] , joined_kde_tbl$x2[1:n] , as.matrix(joined_kde_tbl$fhat_ratio, nrow = n, ncol = n))



## Ratio KDE

crime_kde_fhat_mod <- crime_kde$fhat
crime_kde_fhat_mod[crime_kde_fhat_mod < 5] <- NA
census_kde_fhat_mod <- census_kde$fhat
census_kde_fhat_mod[census_kde_fhat_mod < 5] <- NA
ratio_matrix <- crime_kde_fhat_mod / census_kde_fhat_mod
ratio_matrix[which(is.na(ratio_matrix))] <- 0

contour_lines <- contourLines(census_kde$x1 , census_kde$x2 , ratio_matrix)
levels <- as.factor(sapply(contour_lines, `[[`, "level"))
n_levels <- length(levels(levels))
## CONVERT CONTOUR LINES TO POLYGONS
polygons <- lapply(1:length(contour_lines), function(i)
  Polygons(list(Polygon(cbind(contour_lines[[i]]$x, contour_lines[[i]]$y))), ID=i))
s_polygons = SpatialPolygons(polygons)
## Leaflet map with polygons
leaflet(s_polygons) %>% addTiles() %>%
  addPolygons(color = heat.colors(n_levels, NULL)[levels])

