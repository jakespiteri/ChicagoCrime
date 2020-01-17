#' Fit Kernel Density Estimator
#'
#' @description Fits Kernel density estimates using \code{\link[KernSmooth]{bkde2D}}
#'
#' @param data Data frame containing columns \code{Longitude} and \code{Latitude}.
#' @param h Bandwidth of KDE.
#' @param grid.size Number of rows/columns of grid on which density estimate is returned.
#'
#' @return Kernel density estimate in the form:
#' \item{\code{x1}}{Longitudes of grid}
#' \item{\code{x2}}{Latitudes of grid}
#' \item{\code{fhat}}{Matrix of density estimates at grid points}
#'
#' @importFrom KernSmooth bkde2D
#' @export
fit_kde <- function(data, h, grid.size) {
  location.data <- select(data, Longitude, Latitude)
  kde <- bkde2D(as.matrix(location.data),
                bandwidth = c(h,h),
                gridsize = c(grid.size, grid.size))
  kde
}

#' Plot Kernel Density Estimate
#' @description Plots kernel density estimates on map using \code{\link[leaflet]{leaflet}}
#'
#' @param kde Kernel density estimate object generated using \code{\link{fit_kde}}
#'
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles addPolygons
#' @importFrom grDevices contourLines heat.colors
#' @importFrom sp Polygon SpatialPolygons Polygons
#'
#' @export
plot_kde <- function(kde) {
  contour_lines <- contourLines(kde$x1 , kde$x2 , kde$fhat)
  levels <- as.factor(sapply(contour_lines, `[[`, "level"))
  n_levels <- length(levels(levels))

  polygons <- lapply(1:length(contour_lines), function(i)
    Polygons(list(Polygon(cbind(contour_lines[[i]]$x, contour_lines[[i]]$y))), ID=i))
  s_polygons = SpatialPolygons(polygons)

  leaflet(s_polygons) %>% addTiles() %>%
    addPolygons(color = heat.colors(n_levels, NULL)[levels])
}


#' Geom string process
#' Process \code{the_geom} string to return block center
#'
#' @param x \code{the_geom} string from block boundaries data
#' @param long if TRUE returns longitude, else returns latitide
#'
#' @importFrom stringr str_remove str_split str_remove_all
#' @importFrom magrittr %>% %<>%
#' @return center of block (longitude or latitude)
process_geom_string <- function(x, long = TRUE){
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

#' Add Location Data
#' @description Adds locations information to population data from census block id
#'
#' @param block_boundaries_data dataset of census block boundaries
#' @param population_data dataset of population by census block
#'
#' @importFrom dplyr rowwise mutate right_join select arrange
#' @importFrom tidyr drop_na
#'
#' @export
#' @return population dataset with location data
add_location_to_population_data <- function(block_boundaries_data,
                                    population_data) {
  blocks_centers <- block_boundaries_data %>%
    rowwise() %>%
    mutate(Longitude = sapply(the_geom, process_geom_string, long=TRUE)) %>%
    mutate(Latitude = sapply(the_geom, process_geom_string, long=FALSE)) %>%
    select(CENSUS_BLOCK = TRACT_BLOC, Longitude, Latitude) %>%
    drop_na()

  population_data <- population_data %>%
    select(CENSUS_BLOCK = `CENSUS BLOCK`, TOTAL_POPULATION = `TOTAL POPULATION`) %>%
    right_join(blocks_centers, by = 'CENSUS_BLOCK') %>%
    transform(Longitude = as.numeric(Longitude)) %>%
    transform(Latitude = as.numeric(Latitude)) %>%
    arrange(Longitude, Latitude)

  population_data <- drop_na(population_data)

  population_data
}


#' Format population data
#' @description  Make population data suitable for \code{\link{fit_kde}} function
#'
#' @param population_data dataset of populations at locations
#' @importFrom dplyr select
#' @return dataset of home locations with one row per person
make_population_data_for_kde <- function(population_data) {
  population_data_expanded <- population_data[rep(seq_len(nrow(population_data)), floor(population_data$TOTAL_POPULATION)),]
  population_data_expanded <- select(population_data_expanded, Longitude, Latitude)
}


#' Add density attributes to data
#'
#' @param crime_data full crime dataset
#' @param crime_kde kernel density estimate of crimes
#' @param population_kde kernel density estimate of population
#'
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @export
#' @return crime dataset with three additional columns:
#' \item{\code{Crime Density}}{crime density at the location of crime}
#' \item{\code{Population Density}}{population density at the location of crime}
#' \item{\code{Ratio Density}}{crime/population density at the location of crime}

add_density_attributes_to_data <- function(crime_data, crime_kde, population_kde) {
  gs <- length(crime_kde$x1)
  crime_kde_tbl <- tibble(Longitude = rep(crime_kde$x1, each=gs),
                          Latitude = rep(crime_kde$x2, gs),
                          crime_density = as.vector(crime_kde$fhat))
  population_kde_tbl <- tibble(Longitude = rep(population_kde$x1, each=gs),
                               Latitude = rep(population_kde$x2, gs),
                               population_density = as.vector(population_kde$fhat))
  crimes_data_w_density_attributes <- joinLonLat(crime_data, crime_kde_tbl, function(z1, z2) z2$crime_density, outname="Crime Density", inc_lonlat = FALSE)
  crimes_data_w_density_attributes <- joinLonLat(crimes_data_w_density_attributes, population_kde_tbl, function(z1, z2) z2$population_density, outname="Population Density", inc_lonlat = FALSE)
  crimes_data_w_density_attributes
}


#' Create spatial attributes
#'
#' @param crime_data full crime dataset
#' @param block_boundaries_data dataset of census block boundaries
#' @param population_data dataset of population by census block
#' @param h bandwidth for kernel density estimation
#' @param grid.size grid size for kernel density estimation
#' @export
#' @return dataset with attributes added

create_spatial_attributes <- function(crime_data,
                                      block_boundaries_data,
                                      population_data,
                                      h = 0.01,
                                      grid.size = 100L) {
  population_data_w_location <- add_location_to_population_data(block_boundaries_data, population_data)
  population_data_for_kde <- make_population_data_for_kde(population_data_w_location)
  crime_kde <- fit_kde(crime_data, h, grid.size)
  population_kde <- fit_kde(population_data_for_kde, h, grid.size)
  crimes_data_w_density_attributes <- add_density_attributes_to_data(crime_data, crime_kde, population_kde)
}











