#' Clean Data
#'
#' @param crime_data a data frame to be cleaned
#'
#' @return A cleaned dataset with \code{NA} entries removed
#' @export
#' @importFrom tidyr drop_na
#' @importFrom dplyr filter %>%
#' @importFrom stats quantile IQR
#' @examples
#' \dontrun{
#' data(sub_all_crime)
#' sub_all_crime = cleanData(sub_all_crime) }
cleanData <- function(crime_data){
  crime_data = drop_na(crime_data)
  n = names(table(crime_data$`Primary Type`)[table(crime_data$`Primary Type`)-(nrow(crime_data)*0.01) <= 0])
  for(i in n) crime_data=crime_data[crime_data$`Primary Type`!=i,]

  crime_data <- crime_data %>% filter(Longitude > quantile(Longitude, 0.01) - IQR(Longitude)) %>%
    filter(Longitude < quantile(Longitude, 0.99) + IQR(Longitude)) %>%
    filter(Latitude > quantile(Latitude, 0.01) - IQR(Latitude)) %>%
    filter(Latitude < quantile(Latitude, 0.99) + IQR(Latitude))

  crime_data = crime_data[crime_data$Year > 2001,]

  return(crime_data)
}


#' Format crime data to include census and police station data
#'
#' @param crime_data dataframe from \code{data(all_crime)}
#' @param densities logical; if \code{TRUE}, population densities, crime densities, as well as the ratio between the two will also be joined to \code{crime_data}
#' @param verbose logical; if \code{TRUE}, formatting procedures will be displayed as they are completed
#' @param ... further arguments to be passed to \code{\link{create_spatial_attributes}}
#'
#' @return data of the same form of \code{crime_data}, with appended census and police station data
#' @export
#' @importFrom tidyr drop_na
#' @importFrom dplyr %>% right_join group_by summarise
#' @import utils
#' @examples
#' \dontrun{
#' data(sub_all_crime)
#' sub_all_crime = cleanData(sub_all_crime)
#' sub_all_crime = formatData(sub_all_crime) }
formatData <- function(crime_data, densities = TRUE, verbose = TRUE, ...){

  if(verbose) cat("Appending census data \n")
  data("census", envir = environment())
  colnames(census)[1] = "Community Area"
  crime_data = crime_data %>% right_join(census, by = "Community Area")

  if(verbose) cat("Loading police station data \n")
  data("police_stations", envir = environment())
  colnames(police_stations) = c("Longitude", "Latitude")
  crime_data = drop_na(crime_data)
  crime_data = as.data.frame(crime_data)

  if(verbose) cat("Joining nearest police stations \n")
  crime_data = joinLonLat(crime_data, police_stations, outname = "dist_from_station",
                          inc_lonlat = FALSE, verbose=verbose)
  if(densities){
    if(verbose) cat("Loading population/crime densities \n")
    data("block_populations", envir = environment())
    data("block_boundaries", envir = environment())
    crime_data = create_spatial_attributes(crime_data, block_boundaries,
                                           block_populations, ...)
  }
  crime_data = drop_na(crime_data)

  if(verbose) cat("Done")
  return(crime_data)
}

#' Join by Longitude/Latitude
#'
#' @description Append one data frame to another by nearest longitude and latitude
#'
#' @param x data frame to be appended to; should be the larger of \code{x} and \code{y}
#' @param y data frame to join onto \code{x}
#' @param f function to define what column is added to \code{x}, see 'Details' and 'Examples'
#' @param lonname name of the longitude column shared by both data frames
#' @param latname name of the latitude column shared by both data frames
#' @param outname name of the column added to \code{x} by \code{f}
#' @param inc_lonlat logical; if \code{TRUE}, longitude and latitude co-ordinates from \code{y} will be added to \code{x}
#' @param verbose logical; if \code{TRUE}, progress bars will be displayed as data sets are joining
#'
#' @return the original data frame \code{x}, with one appended column defined by \code{f}, and the longitudes/latitudes if \code{inc_lonlats=TRUE}.
#' %>%
#' @details
#' This function first finds the distance between two sets of co-ordinates, for each data frame, using the
#' haversine distance function (from the \code{geosphere} package),
#' \deqn{
#'  d = 2 r arcsin( \sqrt{ sin^2 ( (\theta_2 - \theta_1) / 2 ) + cos (\theta_1) cos(\theta_2) sin^2( (\phi_2 - \phi_1)/2 ) } ),
#' }
#' where \eqn{(\phi_1, \theta_1)} is the longitude and latitude of point 1, and \eqn{(\phi_2, \theta_2)}
#' is the longitude and latitude of point 2, both in radians, and \eqn{r} is the radius of the Earth (6378137m).
#' Once the distances are found, the smallest distances are calculated and those rows in the data frame \code{y}
#' that are closest to each row in \code{x} are appended
#'
#' The function \code{f} decides what column will be appended to the original data frame \code{x}, it takes two arguments
#' (but both do not need to be used), where the first argument uses data frame \code{x}, and the second uses \code{y}.
#' @importFrom geosphere distHaversine
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
#' @examples
#' coords1 = data.frame("Longitude" = c(-20, -25, -23, -40),
#'                      "Latitude" = c(10, 15, 23, 13))
#' coords2 = data.frame("Longitude" = c(-17, -15, -26),
#'                 "Latitude"= c(11, 31, 42),
#'                 "Names" = c("Frasier", "Niles", "Martin"))
#' joinLonLat(coords1, coords2, f = function(z1, z2) z2$Names, outname="Closest_Names")
#'
joinLonLat <- function(x, y, f = NULL, lonname="Longitude", latname="Latitude",
                       outname = "distance", inc_lonlat = TRUE, verbose=TRUE){
  whichmindists = rep(NA, nrow(y))
  if(verbose) progress = txtProgressBar(1,nrow(x),style=3)
  for(i in 1:nrow(x)) {
    whichmindists[i] = which.min(distHaversine(y[,c(lonname,latname)], x[i,c(lonname,latname)]))
    if(verbose) setTxtProgressBar(progress, i)
  }
  if(verbose) cat("\n")

  min_y = y[whichmindists,]

  if(is.null(f)) f = function(x,y) {distHaversine(x[,c("Longitude","Latitude")],y)/1000}

  x$var99 = f(x, min_y)
  colnames(x)[colnames(x)=="var99"] = outname

  if(inc_lonlat){
    x$y_Longitude = min_y[,lonname]
    x$y_Latitude = min_y[,latname]
  }

  return(x)
}


joinPopulationDensities <- function(crime_data, verbose=TRUE){
  create_spatial_attributes(crimes_subsample, block_boundaries_data, population_data, h=0.01, grid.size = 50L)
}






