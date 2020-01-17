#' Chicago Crime data from 2001 to 2019
#'
#' A dataset containing reported crimes in the city of Chicago in the period 2001 to 2019.
#' @usage data(all_crime)
#' @format A data frame with 6,336,525 rows and 14 variables:
#' \describe{
#'   \item{Date}{date of which the crime was reported}
#'   \item{Primary Type}{type of crime that was reported, one of a finite amount of options}
#'   \item{Description}{description of the reported crime}
#'   \item{Location Description}{type of place where the crime was committed}
#'   \item{Arrest}{logical; if \code{TRUE}, then the crime resulted in an arrest}
#'   \item{Domestic}{logical; if \code{TRUE}, then the incident was domestic-related}
#'   \item{Beat}{the beat where the crime occurred (smallest police geographical area)}
#'   \item{District}{police district where the crime occurred}
#'   \item{Ward}{the ward (City Council district) in Chicago where the crime occurred}
#'   \item{Community Area}{the community area where the incident occurred}
#'   \item{Year}{year when the crime happened}
#'   \item{Latitude}{latitude co-ordinate (in degrees) of the block where the crime occurred}
#'   \item{Longitude}{longitude co-ordinate (in degrees) of the block where the crime occurred}
#'   \item{Hour}{estimated hour of the day when the crime happened}
#' }
#' @source \url{https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2}
"all_crime"

#' @rdname all_crime
#' @usage data(sub_all_crime)
"sub_all_crime"


#' Chicago Census data from 2008-2012
#'
#' A dataset containing selected socioeconomic indicators in the city of Chicago from 2008 to 2012
#'
#' @format A data frame with 78 rows and 9 variables:
#' \describe{
#'   \item{Community Area Number}{identification number of the community area, matches with that in \code{all_crime}}
#'   \item{COMMUNITY AREA NAME}{name of the community area}
#'   \item{PERCENT OF HOUSING CROWDED}{Percentage of occupied housing units, in the community area, with more than one person per room}
#'   \item{PERCENT HOUSEHOLDS BELOW POVERTY}{Percentage of households, in the community area, that are living below the federal poverty level }
#'   \item{PERCENT AGED 16+ UNEMPLOYED}{Percentage of persons, in the community area, that are over the age of 16 years that are unemployed}
#'   \item{PERCENT AGED 25+ WITHOUT HIGH SCHOOL DIPLOMA}{Percentage of persons in the community area that are over the age of 25 years without a high school education}
#'   \item{PERCENT AGED UNDER 18 OR OVER 64}{Percent of the population of the community area under 18 or over 64 years of age}
#'   \item{PER CAPITA INCOME}{Community Area Per capita income; estimated as the sum of tract-level aggregate incomes divided by the total population }
#'   \item{HARDSHIP INDEX}{Score that incorporates each of the six selected socioeconomic indicators}
#' }
#' @source \url{https://data.cityofchicago.org/Health-Human-Services/Census-Data-Selected-socioeconomic-indicators-in-C/kn9c-c2s2}
"census"

#' Chicago Police Station locations
#'
#' A dataset containing the longitudes and latitudes of police stations in the city of Chicago, updated June 2016
#'
#' @format A data frame with 23 rows and 2 variables:
#' \describe{
#'   \item{lon}{longitude of a police station}
#'   \item{lat}{latitude of a police station}
#' }
#' @source \url{https://data.cityofchicago.org/Public-Safety/Police-Stations/z8bn-74gv}
"police_stations"


#' Chicago Population data
#'
#' A dataset containing the population totals of census blocks in the city of Chicago, measured by the longitude and latitude of
#'
#' @format A data frame with 46269 rows and 4 variables:
#' \describe{
#'   \item{GEOID10}{identification number of census block}
#'   \item{lon}{longitude of the centre of the census block}
#'   \item{lat}{latitude of the centre of the census block}
#'   \item{total}{population total of the census block}
#' }
#' @source
#' \url{https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Population-by-2010-Census-Block/5yjb-v3mj}
#' \url{https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Census-Blocks-2010/mfzt-js4n}
"chicago_population"

#' Chicago Population data by Census block
#'
#' A dataset containing the population totals of census blocks in the city of Chicago
#'
#' @format A data frame with 46291 rows and 3 variables:
#' \describe{
#'   \item{CENSUS BLOCK}{shortened identification number of the census block, without the first 5 numbers (relating to area in country), same as \code{TRACT_BLOC} in \code{\link{block_populations}}}
#'   \item{CENSUS BLOCK FULL}{full identification number of the census block, same code as \code{GEOID10} in \code{\link{block_populations}}}
#'   \item{TOTAL POPULATION}{total population per census block}
#' }
#' @source
#' \url{https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Population-by-2010-Census-Block/5yjb-v3mj}
"block_populations"

#' Chicago Census Block Boundaries
#'
#' A dataset containing the locations of census blocks in Chicago
#'
#' @format A data frame with 46291 rows and 3 variables:
#' \describe{
#'   \item{the_geom}{string indicating polygon co-ordinates of the census block}
#'   \item{STATEFP10}{two digit number referring to state code of census block}
#'   \item{COUNTYFP10}{three digit number refferring to county code of census block}
#'   \item{TRACTCE10}{six digit number referring to area in chicago of census block}
#'   \item{BLOCKCE10}{four digit number referring to block in census block}
#'   \item{GEOID10}{full identification number of census block, same as \code{CENSUS BLOCK FULL} in \code{\link{block_boundaries}}}
#'   \item{NAME10}{name of census block}
#'   \item{TRACT_BLOC}{shortened identification number of the census block, without the first 5 numbers (relating to area in country), same as \code{CENSUS BLOCK} in \code{\link{block_boundaries}}}
#'
#' }
#' @source
#' \url{https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Census-Blocks-2010/mfzt-js4n}
"block_boundaries"
