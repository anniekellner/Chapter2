####################################################
####    FUNCTIONS FOR CHAPTER 2   ##################
####################################################


#' Dataframe to SF
#' 
#' @param df Dataframe with lat/long (EPSG: 4326)
#' @param projection what I want the data projected to

DFtoSF <- function(df, projection) {
  df <- droplevels(df)
  sf.wgs <- st_as_sf(df, coords = c('gps_lon', 'gps_lat'), crs = 4326, dim = 'XY')
  sf.project <- st_transform(sf.wgs, projection)
  return(sf.project)
}


#' Drop geometry from sf object and return dataframe
#' 
#' @param x sf object

st_drop_geometry <- function(x) {
  if(inherits(x,"sf")) {
    x <- st_set_geometry(x, NULL)
    class(x) <- 'data.frame'
  }
  return(x)
}

#' Separate geometry column into two columns for X and Y
#' 
#' @param x sf object with geometry columns

geom_into_columns <- function(x){
  x = cbind(x, st_coordinates(x))
  x = st_drop_geometry(x)
}

#' Erase one spatial file from another (erase y from x)
#' 
#' @param x,y sf objects  
st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))



