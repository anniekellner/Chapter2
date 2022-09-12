####################################################
####    FUNCTIONS FOR CHAPTER 2   ##################
####################################################


#' Dataframe to SF
#' 
#' @param df Dataframe 
#' @param x Column name of x coordinate (character)
#' @param y Column name of y coordinate (character)
#' @param setCRS CRS df should be set to initially
#' @param toCRS Desired CRS

DFtoSF <- function(df, x, y, setCRS, toCRS) {
  df <- droplevels(df)
  df.sf <- st_as_sf(df, coords = c('x', 'y'), crs = setCRS, dim = 'XY')
  sf.project <- st_transform(df.sf, toCRS)
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



