#' Convert GEDI level 4A data to \code{sf} object and export it
#'
#' Write GEDI level 4A data to disk as a vector file.
#'
#' @param gediL4 \code{data.table} obtained with the function
#'   \code{l4_getmulti()} or \code{l4_clip}, or an \code{sf} object.
#' @param epsg Numeric: destination EPSG code. Default to NULL.
#' @param filename Character: path to data source name. Usually a file name with
#'   extension. See \code{?st_write} for more details.
#' @param return_path Logical: if TRUE, the path of the saved file will be
#'   returned, otherwise the function will return NULL. Default to FALSE.
#' @param ... Others argument to pass to \code{st_write}, such as \code{driver}
#'   or \code{append}
#' @details If a file extension will cause conflict with the driver, the
#'   extension will be guessed from the file name. Note that in converting data
#'   to ESRI Shapefile, columns names will be abbreviated with a warning.
#' @return The function is called for its side effects. It return \code{NULL}
#'   unless \code{return_path=TRUE}.
#' @seealso \code{\link[sf]{st_write}}
#' @examples
#'
#'outdir = tempdir()
#'l4_zip <- system.file("extdata",
#'                      "GEDI04_A_2020036151358_O06515_02_T00198_02_002_01_V002.zip",
#'                      package="GEDI4R")
#'l4 <- unzip(l4_zip,exdir = outdir)
#'#get GEDI data
#'l4_data <- l4_getmulti(l4)
#'#convert to shp file
#'converted <- l4_convert(l4_data,epsg = 4326,filename=paste0(outdir,"/example.shp"),return_path = T,append=F)
#'example <- sf::read_sf(converted)
#'file.remove(list.files(outdir,pattern = "example",full.names = T))
#' @export


 l4_convert <- function(gediL4,epsg=NULL,filename,return_path=F,...){

  if(is.null(gediL4))return(NULL)

   stopifnot(
     "gediL4 is not of the right class:\n must be one of data.table, data.frame or sf" =
       class(gediL4) %in% c("data.table", "data.frame", "sf"),
     "EPSG code is NULL or is not numeric" = check_num(epsg),
     "Filename is not a character" = check_char(filename)
   )

  epsg <- as.integer(epsg)
  if(!any(class(gediL4)%in%c("sf"))){
    gediL4 <- sf::st_as_sf(gediL4,coords=c("lon_lowestmode","lat_lowestmode"))
    sf::st_crs(gediL4) <- 4326
  }
  suppressWarnings(sf_ob <- sf::st_transform(gediL4, epsg))
  sf::st_write(sf_ob,dsn=filename,...)

  if(return_path){
    return(filename)
  }else{  invisible()}
}

