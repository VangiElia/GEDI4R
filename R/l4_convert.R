#' Convert GEDI level 4A data to \code{sf} object and export it
#'
#' Write GEDI level 4A data to disk as a vector file.
#'
#' @param gediL4 \code{data.table} obtained with the function
#'   \code{l4_getmulti()} or \code{l4_clip}, or a \code{SpatVector} object.
#' @param epsg Numeric: destination EPSG code. Default to NULL.
#' @param filename Character: path to data source name. Usually a file name with
#'   extension. See \code{?writeVector} for more details.
#' @param return_path Logical: if TRUE, the path of the saved file will be
#'   returned, otherwise the function will return NULL. Default to FALSE.
#' @param ... Others argument to pass to \code{writeVector}, such as \code{filetype}
#'   or \code{overwrite}
#' @details If a file extension will cause conflict with the filetype, the
#'   extension will be guessed from the file name. Note that in converting data
#'   to ESRI Shapefile, columns names will be abbreviated.
#' @return The function is called for its side effects. It return \code{NULL}
#'   unless \code{return_path=TRUE}.
#' @seealso \code{\link[sf]{writeVector}}
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
#'converted <- l4_convert(l4_data,epsg = 4326,filename=paste0(outdir,"/example.shp"),return_path = T)
#'example <- terra::vect(converted)
#'file.remove(list.files(outdir,pattern = "example",full.names = T))
#' @export


 l4_convert <- function(gediL4,epsg=NULL,filename,return_path=F,...){

  if(is.null(gediL4))return(NULL)

   stopifnot(
     "gediL4 is not of the right class:\n must be one of data.table, data.frame, sf or SpatVector" =
       class(gediL4) %in% c("data.table", "data.frame", "SpatVector","sf"),
     "EPSG code is NULL or is not numeric" = check_num(epsg),
     "Filename is not a character" = check_char(filename)
   )

  epsg <- as.integer(epsg)
  if(any(class(gediL4)%in%c("sf"))){
    gediL4 <- terra::vect(gediL4)
  }
  if(!any(class(gediL4)%in%c("SpatVector","sf"))){
    gediL4 <- terra::vect(gediL4,geom=c("lon_lowestmode","lat_lowestmode"), crs="EPSG:4326", keepgeom=T)
  }
  suppressWarnings(vect_ob <- terra::project(gediL4, paste0("epsg:",epsg)))
  terra::writeVector(vect_ob,filename=filename,...)

  if(return_path){
    return(filename)
  }else{invisible()}
}

