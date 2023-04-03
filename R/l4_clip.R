#' Clip GEDI level 4A data to an extent or Shapefile boundary
#'
#' Useful to get GEDI data for a specific study area, by clipping the data to a
#' vector boundary or extent. Remember that, by default GEDI coordinates are in
#' lon/lat format (EPSG 4326).
#'
#' @param gediL4 \code{data.table} obtained with the function
#'   \code{l4_getmulti}.
#' @param clip An object from which an extent can be obtained (xmin, xmax, ymin,
#'   ymax). Currently could be a path to a shp or tif file, an object of class
#'   \code{sf}, a Raster* object, a SpatRaster, SpatVector, or a numeric
#'   vector of coordinates.
#' @param usegeometry Logical: should the points be clipped on the boundary of
#'   a vector geometry (or path from which a geometry can be
#'   created). NULL and FALSE will be interpreted as the same, in that case the
#'   extent of \code{clip} will be used. Default to NULL.
#' @param tct Numeric: tree cover threshold to filter GEDI data.
#' @details GEDI coordinates are by default in lon/lat format (EPSG 4326). The
#'   function will try to convert the extent of \code{clip} to lon/lat
#'   coordinate system to ensure compatibility during the clip. The only
#'   exeption is when \code{clip} is a numeric vector or an extent object. In these
#'   cases the user must check that the extent is in lon/lat
#'   projection.
#' @return A \code{data.table} object if \code{usegeometry=NULL} or \code{FALSE},
#'   a \code{SpatVector} otherwise. It return \code{NULL} if \code{gediL4} is
#'   \code{NULL}.
#' @examples
#'
#'outdir = tempdir()
#'l4_zip <- system.file("extdata",
#'                      "GEDI04_A_2020036151358_O06515_02_T00198_02_002_01_V002.zip",
#'                      package="GEDI4R")
#'l4 <- unzip(l4_zip,exdir = outdir)
#'#get GEDI data
#'l4_data <- l4_getmulti(l4)
#'#clip using vector of coordinates
#'b_box <- c(-50,35,52,37)
#'clipped <- l4_clip(l4_data,clip=b_box)
#'#using Shapefile to clip
#'bound <- system.file("extdata","bound4326.shp",package="GEDI4R")
#'#with  extension
#'clipped <- l4_clip(l4_data,clip=bound,usegeometry = F)
#'#with  polygon boundaries
#'clipped2 <- l4_clip(l4_data,clip=bound,usegeometry = T)
#'@export


l4_clip <- function (gediL4,clip,usegeometry=NULL,tct=NULL) {

  if(is.null(gediL4))return(NULL)

  #input check
  stopifnot(
    "clip is missing with no default"= !missing(clip),
    "usegeometry must be logical or NULL"= any(is.logical(usegeometry),is.null(usegeometry))
  )

  if(!is.null(tct)){
    stopifnot("tct must be of lenght 1"=length(tct)==1,
              "tct must be numeric"=is.numeric(tct))
    gediL4 <- gediL4[gediL4$tree_cover>tct,]
  }

  if(is.null(usegeometry)||usegeometry==F){
    if(any(class(clip) == "character")) {
      if (!file.exists(clip))
        stop("clip doesn't exist")
      message("Path detected")
      if (tools::file_ext(clip) == "shp") {
        message("Shp detected")
        x <- terra::vect(clip)
      } else if (tools::file_ext(clip) == "tif") {
        message("Tif detected")
        x <- terra::rast(clip)
      }
      proj4string <- as.character(terra::crs(x, proj=TRUE))
      if(proj4string=="+proj=longlat +datum=WGS84 +no_defs"){
        bbox <- terra::ext(x)
      }else{
        # Transformed data in lat lon
        pj <- terra::project(x,"epsg:4326")
        bbox <- terra::ext(pj)
      }
    } else if (any(class(clip) %in% c("sf", "data.frame"))) {
      message("Sf object detected")
      b <- vect(clip)
      bbox <- terra::ext(b)
      if(proj4string=="+proj=longlat +datum=WGS84 +no_defs"){
        bbox <- terra::ext(b)
      }else{
        # Transformed data in lat lon
        pj <- terra::project(b,"epsg:4326")
        bbox <- terra::ext(pj)
      }
    } else if (class(clip) %in% c("RasterLayer", "RasterStack", "raster")) {
      message("Raster* object detected")
      b <- terra::rast(clip)
      bbox <- terra::ext(b)
      proj4string <- as.character(terra::crs(b, proj=TRUE))
      if(proj4string=="+proj=longlat +datum=WGS84 +no_defs"){
        bbox <- bbox
      }else{
        # Transformed data in lat lon
        pj <- terra::project(b, "epsg:4326")
        bbox <- terra::ext(pj)
      }
    } else if (class(clip) %in% c("bbox", "numeric")) {
      message("Vector detected")
      bbox <- clip
    } else if (class(clip) %in% c("SpatRaster", "SpatVector")) {
      message("Terra object detected")
      b <- clip
      bbox <- terra::ext(b)
      proj4string <- as.character(terra::crs(b, proj=TRUE))
      if(proj4string=="+proj=longlat +datum=WGS84 +no_defs"){
        bbox <- bbox
      }else{
        # Transformed data in lat lon
        pj <- terra::project(b, "epsg:4326")
        bbox <- terra::ext(pj)
      }
    }

    xmin <- bbox[1]
    xmax <- bbox[2]
    ymin <- bbox[3]
    ymax <- bbox[4]

    mask = gediL4$lon_lowestmode >= xmin & gediL4$lon_lowestmode <=
      xmax & gediL4$lat_lowestmode >= ymin & gediL4$lat_lowestmode <=
      ymax & gediL4$lon_lowestmode >= xmin & gediL4$lon_lowestmode <=
      xmax & gediL4$lat_lowestmode >= ymin & gediL4$lat_lowestmode <=
      ymax

    mask[!stats::complete.cases(mask)] = FALSE
    mask  <-  (1:length(gediL4$lat_lowestmode))[mask]
    newFile <- gediL4[mask, ]

  }else {
    if(any(class(clip)=="character")){
      if(tools::file_ext(clip)=="tif"){
        stop("cannot use raster as boundary to clip points when usegeometry=T")
      }else {
        bound <- terra::vect(clip)
        bound <- terra::project(bound, "epsg:4326")
        vect_ob <- terra::vect(gediL4,geom=c("lon_lowestmode","lat_lowestmode"), crs="EPSG:4326", keepgeom=T)
        clipped <- terra::mask(vect_ob,bound)
        newFile <- clipped
      }
    } else if(any(class(clip)%in%c("sf","data.frame"))){
      bound <- terra::vect(clip)
      bound <- terra::project(bound, "epsg:4326")
      vect_ob <- terra::vect(gediL4,geom=c("lon_lowestmode","lat_lowestmode"), crs="EPSG:4326", keepgeom=T)
      clipped <- terra::mask(vect_ob,bound)
      newFile <- clipped

    } else if(any(class(clip)%in%c("SpatVector"))){
      bound <- clip
      bound <- terra::project(bound, "epsg:4326")
      vect_ob <- terra::vect(gediL4,geom=c("lon_lowestmode","lat_lowestmode"), crs="EPSG:4326", keepgeom=T)
      clipped <- terra::mask(vect_ob,bound)
      newFile <- clipped

    } else if(class(clip)%in%c("bbox","numeric")){
      stop("cannot use numeric vector to clip points when usegeometry=T")
    }
  }
  return(newFile)
}




