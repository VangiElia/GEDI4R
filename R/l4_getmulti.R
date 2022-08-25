#' Get  AGBD data from a list of GEDI Level 4 file path
#'
#' Get GEDI L4 data from h5 file format. See
#' \href{https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=2056}{here} for
#' more documentation on GEDI L4 data. The function use the
#' \link[snowfall:snowfall]{snowfall} package to get data in parallel. There
#' could be memory allocation problems for an high number of file path. This is
#' the core function of \code{\link[=l4_process]{l4_process}}, that manage the
#' whole workflow of reading, clipping and converting a list of h5 GEDI
#' file path.

#' @inheritParams l4_get
#' @param gediL4_path List or vector of character path to GEDI level4A h5 file.
#' @param ncore Numeric: numbers of core to be used if the maximum core
#'   available is less than the length of \code{gediL4_path} Default to the
#'   number of cores available minus one.
#' @param merge Logical: if TRUE (default) the resulted list will be merged with
#'   \code{rbind}. Ignored if \code{length(gediL4_path)==1}
#' @param catch Logical: if TRUE (default) files that would throw an error don't
#'   interrupt the processing of subsequent files. For this file the function
#'   will return NULL.Ignored if \code{length(gediL4_path)==1}
#' @param source Logical: if TRUE and \code{merge=TRUE} add a column with the
#'   source path of each observation.Ignored if \code{length(gediL4_path)==1},
#'   and if \code{merge=FALSE}
#' @details Columns specification: Flag indicate likely invalid waveform
#'   (1=valid,0=invalid).
#'   \itemize{
#'   \item \emph{date}: Date of acquisition in yyyy-mm-dd hh:mm:ss
#'   \item \emph{beam}: Beam identifier
#'   \item \emph{shot_number}: Shot number
#'   \item \emph{degrade_flag }: Flag indicating
#'   degraded state of pointing and/or positioning information
#'   \item \emph{l4_quality_flag}: Flag identifying the most useful L4 data for
#'   biomass predictions
#'   \item \emph{l2_quality_flag}: Flag identifying the most
#'   useful L2 data for biomass predictions
#'   \item \emph{algorithm_run_flag}: The
#'   L4A algorithm is run if this flag is set to 1. This flag selects data that
#'   have sufficient waveform fidelity for AGBD estimation.
#'   \item \emph{sensitivity}: Maxmimum canopy cover that can be penetrated
#'   considering the SNR of the waveform
#'   \item \emph{lat_lowestmode}: Latitude of center of lowest mode
#'   \item \emph{lon_lowestmode}: Longitude of center of lowest mode
#'   \item \emph{elev_lowestmode}: Elevation of center of lowest mode relative
#'   to reference ellipsoid
#'   \item \emph{tree_cover}: Tree cover in the year 2010, defined as canopy
#'   closure for all vegetation taller than 5 m in height (Hansen et al., 2013)
#'   and encoded as a percentage per output grid cell.
#'   \item \emph{pft_class}: Plant Functional Type (PFT) derived from the MODIS
#'   MCD12Q1 V006 product. Values follow the Land Cover Type 5 Classification
#'   scheme.
#'   \item \emph{agbd_se}: Aboveground biomass density (Mg/ha) prediction
#'   standard error
#'   \item \emph{agbd}: Aboveground biomass density (Mg/ha)
#'   }
#'   For DOWNLOADING more detailed information on GEDI Level 4A data (version 2.1)
#'   see the
#'   \href{https://daac.ornl.gov/daacdata/gedi/GEDI_L4A_AGB_Density_V2_1/comp/GEDI_L4A_AGB_Density_V2_1.pdf}{user guide} and
#'   \href{https://daac.ornl.gov/daacdata/gedi/GEDI_L4A_AGB_Density_V2/comp/GEDI_L4A_V2_Product_Data_Dictionary.pdf}{product data dictionary}
#'   Note that by default the function will dropp all footprint in which agbd<0.
#'   These observations are considered sensor errors.
#' @return A list of S4 object of class
#'   \link[=data.table:data.table]{data.table} or a single object of
#'   class
#'   \link[=data.table:data.table]{data.table} if \code{merge=TRUE}.
#' @seealso \code{\link[=l4_process]{l4_process}}
#' @examples
#'
#' Specifying the path to GEDI level4A data (zip file)
#' outdir = tempdir()
#' l4_zip <- system.file("extdata",
#'                       c("GEDI04_A_2020036151358_O06515_02_T00198_02_002_01_V002.zip",
#'                         "GEDI04_A_2021150031254_O13948_03_T06447_02_002_01_V002.zip"
#'                       ),
#'                       package="GEDI4R")
#' #Unzipping GEDI level4A data
#' l4 <- lapply(l4_zip,unzip,exdir = outdir)
#' #list all dataset in h5 file
#' l4_getmulti(l4[[1]],just_colnames=T)
#' #read and merge all file
#' l4_data <- l4_getmulti(l4,merge = T)
#' #return individual files as list
#' l4_data <- l4_getmulti(l4,merge = F)
#' #add user-defined dataset
#' col_name <- "delta_time"
#' l4_data <- l4_getmulti(l4,add_col = col_name,merge = T)
#' file.remove(list.files(outdir,full.names = T))
#' @export

l4_getmulti <- function(gediL4_path,just_colnames=F,add_col=NULL,tct=NULL,ncore=parallel::detectCores()-1,merge=T,catch=T,source=F){

  #input check
  stopifnot(
    "Path to file is not a character"=all(sapply(gediL4_path, check_char)),
    "just_colnames is not logical"= check_log(just_colnames),
    "merge is not logical"= check_log(merge),
    "catch is not logical"= check_log(catch),
    "source is not logical"= check_log(source),
    "ncore not defined"=check_wholenumber(ncore),
    "ncore must be of lenght 1"=length(ncore)==1
  )

  if(length(gediL4_path)==1){
    message("gediL4_path has lenght==1,l4_get will be used in single thread mode")
    gediL4_path <- unlist(gediL4_path)
    return(l4_get(gediL4_path,just_colnames = just_colnames,add_col=add_col,tct=tct))
    }

  #return dataset names without reading it
  if(just_colnames){
    colname <- l4_get(gediL4_path,just_colnames = T)
    return(colname)
    stop(invisible())
  }

  readL4 <- function(path,add_col.=add_col,tct.=tct){
    level4a_h5 <- hdf5r::H5File$new(path, mode = "r")#mode="r" indica aprire in sola lettura
    groups_id <- grep("BEAM\\d{4}$", gsub("/", "", hdf5r::list.groups(level4a_h5,recursive = F)), value = T)
    rh.dt <- data.table::data.table()

    raw_date <-substr(basename(path),10,22)
    year <- as.numeric(substr(raw_date,1,4))
    doy <- as.numeric(substr(raw_date,5,7))
    time <- substr(raw_date,8,13)
    time <- substr(gsub("(.{2})", "\\1:", time),1,8)
    date <- paste(as.Date(doy, origin = paste(year-1,"12-31",sep="-")),time)


    for (i in groups_id) {
      level4a_i <- level4a_h5[[i]]

      if (any(hdf5r::list.datasets(level4a_i) == "shot_number")) {

        rhs <- data.table::data.table(date=date,
                                      beam = rep(i, length(level4a_i[["shot_number"]][])),
                                      shot_number = level4a_i[["shot_number"]][],
                                      degrade_flag = level4a_i[["degrade_flag"]][],
                                      l4_quality_flag=level4a_i[["l4_quality_flag"]][],
                                      l2_quality_flag=level4a_i[["l2_quality_flag"]][],
                                      algorithm_run_flag = level4a_i[["algorithm_run_flag"]][],
                                      sensitivity = level4a_i[["sensitivity"]][],
                                      lat_lowestmode = level4a_i[["lat_lowestmode"]][],
                                      lon_lowestmode = level4a_i[["lon_lowestmode"]][],
                                      elev_lowestmode = level4a_i[["elev_lowestmode"]][],
                                      tree_cover=level4a_i[["land_cover_data/landsat_treecover"]][],
                                      pft_class=level4a_i[["land_cover_data/pft_class"]][],
                                      agbd_se=level4a_i[["agbd_se"]][],
                                      agbd = level4a_i[["agbd"]][])
        ncols <- ncol(rhs)

        if(!is.null(add_col.)){
          add_col <-
            add_col[!add_col %in% c("beam",
                                    "shot_number",
                                    "degrade_flag",
                                    "l4_quality_flag",
                                    "l2_quality_flag",
                                    "algorithm_run_flag",
                                    "sensitivity",
                                    "lat_lowestmode",
                                    "lon_lowestmode",
                                    "elev_lowestmode",
                                    "land_cover_data/landsat_treecover",
                                    "land_cover_data/pft_class",
                                    "agbd_se",
                                    "agbd",
                                    "xvar
                                   
            )]

          if(length(add_col)==0){
            message("all columns selected are already present in the defaul output")
          }else {
            df_add_col <- data.table::data.table()
            for (j in add_col) {
              rhs <- cbind(rhs,level4a_i[[j]][])
            }
            colnames(rhs)[(ncols+1):ncol(rhs)] <- add_col
          }
        }
        rh.dt <- rbind(rh.dt, rhs)
        rh.dt <- rh.dt[rh.dt$agbd>=0,]

        if(!is.null(tct.)){
          stopifnot("tct must be of lenght 1"=length(tct)==1,
                    "tct must be numeric"=is.numeric(tct))
          rh.dt <- rh.dt[rh.dt$tree_cover>=tct,]
        }

      }
    }
    rh.dt <- rh.dt[!is.na(rh.dt$lat_lowestmode),]
    rh.dt <- rh.dt[!is.na(rh.dt$lon_lowestmode),]
    if(nrow(rh.dt)==0){
      warning("there are no footprints in this file with agbd >0")
      return(NULL)
    }
    level4a_h5$close_all()
    return(rh.dt)
  }

  if(catch){
    test_function <- function(gediL4_path,add_col.=add_col,tct.=tct){
      return(tryCatch(readL4(gediL4_path,add_col.=add_col,tct.=tct),error=function(e)NULL))
    }
  }else{
    test_function <- readL4
  }

  snowfall::sfInit(
    parallel = TRUE,
    cpus = ifelse(
      length(gediL4_path) < parallel::detectCores() - 1,
      length(gediL4_path),
      ncore
    )
  )
  snowfall::sfExport("gediL4_path","add_col","tct","test_function")
  snowfall::sfLibrary(hdf5r)
  suppressWarnings(snowfall::sfLibrary(data.table))
  l4_list <- snowfall::sfLapply(gediL4_path,test_function,add_col=add_col,tct=tct)
  snowfall::sfStop()
  n <- unlist(lapply(l4_list, function(x){ifelse(is.null(x),0,nrow(x))}))
  failed <- sapply(l4_list,is.null)
  if (all(failed)){warning("failed loading all files");return(invisible())}
  if(any(failed)){
  message("failed loading files n#:\n",paste0(paste(which(failed),": ",gediL4_path[failed]),collapse="\n"))
  }

  if (merge) {
    l4_list <- do.call(rbind, l4_list)
    if (source) {
      l4_list$source <- unlist(rep(gediL4_path, times = n))
    }

  }
  message("Finish!")
  return(l4_list)
}



