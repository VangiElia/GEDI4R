
#' Get  AGBD data from a single path of GEDI Level 4 file
#'
#' Get GEDI L4 data from h5 file format. See
#' \href{https://daac.ornl.gov/GEDI/guides/GEDI_L4A_AGB_Density.html}{here} for
#' more documentation on GEDI L4 data. This
#' function work as \code{l4_getmulti} but for a single file

#' @param gediL4_path Character path to GEDI level 4A h5 file.
#' @param just_colnames Logical: if TRUE, the function return the dataset
#'   names in the h5 file. Useful to get names for the argument \code{add_col}.
#' @param add_col Character vector of extra datasets names to \code{cbind} to
#'   the default output. Names already present will be dropped.
#' @param tct Numeric: tree cover threshold to filter GEDI data.
#' @details Columns specification: Flag indicating likely invalid waveform
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
#'   For downloading more detailed information on GEDI level 4A data (version 1)
#'   see
#'   \href{https://access.earthdata.nasa.gov/datasets/C2114031882-ORNL_CLOUD?action=index&controller=datasets&dataset_page_num=208&dataset_page_size=10&keyword=%2A}{here}
#'   Note that by default the function will dropp all footprint in which agbd<0.
#'   These observations are considered sensor errors.
#'
#' @return an S4 object of class
#'  \link[=data.table:data.table]{data.table}.
#' @seealso \code{\link[=l4_getmulti]{l4_getmulti}},
#'   \code{\link[=l4_process]{l4_process}}
#' @examples
#'
#' #Specifying the path to GEDI level4A data (zip file)
#' outdir = tempdir()
#' l4_zip <- system.file("extdata",
#'                   "GEDI04_A_2020186052327_O08834_T03611_02_001_01.zip",
#'                   package="GEDI4R")
#'
#' #Unzipping GEDI level 4A data
#' l4 <- unzip(l4_zip,exdir = outdir)
#' #dataset names
#' col_name <- l4_get(l4,just_colnames=T)
#' #get default data (see Details section)
#' l4_data <- l4_get(l4)
#' #filter footprint based on tree cover
#' l4_data_filter <- l4_get(l4,tct=10)
#' #add user-define dataset
#' l4_user_data <- l4_get(l4,add_col = col_name[2:3])
#' @export


l4_get <- function(gediL4_path,just_colnames=F,add_col=NULL,tct=NULL,agbd_rm=NULL){

  #input check
  stopifnot(
    "Path to file is not a character"=check_char(gediL4_path),
    "just_colnames is not logical"= check_log(just_colnames)
  )

  #open h5 file
  level4a_h5 <- hdf5r::H5File$new(gediL4_path, mode = "r")#mode="r" to open in reading mode
  #list beams id
  groups_id <- grep("BEAM\\d{4}$", gsub("/", "", hdf5r::list.groups(level4a_h5,
                                                                    recursive = F)), value = T)
  if(just_colnames){
    x <- hdf5r::list.datasets(level4a_h5[[groups_id[1]]])
    level4a_h5$close_all()
    return(x)
   stop(invisible())
  }

  raw_date <-substr(basename(gediL4_path),10,22)
  year <- as.numeric(substr(raw_date,1,4))
  doy <- as.numeric(substr(raw_date,5,7))
  time <- substr(raw_date,8,13)
  time <- substr(gsub("(.{2})", "\\1:", time),1,8)
  date <- paste(as.Date(doy, origin = paste(year-1,"12-31",sep="-")),time)

  rh.dt <- data.table::data.table()
  #initialize progress bar
  pb <- utils::txtProgressBar(min = 0, max = length(groups_id),
                              style = 3)
  i.s = 0
  for (i in groups_id) {
    i.s <- i.s + 1
    utils::setTxtProgressBar(pb, i.s)
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

      if(!is.null(add_col)){
        add_col <-
          add_col[!add_col %in% c(
            "beam",
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
            "xvar"
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

      if(!is.null(tct)){
        stopifnot("tct must be of lenght 1"=length(tct)==1,
                  "tct must be numeric"=is.numeric(tct))
          rh.dt <- rh.dt[rh.dt$tree_cover>=tct,]
        }
      }
  }
  if(!is.null(agbd_rm)){
    stopifnot("agbd_rm must be of lenght 1"=length(agbd_rm)==1,
              "tct must be numeric"=is.numeric(agbd_rm))
    rh.dt <- rh.dt[rh.dt$agbd>=agbd_rm,]
  }
  close(pb)
  level4a_h5$close_all()
  return(rh.dt)
}

