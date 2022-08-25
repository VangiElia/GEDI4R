#' Get  xvar information from a single path of GEDI Level 4 file
#'
#' Get GEDI L4 xvar information from h5 file format.These information are
#' related to the Level 2A predictors used to obtain the AGBD estimates and can
#' be used to reconstruct L2A metrics used for the estimation and the AGBD
#' estimates itself. See
#' \href{https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=2056}{here} for more
#' documentation on GEDI L4 data.

#' @param gediL4_path Character path to GEDI level 4A h5 file.
#' @details Part of columns in the output are also present in the output of
#'   \code{l4_getmulti}. The other columns are described in the chapter
#'   \href{https://daac.ornl.gov/GEDI/guides/GEDI_L4A_AGB_Density_V2_1.html#datacharact}{"data
#'   characteristics" on this page}. Relevant columns are:
#'   \itemize{
#'   \item \emph{rh_index.x}: Vector of the height percentile
#'   associated with the given RH metric in GEDI L2A.
#'   \item \emph{predictor_is.x}: The variable predictor_id provides a mapping
#'   between rh_index and par
#'   \item \emph{par.x}: The vector par contains coefficients of the linear
#'   model used to predict AGBD, where the first element is the intercept and
#'   subsequent elements are slope coefficients
#'   \item \emph{V.x }: Values of scaled and transformed GEDI l2A RH metrics
#'   entered in the selected model to estimate AGBD
#'   }
#'   For more information on these data see the Frequently asked question
#'   \href{https://daac.ornl.gov/GEDI/guides/GEDI_L4A_AGB_Density_V2_1.html#datacharact}{here}.
#'   Note that by default the function will drop all footprint in which agbd<0.
#'   These observations are considered sensor errors.
#'
#' @return an S4 object of class
#'  \link[=data.table:data.table]{data.table}.



l4_xvar <- function(gediL4_path){

  #input check
  stopifnot(
    "Path to file is not a character"=check_char(gediL4_path)
  )
  gedil4 <- l4_get(gediL4_path,add_col = c("predict_stratum","selected_algorithm"))
  if(nrow(gedil4)==0){
    warning("there are no footprints in this file with agbd >0")
    return(NULL)
  }

  #open h5 file
  level4a_h5 <- hdf5r::H5File$new(gediL4_path, mode = "r")#mode="r" to open in reading mode
  #list beams id
  groups_id <- grep("BEAM\\d{4}$", gsub("/", "", hdf5r::list.groups(level4a_h5,
                                                                    recursive = F)), value = T)
  ancillary <- level4a_h5[["ANCILLARY"]][["model_data"]]
  col <- colnames(ancillary$read())
  l <- list()
  for (i in seq_along(col)) {
    l[[i]] <- do.call(rbind,lapply(1:35,function(x)ancillary[x][[col[i]]]))
  }
  names(l) <- col
  dfl <- as.data.frame(l)
  gedil4 <- dplyr::left_join(gedil4,dfl,by="predict_stratum")

  xvar <- as.list(1:length(groups_id))
  for (i in seq_along(groups_id)) {

    level4a_i <- level4a_h5[[groups_id[i]]]

    if (any(hdf5r::list.datasets(level4a_i) == "shot_number")) {
      shot <- data.table::data.table(shot_number=level4a_i[["shot_number"]]$read())
      xvar[[i]] <- cbind(shot,data.table::data.table(t(level4a_i[["xvar"]]$read())))
    }
  }
  xvar <- data.table::as.data.table(do.call(rbind,xvar))
  df <- dplyr::left_join(gedil4,xvar,by="shot_number")
  level4a_h5$close_all()
  return(df)
}
