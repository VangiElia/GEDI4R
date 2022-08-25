#' Get  xvar information from a list of GEDI Level 4 file path
#'
#' Get GEDI L4 xvar information from h5 file format.These information are
#' related to the Level 2A predictors used to obtain the AGBD estimates and can
#' be used to reconstruct L2A metrics used for the estimation and the AGBD
#' estimates itself. See
#' \href{https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=2056}{here} for more
#' documentation on GEDI L4 data. The function use the
#' \link[snowfall:snowfall]{snowfall} package to get data in parallel. There
#' could be memory allocation problems for an high number of file path.

#' @param gediL4_path List or vector of character path to GEDI level4A h5 file.
#' @param ncore Numeric: numbers of core to be used if the maximum core
#'   available is less than the length of \code{gediL4_path} Default to the
#'   number of cores available minus one.
#' @param merge Logical: if TRUE (default) the resulted list will be merged with
#'   \code{rbind}. Ignored if \code{length(gediL4_path)==1}
#' @param source Logical: if TRUE and \code{merge=TRUE} add a column with the
#'   source path of each observation.Ignored if \code{length(gediL4_path)==1},
#'   and if \code{merge=FALSE}
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
#'
#' @export

l4_get_xvar <- function(gediL4_path,ncore=parallel::detectCores()-1,merge=T,source=F){

  #input check
  stopifnot(
    "Path to file is not a character"=all(sapply(gediL4_path, check_char)),
    "merge is not logical"= check_log(merge),
    "source is not logical"= check_log(source),
    "ncore not defined"=check_wholenumber(ncore),
    "ncore must be of lenght 1"=length(ncore)==1
  )

  if(length(gediL4_path)==1){
    message("gediL4_path has lenght==1,l4_xvar will be used in single thread mode")
    gediL4_path <- unlist(gediL4_path)
    return(l4_xvar(gediL4_path))
  }
  snowfall::sfInit(
    parallel = TRUE,
    cpus = ifelse(
      length(gediL4_path) < parallel::detectCores() - 1,
      length(gediL4_path),
      ncore
    )
  )
  snowfall::sfExport("gediL4_path")
  snowfall::sfLibrary(hdf5r)
  snowfall::sfLibrary(GEDI4R)
  suppressWarnings(snowfall::sfLibrary(data.table))
  l4_list <- snowfall::sfLapply(gediL4_path,l4_xvar)
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



