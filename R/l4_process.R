#' Process GEDI level 4 data.
#'
#' Perform the entire processing chain of extracting, clipping and exporting
#' GEDI data. See
#' \href{https://daac.ornl.gov/GEDI/guides/GEDI_L4A_AGB_Density.html}{here} for
#' more documentation on GEDI L4 data. This
#' is the main function of the package. It perform all operations in chunks
#' of files in parallel. It runs in order \code{l4_getmulti, l4_clip,
#' l4_convert} to each chunk.
#'
#' @inheritParams l4_getmulti
#' @inheritParams l4_clip
#' @param nfile Numeric : number of file processed at time. It a affect the
#'   numbers of total chunks in which files will be splitted. Default to NULL.
#' @param epsg Numeric: destination EPSG code. Default to NULL.
#' @param prefix Character: prefix name for exported chunks. Default "".
#' @param outdir Character: directory in which to save ouput files. If don't
#'   exist it will be created.
#' @param parallel Logical: if TRUE (default) the chain process are executetd in
#'   parallel. In this case the function try to guess the best number of cores
#'   to use based on the maximum available and the number of files to be
#'   processed.
#' @param ... Other arguments to be passed to \code{l4_getmulti}.
#' @details The function use by default \code{usegeometry=TRUE} in \code{l4_clip}
#'   and \code{append=FALSE} in \code{l4_convert}. Although they can be changed,
#'   this is usually the desired behavior for the function. If
#'   \code{parallel=TRUE}, the function will try to guess the best number of
#'   cores to be used in \code{l4_getmulti} and in the chunks loop, based on the
#'   maximum cores available and the length of \code{gediL4_path}. The user can
#'   override the number of core used in \code{l4_getmulti} with \code{...}, by
#'   specifying the argument \code{ncore}. This will also affect the number of
#'   cores used to loop over chunks. Usually, the number of cores used by
#'   default is the best option, modifying it can slow down the function.  See
#'   \code{?l4_get} or \code{?l4_getmulti} for columns specification of the
#'   output. For downloading more detailed information on GEDI Level 4A data
#'   (version 1) see
#'   \href{https://access.earthdata.nasa.gov/datasets/C2114031882-ORNL_CLOUD?action=index&controller=datasets&dataset_page_num=208&dataset_page_size=10&keyword=%2A}{here}.
#'    Note that by default, the function will drop all footprints in which agbd<0.
#'   These observations are considered sensor errors.
#' @return A vector of file paths inside \code{outdir}. Outputs will be written
#'   as \code{outdir}/\code{prefix}_x_y.shp, where x and y are the first and last
#'   file number in that chunk.
#' @seealso \code{\link[=l4_getmulti]{l4_getmulti}},
#'   \code{\link[=l4_clip]{l4_clip}},  \code{\link[=l4_convert]{l4_convert}}
#' @examples
#'
#' #Specifying the path to GEDI level4A data (zip file)
#' outdir  <-  tempdir()
#' l4_zip <- system.file("extdata",
#'                       c("GEDI04_A_2020186052327_O08834_T03611_02_001_01.zip",
#'                         "GEDI04_A_2020186065619_O08835_T00766_02_001_01.zip",
#'                         "GEDI04_A_2020187043633_O08849_T04437_02_001_01.zip",
#'                         "GEDI04_A_2020187060925_O08850_T01592_02_001_01.zip"
#'                       ),
#'                       package="GEDI4R")
#' #Unzipping GEDI level4A data
#' l4 <- lapply(l4_zip,unzip,exdir = outdir)
#' bound <- system.file("extdata","Italy.shp",package="GEDI4R")
#' #proces all files in chunk each of 2 files, in sequence
#' l4_data <- l4_process(l4,nfile=2,clip=bound,usegeometry=T,epsg=32632,prefix="block",outdir,parallel=F)
#' file.remove(l4_data)
#' #in parallel
#' l4_data <- l4_process(l4,nfile=2,clip=bound,usegeometry=T,epsg=32632,prefix="block",outdir,parallel=T)
#' file.remove(l4_data)
#' #override the default number of cores to be used
#' l4_data <- l4_process(l4,nfile=2,clip=bound,usegeometry=T,epsg=32632,prefix="block",outdir,parallel=T,ncore=2)
#' file.remove(l4_data)
#' @export



l4_process <- function(gediL4_path,nfile=NULL,clip=NULL,usegeometry=T,epsg=NULL,prefix="",outdir=NULL,parallel=T,ncore=NULL,...){


  l <- length(gediL4_path)
  av_core <- parallel::detectCores()-1
  #args check
  if(is.null(nfile)){
    nfile <- round(l/av_core)
  }

  stopifnot("gediL4_path is missing"=!missing(gediL4_path),
            "nfile is NULL"=!is.null(nfile),
            "nfile is not integer"=check_wholenumber(nfile),
            "clip is NULL"= !is.null(clip),
            "usegeometry must be logical or NULL"= any(is.logical(usegeometry),is.null(usegeometry)),
            "EPSG code is NULL or is not numeric" = check_num(epsg),
            "prefix is NULL" =!is.null(prefix),
            "outdir is NULL"=!is.null(outdir),
            "parallel is not logical"=check_log(parallel))

  if(!dir.exists(outdir)){
    dir.create(outdir)
    message(outdir, " does not exist. It will be created")
  }

  if(!all((sapply(gediL4_path, typeof) == "character"))) {
    stop("input elements are of different type. Expected to be all character")
  }

  l <- length(gediL4_path)
  index <- split(1:l, ceiling(seq_along(1:l) / nfile))

  message("input list will be processed in ",
          length(index),
          " blocks. Block length:\n",
          paste0("[", 1:length(index),"]",lapply(index,length),collapse = "\n")
  )

  #functions to choose number of cores


  #choose number of cores to be used in l4_getmulti
  if(is.null(ncore)){
    ncore <- nfile
  }

  message("using ",ncore, " cores for l4_getmulti")
  #function to perform all the processing chain
  process <- function(index){
    tmp <- l4_getmulti(gediL4_path[index],ncore = ncore,...)
    message("read files ",paste0(index[1],"-",index[length(index)]))
    clipped <- suppressWarnings(suppressMessages(l4_clip(tmp,clip=clip,usegeometry = usegeometry)))
    message("clipped files ",paste0(index[1],"-",index[length(index)]))
    name <- file.path(outdir,paste0(prefix,"_",index[1],"_",index[length(index)],".shp"))
    converted <- l4_convert(clipped,epsg = epsg,filename = name,append=F)
    message("saved files ",paste0(index[1],"-",index[length(index)])," as: ",name)
    return(converted)
  }



  #apply function to files
  o <- Sys.time()
  if(parallel){
  #choose number of cores to be used if parallel=T, based on the remained available
    cpus <- round(av_core/ncore)

    message("using ",cpus, " cores in the outer loop")
    snowfall::sfInit(parallel = T,cpus =cpus)
    snowfall::sfLibrary(GEDI4R)
    snowfall::sfExport("index","process","...")
    output <- snowfall::sfLapply(index,process)
    snowfall::sfStop()
  }else{
    output <- lapply(index,process)
  }
  message("done in : ",round(Sys.time() - o,2))

  return(list.files(outdir,pattern = "shp",full.names = T))
}


