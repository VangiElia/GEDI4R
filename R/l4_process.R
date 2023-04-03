#' Process GEDI level 4 data.
#'
#' Perform the entire processing chain of extracting, clipping and exporting
#' GEDI data. See
#' \href{https://daac.ornl.gov/GEDI/guides/GEDI_L4A_AGB_Density_V2.html}{here} for
#' more documentation on GEDI L4 data. This
#' is the main function of the package. It perform all operations in chunks
#' of files in parallel. It runs in order \code{l4_getmulti, l4_clip,
#' l4_convert} to each chunk.
#'
#' @inheritParams l4_getmulti
#' @inheritParams l4_clip
#' @param nfile Numeric : number of file processed at time. It affect the
#'   numbers of total chunks in which files will be split. Default to NULL.
#' @param epsg Numeric: destination EPSG code. Default to NULL.
#' @param prefix Character: prefix name for exported chunks. Default to "chunk".
#' @param outdir Character: directory in which to save output files. If it doesn't
#'   exist it will be created.
#' @param ext Character: extension of  vector files to be saved.
#' @param parallel Logical: if TRUE (default) the chain process are executed in
#'   parallel. In this case the function try to guess the best number of cores
#'   to use based based on the maximum available and the number of files to be
#'   processed.
#' @details The function use by default \code{usegeometry=TRUE} in \code{l4_clip}
#'   This is usually the desired behavior for the function. If
#'   \code{parallel=TRUE}, the function will try to guess the best number of
#'   cores to be used in \code{l4_getmulti} and in the chunks loop, based on the
#'   maximum cores available and the length of \code{gediL4_path}. The user can
#'   override the number of core used in \code{l4_getmulti} with \code{...}, by
#'   specifying the argument \code{ncore}. This will also affect the number of
#'   cores used to loop over chunks. Usually, the number of cores used by
#'   default is the best option, modifying it can slow down the function.  See
#'   \code{?l4_getmulti} for columns specification of the
#'   output. For downloading more detailed information on GEDI Level 4A data
#'   (version 2) see
#'   \href{https://daac.ornl.gov/daacdata/gedi/GEDI_L4A_AGB_Density_V2/comp/GEDI_L4A_V2_Product_Data_Dictionary.pdf}{product data dictionary}.
#'   Note that by default, the function will drop all footprints in which
#'   agbd<0. These observations are considered sensor errors.
#' @return A vector of file paths inside \code{outdir}. Outputs will be written
#'   as \code{outdir}/\code{prefix}_x_y.ext, where x and y are the first and
#'   last file number in that chunk and ext is the extension specified by the
#'   parameter \code{ext}.
#' @seealso \code{\link[=l4_getmulti]{l4_getmulti}},
#'   \code{\link[=l4_clip]{l4_clip}},  \code{\link[=l4_convert]{l4_convert}}
#' @examples
#'
#'outdir = tempdir()
#'l4_zip <- system.file("extdata",
#'                      c("GEDI04_A_2020036151358_O06515_02_T00198_02_002_01_V002.zip"
#'                      ),
#'                      package="GEDI4R")
#'#Unzipping GEDI level4A data
#'l4 <- unzip(l4_zip,exdir = outdir)
#'#create 4 copy of GEDI file to test the function
#'file.copy(from=l4,to=paste0(tools::file_path_sans_ext(l4),"_copy",1:4,".h5"))
#'#path to Shapefile for clipping the data
#'bound <- system.file("extdata","bound4326.shp",package="GEDI4R")
#'#path to GEDI files
#'l4_path <- list.files(outdir,pattern = "h5",full.names = T)
#'#proces all files in chunk each of 2 files, in sequence
#'l4_data <- l4_process(l4_path,nfile=2,clip=bound,epsg=4326,outdir=outdir,ext="shp",parallel=F,prefix = "ex")
#'file.remove(list.files(outdir,full.names = T, pattern = "ex"))
#'#in parallel
#'l4_data <- l4_process(l4_path,nfile=2,clip=bound,epsg=4326,outdir=outdir,ext="shp",parallel=T)
#'file.remove(list.files(outdir,full.names = T, pattern = "ex"))

#' @export



l4_process <- function(gediL4_path,nfile=NULL,clip=NULL,epsg=NULL,prefix="chunk",outdir=NULL,ext=NULL,parallel=T,ncore=NULL,add_col=NULL,tct=NULL){

  l <- length(gediL4_path)
  av_core <- parallel::detectCores()-1
  #args check
  if(is.null(nfile)){
    nfile <- round(l/av_core)
  }

  stopifnot("gediL4_path is missing"=!missing(gediL4_path),
            "nfile is not integer"=check_wholenumber(nfile),
            "clip is NULL"= !is.null(clip),
            "EPSG code is NULL or is not numeric" = check_num(epsg),
            "prefix is NULL" =!is.null(prefix),
            "ext is NULL" =!is.null(ext),
            "outdir is NULL"=!is.null(outdir),
            "parallel is not logical"=check_log(parallel))

  if(!dir.exists(outdir)){
    message(outdir, " does not exist. It will be created")
    dir.create(outdir)
  }

  if(!all((sapply(gediL4_path, typeof) == "character"))) {
    stop("input elements are of different type. Expected to be all character")
  }

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
    tmp <- l4_getmulti(gediL4_path[index],just_colnames=F,add_col=add_col,tct=tct,ncore = ncore,merge=T,source=T)
    if(nrow(tmp)==0){
      message("in this chunk all files have 0 rows")
      return(invisible())
    }
    message("read files ",paste0(index[1],"-",index[length(index)]))
    clipped <- l4_clip(clipped,clip=clip,usegeometry = T)
    message("clipped files ",paste0(index[1],"-",index[length(index)]))
    name <- file.path(outdir,paste0(prefix,"_",index[1],"_",index[length(index)],".",ext))
    if(nrow(clipped)==0){
      message("in this chunk all files have 0 rows")
      return(invisible())
      }
    converted <- l4_convert(clipped,epsg = epsg,filename = name)
    message("saved files ",paste0(index[1],"-",index[length(index)])," as: ",name)
    return(converted)
  }

  #apply function to files
  o <- Sys.time()
  if(parallel){
  #choose number of cores to be used if parallel=T, based on the remained available

    cpus <- round(av_core/ncore)
    if(cpus>length(index))cpus <- length(index)

    message("using ",cpus, " cores in the outer loop")
    snowfall::sfInit(parallel = T,cpus =cpus)
    snowfall::sfLibrary(GEDI4R)
    snowfall::sfExportAll()
    output <- snowfall::sfLapply(index,process)
    snowfall::sfStop()
  }else{
    output <- lapply(index,process)
  }
  message("done in : ",round(Sys.time() - o,2))

  return(list.files(outdir,pattern = ext,full.names = T))
}


