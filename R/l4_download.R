
#' DOWNLOAD GEDI level 4A data from DAACL.ORNL
#'
#' Download all GEDI footprints from
#' \href{https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1907}{DAACL.ORNL}
#' website that intersect a study area, defined as an extent in lon/lat
#' coordinates. The footprints are located within the global latitude band
#' observed by the International Space Station (ISS), nominally 51.6 degrees N
#' and S and reported for the period 2019-04-18 to 2020-09-02
#'
#' @param ul_lat Numeric: upper left latitude.
#' @param lr_lat Numeric: lower right latitude.
#' @param ul_lon Numeric: upper left longitude.
#' @param lr_lon Numeric: lower right longitude.
#' @param ncore Numeric: numbers of core to be used if the maximum core
#'   available is less than the number of files to be download. Default to the
#'   number of cores available minus one.
#' @param from Character: date from which the data search starts. In the form
#'   "yyyy-mm-dd".
#' @param to Character: date on which the data search end. In the form
#'   "yyyy-mm-dd".
#' @param outdir Character: path of the directory in which to save the
#'   downloaded files. If it doesn't exist it will be created. Ignored if
#'   \code{just_path=TRUE}
#' @param just_path Logical: if TRUE return a character vector of available
#'   files without downloading them. Default to FALSE.
#' @param subset Numeric vector of indices for downloading a subset of files
#'   instead of all. If is not numeric it will be ignored silently.
#' @details During the first use, users will need to enter their Earth Explore
#'   login Information for downloading the data. These information will be saved
#'   in \code{outdir} as a netrc file. This function uses the
#'   \code{\link[foreach]{foreach}} package for downloading files in parallel,
#'   with the \code{\link[doParallel]{doParallel}} configuration. If a file with
#'   the same name is already presented in outdir it will be overwrite.
#' @return List of file path in \code{outdir}
#' @examples
#'
#'#retrive Italy bound
#'bound <- sf::st_as_sf(raster::getData('GADM', country='ITA', level=1))
#'ex <- raster::extent(bound)
#'ul_lat <- ex[4]
#'lr_lat <- ex[3]
#'ul_lon <- ex[2]
#'lr_lon <- ex[1]
#'from <- "2019-05-01"
#'to <- "2019-05-02"
#'#get just files path available for the searched parameters
#'l4_download(ul_lat=ul_lat,
#'            lr_lat=lr_lat,
#'            ul_lon=ul_lon,
#'            lr_lon=lr_lon,
#'            from=from,
#'            to=to,
#'            just_path=T
#')
#'
#'#download files
#'
#'l4_download(ul_lat=ul_lat,
#'            lr_lat=lr_lat,
#'            ul_lon=ul_lon,
#'            lr_lon=lr_lon,
#'            from=from,
#'            to=to,
#'            just_path=F,
#'            outdir = tempdir())
#' @export

l4_download <-
  function(ul_lat,
           lr_lat,
           ul_lon,
           lr_lon,
           ncore = parallel::detectCores() - 1,
           from = NULL,
           to = NULL,
           outdir=getwd(),
           just_path = F,
           subset = NULL) {

    #define function for building netrc file with access credentials
    getnetrc <- function (dl_dir) {
      netrc <- file.path(dl_dir, "netrc")
      if (file.exists(netrc) == FALSE ||
          any(grepl("urs.earthdata.nasa.gov",
                    readLines(netrc))) == FALSE) {
        netrc_conn <- file(netrc)
        writeLines(c(
          "machine urs.earthdata.nasa.gov",
          sprintf(
            "login %s",
            getPass::getPass(msg = "Enter NASA Earthdata Login Username \n (or create an account at urs.earthdata.nasa.gov) :")
          ),
          sprintf(
            "password %s",
            getPass::getPass(msg = "Enter NASA Earthdata Login Password:")
          )
        ),
        netrc_conn)
        close(netrc_conn)
        message(
          "A netrc file with your Earthdata Login credentials was stored in the output directory "
        )
      }
      return(netrc)
    }

    #input check
    stopifnot(
      "coordinates are not numeric" = all(sapply(
        list(ul_lat, lr_lat, ul_lon, lr_lon), check_num
      )),
      "from or to are not character" = all(sapply(list(from, to), check_char)),
      "just_path is not logical" = check_log(just_path)
    )
    if (!just_path) {
      stopifnot("ncore not defined" = check_wholenumber(ncore))
    }

    if (missing(outdir) || is.null(outdir)) {
      if (just_path) {
        outdir <- NULL
      } else{
        stop("outdir is missing or is NULL")
      }
    }

    #check if outdir exist and if there is a netrc file in
    if (!just_path) {
      stopifnot("outdir is not character" = check_char(outdir))
      if (!dir.exists(outdir)) {
        dir.create(outdir)
        message(outdir, " does not exist. It will be created")
        netrc_file <- getnetrc(outdir)
      } else if (length(list.files(outdir, pattern = "netrc")) == 0) {
        netrc_file <- getnetrc(outdir)
      } else{
        netrc_file <- list.files(outdir, pattern = "netrc", full.names = T)
      }
    }

    #time period
    daterange <- c(from, to)

    # Get path to GEDI2B data
    gLevel2B <-
      rGEDI::gedifinder(
        product = "GEDI02_B",
        ul_lat,
        ul_lon,
        lr_lat,
        lr_lon,
        version = "001",
        daterange = daterange
      )

    #Built path fo GEDI4A by changing the source
    gLevel4 <-
      paste0(
        "https://daac.ornl.gov/daacdata/gedi/GEDI_L4A_AGB_Density/data/GEDI04_A",
        substr(basename(gLevel2B), 9, nchar(basename(gLevel2B)))
      )
    message(length(gLevel4), " files found.")

    if (just_path) {
      return(gLevel4)
      stop(invisible())
    }

    if (!is.null(subset) && is.numeric(subset)) {
      gLevel4 <- gLevel4[subset]
    }
    #set ncore equal to the number of files found or to the user defined value
    ncore <- ifelse(length(gLevel4) <= parallel::detectCores()-1, length(gLevel4), ncore)
    message("using ", ncore, " cores")
    #download
    cl <- parallel::makeCluster(ncore)
    doParallel::registerDoParallel(cl)
    message("start download")

    foreach::foreach(
      i = 1:length(gLevel4),
      .packages = "httr"
    ) %dopar% {
      response <-
        httr::GET(
          gLevel4[i],
          write_disk(file.path(outdir, basename(gLevel4)[i]), overwrite = T),
          config(netrc = TRUE, netrc_file = netrc_file),
          set_cookies("LC" = "cookies")
        )
    }
    parallel::stopCluster(cl)
    foreach::registerDoSEQ()
    message("Done")
    files <- list.files(outdir, pattern = "h5", full.names = T)
    return(files)
  }









