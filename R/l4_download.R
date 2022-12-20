
#' DOWNLOAD GEDI level 4A data from DAACL.ORNL
#'
#' Download all GEDI footprints from
#' \href{https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=2056}{the official repository} that intersect a study area, defined as an extent in lon/lat
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
#'   downloaded files.Default to the working directory. If it doesn't exist it
#'   will be created. Ignored if \code{just_path=TRUE}
#' @param just_path Logical: if TRUE return a character vector of available
#'   files without downloading them. Default to FALSE.
#' @param subset Numeric vector of indices for downloading a subset of files
#'   instead of all. If is not numeric it will be ignored silently.
#' @details During the first use, users will be ask to enter their Earth Explore
#'   login Information for downloading the data. If you don't have already an
#'   account, register
#'   \href{https://urs.earthdata.nasa.gov/users/new?client_id=YQOhivHfMTau88rjbMOVyg&redirect_uri=https%3A%2F%2Fdaac.ornl.gov%2Fcgi-bin%2Furs%2Furs_logon_proc.pl&response_type=code&state=https%3A%2F%2Fdaac.ornl.gov%2Fcgi-bin%2Fdataset_lister.pl%3Fp%3D40}{at
#'   this link}. These information will be saved in \code{outdir} as a netrc
#'   file. This function uses the \code{\link[foreach]{foreach}} package for
#'   downloading files in parallel, with the
#'   \code{\link[doParallel]{doParallel}} configuration. If a file with the same
#'   name is already presented in \code{outdir} it will be overwrite.
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
#'from <- "2020-07-01"
#'to <- "2020-07-02"
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
#'#download the first 4 files
#'
#'l4_download(ul_lat=ul_lat,
#'            lr_lat=lr_lat,
#'            ul_lon=ul_lon,
#'            lr_lon=lr_lon,
#'            from=from,
#'            to=to,
#'            just_path=F,
#'            outdir = tempdir(),
#'            subset=1:4)
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

    # Get path to GEDI2A data
    gLevel4 <-
      gedifinder(
        ul_lat,
        ul_lon,
        lr_lat,
        lr_lon,
        daterange = daterange
      )

    lg <- length(gLevel4)

    if(lg==0){stop("there are no GEDI files for this date or coordinates")}

    if (just_path) {
      return(gLevel4)
      stop(invisible())
    }


    #check for existing GEDI file in outdir
   pre <- list.files(outdir,pattern = "h5")
   if(length(pre)!=0) {
     gLevel4 <-
       gLevel4[!basename(tools::file_path_sans_ext(gLevel4)) %in% basename(tools::file_path_sans_ext(pre))]
     nlg <- length(gLevel4)
     message(lg, " files found, of wich ",lg-nlg, " already downloaded in ", outdir)

   }else{ message(lg, " files found.")}


    #subset GEDI files
    if (!is.null(subset) && is.numeric(subset)) {
      if(length(subset)>length(gLevel4)){
        warning("the length of subset is greater than the number of files. Subsetting will not be done")
        }else{ gLevel4 <- gLevel4[subset]}
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









