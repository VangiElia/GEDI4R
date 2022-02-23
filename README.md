# GEDI4R: A package for NASA's Global Ecosystem Dynamics Investigation (GEDI) Level 4A Data Visualizing and Processing.

This package offers a set of functions to work with the GEDI Level 4A data. This
dataset contains GEDI Level 4A version 2.0 (L4A) predictions of the aboveground biomass
density (AGBD; in Mg/ha) and estimates of the prediction standard error within
each sampled geolocated laser footprint derived from parametric models that
relate simulated GEDI Level 2A (L2A) waveform relative height (RH) metrics to
field plot estimates of AGBD. The datasets is available for the period
2019-04-18 to 2020-09-03.  
More information on Level 4A data can be found
[here](https://daac.ornl.gov/GEDI/guides/GEDI_L4A_AGB_Density_V2.html). The package
follows a simple name convention: all functions names start with `l4_` and are
followed by a verb indicating the purpose of the function.

## Getting started

```{r eval=FALSE}
#install.package("devtools")
devtools::install_github("VangiElia/GEDI4R")
#loading GEDI4R package
library(GEDI4R)
```

## Find and download GEDI data within your study area: `l4_download`

`l4_download` uses `gedifinder` to find GEDI Level 2A data intersecting a
user-defined extent and date range and uses the resulting paths to parse the
GEDI Level 4A path. The list of the resulting path is downloaded in parallel
using the `foreach` package.  
At the first run of `l4_download`, users will need
to enter their Earth Explorer Login Information (if you do not have an account,
sign up at this
[link](https://urs.earthdata.nasa.gov/users/new?client_id=YQOhivHfMTau88rjbMOVyg&redirect_uri=https%3A%2F%2Fdaac.ornl.gov%2Fcgi-bin%2Furs%2Furs_logon_proc.pl&response_type=code&state=https%3A%2F%2Fdaac.ornl.gov%2Fcgi-bin%2Fdataset_lister.pl%3Fp%3D40))
for downloading the data. The function will create a _netrc_ file in the
directory specified by the argument `outdir` that stores the access credentials
to the ORNL DAAC database, where GEDI Level 4 datasets are stored. `l4_download`
will search for the _netrc_ file in `outdir` at each run. If it does not find
it, it recreates it by asking the user to enter the login credentials  again. As
long as this file is in `outdir`, the user does not need to enter this
information again. If the user changes `outdir`, a new _netrc_ file will be
created after entering the access credentials.

```{r eval=FALSE}
#using Italy as bounding box for search GEDI data
ita <- sf::st_as_sf(raster::getData('GADM', country = 'ITA', level = 1))
#extract extent
e <- raster::extent(ita)
ul_lat <- e@ymax
lr_lat <- e@ymin
ul_lon <- e@xmin
lr_lon <- e@xmax
outdir = tempdir()
#Get the list of all files available for the study area in the period selected,
#using just_path = T
file_path <- l4_download(
  ul_lat,
  lr_lat,
  ul_lon,
  lr_lon,
  outdir = outdir,
  from = "2020-01-01",
  to = "2020-01-31",
  just_path = T
)
#download the first 4 files
file_download <- l4_download(
  ul_lat,
  lr_lat,
  ul_lon,
  lr_lon,
  ncore = parallel::detectCores()-1,
  outdir = outdir,
  from = "2020-01-01",
  to = "2020-01-31",
  just_path = F,
  subset=1:4
)
```

## Reading GEDI data: `l4_getmulti`

After downloading, files can be read from the original file format (h5) with
`l4_getmulti` as `data.table` objects. The function reads
can accept a list or a vector of file paths
(as the output of `l4_download`) and read them in parallel, using the `snowfall`
package. If the list of file path has lenght=1 the file will be read in single thread mode.
The function can remove footprints with AGBD values corrupted
(agbd<0), and can be used to filter footprints based on the tree cover threshold
derived for the year 2010, from Hansen et al. (2013) and encoded as a percentage
per output grid cell.  
The functions can list the dataset names inside the structure of h5 files.
This is useful for adding other columns to the functions's default output datasets
by specifying the argument `add_col`. 
See the Details section of `?l4_getmulti` for the default dataset extracted
from the h5 file.


```{r results="hide"}
outdir = tempdir()
l4_zip <- system.file("extdata",
                 "GEDI04_A_2020186052327_O08834_T03611_02_001_01.zip",
                 package="GEDI4R")
#Unzipping GEDI level4A data
file <- unzip(l4_zip,exdir = outdir)
#list just datasets names inside h5 file
dataname <- l4_getmulti(file,just_colnames = T)
head(dataname,10)
#return footprints acquired with agbd greater than 0 and tree cover greater than 10%
gediL4_path <- l4_getmulti(file,agbd_rm=0,tct=10)
#select other columns to add to the default output.
#if columns are already present in the default output they will be dropped
col <-
  c("land_cover_data/leaf_off_flag",
    "agbd_pi_lower",
    "agbd_pi_upper",
    "agbd"#this will be dropped as it is included by default in the output
    )
#get level 4 data with the user defined column binded
gediL4 <- l4_getmulti(file,add_col = col,agbd_rm=0,tct=10)
```

```{r}
knitr::kable(head(gediL4_path[,c("date","tree_cover","agbd","agbd_se")]))
```


`l4_getmulti` can be used to read and merge a list of GEDI.h5 files, listed, for
example,  with `l4_download`.


```{r warning=FALSE, eval=FALSE}
outdir = tempdir()
l4_zip <- system.file("extdata",
                     c("GEDI04_A_2020186052327_O08834_T03611_02_001_01.zip",
                       "GEDI04_A_2020186065619_O08835_T00766_02_001_01.zip",
                       "GEDI04_A_2020187043633_O08849_T04437_02_001_01.zip",
                       "GEDI04_A_2020187060925_O08850_T01592_02_001_01.zip"
                     ),
                     package="GEDI4R")
#Unzipping GEDI level4A data
files <- lapply(l4_zip,unzip,exdir = outdir)
#select the number of cores to be used.
core <- ifelse(parallel::detectCores()-1<=length(file),parallel::detectCores()-1,length(file))
#read, filter by agbd, tree cover threshold and merge all files.
#With source=T a column with the source file for each observation will be added
l4_data <- l4_getmulti(files,ncore = core,agbd_rm=0,tct=10,merge = T,source=T)
```

## Clipping GEDI data: `l4_clip`

After downloading and reading GEDI data, a typical pre-processing step is to
clip the data to the extent of a study area. To do so, `l4_clip` can be used.
The function clip footprints by extent or vector boundary provided by the
argument `clip`. Currently, it accepts a path to a shp or tif file, an object of
class `sf`, a `Raster*` object, a numeric vector of coordinates or other objects
from which an extent can be extracted. By specifying the argument `usebound=TRUE`
the points will be clipped on the boundary of an `sf` object (or path from which
an `sf` object can be created).  
GEDI coordinates are by default in lon/lat format (EPSG 4326). The function will
try to convert the extent of `clip` to lon/lat coordinate system to ensure
compatibility during the clip. The only exception is when `clip` is a numeric
vector or a bbox object. In these cases, the user must check that the extent is in
lon/lat projection.

```{r}
#file path of Italian boundary
bound <- system.file("extdata","Italy.shp",package="GEDI4R")
#clipping on polygon extent 
clipped <- l4_clip(gediL4,clip=bound,usegeometry = F)
#clipping on polygon boundary 
clipped <- l4_clip(gediL4,clip=bound,usegeometry = T)
#clip using bbox
#first we need to convert coordinates in lon/lat
tmp <- raster::shapefile(bound)#read boundary
box <- t(raster::bbox(tmp))#extract bounding box
proj4string <- as.character(tmp@proj4string)#retrive original projection
pj <- proj4::project(box, proj4string, inverse=TRUE)# Transformed data in lat lon
box <-c(t(pj))
clipped <- l4_clip(gediL4,clip=box)
```

## Export GEDI data: `l4_convert`

After pre-processing, the next step is converting and exporting the data in a
vector format, usually an ESRI Shapefile. This can be easily accomplished with
the function `l4_convert`, which can also reproject the data to a user-defined
coordinate reference system (specified via the EPSG code by the argument
`epsg`).

```{r warning=FALSE}
converted <- l4_convert(clipped,epsg = 32632,filename=paste0(outdir,"/example.shp"))
list.files(outdir,pattern = "example",full.names = T)
```


## Plot GEDI data: `l4_plotagb` and `l4_plotprofile`

Finally, the package implements two functions for plotting the data:
`l4_plotagb` and `l4_plotprofile`.  
The former function plots the location of footprints, the distribution of AGBD
against the elevation, or both.  
The latter function returns the AGBD against the elevation profile along the
GEDI track. Note that plotting elevation profiles from GEDI data (l4_plotprofile) is only advisable for single beam/track pairs. Plotting profiles from a data.table concatenating multiple GEDI files (orbits) can be misleading by forcing an overlap of data from tracks at different locations.


```{r fig.align="center", fig.width=8, fig.height=8}
#footprints locations
l4_plotagb(clipped,type = "location")
```
```{r fig.align="center",fig.width=8, fig.height=8}
#AGBD distribution against elevation
l4_plotagb(clipped,type = "distribution",n=100,h=c(100,100))
```
```{r fig.align="center", fig.width=8, fig.height=8}
#along-track AGBD profile
l4_plotprofile(clipped)
```

## Pre-processin chain: `l4_process`

All the above pre-processing steps can be performed at once in chunks of files
with the function `l4_process`. It runs, in order, `l4_getmulti`, `l4_clip` and
`l4_convert` to each file chunk.  
By specifying `parallel=TRUE`, the function
will process each chunk in parallel trying to guess the best number of cores to
be used in `l4_getmulti` and in the chunks loop, based on the maximum number of
cores available and the number of files to be processed. The user can override
the number of cores used in `l4_getmulti` with `...`, by specifying the argument
`ncore`. This will also affect the number of cores used to loop over chunks.
Usually, the number of cores used by default is the best option. Modifying it
can slow down the function.

```{r eval=FALSE}
#specifying the path to GEDI level4A data (zip file)
outdir  <-  tempdir()
l4_zip <- system.file(
  "extdata",
  c(
    "GEDI04_A_2020186052327_O08834_T03611_02_001_01.zip",
    "GEDI04_A_2020186065619_O08835_T00766_02_001_01.zip",
    "GEDI04_A_2020187043633_O08849_T04437_02_001_01.zip",
    "GEDI04_A_2020187060925_O08850_T01592_02_001_01.zip"
  ),
  package = "GEDI4R"
)
#Unzipping GEDI level4A data
l4 <- lapply(l4_zip, unzip, exdir = outdir)
bound <- system.file("extdata", "Italy.shp", package = "GEDI4R")
#proces all files in chunk, each of 2 files, in sequence
l4_data <-
  l4_process(
    l4,
    nfile = 2,
    clip = bound,
    usegeometry = T,
    epsg = 32632,
    prefix = "block",
    outdir,
    parallel = F
  )
file.remove(l4_data)
#in parallel
l4_data <-
  l4_process(
    l4,
    nfile = 2,
    clip = bound,
    usegeometry = T,
    epsg = 32632,
    prefix = "block",
    outdir,
    parallel = T
  )
file.remove(l4_data)
#override the default number of cores to be used
l4_data <-
  l4_process(
    l4,
    nfile = 2,
    clip = bound,
    usegeometry = T,
    epsg = 32632,
    prefix = "block",
    outdir,
    parallel = T,
    ncore = 4
  )
file.remove(l4_data)
```
