![](https://github.com/VangiElia/GEDI4R/blob/main/readme/GEDI4.png)<br/>
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
#install.packages("devtools")
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
`l4_getmulti` as `data.table` objects. The function can accept a list or a vector of file paths
(as the output of `l4_download`) and read them in parallel, using the `snowfall`
package. If the list of file path has lenght=1 the file will be read in single thread mode.
The function remove by default footprints with AGBD values corrupted
(agbd<0), and can be used to filter footprints based on the tree cover threshold
derived for the year 2010, from Hansen et al. (2013) and encoded as a percentage
per output grid cell.  
The functions can list the dataset names inside the structure of h5 files.
This is useful for adding other columns to the functions's default output datasets
by specifying the argument `add_col`. 
See the Details section of `?l4_getmulti` for the default dataset extracted
from the h5 file.


```{r results="hide", eval=FALSE}
outdir = tempdir()
l4_zip <- system.file("extdata",
                      c("GEDI04_A_2020036151358_O06515_02_T00198_02_002_01_V002.zip",
                        "GEDI04_A_2021150031254_O13948_03_T06447_02_002_01_V002.zip"
                      ),
                      package="GEDI4R")
#Unzipping GEDI level4A data
l4 <- lapply(l4_zip,unzip,exdir = outdir)
#list all dataset in h5 file
dataname <- l4_getmulti(l4[[1]],just_colnames = T)
head(dataname,10)
#read all footprint and merge them together.
gediL4_path <- l4_getmulti(l4,merge=T)
#select other columns to add to the default output.
#if columns are already present in the default output they will be dropped
col <-
  c("land_cover_data/leaf_off_flag",
    "agbd_pi_lower",
    "agbd_pi_upper",
    "agbd"#this will be dropped as it is already included by default in the output.
    )
#get level 4 data with the user defined column binded and with the source path of each observation
#with source=T a column with the source path for each observation will be added
gediL4 <- l4_getmulti(l4,add_col = col,source=T)
```

```{r}
knitr::kable(head(gediL4[,c("date","tree_cover","agbd","agbd_se")]))
```

## Clipping GEDI data: `l4_clip`

After downloading and reading GEDI data, a typical pre-processing step is to
clip the data to the extent of a study area. To do so, `l4_clip` can be used.
The function clip footprints by extent or vector boundary provided by the
argument `clip`. Currently, it accepts a path to a shp or tif file, an object of
class `sf`, a `Raster*` object, a numeric vector of coordinates or other objects
from which an extent can be extracted. By specifying the argument `usegeometry=TRUE`
foorprints will be clipped on the boundary of an `sf` object (or path from which
an `sf` object can be created).  
GEDI coordinates are by default in lon/lat format (EPSG 4326). The function will
try to convert the extent of `clip` to lon/lat coordinate system to ensure
compatibility during the clip. The only exception is when `clip` is a numeric
vector or a bbox object. In these cases, the user must check that the extent is in
lon/lat projection.

```{r}
outdir = tempdir()
l4_zip <- system.file("extdata",
                     "GEDI04_A_2020036151358_O06515_02_T00198_02_002_01_V002.zip",
                     package="GEDI4R")
l4 <- unzip(l4_zip,exdir = outdir)
#get GEDI data
l4_data <- l4_getmulti(l4)
#clip using vector of coordinates
b_box <- c(-50,35,52,37)
clipped <- l4_clip(l4_data,clip=b_box)
#using Shapefile to clip
bound <- system.file("extdata","bound4326.shp",package="GEDI4R")
#with  extension
clipped <- l4_clip(l4_data,clip=bound,usegeometry = F)
#with  polygon boundaries
clipped2 <- l4_clip(l4_data,clip=bound,usegeometry = T)
```

## Export GEDI data: `l4_convert`

After pre-processing, the next step is converting and exporting the data in a
vector format, usually an ESRI Shapefile. This can be easily accomplished with
the function `l4_convert`, which can also reproject the data to a user-defined
coordinate reference system (specified via the EPSG code by the argument
`epsg`). Note that in converting data to ESRI Shapefile, columns names will be abbreviated with a warning.
The function is called for its side effects. It return NULL unless return_path=TRUE.

```{r}
converted <- l4_convert(l4_data,epsg = 4326,filename=paste0(outdir,"/example.shp"),return_path = T,append=F)
example <- sf::read_sf(converted)
file.remove(list.files(outdir,pattern = "example",full.names = T))
```

## Plot GEDI data: `l4_plotagb` and `l4_plotprofile`

Finally, the package implements two functions for plotting the data:
`l4_plotagb` and `l4_plotprofile`.  
The former function can plots the location of footprints, the distribution of AGBD against the elevation, or both.  
The latter function returns the AGBD against the elevation profile along the
GEDI track. Note that plotting elevation profiles from GEDI data (l4_plotprofile) is only advisable for single beam/track pairs.
Plotting profiles from a data.table concatenating multiple GEDI files (orbits) can be misleading by forcing an overlap of data from tracks at different locations.


```{r eval=FALSE}
gediL4 <- l4_getmulti(l4)
#footprints locations and AGBD distribution against elevation
l4_plotagb(gediL4,,beam_id="all",type = "both",n=100,h=c(100,100))
```
<img align="center" src="https://github.com/VangiElia/GEDI4R/blob/main/readme/fig1.png"  width="800">

```{r eval=FALSE}
#along-track AGBD profile
l4_plotprofile(gediL4,beam_id="all")
```
<img align="center" src="https://github.com/VangiElia/GEDI4R/blob/main/readme/fig2.png"  width="800">


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
outdir = tempdir()
l4_zip <- system.file("extdata",
                      c("GEDI04_A_2020036151358_O06515_02_T00198_02_002_01_V002.zip"
                      ),
                      package="GEDI4R")
#Unzipping GEDI level4A data
l4 <- unzip(l4_zip,exdir = outdir)
#create 4 copy of GEDI file to test the function
file.copy(from=l4,to=paste0(tools::file_path_sans_ext(l4),"_copy",1:4,".h5"))
#path to Shapefile for clipping the data
bound <- system.file("extdata","bound4326.shp",package="GEDI4R")
#path to GEDI files
l4_path <- list.files(outdir,pattern = "h5",full.names = T)
#proces all files in chunk each of 2 files, in sequence
l4_data <- l4_process(l4_path,nfile=2,clip=bound,epsg=4326,outdir=outdir,ext="shp",parallel=F,prefix = "ex")
file.remove(list.files(outdir,full.names = T, pattern = "ex"))
#in parallel
l4_data <- l4_process(l4_path,nfile=2,clip=bound,epsg=4326,outdir=outdir,ext="shp",parallel=T)
file.remove(list.files(outdir,full.names = T, pattern = "ex"))
```
