#' Plot AGBD data from GEDI level 4A
#'
#' The function plot the location of GEDI footprints and AGBD values against the
#' elevation. Note that the coordinate reference system must be lon/lat (EPSG
#' 4326).
#' @param gediL4 \code{data.table} or \code{sf_object}: obtained with the function
#'   \code{l4_getmulti}.
#' @param beam_id Character: one of "BEAM0000" "BEAM0001" "BEAM0010" "BEAM0011"
#'   "BEAM0101" "BEAM0110" "BEAM1000" "BEAM1011" or "all".
#' @param tct Numeric: tree cover threshold from 0 to 100 (in %).
#' @param type Character: one of "location","distribution" or "both". The type
#'   of plot returned. If missing it will be set to "both".
#' @param ... Others argument to pass to \code{\link[MASS]{MASS::kde2d}} inside
#'   the function \code{get_density}. Only used if \code{type="distribution" or "both"}.
#'   Usually these arguments are: \code{n,h}.
#' @return Returns a ggplot object. See \link[ggplot2:ggplot]{ggplot2::ggplot}
#'   package.
#' @seealso \code{\link{l4_plotprofile}}
#' @examples
#' data("gedil4")
#' #plot location of footprints
#' l4_plotagb(gedil4,type="location")
#' #plot distribution of agbd based on elevation
#' l4_plotagb(gedil4,type="distribution")
#' #change density parameters
#' l4_plotagb(gedil4,type="distribution",n=200,h=c(200,200))
#' #plot both types
#' l4_plotagb(gedil4,type="both",n=200,h=c(200,200))
#' #plot location of footprint in tree cover >50%
#' l4_plotagb(gedil4,type="location",tct=50)
#' @export

l4_plotagb <- function(gediL4,beam_id="BEAM0000",tct=NULL,type=c("location","distribution","both"),...){

  beam_id <-
    eval(bquote(match.arg(.(beam_id),c(
      "BEAM0000",
      "BEAM0001",
      "BEAM0010",
      "BEAM0011",
      "BEAM0101",
      "BEAM0110",
      "BEAM1000",
      "BEAM1011",
      "all"
    ))))

  if(missing(type)){
  type <- "both"
  }else{
  type <- eval(bquote(match.arg(.(type),c("location","distribution","both"))))
  }

  if(any(class(gediL4)=="sf")){
    gediL4 <- sf::st_transform(gediL4, 4326)
    coord <- sf::st_coordinates(gediL4)
    gediL4$lon_lowestmode <- coord[,1]
    gediL4$lat_lowestmode <- coord[,2]
    gediL4 <- data.table::as.data.table(sf::st_set_geometry(gediL4,NULL))
  }

  if(!beam_id%in%c(unique(gediL4$beam),"all"))stop("The selected beam is not in the database of GEDI Level4 data")

  if(beam_id=="all"){
    sub <- gediL4
  }else{
    sub <- gediL4[gediL4$beam==beam_id,]
  }
  bbox <- c(range(sub$lon_lowestmode),range(sub$lat_lowestmode))[c(1,3,2,4)]

  if(!is.null(tct)){
    stopifnot("tct must be of lenght 1"=length(tct)==1,
              "tct must be numeric"=is.numeric(tct))
    tree_cover <- colnames(sub)[grep("tre_|tree_",colnames(sub))]
    index <- sub[[tree_cover]]>=tct
    sub <-sub[index,]
  }

  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  g1 <- ggplot2::ggplot(sub) +
    ggplot2::geom_sf(data = world,fill = c("#F0F0F0"),col="black") +
    ggplot2::coord_sf(ylim = bbox[c(2,4)], xlim =  bbox[c(1,3)], expand = T)+
    ggplot2::geom_point(ggplot2::aes(y=lat_lowestmode,x=lon_lowestmode,col=agbd,size=agbd),alpha=0.65)+
    ggplot2::scale_colour_gradientn(name= "AGBD (Mg/ha)",colours =  c("#68FA3C", "#20BA1A", "#10960C", "#065403", "#042600"))+
    ggplot2::guides(size="none")+
    ggplot2::labs(x="Longitude",y="Latitude",caption=paste0("n# footprints: ",nrow(sub)))+
    ggplot2::theme_minimal(base_family = "serif")+
    ggplot2::theme(axis.text = ggplot2::element_text(colour = "black",size=12),
          axis.title = ggplot2::element_text(colour = "black",size=15))

  get_density <- function(x, y, ...) {
    dens <- MASS::kde2d(x, y, ...)
    ix <- findInterval(x, dens$x)
    iy <- findInterval(y, dens$y)
    ii <- cbind(ix, iy)
    return(dens$z[ii])
  }
  #calcola la densita

  elevation <- colnames(sub)[grep("elv|elev",colnames(sub))]
  sub$density <- get_density(sub[[elevation]], sub[["agbd"]], ...)

  p <- ggplot2::ggplot(sub,ggplot2::aes(y=agbd,x=elev_lowestmode))+
    viridis::scale_color_viridis() +
    ggplot2::geom_point(ggplot2::aes(col=density),alpha=0.7)+
    ggplot2::labs(x="Elevation (m a.s.l.)",y="AGBD (Mg/ha)",caption=paste0("n# footprints: ",nrow(sub)))+
    ggplot2::theme_bw(base_family = "serif")+
    ggplot2::theme(legend.position = "none",
                   axis.text = ggplot2::element_text(colour = "black",size=12),
                   axis.title = ggplot2::element_text(colour = "black",size=15))

  g2 <-
    ggExtra::ggMarginal(
      p,
      type = "densigram",
      fill = "white",
      colour = c("#3D3D3D"),
      size = 4,
      xparams = list(size = 1),
      yparams = list(size = 1)
    )

  if(type=="location"){
    return(g1)
  }else if(type=="distribution"){
    return(g2)
  }else{
    gridExtra::grid.arrange(g1,g2,ncol=2)
  }
}


