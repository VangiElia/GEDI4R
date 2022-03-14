#' Plot AGBD profile from GEDI level 4A
#'
#' The function plot the AGBD values against the along-track elevation profile.
#' The function is mainly used for plotting a single track instead of a full
#' area coverage of GEDI footprints.
#' @inheritParams l4_plotagb
#' @inheritParams l4_clip
#' @return Returns a \code{ggplot} object. See \link[ggplot2:ggplot]{ggplot2::ggplot} package.
#' @seealso \code{\link{l4_plotagb}}
#' @examples
#' data("gedil4")
#' #plot first beam
#' l4_plotprofile(gedil4)
#' #plot all beams
#' l4_plotprofile(gedil4,beam_id="all")
#' @export

l4_plotprofile <- function (gediL4, beam_id = "BEAM0000",tct=NULL) {

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


  if(!beam_id%in%c(unique(gediL4$beam),"all"))stop("The selected beam is not in the database of GEDI Level4 data")
  if(!is.null(tct)){
    tree_c <- colnames(gediL4)[grep("tre|tree",colnames(gediL4))]
    gediL4 <- gediL4[gediL4[[tree_c]]>tct,]
  }

  prep_df <- function(id){

    sub <- gediL4[gediL4$beam==id,]

    n0 <- nrow(sub)
    dft <-sub

    dft$rowids <- 1:n0

    dft$xp <- ((dft$rowids * 60) - 60)/1000
    dft$yp<- round(dft$elev_lowestmode)

    gg <- ggplot2::ggplot(dft) +
      ggplot2::geom_tile(ggplot2::aes(
        x = xp,
        y = yp+scales::rescale(agbd, to = c(min(yp), max(yp)))/2,
        height = scales::rescale(agbd, to = c(min(yp), max(yp))) ,
        width=0.1,
        fill = agbd
      ),alpha=1) +
      ggplot2::geom_line(ggplot2::aes(x = xp, y = yp,color="Ground \nElevation (m)")) +
      ggplot2::scale_color_manual(name = "",values = c("black"))+
      ggplot2::scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 8, name = "RdYlGn")) +
      ggplot2::theme(panel.border = ggplot2::element_rect(
        colour = "gray70",
        fill = NA,
        size = 0.2
      )) +
      ggplot2::labs(fill = expression(AGBD ~ (t / ha)),title=id,caption=paste0("n# footprints: ",nrow(dft))) +
      ggplot2::theme(legend.key.height = ggplot2::unit(1, "cm"))+
      ggplot2::xlab("Distance Along Track (km)") +
      ggplot2::ylab("Elevation (m)") +
      ggplot2::theme_minimal(base_family = "serif")
    return(gg)
  }

  if(any(colnames(gediL4) %in% c("source"))) {
    if(length(unique(gediL4$source))!=1){
      warning(
        "Plotting profiles from multiple GEDI files (orbits) can be misleading by forcing an overlap of data from tracks at different locations.")
    }
  }

   if(beam_id!="all"){

    id <- beam_id
    plot <- prep_df(id)
    return(plot)

  }else {
    all_beam <- unique(gediL4$beam)
    figure <- lapply(all_beam,prep_df)
    n <- length(figure)
    nCol <- floor(sqrt(n))
    plot <-do.call(gridExtra::grid.arrange, c(figure, ncol=nCol))
    return(invisible(plot))
  }
}
