# Selection Tables ####

#' Plotting selection tables
#' @description
#'   These are functions for constructing and presenting selection tables
#'   for the interpretation of results for network dynamics obtained with 
#'   the RSiena package.
#' @author Tom Snijders
#' @references Consult also the RSiena manual, Sections 13.1 and 13.3.
#' @param quad When TRUE (the default), a quadratic function
#'   (average and total alter) is plotted.
#'   Use `quad = FALSE` for similarity effects.
#' @param separation This can be used to make the curves visually distinguishable
#'   if they overlap too much without it.
#'   An advisable value then is, e.g., 0.01.
#' @param bw Whether the plot should be rendered in black and white,
#'   e.g. for publication, or in colour.
#' @examples
#' mynet <- sienaDependent(array(c(s501, s502), dim=c(50, 50, 2)))
#' mycov  <- coCovar(s50a[,1])
#' mydata <- sienaDataCreate(mynet, mycov)
#' myeff <- getEffects(mydata)
#' myeff <- includeEffects(myeff, simX, interaction1="mycov")
#' myalgorithm <- sienaAlgorithmCreate(nsub=2, n3=100, seed=1291)
#' # nsub=2, n3=100 is used here for having a brief computation, not for practice.
#' ans <- siena07(myalgorithm, data=mydata, effects=myeff, silent=TRUE, batch=TRUE)
#' x <- selectionTable(ans, mydata, "mynet", "mycov")
#' plot(x, xd, name, vname, levls)
#' @export
plot.selectionTable <- function(x, 
                                quad=TRUE,
                                base_size=32, separation=0, bw=FALSE,
                                ...){
  # vnametext and nametext: the names of the variable and of the network,
  # as will be given as texts in the plot.
  # vnametext.l: the names of the variable as will be given as text in the
  # plot legend indicating the curves for ego's values.
  # labels are for the curves, corresponding to ego's values.
  vselect <- x
  levls <- attr(x, "levls")
  multiplier <- attr(x, "multiplier")
  vr <- max(vselect$select) - min(vselect$select) # only for separation
  vselect$select <- vselect$select + separation*vr*as.numeric(factor(vselect$ego))
  levls <- multiplier*levls
  levls.alt <- multiplier*levls.alt
  # or try  sp <- ggplot(vselect, aes(vego, valter, fill=select)) + geom_tile()
  if (bw) {
    sp <- ggplot2::ggplot(vselect, aes(valter, select, group=ego, linetype=ego))
  } else {
    sp <- ggplot2::ggplot(vselect, aes(valter, select, group=ego, colour=ego))
  }
  if (quad) {
    if (bw) {
      gs <- ggplot2::geom_smooth(linewidth=1.7, span=3, color='black')  # size is line width
    } else {
      gs <- ggplot2::geom_smooth(linewidth=1.7, span=3)
    }
  } else {
    if (bw) {
      gs <- ggplot2::geom_line(linewidth=1.7, color='black')
    } else {
      gs <- ggplot2::geom_line(linewidth=1.7)
    }
  }
  if (bw) {
    sp <- sp + ggplot2::geom_point(color='black') + gs + scale_linetype_manual(
      values= c('solid',  'longdash','dashed',
                'twodash', 'dotdash', 'dotted'), labels=labels) +
      ggplot2::theme_bw(base_size=base_size, base_family="") +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank())
  } else {
    sp <- sp + ggplot2::geom_point() + gs + ggplot2::scale_colour_hue(labels=labels)+
      ggplot2::theme_grey(base_size=base_size, base_family="")
  }
  
  nametext <- attr(x, "name")
  vnametext <- vnametext.l <- attr(x, "vname")
  
  ssp <- sp + ggplot2::scale_x_continuous(breaks=levls.alt) +
    ggplot2::theme(legend.key=element_blank())+
    ggplot2::labs(x=paste(vnametext,'alter'),
         y=paste('Selection function'),
         title=paste('Effect',vnametext,'on',nametext),
         linetype=paste(vnametext.l,'\n ego\n', sep=''),
         colour=paste(vnametext.l,'\n ego\n', sep='')) +
    ggplot2::theme(legend.key.width=unit(2.5, "cm")) +
    # of course you could vary the key.width - or anything else...
    ggplot2::theme(plot.title=element_text(hjust=0.5))
  ssp
}

