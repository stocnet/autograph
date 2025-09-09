#' Plotting effects interpretation
#' @name plot_interp
#' @description
#'   These functions support the interpretation of network and behavior effects
#'   found in stochastic actor-oriented models.
#'   They are S3 plotting methods for objects of class "selectionTable"
#'   or "influenceTable", created using `RSiena::selectionTable()`
#'   or `RSiena::influenceTable()`, respectively.
#'   They plot how the evaluation function for selection or influence
#'   changes based on ego's value and alter's value of some covariate.
#'   This helps to interpret the effect of that covariate on the network dynamics
#'   or behavior dynamics, respectively.
#' @details
#'   These functions were originally written by Tom Snijders, and adapted
#'   for use in the `{autograph}` package.
#' @inheritParams plot.diffusion
#' @param x An object of class "selectionTable" or "influenceTable",
#'   created using `RSiena::selectionTable()` or `RSiena::influenceTable()`,
#'   respectively.
#' @param separation This can be used to make the curves visually distinguishable
#'   if they overlap too much without it.
#'   An advisable value then is, e.g., 0.01.
#' @importFrom ggplot2 ggplot geom_smooth geom_line geom_point theme_bw
#' @importFrom stats setNames
#' @returns A plot showing how the selection/influence evaluation function 
#'   changes based on ego's value and alter's value of some covariate.
NULL

#' @rdname plot_interp
#' @family RSiena
#' @author Tom Snijders
#' @references 
#'   For plotting selection tables, 
#'   please consult the RSiena manual, Sections 13.1 and 13.3.
#' @param quad When TRUE (the default), a quadratic function
#'   (average and total alter) is plotted.
#'   Use `quad = FALSE` for similarity effects.
#' @examples
#' plot(siena_selection)
#' @export
plot.selectionTable <- function(x, quad = TRUE, separation = 0, ...){

  bw <- ifelse(getOption("snet_theme","bw") %in% c("bw","crisp"), 
               TRUE, FALSE)
  vselect <- x
  levls <- attr(x, "levls")
  multiplier <- attr(x, "multiplier")
  vr <- max(vselect$select) - min(vselect$select) # only for separation
  vselect$select <- vselect$select + separation * vr * 
    as.numeric(factor(vselect$ego))
  levls <- multiplier * levls
  # levls.alt <- multiplier*levls.alt
  labs <- unique(vselect$ego)

  sp <- ggplot2::ggplot(vselect, ggplot2::aes(valter, select, 
                                              group=ego, colour=ego))

  if (quad) {
    gs <- ggplot2::geom_smooth(linewidth=1.7, span=3, # size is line width
                               method = "loess", formula = 'y ~ x', se = FALSE)  
  } else {
    gs <- ggplot2::geom_line(linewidth=1.7)
  }
  
  if (bw) {
    sp <- sp + #ggplot2::geom_point(color='black') + 
      gs + 
      ggplot2::scale_colour_manual(values = setNames(ag_sequential(length(labs)), 
                                                     labs)) + 
      # ggplot2::scale_linetype_manual(
      # values= c('solid',  'longdash','dashed',
      #           'twodash', 'dotdash', 'dotted'), labels=labels) +
      ggplot2::theme_minimal(base_size=8, base_family=ag_font()) 
    # + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
    #         panel.grid.minor = ggplot2::element_blank())
  } else {
    sp <- sp + #ggplot2::geom_point() + 
      gs +
      ggplot2::scale_colour_manual(values = setNames(ag_sequential(length(labs)), 
                                                     labs)) + 
      ggplot2::theme_minimal(base_size=8, base_family=ag_font())
  }
  
  nametext <- attr(x, "name")
  vnametext <- vnametext.l <- attr(x, "vname")
  
  ssp <- sp + ggplot2::scale_x_continuous(breaks = levls) +
    ggplot2::theme(legend.key=element_blank())+
    ggplot2::labs(x=paste(vnametext,'alter'),
         y=paste('Selection function'),
         title=paste('Effect',vnametext,'on',nametext),
         # linetype=paste(vnametext.l,'\n ego\n', sep=''),
         colour=paste(vnametext.l,'\n ego\n', sep='')) +
    ggplot2::theme(legend.key.width = ggplot2::unit(2.5, "cm")) +
    # of course you could vary the key.width - or anything else...
    ggplot2::theme(plot.title=element_text(hjust=0.5),
                   base_family=ag_font())
  ssp
}

#' @rdname plot_interp
#' @author Thanks to Steffen Triebel and Rene Veenstra for corrections.
#' @family RSiena
#' @references
#'   For plotting selection tables, 
#'   please consult the RSiena manual, Sections 13.2 and 13.4.
#' @examples
#' plot(siena_influence)
#' @export
plot.influenceTable <- function(x, separation=0, ...){
  zselect <- x
  bw <- ifelse(getOption("snet_theme","bw") %in% c("bw","crisp"), TRUE, FALSE)
  quad <- attr(x, "quad")
  netname <- attr(x, "netname")
  behname <- attr(x, "behname")
  zr <- max(zselect$select) - min(zselect$select) # only for separation
  zselect$select <- zselect$select + separation * zr * 
    as.numeric(factor(zselect$alter))
  labs <- unique(zselect$alter)
  
  sp <- ggplot2::ggplot(zselect, ggplot2::aes(zego, select, 
                                              group=alter, colour=alter)) +
    ggplot2::theme_bw()
  
  if (quad) {
    gs <- ggplot2::geom_smooth(linewidth=1.2, span=3, 
                               method = "loess", formula = 'y ~ x', se = FALSE)
  } else {
    gs <- ggplot2::geom_line(linewidth=1.2)
  }
  
  # if (bw) {
  #   sp <- sp + ggplot2::geom_point() + gs + 
  #     ggplot2::scale_linetype_manual(values =
  #                                      c('solid',  'longdash','dashed', 'twodash', 'dotdash', 'dotted'), labels=labs)
  # } else {
    sp <- sp + ggplot2::geom_point() + gs + 
      ggplot2::scale_colour_manual(values = setNames(ag_sequential(length(labs)), labs))
  # }
  beh.label <- paste0('"',behname,'"')
  ylabel <- "Evaluation function"
  title <- paste0('Influence effect "',netname,'" on "',behname,'"')
  sp + ggplot2::theme(legend.key=element_blank()) +
    ggplot2::labs(x=paste(beh.label,'ego value'), y=ylabel, title=title,
                  linetype=paste(beh.label,'\nalter\nvalue'),
         colour=paste(beh.label,'\nalter\nvalue')) +
    # ggplot2::theme_grey(base_size=14, base_family="") +
    ggplot2::theme(legend.key.width = ggplot2::unit(1, "cm")) +
    ggplot2::theme(plot.title=element_text(hjust=0.5),
                   base_family=ag_font())
}
