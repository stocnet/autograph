# Selection Tables ####

#' Plotting selection tables
#' @description
#'   These are functions for constructing and presenting selection tables
#'   for the interpretation of results for network dynamics obtained with 
#'   the RSiena package.
#' @author Tom Snijders
#' @references Consult also the RSiena manual, Sections 13.1 and 13.3.
#' @inheritParams plot.diffusion
#' @param x An object of class "selectionTable",
#'   created using `RSiena::selectionTable()`.
#' @param quad When TRUE (the default), a quadratic function
#'   (average and total alter) is plotted.
#'   Use `quad = FALSE` for similarity effects.
#' @param separation This can be used to make the curves visually distinguishable
#'   if they overlap too much without it.
#'   An advisable value then is, e.g., 0.01.
#' @importFrom ggplot2 ggplot geom_smooth geom_line geom_point theme_bw
#' @importFrom stats setNames
#' @returns A plot showing how the selection evaluation function changes based 
#'   on ego's value and alter's value of some covariate.
#' @examples
#' plot(res_siena_selection)
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
      ggplot2::theme_minimal(base_size=8, base_family="") 
    # + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
    #         panel.grid.minor = ggplot2::element_blank())
  } else {
    sp <- sp + #ggplot2::geom_point() + 
      gs +
      ggplot2::scale_colour_manual(values = setNames(ag_sequential(length(labs)), 
                                                     labs)) + 
      ggplot2::theme_minimal(base_size=8, base_family="")
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
    ggplot2::theme(plot.title=element_text(hjust=0.5))
  ssp
}

# Influence Tables ####

#' Plotting influence tables
#' @description
#'   These are functions for constructing and presenting influence tables
#'   for the interpretation of results for network and behavior dynamics
#'   obtained with the RSiena or multiSiena packages.
#' @author Tom Snijders
#' @references Consult also the RSiena manual, Sections 13.2 and 13.4.
#'   Gratitude to Steffen Triebel and Rene Veenstra for corrections.
#' @inheritParams plot.selectionTable
#' @param x An object of class "influenceTable",
#'   created using `RSiena::influenceTable()`.
#' @returns A plot showing how the influence evaluation function changes based 
#'   on ego's value and alter's value of some covariate.
#' @examples
#' plot(res_siena_influence)
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
    ggplot2::theme(plot.title=element_text(hjust=0.5))
}

# Goodness of Fit ####

#' SIENA Goodness of Fit
#' @description
#'   This function plots goodness of fit objects created using RSiena.
#'   Unlike the plot method included in the `{RSiena}` package,
#'   this function utilises `{ggplot2}` and not `{lattice}`,
#'   which makes the output more compatible and themeable.
#' @param x A sienaGOF object, as returned by `RSiena::sienaGOF()`.
#' @param ... Other parameters to be passed to the plotting funciton,
#'   for example `main = "Title"` for a different title than the default.
#' @importFrom tidyr pivot_longer
#' @returns A violin plot showing the distribution of statistics from the 
#'   simulations and a line highlighting the observed statistics.
#' @examples
#' plot(res_siena_gof)
#' @export
plot.sienaGOF <- function(x, ...){
  
  args <- list(...)
  if (is.null(args$main)) {
    main = paste("Goodness of Fit of", attr(x, "auxiliaryStatisticName"))
    if (!attr(x, "joined")) {
      main = paste(main, "Period", period)
    }
  }
  else {
    main = args$main
  }
  if (attr(x, "joined")) {
    x <- x[[1]]
  }
  else {
    x <- x[[period]]
  }
  sims <- x$Simulations
  sims.min <- apply(sims, 2, min)
  sims.max <- apply(sims, 2, max)
  obs <- x$Observations
  no_vary <- sims.min == obs & sims.min == sims.max
  if (any((diag(stats::var(rbind(sims, obs))) == 0))) {
    cli::cli_alert_info("Note: some statistics are not plotted because their variance is 0.")
    statkeys <- attr(x, "key")[which(diag(stats::var(rbind(sims, obs))) == 0)]
    cli::cli_alert_info("This holds for the statistic{?s} {statkeys}.")
  }
  
  itns <- nrow(sims)
  n.obs <- nrow(obs)
  sims <- sims[,!no_vary]
  sims <- as.data.frame(sims) %>% 
    dplyr::mutate(sim = 1:nrow(sims)) %>% 
    tidyr::pivot_longer(!sim)
  obs <- obs[!no_vary]
  obs <- as.data.frame(obs) %>% 
    tidyr::pivot_longer(cols = dplyr::everything()) %>% 
    dplyr::mutate(name = as.character((1:length(obs))-1))
  
  ggplot2::ggplot(sims, aes(x = name, y = value)) +
    ggplot2::geom_violin(scale = "width", trim = FALSE, color = ag_base(),
                         draw_quantiles = c(0.05,0.95)) +
    ggplot2::geom_point(data = obs, aes(x = name, y = value),
                        color = ag_highlight()) +
    ggplot2::geom_line(data = obs, aes(x = name, y = value),
                       group = 1,
                       color = ag_highlight()) +
    ggplot2::theme_minimal() +
    ggplot2::labs(y = "Statistic", title = main, 
                  x = paste("p:", round(x$p, 3), collapse = " "))
}