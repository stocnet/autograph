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
#' @examples
#' library(RSiena)
#' mynet <- sienaDependent(array(c(s501, s502), dim=c(50, 50, 2)))
#' mycov  <- coCovar(s50a[,1])
#' mydata <- sienaDataCreate(mynet, mycov)
#' myeff <- getEffects(mydata)
#' myeff <- includeEffects(myeff, simX, interaction1="mycov")
#' myalgorithm <- sienaAlgorithmCreate(nsub=2, n3=100, seed=1291)
#' # nsub=2, n3=100 is used here for having a brief computation, not for practice.
#' ans <- siena07(myalgorithm, data=mydata, effects=myeff, silent=TRUE, batch=TRUE)
#' x <- selectionTable(ans, mydata, "mynet", "mycov")
#' plot(x)
#' @export
plot.selectionTable <- function(x, 
                                quad=TRUE, separation=0,
                                ...){

  bw <- ifelse(getOption("snet_theme","bw") %in% c("bw","crisp"), 
               TRUE, FALSE)
  vselect <- x
  levls <- attr(x, "levls")
  multiplier <- attr(x, "multiplier")
  vr <- max(vselect$select) - min(vselect$select) # only for separation
  vselect$select <- vselect$select + separation*vr*as.numeric(factor(vselect$ego))
  levls <- multiplier*levls
  levls.alt <- multiplier*levls.alt
  labs <- unique(vselect$ego)

  sp <- ggplot2::ggplot(vselect, ggplot2::aes(valter, select, group=ego, colour=ego))

  if (quad) {
    gs <- ggplot2::geom_smooth(linewidth=1.7, span=3, 
                               method = "loess", formula = 'y ~ x', se = FALSE)  # size is line width
  } else {
    gs <- ggplot2::geom_line(linewidth=1.7)
  }
  
  if (bw) {
    sp <- sp + #ggplot2::geom_point(color='black') + 
      gs + 
      ggplot2::scale_colour_manual(values = setNames(ag_sequential(length(labs)), labs)) + 
      # ggplot2::scale_linetype_manual(
      # values= c('solid',  'longdash','dashed',
      #           'twodash', 'dotdash', 'dotted'), labels=labels) +
      ggplot2::theme_minimal(base_size=8, base_family="") 
    # + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
    #         panel.grid.minor = ggplot2::element_blank())
  } else {
    sp <- sp + #ggplot2::geom_point() + 
      gs +
      ggplot2::scale_colour_manual(values = setNames(ag_sequential(length(labs)), labs)) + 
      ggplot2::theme_minimal(base_size=8, base_family="")
  }
  
  nametext <- attr(x, "name")
  vnametext <- vnametext.l <- attr(x, "vname")
  
  ssp <- sp + ggplot2::scale_x_continuous(breaks=levls.alt) +
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
#' @examples
#' library(RSiena)
#' mynet <- sienaDependent(array(c(s501, s502), dim=c(50, 50, 2)))
#' mybeh  <- sienaDependent(s50a[,1:2], type="behavior")
#' mydata <- sienaDataCreate(mynet, mybeh)
#' myeff <- getEffects(mydata)
#' myeff <- includeEffects(myeff, avAlt, name="mybeh", interaction1="mynet")
#' myalgorithm <- sienaAlgorithmCreate(nsub=2, n3=100, seed=1291)
#' # nsub=2, n3=100 is used here for having a brief computation, not for practice.
#' ans <- siena07(myalgorithm, data=mydata, effects=myeff, silent=TRUE, batch=TRUE)
#' x <- influenceTable(ans, mydata, "mynet", "mybeh")
#' plot(x)
#' @export
plot.influenceTable <- function(x, separation=0, ...){
  zselect <- x
  bw <- ifelse(getOption("snet_theme","bw") %in% c("bw","crisp"), TRUE, FALSE)
  quad <- attr(x, "quad")
  netname <- attr(x, "netname")
  behname <- attr(x, "behname")
  zr <- max(zselect$select) - min(zselect$select) # only for separation
  zselect$select <- zselect$select + separation*zr*as.numeric(factor(zselect$alter))
  labs <- unique(zselect$alter)
  
  sp <- ggplot2::ggplot(zselect, ggplot2::aes(zego, select, group=alter, colour=alter)) +
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

#' @importFrom lattice trellis.par.get trellis.par.set bwplot panel.violin
#' @examples
#' mynet <- sienaDependent(array(c(s501, s502), dim=c(50, 50, 2)))
#' mybeh <- sienaDependent(s50a[,1:2], type="behavior")
#' mydata <- sienaDataCreate(mynet, mybeh)
#' myeff <- getEffects(mydata)
#' myeff <- includeEffects(myeff, transTrip)
#' myeff <- setEffect(myeff, cycle3, fix=TRUE, test=TRUE)
#' myeff <- setEffect(myeff, transTies, fix=TRUE, test=TRUE)
#' myalgorithm <- sienaAlgorithmCreate(nsub=1, n3=10, projname=NULL)
#' # Shorter phases 2 and 3, just for example.
#' ans <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE, returnDeps=TRUE)
#' gofi <- sienaGOF(ans, IndegreeDistribution, verbose=TRUE, join=TRUE,
#'                  varName="mynet")
#' plot(gofi)
#' @export
plot.sienaGOF <- function(x, center = FALSE, scale = FALSE, violin = TRUE, key = NULL, 
                          perc = 0.05, period = 1, position = 4, fontsize = 12, ...){
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
  obs <- x$Observations
  itns <- nrow(sims)
  n.obs <- nrow(obs)
  screen <- sapply(1:ncol(obs), function(i) {
    (sum(is.nan(rbind(sims, obs)[, i])) == 0)
  }) & (diag(var(rbind(sims, obs))) != 0)
  if (any((diag(var(rbind(sims, obs))) == 0))) {
    cat("Note: some statistics are not plotted because their variance is 0.\n")
    cat("This holds for the statistic")
    if (sum(diag(var(rbind(sims, obs))) == 0) > 1) {
      cat("s")
    }
    cat(": ")
    cat(paste(attr(x, "key")[which(diag(var(rbind(sims, obs))) == 
                                     0)], sep = ", "))
    cat(".\n")
  }
  sims <- sims[, screen, drop = FALSE]
  obs <- obs[, screen, drop = FALSE]
  obsLabels <- round(x$Observations[, screen, drop = FALSE], 
                     3)
  sims.min <- apply(sims, 2, min)
  sims.max <- apply(sims, 2, max)
  sims.min <- pmin(sims.min, obs)
  sims.max <- pmax(sims.max, obs)
  if (center) {
    sims.median <- apply(sims, 2, median)
    sims <- sapply(1:ncol(sims), function(i) (sims[, i] - 
                                                sims.median[i]))
    obs <- matrix(sapply(1:ncol(sims), function(i) (obs[, 
                                                        i] - sims.median[i])), nrow = n.obs)
    sims.min <- sims.min - sims.median
    sims.max <- sims.max - sims.median
  }
  if (scale) {
    sims.range <- sims.max - sims.min + 1e-06
    sims <- sapply(1:ncol(sims), function(i) sims[, i]/(sims.range[i]))
    obs <- matrix(sapply(1:ncol(sims), function(i) obs[, 
                                                       i]/(sims.range[i])), nrow = n.obs)
    sims.min <- sims.min/sims.range
    sims.max <- sims.max/sims.range
  }
  ymin <- 1.05 * min(sims.min) - 0.05 * max(sims.max)
  ymax <- -0.05 * min(sims.min) + 1.05 * max(sims.max)
  if (is.null(args$ylab)) {
    ylabel = "Statistic"
    if (center && scale) {
      ylabel = "Statistic (centered and scaled)"
    }
    else if (scale) {
      ylabel = "Statistic (scaled)"
    }
    else if (center) {
      ylabel = "Statistic (center)"
    }
    else {
      ylabel = "Statistic"
    }
  }
  else {
    ylabel = args$ylab
  }
  if (is.null(args$xlab)) {
    xlabel = paste(paste("p:", round(x$p, 3), collapse = " "), 
                   collapse = "\n")
  }
  else {
    xlabel = args$xlab
  }
  if (is.null(args$cex)) {
    cexpar <- par("cex")
  }
  else {
    cexpar <- args$cex
  }
  if (is.null(args$cex.axis)) {
    cexaxispar <- par("cex.axis")
  }
  else {
    cexaxispar <- args$cex.axis
  }
  if (is.null(args$cex.main)) {
    cexmainpar <- par("cex.main")
  }
  else {
    cexmainpar <- args$cex.main
  }
  if (is.null(args$cex.lab)) {
    cexlabpar <- par("cex.lab")
  }
  else {
    cexlabpar <- args$cex.lab
  }
  if (is.null(args$cex.sub)) {
    cexsubpar <- par("cex.sub")
  }
  else {
    cexsubpar <- args$cex.sub
  }
  xAxis <- (1:sum(screen))
  if (is.null(key)) {
    if (is.null(attr(x, "key"))) {
      key = xAxis
    }
    else {
      key <- attr(x, "key")[screen]
    }
  }
  else {
    key <- key[screen]
    if (length(key) != ncol(obs)) {
      stop("Key length does not match the number of variates.")
    }
  }
  br <- lattice::trellis.par.get("box.rectangle")
  br$col <- 1
  trellis.par.set("box.rectangle", br)
  bu <- lattice::trellis.par.get("box.umbrella")
  bu$col <- 1
  lattice::trellis.par.set("box.umbrella", bu)
  plot.symbol <- lattice::trellis.par.get("plot.symbol")
  plot.symbol$col <- "black"
  plot.symbol$pch <- 4
  plot.symbol$cex <- cexpar
  lattice::trellis.par.set("plot.symbol", plot.symbol)
  lattice::trellis.par.set("axis.text", list(cex = cexaxispar))
  lattice::trellis.par.set("par.xlab.text", list(cex = cexlabpar))
  lattice::trellis.par.set("par.ylab.text", list(cex = cexlabpar))
  lattice::trellis.par.set("par.main.text", list(cex = cexmainpar))
  lattice::trellis.par.set("fontsize", list(text = fontsize))
  panelFunction <- function(..., x = x, y = y, box.ratio) {
    ind.lower <- max(round(itns * perc/2), 1)
    ind.upper <- round(itns * (1 - perc/2))
    yperc.lower <- sapply(1:ncol(sims), function(i) sort(sims[, 
                                                              i])[ind.lower])
    yperc.upper <- sapply(1:ncol(sims), function(i) sort(sims[, 
                                                              i])[ind.upper])
    if (violin) {
      lattice::panel.violin(x, y, box.ratio = box.ratio, col = "transparent", 
                   ...)
    }
    lattice::panel.bwplot(x, y, box.ratio = 0.1, fill = ag_base(), ...)
    lattice::panel.xyplot(xAxis, yperc.lower, lty = 3, col = ag_base(), 
                 lwd = 3, type = "l", ...)
    lattice::panel.xyplot(xAxis, yperc.upper, lty = 3, col = ag_base(), 
                 lwd = 3, type = "l", ...)
    for (i in 1:nrow(obs)) {
      lattice::panel.xyplot(xAxis, obs[i, ], col = ag_highlight(), type = "l", 
                   lwd = 1, ...)
      lattice::panel.xyplot(xAxis, obs[i, ], col = ag_highlight(), type = "p", 
                   lwd = 3, pch = 19, ...)
      lattice::panel.text(xAxis, obs[i, ], labels = obsLabels[i, 
      ], pos = position)
    }
  }
  lattice::bwplot(as.numeric(sims) ~ rep(xAxis, each = itns), horizontal = FALSE, 
         panel = panelFunction, xlab = xlabel, ylab = ylabel, 
         ylim = c(ymin, ymax), scales = list(x = list(labels = key), 
                                             y = list(draw = FALSE)), main = main)
}