#' Themed scales for further customization
#' 
#' @description These functions enable to add color scales to be graphs.
#' @name theme_scales
#' @param direction Direction for using palette colors.
#' @param ... Extra arguments passed to `ggplot2::discrete_scale()`.
#' @examples
#' #ison_brandes %>%
#' #mutate(core = migraph::node_is_core(ison_brandes)) %>%
#' #graphr(node_color = "core") +
#' #scale_color_iheid()
#' #graphr(ison_physicians[[1]], edge_color = "type") +
#' #scale_edge_color_ethz()
NULL

#' @rdname theme_scales
#' @export
scale_fill_iheid <- function(direction = 1, ...) {
  ggplot2::discrete_scale("fill",
                          palette = palette_gen(palette = "IHEID", direction),
                          na.value = "black", name = "", ...)
}

#' @rdname theme_scales
#' @export
scale_colour_iheid <- function(direction = 1, ...) {
  ggplot2::discrete_scale("colour",
                          palette = palette_gen(palette = "IHEID", direction),
                          na.value = "black", name = "", ...)
}

#' @rdname theme_scales
#' @export
scale_color_iheid <- scale_colour_iheid

#' @rdname theme_scales
#' @export
scale_edge_colour_iheid <- function(direction = 1, ...) {
  ggplot2::discrete_scale("edge_colour",
                          palette = palette_gen(palette = "IHEID", direction),
                          na.value = "black", name = "", ...)
}

#' @rdname theme_scales
#' @export
scale_edge_color_iheid <- scale_edge_colour_iheid

#' Centres color scales
#' 
#' @rdname theme_scales
#' @export
scale_fill_centres <- function(direction = 1, ...) {
  ggplot2::discrete_scale("fill",
                          palette = palette_gen(palette = "Centres", direction),
                          na.value = "black", name = "", ...)
}

#' @rdname theme_scales
#' @export
scale_colour_centres <- function(direction = 1, ...) {
  ggplot2::discrete_scale("colour",
                          palette = palette_gen(palette = "Centres", direction),
                          na.value = "black", name = "", ...)
}

#' @rdname theme_scales
#' @export
scale_color_centres <- scale_colour_centres

#' @rdname theme_scales
#' @export
scale_edge_colour_centres <- function(direction = 1, ...) {
  ggplot2::discrete_scale("edge_colour",
                          palette = palette_gen(palette = "Centres", direction),
                          na.value = "black", name = "", ...)
}

#' @rdname theme_scales
#' @export
scale_edge_color_centres <- scale_edge_colour_centres

#' SDGs color scales
#' 
#' @rdname theme_scales
#' @export
scale_fill_sdgs <- function(direction = 1, ...) {
  ggplot2::discrete_scale("fill",
                          palette = palette_gen(palette = "SDGs", direction),
                          na.value = "black", name = "", ...)
}

#' @rdname theme_scales
#' @export
scale_colour_sdgs <- function(direction = 1, ...) {
  ggplot2::discrete_scale("colour",
                          palette = palette_gen(palette = "SDGs", direction),
                          na.value = "black", name = "", ...)
}

#' @rdname theme_scales
#' @export
scale_color_sdgs <- scale_colour_sdgs

#' @rdname theme_scales
#' @export
scale_edge_colour_sdgs <- function(direction = 1, ...) {
  ggplot2::discrete_scale("edge_colour",
                          palette = palette_gen(palette = "SDGs", direction),
                          na.value = "black", name = "", ...)
}

#' @rdname theme_scales
#' @export
scale_edge_color_sdgs <- scale_edge_colour_sdgs

#' ETHZ color scales
#' 
#' @rdname theme_scales
#' @export
scale_fill_ethz <- function(direction = 1, ...) {
  ggplot2::discrete_scale("fill",
                          palette = palette_gen(palette = "ETHZ", direction), 
                          na.value = "black", name = "", ...)
}

#' @rdname theme_scales
#' @export
scale_colour_ethz <- function(direction = 1, ...) {
  ggplot2::discrete_scale("colour",
                          palette = palette_gen(palette = "ETHZ", direction), 
                          na.value = "black", name = "", ...)
}

#' @rdname theme_scales
#' @export
scale_color_ethz <- scale_colour_ethz

#' @rdname theme_scales
#' @export
scale_edge_colour_ethz <- function(direction = 1, ...) {
  ggplot2::discrete_scale("edge_colour",
                          palette = palette_gen(palette = "ETHZ", direction),
                          na.value = "black", name = "", ...)
}

#' @rdname theme_scales
#' @export
scale_edge_color_ethz <- scale_edge_colour_ethz

#' UZH color scales
#' 
#' @rdname theme_scales
#' @export
scale_fill_uzh <- function(direction = 1, ...) {
  ggplot2::discrete_scale("fill",
                          palette = palette_gen(palette = "UZH", direction),
                          na.value = "black", name = "", ...)
}

#' @rdname theme_scales
#' @export
scale_colour_uzh <- function(direction = 1, ...) {
  ggplot2::discrete_scale("colour",
                          palette = palette_gen(palette = "UZH", direction),
                          na.value = "black", name = "", ...)
}

#' @rdname theme_scales
#' @export
scale_color_uzh <- scale_colour_uzh

#' @rdname theme_scales
#' @export
scale_edge_colour_uzh <- function(direction = 1, ...) {
  ggplot2::discrete_scale("edge_colour",
                          palette = palette_gen(palette = "UZH", direction),
                          na.value = "black", name = "", ...)
}

#' @rdname theme_scales
#' @export
scale_edge_color_uzh <- scale_edge_colour_uzh

#' RUG color scales
#' 
#' @rdname theme_scales
#' @export
scale_fill_rug <- function(direction = 1, ...) {
  ggplot2::discrete_scale("fill",
                          palette = palette_gen(palette = "RUG", direction),
                          na.value = "grey", name = "", ...)
}

#' @rdname theme_scales
#' @export
scale_colour_rug <- function(direction = 1, ...) {
  ggplot2::discrete_scale("colour",
                          palette = palette_gen(palette = "RUG", direction),
                          na.value = "grey", name = "", ...)
}

#' @rdname theme_scales
#' @export
scale_color_rug <- scale_colour_rug

#' @rdname theme_scales
#' @export
scale_edge_colour_rug <- function(direction = 1, ...) {
  ggplot2::discrete_scale("edge_colour",
                          palette = palette_gen(palette = "RUG", direction),
                          na.value = "black", name = "", ...)
}

#' @rdname theme_scales
#' @export
scale_edge_color_rug <- scale_edge_colour_rug

# Helper functions
corp_color <- function(...) {
  corp_colors <- c(`IHEIDRed` = "#E20020", `IHEIDBlack` = "#000010",
                   `IHEIDGrey` = "#6f7072", `AHCD` = "#622550",
                   `CFFD` = "#0094D8", `CIES` = "#268D2B",
                   `CTEI` = "#008F92", `CGEN` = "#820C2B",
                   `CCDP` = "#3E2682", `GLGC` = "#006564",
                   `GLHC` = "#A8086E", `GLMC` = "#006EAA",
                   `NoPoverty` = "#e5243b", `ZeroHunger` = "#DDA63A",
                   `GoodHealth` = "#4C9F38",
                   `QualityEducation` = "#C5192D",
                   `GenderEquality` = "#FF3A21", `CleanWater` = "#26BDE2",
                   `CleanEnergy` = "#FCC30B",
                   `EconomicGrowth` = "#A21942",
                   `Innovation` = "#FD6925",
                   `ReducedInequalities` = "#DD1367",
                   `SustainableCities` = "#FD9D24", 
                   `ResponsibleConsumption` = "#BF8B2E",
                   `ClimateAction` = "#3F7E44", `BelowWater` = "#0A97D9",
                   `OnLand` = "#56C02B", `StrongInstitutions` = "#00689D",
                   `GoalPartnerships` = "#19486A",
                   `ETHZ_Blue` = "#215CAF", `ETHZ_Petrol` = "#007894",
                   `ETHZ_Green` = "#627313", `ETHZ_Bronze` = "#8E6713",
                   `ETHZ_Red`=	"#B7352D", `ETHZ_Purple` = "#A7117A",
                   `ETHZ_Grey`	= "#6F6F6F", `UZH_Blue` = "#0028a5",
                   `UZH_Grey` = "#a3adb7", `UZH_Orange` = "#dc6027",
                   `RUG_Red` = "#dc002d", `RUG_Black` = "#000000")
  cols <- c(...)
  if (is.null(cols))
    return (corp_colors)
  corp_colors[cols]
}

corp_palette <- function(palette, ...) {
  corp_palettes <- list(`IHEID` = corp_color("IHEIDRed",
                                             "IHEIDBlack",
                                             "IHEIDGrey"),
                        `Centres` = corp_color("AHCD", "CFFD", 
                                               "CIES", "CTEI",
                                               "CGEN", "CCDP",
                                               "GLGC", "GLHC",
                                               "GLMC"),
                        `SDGs` = corp_color("NoPoverty",
                                            "ZeroHunger",
                                            "GoodHealth", 
                                            "QualityEducation",
                                            "GenderEquality", 
                                            "CleanWater",
                                            "CleanEnergy",
                                            "EconomicGrowth",
                                            "Innovation", 
                                            "ReducedInequalities",
                                            "SustainableCities",
                                            "ResponsibleConsumption",
                                            "ClimateAction", 
                                            "BelowWater", 
                                            "OnLand",
                                            "StrongInstitutions",
                                            "GoalPartnerships"),
                        `ETHZ` = corp_color("ETHZ_Blue", "ETHZ_Petrol",
                                           "ETHZ_Green", "ETHZ_Bronze",
                                           "ETHZ_Red", "ETHZ_Purple", "ETHZ_Grey"),
                        `UZH` = corp_color("UZH_Blue", "UZH_Grey", "UZH_Orange"),
                        `RUG` = corp_color("RUG_Red", "RUG_Black"))
  unlist(unname(corp_palettes[c(palette)]))
}

palette_gen <- function(palette, direction = 1) {
  function(n) {
    if (n > length(corp_palette(palette)))
      warning("Not enough colors in this palette!")
    else {
      all_colors <- corp_palette(palette)
      all_colors <- unname(unlist(all_colors))
      all_colors <- if (direction >= 0) all_colors else rev(all_colors)
      color_list <- all_colors[1:n]
    }
  }
}

