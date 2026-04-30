#' autograph: Automatic Plotting of Many Graphs
#' @keywords internal
#' @details
#'   For more details, please consult the README and work through the tutorial.
#'   The tutorial is available in the package documentation and on the package website.
#' @author James Hollway
"_PACKAGE"

# Global variables ####
# defining global variables more centrally
utils::globalVariables(c(".data", "obs", "valter","select","ego","zego","alter",
                         "from","to","weight","unit","Step","Freq","Var1","n",
                         "density","wave","period","name","value","sim","time",
                         "q05","q95","color","group","label","x","y",
                         "xend","yend","id",
                         "event","nodes","node",
                         "ecolor_var","esize_var","ncolor",
                         "status","frame","framen",
                         "S","E","I_new","R"))
