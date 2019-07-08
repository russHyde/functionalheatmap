#' `boolean_colour` return a vector that is formatted for colouring a boolean
#' variable within a ComplexHeatmap row or column annotation
#'
#' @param        true_colour   a colour (string, rgb etc) to indicate the TRUE
#' value. Required
#' @param        false_colour   a colour (string, rgb etc) to indicate the FALSE
#' value. Default: grey
#'
#' @export
#'
boolean_colour <- function(true_colour,
                           false_colour = "grey") {
  c("FALSE" = false_colour, "TRUE" = true_colour)
}

#' define_annotation_colours: make a named list of colour palettes (discrete
#' valued colour palettes or functions that return interpolated colours)
#'
#' There should be no overlap between the names defined in `boolean_var_names`
#' and the `names` of the list `custom_colour_list` since any given variable
#' can have it's colour-mapping set at most one time.
#'
#' @param        boolean_var_names   a character vector. each of the entries
#' corresponds to a boolean variable that will be presented in the heatmap.
#' Boolean state will be indicated using true_colour and false_colour. If you
#' want more control for specific boolean vars, use `boolean_colour` and
#' `custom_colour_list`
#'
#' @param        custom_colour_list   a named list. Each entry corresponds to
#' an annotation variable for a heatmaps rows or columns. The names should
#' match an annotation variable. The values may be functions or named vectors
#' mapping observable values for the annotation variable to a colour.
#'
#' @param        true_colour,false_colour   default colours for any TRUE/FALSE
#' variables that are present in `boolean_var_names`
#'
#' @export
#'
define_annotation_colours <- function(boolean_var_names,
                                      custom_colour_list = list(),
                                      true_colour = "black",
                                      false_colour = "grey") {
  .names_are_unique <- function(my_list){
    ! any(duplicated(names(my_list)))
  }

  result_list <- if (missing(boolean_var_names)) {
    custom_colour_list
  } else {
    boolean_colours <- Map(
      function(x) {
        boolean_colour(true_colour, false_colour)
      },
      boolean_var_names
    )

    append(custom_colour_list, boolean_colours)
  }

  stopifnot(.names_are_unique(result_list))
  result_list
}
