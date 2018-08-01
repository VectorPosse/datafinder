#' Visualize the variables contained in a data frame.
#'
#' The visualize_dataframe function helps visualize data frames with a reasonable
#' number of variables
#'
#' @param dataframe One data frame (as a character string).
#' @param override Defaults to False. Set to true to override imposed limitations
#' on the number of variables included in the plot
#'
#' @return A plot that plots each variable against another. This function
#' is dependent on the number of rows and variables
#'
#' @author Sean Raleigh, \email{sraleigh@westminstercollege.edu}
#' @author Jack Wilburn \email{jackwilburn@tutanota.com}
#'
#' @examples visualize_dataframe("CO2")
#'
#' @importFrom dplyr %>% group_by_ summarise_ ungroup select_
#' @importFrom tidyr spread_
#' @importFrom GGally ggpairs
#'
#' @export
#'
visualize_dataframe <- function(dataframe, override = FALSE) {
    ggpairs(dataframe)
}