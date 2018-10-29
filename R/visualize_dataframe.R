#' Visualize the variables contained in a data frame.
#'
#' The visualize_dataframe function helps visualize data frames with a reasonable
#' number of variables. This function is dependent on the number of rows and variables
#' and will warn you accordingly. The warnings will come for long and wide datasets
#' as these properties affect performance and the legibility of the plot produced.
#'
#' @param dataframe One data frame (as a character string).
#' @param override Defaults to False. Set to true to override imposed limitations
#' on the number of variables included in the plot.
#'
#' @return A matrix of plots. Each variable included is graphed against all
#' other variables and itself.
#'
#' @author Sean Raleigh, \email{sraleigh@westminstercollege.edu}
#' @author Jack Wilburn \email{jackwilburn@tutanota.com}
#'
#' @examples
#' \dontrun{
#' visualize_dataframe("CO2")
#' visualize_dataframe(mtcars, override = TRUE)
#' }
#'
#'
#'
#' @importFrom dplyr %>% group_by_ summarise_ ungroup select_
#' @importFrom tidyr spread_
#' @importFrom GGally ggpairs
#'
#' @export
#'
visualize_dataframe <- function(dataframe, override = FALSE) {

    # Pass package in as string or not
    if (class(dataframe)[1] == "character") {
        dataframe <- eval(parse(text = dataframe))
    }

    # If the data frame is long
    if (nrow(dataframe) > 10000){
        # Notify about performance issues inherited from ggplot
        warning("This data set has a lot of rows,
                you may experience some perfomance issues.")
    }

    # If manual overide is set to true
    if (override) {
        # Plot everything
        ggpairs(dataframe, progress = FALSE)
    } else
        # if the data frame is wide
        if (ncol(dataframe) > 10) {
            # Notify about legibility
            warning("This data set has a lot of columns
                    we will only take the first 10 so that
                    the plot is still legible.")
            # Plot the first 10 variables
            ggpairs(dataframe[, 1:10], progress = FALSE)
        } else
        # Plot everything
        ggpairs(dataframe, progress = FALSE)
}