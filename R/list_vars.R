#' List all the variables in a data frame along with their classes
#'
#' The list_vars function lists all the variables in a data frame along
#' with their classes.
#'
#' @param dataframe The name of a data frame (as a character string in quotes).
#'
#' @return A data frame. Each row of output represents a variable from the
#' original data frame passed to the dataframe argument. If the input
#' dataframe is not actually a data frame, then an empty data frame
#' will be returned.
#'
#' @author Sean Raleigh, \email{sraleigh@westminstercollege.edu}
#' @author Jack Wilburn \email{jackwilburn@tutanota.com}
#'
#' @examples list_vars("CO2")
#'
#' @export
list_vars <- function(dataframe) {
    dataframe_name <- dataframe
    tryCatch(dataframe <- eval(
        parse(text = dataframe_name)),
        error = function(e){
            data(list = dataframe_name)
        print("These dataframes needed to be loaded with data.")
        print("We have removed them from the global environment.")
        print(paste(
            "If you need this dataframe",
            "use data() with the dataframe inside."))
            })

    # Trycatch to see if we can coerce to data frame
    tryCatch(dataframe <- data.frame(dataframe), error = function(e){})

    # Check to make sure we have a data frame or something coerced
    if ("data.frame" %in% class(dataframe)) {
        output <- data.frame(
            Dataframe = dataframe_name,
            Variable = names(dataframe),
            # Variables can have more than one class.
            # (Seems to happen only with POSIX* classes.)
            # So this grabs the first class listed, and then makes
            # a vector out of it.
            Class = unname(vapply(
                lapply(dataframe, class), `[[`, 1,
                FUN.VALUE = ""
            ))
        )
    } else {
        # If the data set is not a data frame, return an empty data frame.
        output <- data.frame()
    }
    return(output)
}
