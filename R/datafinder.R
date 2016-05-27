#' Catalog the variables in available data sets
#'
#' The datafinder function will search a package for data frames and will
#' return useful information about the variables and their types.
#' Hopefully there will be more information available in the future.
#' (For example: Is there missing data? How many factors are present?
#' Are numerical variables normally distributed?)
#'
#' @param package The name of a package (as a character string in quotation marks).
#' @param summary If summary is TRUE, each row of output will be a single dataframe,
#'     along with the number of each type of variable present. If summary is
#'     FALSE, each row of output will be a variable along with its type.
#'
#' @return A data frame. If summary is TRUE, each row of output will be a
#'     single dataframe, along with the number of each type of variable
#'     present. If summary is FALSE, each row of output will be a variable
#'     along with its type.
#'
#' @author Sean Raleigh, \email{sraleigh@westminstercollege.edu}
#'
#' @examples
#' datafinder("datasets")
#'
#' @importFrom utils data
#'
#' @export
datafinder <- function(package, summary = TRUE) {
    # The names of the data sets occupy the third column of $results
    datalist <- data(package = package)$results[, 3]

    summary_vars <- function(i) {
        dataframe_name <- datalist[i]
        dataframe <- get(datalist[i])

        # Check to make sure we have a data frame
        # (This function won't work for matrices, for example)
        if (class(dataframe) == "data.frame") {
            partial_output <- data.frame(
                Package = package,
                Data_Frame = dataframe_name,
                Variables = names(dataframe),
                # Variables can have more than one class.
                # (Seems to happen only with POSIX* classes.)
                # So this grabs the first class listed, and then makes
                # a vector out of it.
                Class = unname(sapply(
                    lapply(dataframe, class), `[[`, 1
                )),
                stringsAsFactors = FALSE
            )
        }
        else {
            # If the data set is not a data frame, nothing gets recorded.
            partial_output <- data.frame()
        }
        return(partial_output)
    }

    # Gather up all the info from each data set into one big data frame.
    output <-
        do.call("rbind", lapply(1:length(datalist),  summary_vars))

    if (summary == FALSE) {
        return(output)
    }
    else if (summary == TRUE) {
        return(output)
    }
}
