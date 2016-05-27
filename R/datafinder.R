#' Catalog the variables in available data sets
#'
#' The datafinder function will search a package for data frames and will
#' return useful information about the variables and their types.
#' Hopefully there will be more information available in the future.
#' (For example: Is there missing data? How many factors are present?
#' Are numerical variables normally distributed?) This will be especially
#' useful for statistics educators who are always on the lookout for
#' example data sets that have the features needed for the topic at hand.
#'
#' @param pkg The name of a package (as a character string in quotation marks).
#' @param summary If summary is TRUE (the default), each row of output will be
#'     a single dataframe listing the number of each type of variable present.
#'     If summary is FALSE, each row of output show the name of a variable
#'     along with its type.
#'
#' @return A data frame.
#'
#' @author Sean Raleigh, \email{sraleigh@westminstercollege.edu}
#'
#' @examples
#' datafinder("datasets")
#'
#' @importFrom utils data
#' @importFrom stringr word
#' @importFrom dplyr n data_frame tbl_df group_by ungroup select summarise
#'     mutate transmute  %>%
#' @importFrom tidyr spread
#'
#' @export
datafinder <- function(pkg, summary = TRUE) {
    # Check if package is already loaded. If not, we need
    # to attach its namespace and then remember to unload it later.

    # turn_off keeps track of packages that were already loaded
    turn_off = FALSE
    if (!requireNamespace(pkg, quietly = TRUE)) {
        stop("Package is not installed.",
             call. = FALSE)
    } else {
        pkg_fullname <- paste("package", pkg,  sep = ":")
        if (!(pkg_fullname %in% search())) {
            attachNamespace(pkg)
            turn_off = TRUE
        }
    }

    # The names of the data sets occupy the third column of $results
    datalist <- data(package = pkg)$results[, 3]
    if (length(datalist) == 0) {
        stop("Package has no data frames.",
             call. = FALSE)
    }


    summary_vars <- function(i) {
        # The name of a data set in the output of data() often
        # includes parenthesis. For example, "beaver1 (beavers)".
        # Therefore, we need to extact only the first word (the
        # actual name of the data set).
        dataframe_name <- word(datalist[i])
        dataframe <- get(dataframe_name)

        # Check to make sure we have a data frame
        # (This function won't work for matrices, for example)
        if ("data.frame" %in% class(dataframe)) {
            partial_output <- data_frame(
                Package = pkg,
                Data_set = dataframe_name,
                Variables = names(dataframe),
                # Variables can have more than one class.
                # (Seems to happen only with POSIX* classes.)
                # So this grabs the first class listed, and then makes
                # a vector out of it.
                Class = unname(sapply(
                    lapply(dataframe, class), `[[`, 1
                ))
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

    # If we attached pkg, we need to remove it to leave no trace.
    if (turn_off == TRUE) {
        unloadNamespace(pkg)
    }

    if (summary == FALSE) {
        return(output)
    }
    else if (summary == TRUE) {
        output <- output %>%
            group_by(Package, Data_set, Class) %>%
            summarise(Count = n()) %>%
            spread(Class, Count, fill = 0) %>%
            ungroup()
        # Extract only the count columns in order to sum them.
        # (Maybe there's a way to include this in the pipeline.)
        total_vars <- output %>%
            select(-Package,-Data_set) %>%
            transmute(total_vars = rowSums(.))
        # Attach total_vars to the original output
        output <- tbl_df(cbind(output, total_vars))

        return(output)
    }

}
