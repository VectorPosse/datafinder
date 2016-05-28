#' Catalog the variables from data sets
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
#'     If summary is FALSE, each row of output shows the name of each variable
#'     along with its type.
#'
#' @return A data frame.
#'
#' @author Sean Raleigh, \email{sraleigh@westminstercollege.edu}
#'
#' @examples
#'    datafinder("datasets")
#'    datafinder("datasets", summary = FALSE)
#'
#' @importFrom utils data
#' @importFrom dplyr data_frame tbl_df group_by_ ungroup select_ summarise_ %>%
#' @importFrom tidyr spread_
#'
#' @export
datafinder <- function(pkg, summary = TRUE) {

    # Check if package already has a loaded namespace.
    if (pkg %in% loadedNamespaces()) {
        # If so, no worries.
        unload_later = FALSE
    # If not, require it.
    } else if (!requireNamespace(pkg, quietly = TRUE)) {
        # If the package isn't even installed, stop with an error.
        stop("Package is not installed.",
             call. = FALSE)
    } else {
        # We will manually unload the namespace later.
        unload_later = TRUE
    }

    # The names of the data sets occupy the third column of data()$results
    datalist <- data(package = pkg)$results[, 3]
    if (length(datalist) == 0) {
        stop("Package has no data frames.",
             call. = FALSE)
    }

    # Summarize all the variables in a given dataset.
    summary_vars <- function(i) {
        # The name of a data set in the output of data() often
        # includes parenthesis. For example, "beaver1 (beavers)".
        # Therefore, we need to extact only the first word (the
        # actual name of the data set).
        dataframe_name <- strsplit(datalist[i], split = " ")[[1]][1]
        # Get the data set.
        dataframe <- getExportedValue(pkg, dataframe_name)
        # Careful, though. Some data sets are not exported by packages
        # (for example, those that don't use namespaces)

        # [NEED CODE HERE TO HANDLE THAT.]

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
                Class = unname(vapply(
                    lapply(dataframe, class), `[[`, 1,
                    FUN.VALUE = ""
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

    # If we loaded a namsepace, we need to remove it to leave no trace.
    if (unload_later == TRUE) {
        unloadNamespace(pkg)
    }

    if (summary == FALSE) {
        return(output)
    }
    else if (summary == TRUE) {
        output <- output %>%
            group_by_("Package", "Data_set", "Class") %>%
            summarise_(Count = "n()") %>%
            spread_("Class", "Count", fill = 0) %>%
            ungroup()
        # Extract only the class count columns in order to sum them.
        var_classes <- output %>%
            select_("-Package","-Data_set")
        total_vars =  rowSums(var_classes)
        # Attach total_vars to the original output
        output <- tbl_df(cbind(output, total_vars))

        return(output)
    }

}
