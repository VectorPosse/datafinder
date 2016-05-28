#' Explore all the datasets available in an installed package
#'
#' The explore_package function finds all the data frames in an installed
#'  package and summarizes the number and classes of variables in each.
#'
#' @param pkg The name of a package (as a character string in quotation marks).
#'
#' @return A data frame. Each row gives the counts (by class) of all the
#' variables in each data frame in the package. If there are no data frames in
#' the package, an empty data frame will be returned.
#'
#' @author Sean Raleigh, \email{sraleigh@westminstercollege.edu}
#'
#' @examples explore_package("datasets")
#'
#' @importFrom utils data
#'
#' @export
explore_package <- function(pkg) {
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

    # The name of a data set in the output of data() sometimes
    # includes parenthesis. For example, "beaver1 (beavers)".
    # Therefore, we need to extact only the first word (the
    # actual name of the data set).
    dataframe_names <- vapply(datalist,
                              function(x)
                                  strsplit(x, split = " ")[[1]][1],
                              FUN.VALUE = "",
                              USE.NAMES = FALSE)

    # Gather up all the info from each data set into one big data frame.
    list_vars_pkg <-
        do.call("rbind", lapply(dataframe_names, list_vars))

    # Careful, though. Some data sets are not exported by packages
    # (for example, those that don't use namespaces
    # or those where LazyData is FALSE).
    # [NEED TO FIX THIS!]

    output <- get_counts(list_vars_pkg)

    # If we loaded a namsepace, we need to remove it to leave no trace.
    if (unload_later == TRUE) {
        unloadNamespace(pkg)
    }

    return(output)
}
