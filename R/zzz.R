.onAttach <- function(libname, pkgname) {
    # Gives message on library()
    packageStartupMessage(paste(
        "Thanks for using datafinder to",
        "find data for your next project!"))
}