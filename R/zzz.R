.onAttach <- function(libname, pkgname) {
    # Gives message on library()
    packageStartupMessage("Thanks for using datafinder to find your data for your next project!")
}