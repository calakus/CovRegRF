.onAttach <- function(libname, pkgname) {
  corerf.version <- read.dcf(file=system.file("DESCRIPTION", package=pkgname),
                             fields="Version")
  packageStartupMessage(paste("\n",
                              pkgname,
                              corerf.version))
}
