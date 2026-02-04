.onAttach <- function(libname, pkgname) {
  desc <- utils::packageDescription(pkgname)
  packageStartupMessage(
    paste0("Welcome to EggImpute ", desc$Version, "\n"),
    "For documentation: help(package = 'EggImpute')"
  )
}