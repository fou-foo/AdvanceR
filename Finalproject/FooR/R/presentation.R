
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the first foo´s package, jose.ramirez@cimat.mx")
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "Jose Antonio Garcia Ramirez",
    devtools.desc.author = '"jose.ramirez@cimat.mx"',
    devtools.desc.license = "GNU",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])
  invisible()
}
