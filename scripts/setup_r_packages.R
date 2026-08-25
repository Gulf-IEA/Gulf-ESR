options(repos = c(CRAN = "https://cloud.r-project.org"))

packages <- c("dplyr", "tidyr", "lubridate", "httr")
missing_packages <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  user_library <- Sys.getenv("R_LIBS_USER")
  dir.create(user_library, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(user_library, .libPaths()))
  install.packages(missing_packages, lib = user_library)
}

unavailable_packages <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(unavailable_packages) > 0) {
  stop(
    "Package installation failed: ",
    paste(unavailable_packages, collapse = ", ")
  )
}
