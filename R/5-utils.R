
####This code is from MicrobiotaProcess, credit should go its developer

####This code is from MicrobiotaProcess, credit should go its developer

# ####This code is from MicrobiotaProcess, credit should go its developer
# build_refseq <- function(x) {
#   flag <- guess_rownames(x)
#   refseq <- switch(
#     flag,
#     DNA = Biostrings::DNAStringSet(x),
#     AA = Biostrings::AAStringSet(x),
#     RNA = Biostrings::RNAStringSet(x),
#     Other = NULL
#   )
#   return(refseq)
# }

####This code is from MicrobiotaProcess, credit should go its developer
grepl_data.frame <- function(pattern, x, ...) {
  y <- if (length(x)) {
    do.call("cbind", lapply(x, "grepl", pattern = pattern, ...))
  } else{
    matrix(FALSE, length(row.names(x)), 0)
  }
  if (.row_names_info(x) > 0L)
    rownames(y) <- row.names(x)
  y
}


msg <-
  function(..., startup = FALSE) {
    if (startup) {
      if (!isTRUE(getOption("wearabledataset.quiet"))) {
        packageStartupMessage(text_col(...))
      }
    } else {
      message(text_col(...))
    }
  }

text_col <- function(x) {
  # If RStudio not available, messages already printed in black
  if (!rstudioapi::isAvailable()) {
    return(x)
  }

  if (!rstudioapi::hasFun("getThemeInfo")) {
    return(x)
  }

  theme <- rstudioapi::getThemeInfo()

  if (isTRUE(theme$dark))
    crayon::white(x)
  else
    crayon::black(x)

}

#' List all packages in the wearabledataset
#'
#' @param include_self Include wearabledataset in the list?
#' @export
#' @return wearabledataset packages
#' @examples
#' wearabledataset_packages()
wearabledataset_packages <-
  function(include_self = TRUE) {
    raw <- utils::packageDescription("wearabledataset")$Imports
    imports <- strsplit(raw, ",")[[1]]
    parsed <- gsub("^\\s+|\\s+$", "", imports)
    names <-
      vapply(strsplit(parsed, "\\s+"), "[[", 1, FUN.VALUE = character(1))

    if (include_self) {
      names <- c(names, "wearabledataset")
    }

    names
  }

invert <- function(x) {
  if (length(x) == 0)
    return()
  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}


style_grey <- function(level, ...) {
  crayon::style(paste0(...),
                crayon::make_style(grDevices::grey(level), grey = TRUE))
}


calculate <-
  function(value,
           what = c("mean_intensity",
                    "median_intensity",
                    "sum_intensity",
                    "na_number",
                    "na_freq"),
           ...) {
    switch(
      EXPR = what,
      mean_intensity = mean(value, ...),
      median_intensity = median(value, ...),
      sum_intensity = sum(value, ...)
    )
  }