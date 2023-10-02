#' @title wearabledataset_logo
#' @description Get the detailed information of wearabledataset package.
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @importFrom stringr str_replace str_split str_replace_all str_trim
#' @importFrom grid gpar
#' @importFrom dplyr filter mutate select everything left_join bind_rows arrange
#' @importFrom dplyr desc distinct bind_cols bind_rows pull
#' @importFrom plyr dlply .
#' @importFrom rstudioapi isAvailable hasFun getThemeInfo
#' @importFrom utils packageDescription write.csv
#' @importFrom cli rule symbol
#' @importFrom crayon green blue col_align red black white style make_style num_colors
#' @importFrom purrr map map2
#' @importFrom methods slot slot<-
#' @import ggplot2
#' @importFrom methods .hasSlot new is
#' @importFrom stats p.adjust rgamma sd median time setNames
#' @importFrom utils data str head tail packageVersion write.table
#' @importFrom magrittr %>%
#' @importFrom masstools read_mgf read_mzxml ms2_plot
#' @importFrom rlang warn quo_is_null abort seq2 syms
#' @importFrom tibble add_column as_tibble
#' @importFrom ggraph ggraph
#' @importFrom massdataset check_column_name extract_expression_data extract_sample_info
#' @importFrom massdataset extract_variable_info
#' @import dtplyr
#' @import data.table
#' @export
#' @return logo
#' @examples
#' wearabledataset_logo()

wearabledataset_logo <-
  function() {
    message(crayon::green("Thank you for using wearabledataset!"))
    message(crayon::green("Version ",
                          wearabledataset_version,
                          " (",
                          update_date,
                          ')'))
    message(crayon::green(
      "More information: search 'tidywearable wearabledataset'."
    ))
    cat(crayon::green(
      c("                               _     _      _____        _                 _",
        "                              | |   | |    |  __ \\      | |               | |",
        " __      _____  __ _ _ __ __ _| |__ | | ___| |  | | __ _| |_ __ _ ___  ___| |_",
        " \\ \\ /\\ / / _ \\/ _` | '__/ _` | '_ \\| |/ _ \\ |  | |/ _` | __/ _` / __|/ _ \\ __|",
        "  \\ V  V /  __/ (_| | | | (_| | |_) | |  __/ |__| | (_| | || (_| \\__ \\  __/ |_",
        "   \\_/\\_/ \\___|\\__,_|_|  \\__,_|_.__/|_|\\___|_____/ \\__,_|\\__\\__,_|___/\\___|\\__|",
        "")

    ), sep = "\n")
  }

wearabledataset_version <-
  as.character(utils::packageVersion(pkg = "wearabledataset"))

update_date <- as.character(Sys.time())

#' @title get_wearabledataset_version
#' @description Get wearabledataset package version
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @export
#' @return version
#' @examples
#' get_wearabledataset_version()
get_wearabledataset_version = function() {
  return(as.character(utils::packageVersion(pkg = "wearabledataset")))
}

# library(cowsay)
# # https://onlineasciitools.com/convert-text-to-ascii-art
# # writeLines(capture.output(say("Hello"), type = "message"), con = "ascii_art.txt")
# art <- readLines("logo.txt")
# dput(art)
# wearabledataset_logo <-
#   c("                               _     _      _____        _                 _",
#     "                              | |   | |    |  __ \\      | |               | |",
#     " __      _____  __ _ _ __ __ _| |__ | | ___| |  | | __ _| |_ __ _ ___  ___| |_",
#     " \\ \\ /\\ / / _ \\/ _` | '__/ _` | '_ \\| |/ _ \\ |  | |/ _` | __/ _` / __|/ _ \\ __|",
#     "  \\ V  V /  __/ (_| | | | (_| | |_) | |  __/ |__| | (_| | || (_| \\__ \\  __/ |_",
#     "   \\_/\\_/ \\___|\\__,_|_|  \\__,_|_.__/|_|\\___|_____/ \\__,_|\\__\\__,_|___/\\___|\\__|",
#     "")
# cat(wearabledataset_logo, sep = "\n")
