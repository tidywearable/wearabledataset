# library(rjson)
# data <- fromJSON("garmin_hr.json")
#
# file_name <- "garmin_hr.json"
# read_smartwatch_data <-
#   function(file_name,
#            path = ".",
#            device = c("garmin", "apple", "fitbit")) {
#     device <-
#       match.arg(device)
#     if (device == "garmin") {
#       data <-
#         rjson::fromJSON(file = file.path(path, file_name), simplify = TRUE)
#
#     }
#   }
#
# read_smartwatch_data_apple <-
#   function(file_name,
#            path = ".") {
#     data <-
#       rjson::fromJSON(file = file.path(path, file_name), simplify = TRUE)
#   }


