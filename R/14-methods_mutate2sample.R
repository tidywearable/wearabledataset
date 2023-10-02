#' Mutate Sample-based Attributes to a `wearable_dataset` Object
#'
#' This generic function mutates or adds specific attributes to samples in the provided `wearable_dataset` object.
#'
#' @param object An object to be mutated. The specific class-based method to use will depend on the class of this object.
#' @param what A character vector specifying which attributes to compute or mutate. Possible values include
#'   "mean_intensity", "median_intensity", "sum_intensity", "na_number", "na_prop", "date", "week", and "time".
#' @param according_to_variables Specifies which variables to consider when performing the mutation.
#'   Defaults to "all".
#' @param ... Further arguments to be passed to specific methods.
#'
#' @return An object with mutated or added sample-based attributes.
#'
#' @author Xiaotao Shen \email{shenxt1990@outlook.com}
#'
#' @export
mutate2sample <-
  function(object,
           what = c(
             "mean_intensity",
             "median_intensity",
             "sum_intensity",
             "na_number",
             "na_prop",
             "date",
             "week",
             "time"
           ),
           according_to_variables = "all",
           ...) {
    UseMethod("mutate2sample")
  }

#' @method mutate2sample wearable_dataset
#' @rdname mutate2sample
#' @param object A `wearable_dataset` object to be mutated.
#' @param what A character vector specifying which attributes to compute or mutate. Possible values include
#'   "sum_intensity", "mean_intensity", "median_intensity", "na_number", "na_prop", "date", "week", and "time".
#' @param according_to_variables Specifies which variables to consider when performing the mutation.
#'   Defaults to "all".
#' @param ... Additional parameters to be passed on.
#' @importFrom tibble column_to_rownames
#' @importFrom massdataset check_column_name
#' @export

mutate2sample.wearable_dataset <-
  function(object,
           what = c(
             "sum_intensity",
             "mean_intensity",
             "median_intensity",
             "na_number",
             "na_prop",
             "date",
             "week",
             "time"
           ),
           according_to_variables = "all",
           ...) {
    what <-
      match.arg(what)

    variable_info <-
      extract_variable_info(object)

    sample_info <-
      extract_sample_info(object)

    variable_id <- get_variable_id(object)
    sample_id <- get_sample_id(object)

    if (any(according_to_variables == "all")) {
      according_to_variables <- variable_id
    } else{
      according_to_variables <-
        variable_id[variable_id %in% according_to_variables]
    }

    if (length(according_to_variables) == 0) {
      stop(
        "All the variables you provide in according_to_variables are not in the object. Please check."
      )
    }

    expression_data <-
      extract_expression_data(object)

    if (grepl("na", what)) {
      if (what == "na_number") {
        new_info <-
          expression_data[according_to_variables, , drop = FALSE] %>%
          apply(2, function(x) {
            sum(is.na(x))
          })
      }

      if (what == "na_prop") {
        new_info <-
          expression_data[according_to_variables, , drop = FALSE] %>%
          apply(2, function(x) {
            sum(is.na(x)) / length(x)
          })
      }
    }

    if (what == "sum_intensity" |
        what == "mean_intensity" | what == "median_intensity") {
      new_info <-
        expression_data[according_to_variables, , drop = FALSE] %>%
        apply(2, function(x) {
          calculate(x, what = what)
        })
    }

    if (what == "date") {
      new_info <-
        lubridate::date(sample_info$accurate_time)
    }

    if (what == "week") {
      new_info <-
        format(sample_info$accurate_time, "%a")
    }

    if (what == "time") {
      new_info <-
        strftime(
          sample_info$accurate_time,
          format = "%H:%M:%S",
          tz = lubridate::tz(sample_info$accurate_time[1])
        ) %>%
        hms::as_hms() %>%
        as.POSIXct()
    }

    new_column_name <-
      massdataset::check_column_name(slot(object, name = "sample_info") ,
                                     column.name = what)

    sample_info <-
      cbind(sample_info,
            new_info)

    colnames(sample_info)[ncol(sample_info)] <-
      new_column_name

    slot(object, "sample_info") <-
      sample_info

    object <-
      massdataset::update_sample_info(object = object)

    process_info <-
      slot(object, name = "process_info")

    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "wearabledataset",
      function_name = "mutate2sample()",
      parameter = list("what" = what,
                       "according_to_variables" = according_to_variables),
      time = Sys.time()
    )

    if (all(names(process_info) != "mutate2sample")) {
      process_info$mutate2sample <- parameter
    } else{
      process_info$mutate2sample <-
        c(process_info$mutate2sample,
          parameter)
    }
    slot(object, "process_info") <- process_info
    return(object)
  }
