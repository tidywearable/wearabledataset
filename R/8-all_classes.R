#' @title wearable_dataset class
#' @docType class
#' @slot ... Other slots from mass_dataset class object in massdataset pacakge
#' @importClassesFrom massdataset mass_dataset
#' @exportClass wearable_dataset
setClass(
  "wearable_dataset",
  contains = "mass_dataset",
  validity = check_wearable_dataset_class
)


#' @title create_wearable_dataset
#' @description Create the wearable_dataset object.
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param expression_data expression_data.
#' @param sample_info Sample information name.
#' @param variable_info Variable information.
#' Columns are samples and rows are variables.
#' @param sample_info_note Sample information name.
#' \url{https://tidywearable.github.io/wearabledataset/articles/data_import_and_export.html}
#' @param variable_info_note Sample information name.
#' \url{https://tidywearable.github.io/wearabledataset/articles/data_import_and_export.html}
#' @return A wearable_dataset-class object.
#' @importClassesFrom massdataset tidymass_parameter
#' @export

create_wearable_dataset <-
  function(expression_data,
           sample_info,
           variable_info,
           sample_info_note,
           variable_info_note) {
    if (!missing(expression_data)) {
      expression_data <-
        as.data.frame(expression_data)
    }

    if (!missing(sample_info)) {
      sample_info <-
        as.data.frame(sample_info)
    }

    if (!missing(variable_info)) {
      variable_info <-
        as.data.frame(variable_info)
    }

    if (!missing(sample_info_note)) {
      sample_info_note <-
        as.data.frame(sample_info_note)
    } else{
      sample_info_note <-
        data.frame(
          name = colnames(sample_info),
          meaning = colnames(sample_info),
          check.names = FALSE
        )
    }

    if (!missing(variable_info_note)) {
      variable_info_note <-
        as.data.frame(variable_info_note)
    } else{
      variable_info_note <-
        data.frame(
          name = colnames(variable_info),
          meaning = colnames(variable_info),
          check.names = FALSE
        )
    }

    if(all(colnames(sample_info) != "class")){
      sample_info <-
        sample_info %>%
        dplyr::mutate(class = "Subject")
    }

    check_result <-
      check_wearable_dataset(
        expression_data = expression_data,
        sample_info = sample_info,
        variable_info = variable_info,
        sample_info_note = sample_info_note,
        variable_info_note = variable_info_note
      )

    if (stringr::str_detect(check_result, "error")) {
      stop(check_result)
    }

    process_info <- list()

    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "wearabledataset",
      function_name = "create_wearable_dataset()",
      parameter = list("no" = "no"),
      time = Sys.time()
    )

    process_info$create_wearable_dataset = parameter

    expression_data <- as.data.frame(expression_data)
    sample_info <- as.data.frame(sample_info)
    variable_info <- as.data.frame(variable_info)
    sample_info_note <- as.data.frame(sample_info_note)
    variable_info_note <- as.data.frame(variable_info_note)

    object <- new(
      Class = "wearable_dataset",
      expression_data = expression_data,
      sample_info = sample_info,
      variable_info = variable_info,
      sample_info_note = sample_info_note,
      variable_info_note = variable_info_note,
      process_info = process_info,
      version = "0.99.1"
    )
    invisible(object)
  }
