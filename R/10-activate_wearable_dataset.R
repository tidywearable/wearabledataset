#' Activate Specific Dataset in wearabledataset Package
#'
#' This function sets the "activated" slot of a given `wearable_dataset` object
#' from the wearabledataset package to the specified dataset type.
#'
#' @param .data An object of class `wearable_dataset`.
#' @param what A character vector specifying which dataset to activate.
#'   Can be one of "sample_info", "variable_info", or "expression_data".
#'
#' @return A modified version of the `wearable_dataset` object with the "activated" slot set.
#'
#' @examples
#' \dontrun{
#' dataset <- your_dataset_function() # replace with how you get a wearable_dataset object
#' activate_wearable_dataset(dataset, what = "sample_info")
#' }
#'
#' @author Xiaotao Shen \email{shenxt1990@outlook.com}
#'
#' @export

activate_wearable_dataset <-
  function(.data,
           what = c("sample_info",
                    "variable_info",
                    "expression_data")) {
    what <- match.arg(what)
    slot(object = .data, name = "activated") <- what
    return(.data)
  }
