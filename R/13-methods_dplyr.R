#' @method filter wearable_dataset
#' @docType methods
#' @importFrom rlang quos !!!
#' @importFrom dplyr filter
#' @export
filter.wearable_dataset <-
  function(.data, ..., .preserve = FALSE) {
    dots <- rlang::quos(...)

    if (length(.data@activated) == 0) {
      stop("activate you object using activate_wearable_dataset first.\n")
    }

    x <-
      slot(object = .data, name = .data@activated)

    x <-
      filter(dtplyr::lazy_dt(x), !!!dots, .preserve = .preserve) %>%
      as.data.frame()

    slot(object = .data, name = .data@activated) = x

    if (.data@activated == "sample_info") {
      .data@expression_data <-
        .data@expression_data[, x$sample_id, drop = FALSE]
    }

    if (.data@activated == "variable_info") {
      # if (nrow(.data@annotation_table) > 0) {
      #   variable_info <- .data@variable_info
      #   .data@annotation_table <-
      #     .data@annotation_table %>%
      #     dplyr::filter(variable_id %in% variable_info$variable_id)
      # }
      .data@expression_data <-
        .data@expression_data[x$variable_id, , drop = FALSE]
    }

    if (.data@activated == "expression_data") {
      .data@variable_info <-
        .data@variable_info[match(rownames(x), .data@variable_info$variable_id), , drop = FALSE]
    }

    process_info <-
      .data@process_info

    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "wearabledataset",
      function_name = "filter()",
      parameter = list(parameter = rlang::expr_label(dots[[1]])),
      time = Sys.time()
    )

    if (all(names(process_info) != "filter")) {
      process_info$filter <- parameter
    } else{
      process_info$filter <- c(process_info$filter, parameter)
    }

    .data@process_info <- process_info
    return(.data)
  }

#' @importFrom dplyr filter
#' @export
dplyr::filter


#' @method select wearable_dataset
#' @docType methods
#' @importFrom rlang quos !!!
#' @importFrom dplyr select
#' @importFrom massdataset update_sample_info
#' @importFrom massdataset update_variable_info
#' @export
select.wearable_dataset <-
  function(.data, ...) {
    dots <- rlang::quos(...)

    if (length(.data@activated) == 0) {
      stop("activate you object using activate_wearable_dataset first.\n")
    }

    x <-
      slot(object = .data, name = .data@activated)

    x <-
      select(dtplyr::lazy_dt(x), !!!dots) %>%
      as.data.frame()

    slot(object = .data, name = .data@activated) <- x

    if (.data@activated == "expression_data") {
      .data@sample_info <-
        .data@sample_info[match(colnames(x), .data@sample_info$sample_id), ]
    }

    if (.data@activated == "sample_info") {
      ##we can not remove sample_id
      if (!"sample_id" %in% colnames(x)) {
        stop("You can't remove sample_id.\n")
      }
      .data@sample_info_note <- .data@sample_info_note %>%
        dplyr::filter(name %in% colnames(x))
      .data <- massdataset::update_sample_info(.data)
    }

    if (.data@activated == "variable_info") {
      ##we can not remove variable_id
      if (!"variable_id" %in% colnames(x)) {
        stop("You can't remove variable_id\n")
      }
      .data@variable_info_note <- .data@variable_info_note %>%
        dplyr::filter(name %in% colnames(x))
      .data <- massdataset::update_variable_info(.data)
    }

    return(.data)
  }

#' @importFrom dplyr select
#' @export
dplyr::select

#' @importFrom dplyr desc
#' @export
dplyr::desc



#' @method arrange wearable_dataset
#' @docType methods
#' @importFrom rlang quos !!!
#' @importFrom dplyr arrange
#' @export
arrange.wearable_dataset <-
  function(.data, ...) {
    dots <- rlang::quos(...)

    if (length(.data@activated) == 0) {
      stop("activate you object using activate_wearable_dataset() first.\n")
    }

    x <-
      slot(object = .data, name = .data@activated)

    x <-
      arrange(dtplyr::lazy_dt(x), !!!dots) %>%
      as.data.frame()

    slot(object = .data, name = .data@activated) <- x

    if (.data@activated == "sample_info") {
      .data@expression_data = .data@expression_data[, x$sample_id, drop = FALSE]
    }

    if (.data@activated == "variable_info") {
      .data@expression_data = .data@expression_data[x$variable_id,]
    }

    if (.data@activated == "expression_data") {
      .data@variable_info <-
        .data@variable_info[match(rownames(.data@expression_data),
                                  .data@variable_info$variable_id),]
    }

    return(.data)
  }

#' @importFrom dplyr arrange
#' @export
dplyr::arrange




#' @method count wearable_dataset
#' @docType methods
#' @importFrom rlang quos !!! !! enquo
#' @importFrom dplyr count
#' @export
count.wearable_dataset <-
  function(x,
           ...,
           wt = NULL,
           sort = FALSE,
           name = NULL) {
    dots <- rlang::quos(...)

    if (length(x@activated) == 0) {
      stop("activate you object using activate_wearable_dataset first.\n")
    }

    x =
      slot(object = x, name = x@activated)

    dtplyr::lazy_dt(x) %>%
      count(!!!dots,
            wt = !!enquo(wt),
            sort = sort,
            name = name) %>%
      as.data.frame()
  }



#' @method tally wearable_dataset
#' @docType methods
#' @importFrom rlang quos !!! !! enquo
#' @importFrom dplyr tally
#' @export
tally.wearable_dataset <-
  function(x,
           wt = NULL,
           sort = FALSE,
           name = NULL) {
    if (length(x@activated) == 0) {
      stop("activate you object using activate_wearable_dataset first.\n")
    }

    x =
      slot(object = x, name = x@activated)

      dtplyr::lazy_dt(x) %>%
      tally(wt = !!enquo(wt),
            sort = sort,
            name = name) %>%
      as.data.frame()
  }

#' @importFrom dplyr count
#' @export
dplyr::count

#' @importFrom dplyr count
#' @export
dplyr::count




#' @method glimpse wearable_dataset
#' @docType methods
#' @importFrom rlang quos !!!
#' @importFrom dplyr glimpse
#' @export
glimpse.wearable_dataset <-
  function(x, width, ...) {
    if (length(x@activated) == 0) {
      stop("activate you object using activate_wearable_dataset first.\n")
    }
    x <-
      slot(object = x, name = x@activated)
    glimpse(x)
  }

#' @importFrom dplyr glimpse
#' @export
dplyr::glimpse



#' @method group_by wearable_dataset
#' @docType methods
#' @importFrom rlang quos !!!
#' @importFrom dplyr group_by
#' @export
group_by.wearable_dataset <-
  function(.data, ...) {
    dots <- rlang::quos(...)

    if (length(.data@activated) == 0) {
      stop("activate you object using activate_wearable_dataset first.\n")
    }

    x <-
      slot(object = .data, name = .data@activated)

    x <-
      group_by(x, !!!dots)

    slot(object = .data, name = .data@activated) <- x

    if (.data@activated == "sample_info") {
      .data@expression_data <-
        .data@expression_data[, x$sample_id, drop = FALSE]
    }

    if (.data@activated == "variable_info") {
      .data@expression_data <-
        .data@expression_data[x$variable_id, ]
    }

    return(.data)
  }

#' @importFrom dplyr group_by
#' @export
dplyr::group_by

#' @importFrom dplyr desc
#' @export
dplyr::desc





#' @method left_join wearable_dataset
#' @importFrom dplyr left_join
#' @export
left_join.wearable_dataset <-
  function(x,
           y,
           by = NULL,
           copy = FALSE,
           suffix = c(".x", ".y"),
           ...,
           keep = FALSE) {
    left_join_wearable_dataset(
      x = dtplyr::lazy_dt(x),
      y = dtplyr::lazy_dt(y),
      by = by,
      copy = copy,
      suffix = suffix,
      keep = keep,
      ...
    ) %>%
      as.data.frame()
  }

#' @importFrom dplyr left_join
#' @export
dplyr::left_join

#' @title Left join for wearable_dataset
#' @description Left join for wearable_dataset
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param x (required) wearable_dataset class object.
#' @param y A data frame.
#' @param by by, see ?_left_join
#' @param copy copy, see ?_left_join
#' @param suffix suffix, see ?_left_join
#' @param keep keep, see ?_left_join
#' @param ... Other arguments, see ?_left_join
#' @return a wearable_dataset class object.
#' @export
#' @importFrom dplyr left_join
#' @export
left_join_wearable_dataset <-
  function(x,
           y,
           by = NULL,
           copy = FALSE,
           suffix = c(".x", ".y"),
           ...,
           keep = FALSE) {
    if (length(x@activated) == 0) {
      stop("activate you object using activate_wearable_dataset first.\n")
    }

    if (!is(x, class2 = "wearable_dataset")) {
      stop("x must be wearable_dataset class.\n")
    }

    new_x <-
      slot(object = x, name = x@activated)

    new_x <-
      left_join(
        dtplyr::lazy_dt(new_x),
        dtplyr::lazy_dt(y),
        by = by,
        copy = copy,
        suffix = suffix,
        ...
      ) %>%
      as.data.frame()

    slot(object = x, name = x@activated) <- new_x

    ###sample_info
    if (x@activated == "sample_info") {
      if (!"sample_id" %in% colnames(new_x)) {
        stop("You can't remove sample_id.\n")
      }
    }

    ###variable_info
    if (x@activated == "variable_info") {
      if (!"variable_id" %in% colnames(new_x)) {
        stop("You can't remove variable_id\n")
      }
    }

    x <- massdataset::update_sample_info(x)
    x <- massdataset::update_variable_info(x)
    return(x)
  }



#' @method mutate wearable_dataset
#' @docType methods
#' @importFrom rlang quos !!!
#' @importFrom dplyr mutate
#' @export
mutate.wearable_dataset <- function(.data, ...) {
  dots <- rlang::quos(...)
  if (length(.data@activated) == 0) {
    stop("activate you object using activate_wearable_dataset first.\n")
  }

  temp_slot <-
    slot(object = .data, name = .data@activated)

  temp_slot <-
    mutate(dtplyr::lazy_dt(temp_slot), !!!dots) %>%
    as.data.frame()

  slot(object = .data, name = .data@activated) = temp_slot

  if (.data@activated == "expression_data") {
    new_sample_id =
      setdiff(colnames(temp_slot), .data@sample_info$sample_id)
    if (length(new_sample_id) > 0) {
      new_sample_info =
        matrix(ncol = ncol(.data@sample_info),
               nrow = length(new_sample_id)) %>%
        as.data.frame()
      colnames(new_sample_info) = colnames(.data@sample_info)
      new_sample_info$sample_id = new_sample_id
      .data@sample_info =
        rbind(.data@sample_info,
              new_sample_info)
      .data@expression_data <-
        .data@expression_data[, .data@sample_info$sample_id, drop = FALSE]
    }
  }

  if (.data@activated == "sample_info") {
    if (ncol(temp_slot) > nrow(.data@sample_info_note)) {
      new_sample_info_note =
        data.frame(
          name = setdiff(colnames(temp_slot), .data@sample_info_note$name),
          meaning = setdiff(colnames(temp_slot), .data@sample_info_note$name),
          check.names = FALSE
        )
      .data@sample_info_note <-
        rbind(.data@sample_info_note,
              new_sample_info_note)
      .data@sample_info <-
        .data@sample_info[, .data@sample_info_note$name, drop = FALSE]
    }

    ###if changed the sample_id in sample_info
    if (any(colnames(.data@expression_data) != .data@sample_info$sample_id)) {
      colnames(.data@expression_data) <-
        .data@sample_info$sample_id
    }
  }

  if (.data@activated == "variable_info") {
    if (ncol(temp_slot) > nrow(.data@variable_info_note)) {
      new_variable_info_note =
        data.frame(
          name = setdiff(colnames(temp_slot), .data@variable_info_note$name),
          meaning = setdiff(colnames(temp_slot), .data@variable_info_note$name),
          check.names = FALSE
        )
      .data@variable_info_note =
        rbind(.data@variable_info_note,
              new_variable_info_note)
      .data@variable_info <-
        .data@variable_info[, .data@variable_info_note$name, drop = FALSE]
    }

    ###if changed the variable_id in variable_info
    if (any(rownames(.data@expression_data) != .data@variable_info$variable_id)) {
      ###if change the variable_id in variable_info
      ##we need to change the same for annotation_table
      if (nrow(.data@annotation_table) > 0) {
        .data@annotation_table <-
          mutate(.data@annotation_table, !!!dots)
      }

      rownames(.data@expression_data) <-
        .data@variable_info$variable_id
    }
  }

  process_info <- .data@process_info

  internal_parameter <-
    purrr::map2(names(dots), dots, function(x, y) {
      y = rlang::expr_label(y)
      y = stringr::str_replace_all(y, "\\`", "") %>%
        stringr::str_replace("\\~", "")
      paste(x, y, sep = '=')
    })

  parameter <- new(
    Class = "tidymass_parameter",
    pacakge_name = "wearabledataset",
    function_name = "mutate()",
    parameter = internal_parameter,
    time = Sys.time()
  )

  if (all(names(process_info) != "mutate")) {
    process_info$mutate = parameter
  } else{
    process_info$mutate = c(process_info$mutate, parameter)
  }

  .data@process_info <- process_info

  ###if the new samples are from the exist samples
  if (.data@activated == "expression_data") {
    new_sample_name <-
      lapply(parameter@parameter, function(x) {
        stringr::str_split(x, pattern = "\\=")[[1]][1]
      }) %>%
      unlist()
    old_sample_name <-
      lapply(parameter@parameter, function(x) {
        stringr::str_split(x, pattern = "\\=")[[1]][2]
      }) %>%
      unlist()
    sample_name <-
      data.frame(new_sample_name,
                 old_sample_name,
                 check.names = FALSE)
    sample_name <-
      sample_name[sample_name$old_sample_name %in% colnames(temp_slot), , drop = FALSE]
    sample_name <-
      sample_name %>%
      dplyr::filter(new_sample_name != old_sample_name)
    if (nrow(sample_name) > 0) {
      sample_id_idx = which(colnames(.data@sample_info) == "sample_id")
      .data@sample_info[match(sample_name$new_sample_name,
                              .data@sample_info$sample_id), -sample_id_idx] =
        .data@sample_info[match(sample_name$old_sample_name,
                                .data@sample_info$sample_id), -sample_id_idx]
    }
  }

  return(.data)
}

#' @importFrom dplyr mutate
#' @export
dplyr::mutate

#' @importFrom dplyr transmute
#' @export
dplyr::transmute

#' @importFrom dplyr mutate_all
#' @export
dplyr::mutate_all

#' @importFrom dplyr mutate_at
#' @export
dplyr::mutate_at

#' @importFrom dplyr n
#' @export
dplyr::n




#' @method pull wearable_dataset
#' @docType methods
#' @importFrom rlang quos !!! enquo
#' @importFrom dplyr pull
#' @export
pull.wearable_dataset <- function(.data, var = -1, ...) {
  # dots <- quos(...)
  var <- enquo(var)
  if (length(.data@activated) == 0) {
    stop("activate you object using activate_wearable_dataset first.\n")
  }

  temp_slot =
    slot(object = .data, name = .data@activated)
  pull(dtplyr::lazy_dt(temp_slot), !!var, ...)
}

#' @importFrom dplyr pull
#' @export
dplyr::pull




#' @method relocate wearable_dataset
#' @docType methods
#' @importFrom rlang quos !!! !! enquo
#' @importFrom dplyr relocate any_of
#' @importFrom tidyselect eval_select
#' @importFrom vctrs vec_unique
#' @export
relocate.wearable_dataset <-
  function(.data,
           ...,
           .before = NULL,
           .after = NULL) {
    if (length(.data@activated) == 0) {
      stop("activate you object using activate_wearable_dataset first.\n")
    }

    x <-
      slot(object = .data, name = .data@activated)

    to_move <- tidyselect::eval_select(expr(c(...)), x)

    .before <- enquo(.before)
    .after <- enquo(.after)
    has_before <- !quo_is_null(.before)
    has_after <- !quo_is_null(.after)

    if (has_before && has_after) {
      abort("Must supply only one of `.before` and `.after`.")
    } else if (has_before) {
      where <- min(unname(tidyselect::eval_select(.before, x)))
      if (!where %in% to_move) {
        to_move <- c(to_move, where)
      }
    } else if (has_after) {
      where <- max(unname(tidyselect::eval_select(.after, x)))
      if (!where %in% to_move) {
        to_move <- c(where, to_move)
      }
    } else {
      where <- 1L
      if (!where %in% to_move) {
        to_move <- c(to_move, where)
      }
    }

    lhs <- setdiff(seq2(1, where - 1), to_move)
    rhs <- setdiff(seq2(where + 1, ncol(x)), to_move)

    pos <- vctrs::vec_unique(c(lhs, to_move, rhs))
    out <- x[pos]
    new_names <- names(pos)

    if (!is.null(new_names)) {
      names(out)[new_names != ""] <- new_names[new_names != ""]
    }

    slot(object = .data, name = .data@activated) = out

    if (.data@activated == "expression_data") {
      .data@sample_info = .data@sample_info[match(colnames(out), .data@sample_info$sample_id),]
    }

    return(.data)
  }


#' @importFrom dplyr relocate
#' @export
dplyr::relocate





#' @method rename wearable_dataset
#' @docType methods
#' @importFrom rlang quos !!!
#' @importFrom dplyr rename
#' @export
rename.wearable_dataset <-
  function(.data, ...) {
    dots <- rlang::quos(...)

    if (length(.data@activated) == 0) {
      stop("activate you object using activate_wearable_dataset first.\n")
    }

    x <-
      slot(object = .data, name = .data@activated)

    x <-
      rename(dtplyr::lazy_dt(x), !!!dots) %>%
      as.data.frame()

    slot(object = .data, name = .data@activated) = x

    if (.data@activated == "expression_data") {
      .data@sample_info$sample_id = colnames(x)
    }

    if (.data@activated == "sample_info") {
      .data@sample_info_note$name <-
        colnames(.data@sample_info)
    }

    if (.data@activated == "variable_info") {
      .data@variable_info_note$name <-
        colnames(.data@variable_info)
    }

    return(.data)
  }

#' @importFrom dplyr rename
#' @export
dplyr::rename








#' @method slice wearable_dataset
#' @docType methods
#' @importFrom rlang quos !!! !! enquo
#' @importFrom dplyr slice any_of
#' @importFrom tidyselect eval_select
#' @importFrom vctrs vec_unique
#' @export
slice.wearable_dataset <-
  function(.data, ..., .preserve = FALSE) {
    dots <- rlang::quos(...)

    if (length(.data@activated) == 0) {
      stop("activate you object using activate_wearable_dataset first.\n")
    }

    x =
      slot(object = .data, name = .data@activated)

    x =
      slice(dtplyr::lazy_dt(x), !!!dots, .preserve = .preserve) %>%
      as.data.frame()

    slot(object = .data, name = .data@activated) = x

    if (.data@activated == "sample_info") {
      .data@expression_data <-
        .data@expression_data[, x$sample_id, drop = FALSE]
    }

    if (.data@activated == "variable_info") {
      .data@expression_data = .data@expression_data[x$variable_id, , drop = FALSE]
    }

    if (.data@activated == "expression_data") {
      .data@variable_info = .data@variable_info[match(rownames(x), .data@variable_info$variable_id), , drop = FALSE]
    }

    process_info = .data@process_info

    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "wearabledataset",
      function_name = "slice()",
      parameter = list(parameter = rlang::expr_label(dots[[1]])),
      time = Sys.time()
    )

    if (all(names(process_info) != "slice")) {
      process_info$slice = parameter
    } else{
      process_info$slice = c(process_info$slice, parameter)
    }

    .data@process_info = process_info

    return(.data)
  }


#' @importFrom dplyr slice
#' @export
dplyr::slice


#' @method slice_head wearable_dataset
#' @docType methods
#' @importFrom rlang quos !!! !! enquo
#' @importFrom dplyr slice_head any_of
#' @export
slice_head.wearable_dataset <-
  function(.data, ..., n, prop) {
    # check_slice_dots(..., n = n, prop = prop)
    # size <- get_slice_size(n = n, prop = prop)
    # idx <- function(n)
    #   seq2(1, size(n))

    if (length(.data@activated) == 0) {
      stop("activate you object using activate_wearable_dataset first.\n")
    }

    x =
      slot(object = .data, name = .data@activated)

    # x = slice(x, idx(dplyr::n()))

    if (missing(n) & missing(prop)) {
      n = "missing"
      prop = "missing"
      x =
        slice_head(dtplyr::lazy_dt(x), n = 1) %>%
        as.data.frame()
    }

    if (!missing(n)) {
      prop = "missing"
      x =
        slice_head(dtplyr::lazy_dt(x), n = n) %>%
        as.data.frame()
    }

    if (missing(n) & !missing(prop)) {
      n = "missing"
      x =
        slice_head(dtplyr::lazy_dt(x), prop = prop) %>%
        as.data.frame()
    }

    slot(object = .data, name = .data@activated) = x

    if (.data@activated == "sample_info") {
      .data@expression_data <-
        .data@expression_data[, x$sample_id, drop = FALSE]
    }

    if (.data@activated == "variable_info") {
      .data@expression_data <-
        .data@expression_data[x$variable_id, , drop = FALSE]
    }

    if (.data@activated == "expression_data") {
      .data@variable_info <-
        .data@variable_info[match(rownames(x),
                                  .data@variable_info$variable_id),
                            , drop = FALSE]
    }

    process_info <-
      .data@process_info

    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "wearabledataset",
      function_name = "slice_head()",
      parameter = list(n = n,
                       prop = prop),
      time = Sys.time()
    )

    if (all(names(process_info) != "slice_head")) {
      process_info$slice_head <- parameter
    } else{
      process_info$slice_head <- c(process_info$slice_head, parameter)
    }

    .data@process_info <- process_info

    return(.data)
  }


#' @importFrom dplyr slice_head
#' @export
dplyr::slice_head







#' @method slice_tail wearable_dataset
#' @docType methods
#' @importFrom rlang quos !!! !! enquo
#' @importFrom dplyr slice_tail any_of
#' @export
slice_tail.wearable_dataset <-
  function(.data, ..., n, prop) {
    # check_slice_dots(..., n = n, prop = prop)
    # size <- get_slice_size(n = n, prop = prop)
    # idx <- function(n)
    #   seq2(1, size(n))

    if (length(.data@activated) == 0) {
      stop("activate you object using activate_wearable_dataset first.\n")
    }

    x =
      slot(object = .data, name = .data@activated)

    # x = slice(x, idx(dplyr::n()))

    if (missing(n) & missing(prop)) {
      n = "missing"
      prop = "missing"
      x =
        slice_tail(dtplyr::lazy_dt(x), n = 1) %>%
        as.data.frame()
    }

    if (!missing(n)) {
      prop = "missing"
      x =
        slice_tail(dtplyr::lazy_dt(x), n = n) %>%
        as.data.frame()
    }

    if (missing(n) & !missing(prop)) {
      n = "missing"
      x =
        slice_tail(dtplyr::lazy_dt(x), prop = prop) %>%
        as.data.frame()
    }

    slot(object = .data, name = .data@activated) = x

    if (.data@activated == "sample_info") {
      .data@expression_data = .data@expression_data[, x$sample_id, drop = FALSE]
    }

    if (.data@activated == "variable_info") {
      .data@expression_data = .data@expression_data[x$variable_id, , drop = FALSE]
    }

    if (.data@activated == "expression_data") {
      .data@variable_info = .data@variable_info[match(rownames(x), .data@variable_info$variable_id), , drop = FALSE]
    }

    process_info = .data@process_info

    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "wearabledataset",
      function_name = "slice_tail()",
      parameter = list(n = n,
                       prop = prop),
      time = Sys.time()
    )

    if (all(names(process_info) != "slice_tail")) {
      process_info$slice_tail = parameter
    } else{
      process_info$slice_tail = c(process_info$slice_tail, parameter)
    }

    .data@process_info = process_info

    return(.data)
  }


#' @importFrom dplyr slice_tail
#' @export
dplyr::slice_tail





#' @method slice_sample wearable_dataset
#' @docType methods
#' @importFrom rlang quos !!! !! enquo
#' @importFrom dplyr slice_sample any_of
#' @export
slice_sample.wearable_dataset <-
  function(.data,
           ...,
           n,
           prop,
           weight_by = NULL,
           replace = FALSE) {
    # check_slice_dots(..., n = n, prop = prop)
    # size <- get_slice_size(n = n, prop = prop)
    # idx <- function(n)
    #   seq2(1, size(n))

    if (length(.data@activated) == 0) {
      stop("activate you object using activate_wearable_dataset first.\n")
    }

    x =
      slot(object = .data, name = .data@activated)

    # x = slice(x, idx(dplyr::n()))

    if (missing(n) & missing(prop)) {
      n = "missing"
      prop = "missing"
      x =
        slice_sample(
          dtplyr::lazy_dt(x),
          n = 1,
          weight_by = weight_by,
          replace = FALSE
        ) %>%
        as.data.frame()
    }

    if (!missing(n)) {
      prop = "missing"
      x =
        slice_sample(
          dtplyr::lazy_dt(x),
          n = n,
          weight_by = weight_by,
          replace = FALSE
        ) %>%
        as.data.frame()
    }

    if (missing(n) & !missing(prop)) {
      n = "missing"
      x =
        slice_sample(
          dtplyr::lazy_dt(x),
          prop = prop,
          weight_by = weight_by,
          replace = FALSE
        ) %>%
        as.data.frame()
    }

    slot(object = .data, name = .data@activated) = x

    if (.data@activated == "sample_info") {
      .data@expression_data = .data@expression_data[, x$sample_id, drop = FALSE]
    }

    if (.data@activated == "variable_info") {
      .data@expression_data = .data@expression_data[x$variable_id, , drop = FALSE]
    }

    if (.data@activated == "expression_data") {
      .data@variable_info = .data@variable_info[match(rownames(x), .data@variable_info$variable_id), , drop = FALSE]
    }

    process_info = .data@process_info

    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "wearabledataset",
      function_name = "slice_sample()",
      parameter = list(
        n = n,
        prop = prop,
        weight_by = weight_by,
        replace = replace
      ),
      time = Sys.time()
    )

    if (all(names(process_info) != "slice_sample")) {
      process_info$slice_sample = parameter
    } else{
      process_info$slice_sample = c(process_info$slice_sample, parameter)
    }

    .data@process_info = process_info

    return(.data)
  }


#' @importFrom dplyr slice_sample
#' @export
dplyr::slice_sample







#' @method slice_min wearable_dataset
#' @docType methods
#' @importFrom rlang quos !!! !! enquo
#' @importFrom dplyr slice_min any_of
#' @export
slice_min.wearable_dataset <-
  function(.data,
           order_by,
           ...,
           n,
           prop,
           with_ties = TRUE) {
    order_by <- enquo(order_by)

    if (length(.data@activated) == 0) {
      stop("activate you object using activate_wearable_dataset first.\n")
    }

    x =
      slot(object = .data, name = .data@activated)

    # x = slice(x, idx(dplyr::n()))

    if (missing(n) & missing(prop)) {
      n = "missing"
      prop = "missing"
      x =
        slice_min(
          dtplyr::lazy_dt(x),
          order_by = !!order_by,
          n = 1,
          with_ties = with_ties
        ) %>%
        as.data.frame()
    }

    if (!missing(n)) {
      prop = "missing"
      x =
        slice_min(
          dtplyr::lazy_dt(x),
          order_by = !!order_by,
          n = n,
          with_ties = with_ties
        ) %>%
        as.data.frame()
    }

    if (missing(n) & !missing(prop)) {
      n = "missing"
      x =
        slice_min(
          dtplyr::lazy_dt(x),
          order_by = !!order_by,
          prop = prop,
          with_ties = with_ties
        ) %>%
        as.data.frame()
    }

    slot(object = .data, name = .data@activated) = x

    if (.data@activated == "sample_info") {
      .data@expression_data = .data@expression_data[, x$sample_id, drop = FALSE]
    }

    if (.data@activated == "variable_info") {
      .data@expression_data = .data@expression_data[x$variable_id, , drop = FALSE]
    }

    if (.data@activated == "expression_data") {
      .data@variable_info = .data@variable_info[match(rownames(x), .data@variable_info$variable_id), , drop = FALSE]
    }

    process_info = .data@process_info

    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "wearabledataset",
      function_name = "slice_min()",
      parameter = list(
        order_by = rlang::expr_label(order_by),
        n = n,
        prop = prop,
        with_ties = with_ties
      ),
      time = Sys.time()
    )

    if (all(names(process_info) != "slice_min")) {
      process_info$slice_min = parameter
    } else{
      process_info$slice_min = c(process_info$slice_min, parameter)
    }

    .data@process_info = process_info

    return(.data)
  }


#' @importFrom dplyr slice_min
#' @export
dplyr::slice_min









#' @method slice_max wearable_dataset
#' @docType methods
#' @importFrom rlang quos !!! !! enquo
#' @importFrom dplyr slice_max any_of
#' @export
slice_max.wearable_dataset <-
  function(.data,
           order_by,
           ...,
           n,
           prop,
           with_ties = TRUE) {
    order_by <- enquo(order_by)

    if (length(.data@activated) == 0) {
      stop("activate you object using activate_wearable_dataset first.\n")
    }

    x <-
      slot(object = .data, name = .data@activated)

    # x = slice(x, idx(dplyr::n()))

    if (missing(n) & missing(prop)) {
      n <- "missing"
      prop <- "missing"
      x <-
        slice_max(
          dtplyr::lazy_dt(x),
          order_by = !!order_by,
          n = 1,
          with_ties = with_ties
        ) %>%
        as.data.frame()
    }

    if (!missing(n)) {
      prop <- "missing"
      x <-
        slice_max(
          dtplyr::lazy_dt(x),
          order_by = !!order_by,
          n = n,
          with_ties = with_ties
        ) %>%
        as.data.frame()
    }

    if (missing(n) & !missing(prop)) {
      n <- "missing"
      x <-
        slice_max(
          dtplyr::lazy_dt(x),
          order_by = !!order_by,
          prop = prop,
          with_ties = with_ties
        ) %>%
        as.data.frame()
    }

    slot(object = .data, name = .data@activated) <- x

    if (.data@activated == "sample_info") {
      .data@expression_data <-
        .data@expression_data[, x$sample_id, drop = FALSE]
    }

    if (.data@activated == "variable_info") {
      .data@expression_data <-
        .data@expression_data[x$variable_id, , drop = FALSE]
    }

    if (.data@activated == "expression_data") {
      .data@variable_info <-
        .data@variable_info[match(rownames(x), .data@variable_info$variable_id), , drop = FALSE]
    }

    process_info <- .data@process_info

    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "wearabledataset",
      function_name = "slice_max()",
      parameter = list(
        order_by = rlang::expr_label(order_by),
        n = n,
        prop = prop,
        with_ties = with_ties
      ),
      time = Sys.time()
    )

    if (all(names(process_info) != "slice_max")) {
      process_info$slice_max <- parameter
    } else{
      process_info$slice_max <- c(process_info$slice_max, parameter)
    }

    .data@process_info <- process_info

    return(.data)
  }


#' @importFrom dplyr slice_max
#' @export
dplyr::slice_max










#' @method summarize wearable_dataset
#' @docType methods
#' @importFrom rlang quos !!!
#' @importFrom dplyr summarize
#' @export
summarize.wearable_dataset <-
  function(.data, ...) {
    dots <- rlang::quos(...)

    if (length(.data@activated) == 0) {
      stop("activate you object using activate_wearable_dataset first.\n")
    }

    x =
      slot(object = .data, name = .data@activated)

    x =
      summarize(dtplyr::lazy_dt(x), !!!dots) %>%
      as.data.frame()

    return(x)
  }


#' @method summarise wearable_dataset
#' @docType methods
#' @importFrom rlang quos !!!
#' @importFrom dplyr summarise
#' @export
summarise.wearable_dataset <-
  function(.data, ...) {
    dots <- rlang::quos(...)

    if (length(.data@activated) == 0) {
      stop("activate you object using activate_wearable_dataset first.\n")
    }

    x =
      slot(object = .data, name = .data@activated)

    x =
      summarise(dtplyr::lazy_dt(x), !!!dots) %>%
      as.data.frame()

    return(x)
  }


#' @importFrom dplyr summarize
#' @export
dplyr::summarize


#' @importFrom dplyr n
#' @export
dplyr::n
