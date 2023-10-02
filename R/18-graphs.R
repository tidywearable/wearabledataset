#' @title Draw a time serial plot
#' @description This generic function creates a time-based plot based
#' on the input data and the specified parameters.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param x A numeric vector or wearable_dataset object.
#' @param accurate_time A vector or data frame column representing the time points corresponding to `x`.
#' @param variable_index Index or name of the variable to be plotted (used if `x` is a data frame).
#' @param color Color for the plot. Default is "blue".
#' @param y_axis_name Name for the y-axis. Default is "Value".
#' @param sun_rise_time Time at which sunrise occurs. Default is "6:00:00".
#' @param sun_set_time Time at which sunset occurs. Default is "18:00:00".
#' @param time_gap Gap for time ticks on the x-axis. Default is 12.
#' @param add_point Logical; whether to add individual data points to the plot. Default is FALSE.
#' @param facet Logical; whether to use faceting in the plot. Default is FALSE.
#' @param facet_function Character; which faceting function to use. Options are "facet_grid" and "facet_wrap".
#' @param facet_grid_by Character; specifies whether to facet by "rows" or "cols" when using `facet_grid`.
#' @param facet_wrap_nrow Numeric; number of rows for the facet wrap layout.
#' @param facet_wrap_ncol Numeric; number of columns for the facet wrap layout.
#' @param ... Further arguments to be passed to specific methods.
#' @export
#' @return A time plot with specified attributes.
#' @examples
#' data("expression_data", package = "wearabledataset")
#' data("sample_info", package = "wearabledataset")
#' data("variable_info", package = "wearabledataset")
#' object <-
#' create_wearable_dataset(expression_data, sample_info, variable_info)
#' object <- object %>% mutate2sample(what = "date")
#' unique(object@sample_info$date)
#' object <-
#' object %>% activate_wearable_dataset(what = "sample_info") %>%
#' filter(date %in% c("2022-08-08", "2022-08-09"))
#' expression_data <- extract_expression_data(object)
#' sample_info <- extract_sample_info(object)
#' x = as.numeric(expression_data[1,])
#' accurate_time = sample_info$accurate_time
#' draw_time_plot(x = x, accurate_time = accurate_time)
#' draw_time_plot(x = object, variable_index = 1,
#' facet = TRUE,
#' facet_function = "facet_wrap")
#' draw_time_plot(x = object,
#'                variable_index = 1,
#'                facet = TRUE,
#'                facet_function = "facet_grid",
#'                facet_grid_by = "rows", time_gap = 2)

draw_time_plot <-
  function(x,
           accurate_time,
           variable_index,
           color = "blue",
           y_axis_name = "Value",
           sun_rise_time = "6:00:00",
           sun_set_time = "18:00:00",
           time_gap = 12,
           add_point = FALSE,
           facet = FALSE,
           facet_function = c("facet_grid", "facet_wrap"),
           facet_grid_by = c("rows", "cols"),
           facet_wrap_nrow,
           facet_wrap_ncol,
           ...) {
    UseMethod("draw_time_plot")
  }

#' @method draw_time_plot numeric
#' @rdname draw_time_plot
#' @param x A numeric vector
#' @param accurate_time Accurate time
#' @param variable_index index of variable
#' @param color color
#' @param y_axis_name y_axis_name
#' @param sun_rise_time should be 24 hour format, default "6:00:00"
#' @param sun_set_time should be 24 hour format, default "18:00:00"
#' @param time_gap time gap for x axis text.
#' @param add_point add point or not.
#' @param facet facet or not.
#' @param facet_function facet function, facet_grid or facet_wrap
#' @param facet_grid_by facet_grid, rows or cols
#' @param facet_wrap_nrow nrow for facet_wrap
#' @param facet_wrap_ncol ncol for facet_wrap,
#' @param ... othter arguments
#' @importFrom lubridate date tz month day ymd_hms
#' @import ggplot2
#' @importFrom hms as_hms
#' @export

draw_time_plot.numeric <-
  function(x,
           accurate_time,
           variable_index,
           color = "blue",
           y_axis_name = "Value",
           sun_rise_time = "6:00:00",
           sun_set_time = "18:00:00",
           time_gap = 12,
           add_point = FALSE,
           facet = FALSE,
           facet_function = c("facet_grid", "facet_wrap"),
           facet_grid_by = c("rows", "cols"),
           facet_wrap_nrow,
           facet_wrap_ncol,
           ...) {
    facet_function <-
      match.arg(facet_function)
    facet_grid_by <-
      match.arg(facet_grid_by)

    temp_data <-
      data.frame(accurate_time, x) %>%
      dtplyr::lazy_dt() %>%
      filter(!is.na(x)) %>%
      mutate(date = lubridate::date(accurate_time)) %>%
      mutate(time = strftime(
        accurate_time,
        format = "%H:%M:%S",
        tz = lubridate::tz(accurate_time)
      ) %>%
        hms::as_hms()) %>%
      rename(value = x) %>%
      dplyr::mutate(time = as.POSIXct(time),
                    week = format(accurate_time, "%a")) %>%
      dplyr::mutate(week = paste(week,
                                 lubridate::month(date),
                                 lubridate::day(date),
                                 sep = "-")) %>%
      dplyr::mutate(week = factor(week, unique(week))) %>%
      as.data.frame()

    rm(accurate_time)
    rm(x)
    gc()

    draw_time_plot_inner(
      temp_data = temp_data,
      color = color,
      y_axis_name = y_axis_name,
      sun_rise_time = sun_rise_time,
      sun_set_time = sun_set_time,
      time_gap = time_gap,
      add_point = add_point,
      facet = facet,
      facet_function = facet_function,
      facet_grid_by = facet_grid_by,
      facet_wrap_nrow = facet_wrap_nrow,
      facet_wrap_ncol = facet_wrap_ncol
    )
  }



#' @method draw_time_plot wearable_dataset
#' @rdname draw_time_plot
#' @param x A wearable_dataset class object
#' @param accurate_time Accurate time
#' @param variable_index index of variable
#' @param color color
#' @param y_axis_name y_axis_name
#' @param sun_rise_time should be 24 hour format, default "6:00:00"
#' @param sun_set_time should be 24 hour format, default "18:00:00"
#' @param time_gap time gap for x axis text.
#' @param add_point add point or not.
#' @param facet facet or not.
#' @param facet_function facet function, facet_grid or facet_wrap
#' @param facet_grid_by facet_grid, rows or cols
#' @param facet_wrap_nrow nrow for facet_wrap
#' @param facet_wrap_ncol ncol for facet_wrap,
#' @param ... othter arguments
#' @importFrom lubridate date tz month day ymd_hms
#' @import ggplot2
#' @importFrom hms as_hms
#' @importFrom scales date_breaks
#' @export
draw_time_plot.wearable_dataset <-
  function(x,
           accurate_time,
           variable_index,
           color = "blue",
           y_axis_name = "Value",
           sun_rise_time = "6:00:00",
           sun_set_time = "18:00:00",
           time_gap = 12,
           add_point = FALSE,
           facet = FALSE,
           facet_function = c("facet_grid", "facet_wrap"),
           facet_grid_by = c("rows", "cols"),
           facet_wrap_nrow,
           facet_wrap_ncol,
           ...) {
    facet_function <-
      match.arg(facet_function)
    facet_grid_by <-
      match.arg(facet_grid_by)

    accurate_time <-
      x %>%
      activate_wearable_dataset(what = "sample_info") %>%
      pull(accurate_time)

    x <- as.numeric(unlist(x[variable_index, , drop = TRUE]))

    temp_data <-
      data.frame(accurate_time, x) %>%
      dtplyr::lazy_dt() %>%
      filter(!is.na(x)) %>%
      mutate(date = lubridate::date(accurate_time)) %>%
      mutate(time = strftime(
        accurate_time,
        format = "%H:%M:%S",
        tz = lubridate::tz(accurate_time)
      ) %>%
        hms::as_hms()) %>%
      rename(value = x) %>%
      dplyr::mutate(time = as.POSIXct(time),
                    week = format(accurate_time, "%a")) %>%
      dplyr::mutate(week = paste(week,
                                 lubridate::month(date),
                                 lubridate::day(date),
                                 sep = "-")) %>%
      dplyr::mutate(week = factor(week, unique(week))) %>%
      as.data.frame()

    rm(accurate_time)
    rm(x)
    gc()

    draw_time_plot_inner(
      temp_data = temp_data,
      color = color,
      y_axis_name = y_axis_name,
      sun_rise_time = sun_rise_time,
      sun_set_time = sun_set_time,
      time_gap = time_gap,
      add_point = add_point,
      facet = facet,
      facet_function = facet_function,
      facet_grid_by = facet_grid_by,
      facet_wrap_nrow = facet_wrap_nrow,
      facet_wrap_ncol = facet_wrap_ncol
    )
  }



#' @method draw_time_plot default
#' @rdname draw_time_plot
#' @title Draw time serial plot
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param x A numeric vector or wearable_dataset object.
#' @param variable_index index of variable
#' @param accurate_time Accurate time
#' @param color color
#' @param y_axis_name y_axis_name
#' @param sun_rise_time should be 24 hour format, default "6:00:00"
#' @param sun_set_time should be 24 hour format, default "18:00:00"
#' @param time_gap time gap for x axis text.
#' @param add_point add point or not.
#' @param facet facet or not.
#' @param facet_function facet function, facet_grid or facet_wrap
#' @param facet_grid_by facet_grid, rows or cols
#' @param facet_wrap_nrow nrow for facet_wrap
#' @param facet_wrap_ncol ncol for facet_wrap,
#' @param ... othter arguments
#' @export
#' @return NULL
draw_time_plot.default <-
  function(x,
           ...) {
    stop("Unsupported input type.")
  }


draw_time_plot_inner <-
  function(temp_data,
           color = "blue",
           y_axis_name = "Value",
           sun_rise_time = "6:00:00",
           sun_set_time = "18:00:00",
           time_gap = 12,
           add_point = FALSE,
           facet = FALSE,
           facet_function = c("facet_grid", "facet_wrap"),
           facet_grid_by = c("rows", "cols"),
           facet_wrap_nrow,
           facet_wrap_ncol) {
    facet_function <-
      match.arg(facet_function)
    facet_grid_by <-
      match.arg(facet_grid_by)

    sun_rise <-
      lubridate::ymd_hms(paste(unique(
        lubridate::date(temp_data$accurate_time)
      ), c(sun_rise_time)),
      tz = lubridate::tz(temp_data$accurate_time))

    sun_set =
      lubridate::ymd_hms(paste(unique(
        lubridate::date(temp_data$accurate_time)
      ), c(sun_set_time)),
      tz = lubridate::tz(temp_data$accurate_time))

    day_night_df <-
      data.frame(start = sun_rise,
                 end = sun_set,
                 date = lubridate::date(sun_rise)) %>%
      dplyr::mutate(
        start_time = as.POSIXct(hms::as_hms(start)),
        end_time = as.POSIXct(hms::as_hms(end)),
        week = format(date, "%a")
      ) %>%
      dplyr::mutate(week = paste(week,
                                 lubridate::month(date),
                                 lubridate::day(date),
                                 sep = "-")) %>%
      dplyr::mutate(week = factor(week, unique(week)))

    if (facet) {
      plot <-
        ggplot2::ggplot() +
        ggplot2::geom_rect(
          mapping = aes(
            xmin = start_time,
            xmax = end_time,
            ymin = -Inf,
            ymax = Inf
          ),
          fill = "lightyellow",
          data = day_night_df,
          show.legend = FALSE
        ) +
        geom_line(aes(x = time,
                      y = value),
                  color = color,
                  data = temp_data) +
        scale_x_datetime(
          breaks = scales::date_breaks(paste(time_gap, "hour")),
          date_labels = "%H:%M",
          timezone = lubridate::tz(temp_data$time)
        )
    } else{
      plot <-
        ggplot2::ggplot() +
        ggplot2::geom_rect(
          mapping = aes(
            xmin = start,
            xmax = end,
            ymin = -Inf,
            ymax = Inf
          ),
          fill = "lightyellow",
          data = day_night_df,
          show.legend = FALSE
        ) +
        geom_line(aes(x = accurate_time,
                      y = value),
                  color = color,
                  data = temp_data) +
        scale_x_datetime(
          breaks = scales::date_breaks(paste(time_gap, "hour")),
          date_labels = "%a %H:%M",
          timezone = lubridate::tz(temp_data$time)
        )
    }

    plot <-
      plot +
      labs(x = "", y = y_axis_name) +
      scale_y_continuous(expand = expansion(mult = c(0.02, 0.1))) +
      base_theme +
      theme(
        axis.text.x = element_text(
          angle = 45,
          vjust = 1,
          hjust = 1,
          size = 10
        ),
        axis.line.x = element_blank(),
        # axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = alpha("grey", 0.2)),
        plot.margin = margin(
          t = 0,
          r = 0,
          b = 0,
          l = 0,
          unit = "pt"
        )
      )

    if (facet) {
      if (facet_function == "facet_grid") {
        if (facet_grid_by == "rows") {
          plot <-
            plot +
            facet_grid(rows = vars(date))
        } else{
          plot <-
            plot +
            facet_grid(cols = vars(date))
        }
      } else{
        if (!missing(facet_wrap_nrow)) {
          plot <-
            plot +
            facet_wrap(facets = vars(date), nrow = nrow)
        } else{
          if (!missing(facet_wrap_ncol)) {
            plot <-
              plot +
              facet_wrap(facets = vars(date), ncol = facet_wrap_ncol)
          } else{
            plot <-
              plot +
              facet_wrap(facets = vars(date))
          }
        }
      }
    }

    # if (add_point) {
    #   plot =
    #     plot +
    #     geom_point(
    #       aes(x = time,
    #           y = value),
    #       color = color,
    #       shape = 16,
    #       data = temp_data
    #     )
    # }
    return(plot)
  }
