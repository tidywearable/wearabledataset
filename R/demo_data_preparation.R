# setwd(r4projects::get_project_wd())
# setwd("demo_data/")
#
# rm(list = ls())
#
# library(dtplyr)
# library(data.table)
# library(dplyr, warn.conflicts = FALSE)
#
# load("food_data")
# load("cgm_data")
# load("hr_data")
# load("step_data")
#
# cgm_data <-
#   cgm_data %>%
#   tibble::as_tibble()
#
# hr_data <-
#   hr_data %>%
#   tibble::as_tibble()
#
# step_data <-
#   step_data %>%
#   tibble::as_tibble()
#
# cgm_data <-
#   cgm_data %>%
#   dtplyr::lazy_dt() %>%
#   dplyr::filter(!is.na(value)) %>%
#   dplyr::filter(value != 0) %>%
#   dplyr::mutate(sample_id = paste(subject_id, accurate_time, sep = "_")) %>%
#   tibble::as_tibble()
#
# cgm_expression_data <-
#   cgm_data %>%
#   dtplyr::lazy_dt() %>%
#   dplyr::select(sample_id,
#                 value) %>%
#   mutate(variable_id = "cgm") %>%
#   tidyr::pivot_wider(names_from = "sample_id", values_from = "value") %>%
#   tibble::as_tibble() %>%
#   tibble::column_to_rownames(var = "variable_id") %>%
#   tibble::as_tibble()
#
# cgm_sample_info <-
#   cgm_data %>%
#   dtplyr::lazy_dt() %>%
#   dplyr::select(subject_id,
#                 accurate_time,
#                 transmitter_id,
#                 sample_id) %>%
#   dplyr::distinct(sample_id,
#                   .keep_all = TRUE) %>%
#   tibble::as_tibble()
#
# cgm_variable_info <-
#   data.frame(variable_id = "cgm",
#              name = "Continuous Gluctose Monitoring")
#
# cgm_expression_data <-
#   as_tibble(cgm_expression_data)[, cgm_sample_info$sample_id]
#
# hr_data <-
#   hr_data %>%
#   dtplyr::lazy_dt() %>%
#   dplyr::mutate(sample_id = paste(subject_id, accurate_time, sep = "_")) %>%
#   tibble::as_tibble()
#
# hr_expression_data <-
#   hr_data %>%
#   dtplyr::lazy_dt() %>%
#   dplyr::filter(!is.na(value)) %>%
#   dplyr::filter(value != 0) %>%
#   dplyr::select(sample_id,
#                 value) %>%
#   mutate(variable_id = "hr") %>%
#   tidyr::pivot_wider(names_from = "sample_id", values_from = "value") %>%
#   tibble::as_tibble() %>%
#   tibble::column_to_rownames(var = "variable_id")
#
# hr_sample_info <-
#   hr_data %>%
#   dtplyr::lazy_dt() %>%
#   dplyr::filter(!is.na(value)) %>%
#   dplyr::filter(value != 0) %>%
#   dplyr::select(subject_id,
#                 sample_id,
#                 accurate_time) %>%
#   dplyr::distinct(sample_id,
#                   .keep_all = TRUE) %>%
#   tibble::as_tibble()
#
# hr_expression_data <-
#   tibble::as_tibble(hr_expression_data)[, hr_sample_info$sample_id]
#
# hr_variable_info <-
#   data.frame(variable_id = "hr",
#              name = "Heart Rate")
#
# step_data <-
#   step_data %>%
#   dtplyr::lazy_dt() %>%
#   dplyr::filter(!is.na(value)) %>%
#   dplyr::filter(value != 0) %>%
#   dplyr::mutate(sample_id = paste(subject_id, accurate_time, sep = "_")) %>%
#   tibble::as_tibble()
#
# step_expression_data <-
#   step_data %>%
#   dtplyr::lazy_dt() %>%
#   dplyr::select(sample_id,
#                 value) %>%
#   mutate(variable_id = "hr") %>%
#   tidyr::pivot_wider(names_from = "sample_id", values_from = "value") %>%
#   tibble::as_tibble() %>%
#   tibble::column_to_rownames(var = "variable_id")
#
# step_sample_info <-
#   step_data %>%
#   dtplyr::lazy_dt() %>%
#   dplyr::filter(!is.na(value)) %>%
#   dplyr::filter(value != 0) %>%
#   dplyr::select(subject_id,
#                 sample_id,
#                 accurate_time,
#                 mets,
#                 subject_id2) %>%
#   dplyr::distinct(sample_id,
#                   .keep_all = TRUE) %>%
#   tibble::as_tibble()
#
# step_expression_data <-
#   tibble::as_tibble(step_expression_data)[, step_sample_info$sample_id]
#
# step_variable_info <-
#   data.frame(variable_id = "step",
#              name = "Step")
#
# intersect(cgm_sample_info$sample_id,
#           hr_sample_info$sample_id)
#
# intersect(cgm_sample_info$sample_id,
#           step_sample_info$sample_id)
#
# sample_info <-
#   cgm_sample_info %>%
#   dplyr::full_join(hr_sample_info,
#                    by = c("sample_id", "accurate_time", "subject_id")) %>%
#   dplyr::full_join(step_sample_info,
#                    by = c("sample_id", "accurate_time", "subject_id"))
#
#
# expression_data <-
#   dtplyr::lazy_dt(cgm_expression_data) %>%
#   dplyr::full_join(dtplyr::lazy_dt(hr_expression_data),
#                    by = intersect(colnames(hr_expression_data), colnames(cgm_expression_data))) %>%
#   tibble::as_tibble()
#
# expression_data <-
#   dtplyr::lazy_dt(expression_data) %>%
#   dplyr::full_join(dtplyr::lazy_dt(step_expression_data),
#                    by = intersect(colnames(expression_data), colnames(step_expression_data))) %>%
#   tibble::as_tibble()
#
#
# dim(sample_info)
# dim(expression_data)
#
# expression_data <-
#   as.data.frame(expression_data)[,sample_info$sample_id]
#
# sample_info <-
#   as.data.frame(sample_info)
#
# rownames(expression_data) <-
#   variable_info$variable_id
#
# expression_data <-
#   as.data.frame(expression_data)
#
# sample_info <-
#   as.data.frame(sample_info)
#
# variable_info <-
#   as.data.frame(variable_info)
#
# colnames(expression_data) ==
#   sample_info$sample_id
#
# rownames(expression_data) ==
#   variable_info$variable_id
#
# save(expression_data, file = "expression_data")
# save(sample_info, file = "sample_info")
# save(variable_info, file = "variable_info")
