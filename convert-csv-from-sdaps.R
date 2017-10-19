#!/usr/bin/env Rscript
library("plyr")
library("stringr")
library("dplyr")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) {
  stop("Usage: convert-csv-from-sdaps <import-spec> <sdaps-exported-data> <csv-for-redcap>")
}
import_spec <- read.csv(args[1], stringsAsFactors = FALSE, check.names = FALSE)
exported_data <- read.csv(args[2], stringsAsFactors = FALSE, check.names = FALSE)
read_answer <- function(spec, data) {
  type <- spec$type[1]
  out <- NULL
  if (!spec$main_id[1]) {
    if (type %in% c("dropdown", "radio", "yesno", "truefalse")) {
      rel_answers <- data.matrix(data[, spec$input_id, drop = FALSE])
      stopifnot(all(spec$input_id == colnames(rel_answers)))
      out <- aaply(rel_answers, 1, function(row) {
        if (sum(row) == 1) {
          spec$choice_value[which(row == 1)]
        } else {
          ""
        }
      })
      out <- data_frame(x = out)
      colnames(out) <- spec$output_field_name[1]
    } else if (type == "checkbox") {
      rel_answers <- data[, spec$input_id, drop = FALSE]
      stopifnot(all(spec$input_id == colnames(rel_answers)))
      colnames(rel_answers) <- spec$output_field_name
      out <- rel_answers
    } else if (type == "slider") {
      input_id <- sub("_0$", "", spec$input_id[1])
      stopifnot(input_id %in% colnames(data))
      out <- data_frame(x = (data[, input_id] - 1) * 10)
      colnames(out) <- spec$output_field_name[1]
    } else if (type %in% c("text", "notes")) {
      stopifnot(spec$input_id[1] %in% colnames(data))
      out <- data[, spec$input_id[1]]
      out <- ifelse(out == "0", "", out)
      out <- data_frame(x = out)
      colnames(out) <- spec$output_field_name[1]
    }
  } else if (spec$main_id[1]) {
    ids <- as.integer(str_replace(data$questionnaire_id, "\\[('(\\d+)',? ?)+\\]", "\\2"))
    out <- data_frame(x = ids)
    colnames(out) <- spec$output_field_name[1]
  }
  out
}

out_data <- llply(unique(import_spec$field), function(field) read_answer(import_spec[import_spec$field == field, ], exported_data))
out_data <- bind_cols(out_data)
write.csv(out_data, args[3], row.names = FALSE)
