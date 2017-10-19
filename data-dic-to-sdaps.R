library("plyr")
library("dplyr")
data_dic_to_sdaps <- function(in_dic, out_tex, out_spec, title, which_forms = NULL) {
  data_dic <- read.csv(in_dic,
                       check.names = FALSE,
                       stringsAsFactors = FALSE)
  
  data_dic$main_id <- c(TRUE, rep(FALSE, nrow(data_dic) - 1))
  
  split_choices <- function(choices) {
    choice_list <- strsplit(choices, " *\\| *")[[1]]
    regexec("^([^,]+), *(.*)$", choice_list) %>%
      regmatches(choice_list, .) %>%
      ldply(function(x) {
        data_frame(value = x[2],
                   label = x[3])
      })
  }
  
  process_item <- function(data_dic_row) {
    out <- "!@#"
    included_csv <- FALSE
    cat(data_dic_row$`Variable / Field Name`, data_dic_row$`Field Type`, "\n")
    if (data_dic_row$`Field Type` %in% c("text", "notes")) {
      if (data_dic_row$main_id) {
        out <- ""
        included_csv <- FALSE
      } else {
        if (data_dic_row$`Field Type` == "text") {
          text_height <- 1L
        } else {
          text_height <- 3L
        }
        out <- sprintf("\\textbox{%dcm}{%s}", text_height, data_dic_row$`Field Label`)
        included_csv <- TRUE
      }
    } else if (data_dic_row$`Field Type` == "descriptive") {
      out <- data_dic_row$`Field Label`
    } else if (data_dic_row$`Field Type` %in% c("dropdown", "radio", "yesno", "truefalse", "checkbox")) {
      included_csv <- TRUE
      if (data_dic_row$`Field Type` == "yesno") {
        data_dic_row$`Choices, Calculations, OR Slider Labels` <- "1, Yes | 0, No"
      } else if (data_dic_row$`Field Type` == "yesno") {
        data_dic_row$`Choices, Calculations, OR Slider Labels` <- "1, True | 0, False"
      }
      choices <- split_choices(data_dic_row$`Choices, Calculations, OR Slider Labels`)
      if (max(nchar(choices$label)) > 100L) {
        choice_cols <- 1L
      } else if (max(nchar(choices$label)) > 50L) {
        choice_cols <- 2L
      } else {
        choice_cols <- 3L
      }
      out <- sprintf("\\begin{choicequestion}[%d]{%s}", choice_cols, data_dic_row$`Field Label`)
      out <- c(out, sprintf("\\choiceitem{%s}", choices$label), "\\end{choicequestion}")
    } else if (data_dic_row$`Field Type` == "slider") {
      included_csv <- TRUE
      choices <- strsplit(data_dic_row$`Choices, Calculations, OR Slider Labels`, " *\\| *")[[1]]
      out <- sprintf("\\singlemark{%s}{%s}{%s}", data_dic_row$`Field Label`,
                     choices[1], choices[2])
    }
    if (data_dic_row$`Section Header` != "") {
      out <- c(sprintf("\\section{%s}", data_dic_row$`Section Header`), out)
    }
    data_frame(form = data_dic_row$`Form Name`,
               field = data_dic_row$`Variable / Field Name`,
               type = data_dic_row$`Field Type`,
               choices = data_dic_row$`Choices, Calculations, OR Slider Labels`,
               latex = paste(out, collapse = "\n"),
               included_csv = included_csv,
               main_id = data_dic_row$main_id
    )
  }
  
  process_matrix <- function(group_data) {
    matrix_header <- group_data$`Section Header`[1]
    if (matrix_header == "") {
      matrix_header <- " "
    }
    choices <- split_choices(group_data$`Choices, Calculations, OR Slider Labels`[1])
    out <- paste(paste(sprintf("\\begin{choicegroup}{%s}", matrix_header), collapse = "\n"),
                 paste(sprintf("\\groupaddchoice{%s}", choices$label), collapse = "\n"),
                 paste(sprintf("\\choiceline{%s}", group_data$`Field Label`), collapse = "\n"),
                 "\\end{choicegroup}",
                 sep = "\n")
    data_frame(form = group_data$`Form Name`,
               field = group_data$`Variable / Field Name`,
               type = group_data$`Field Type`,
               choices = group_data$`Choices, Calculations, OR Slider Labels`,
               latex = c(out, rep("", nrow(group_data) - 1)),
               included_csv = TRUE
    )
  }
  
  process_form <- function(section, data) {
    form_data <- data %>%
      filter(`Form Name` == section)
    matrix_groups <- rle(form_data$`Matrix Group Name`)
    form_out <- ldply(1:length(matrix_groups$lengths), function(i) {
      group_data <- form_data[sum(c(1, matrix_groups$lengths)[1:i]):sum(matrix_groups$lengths[1:i]), ]
      if (is.na(matrix_groups$values[i]) || matrix_groups$values[i] == "") {
        adply(group_data, 1, process_item, .expand = FALSE)
      } else {
        process_matrix(group_data)
      }
    })
    form_out <- form_out[form_out$latex != "!@#", ]
    form_out$latex <- gsub("_", "\\\\_", form_out$latex)
    form_out$latex <- gsub("&", "\\\\&", form_out$latex)
    form_out$latex <- gsub("<b>([^<]*)</b>", "\\\\textbf{\\1}", form_out$latex)
    # form_out$latex[1] <- paste(sprintf("\\section{%s}\\nopagebreak", gsub("_", " ", section)),
    #                           form_out$latex[1], sep = "\n", collapse = "\n")
    form_out
  }
  
  if (is.null(which_forms)) {
    which_forms <- unique(data_dic$`Form Name`)
  }
  
  tex_data <- ldply(which_forms, process_form, data_dic)
  tex_data$X1 <- NULL
  #tex_data$sec_number <- as.integer(factor(tex_data$form, levels = unique(factor(tex_data$form))))
  tex_data$sec_number <- cumsum(grepl("\\\\section", tex_data$latex))
  tex_data$question_number <- tapply(tex_data$included_csv, tex_data$sec_number, cumsum) %>%
    unlist %>%
    unname
  tex_data$question_number[!tex_data$included_csv] <- NA

  import_spec <- tex_data %>%
    filter(included_csv | main_id) %>%
    select(sec_number, question_number, form, field, type, choices, main_id) %>%
    adply(1, function(spec_row) {
      if (spec_row$choices != "") {
        split_choices(spec_row$choices) %>%
          mutate(., choice_value = value, choice_label = label, choice_number = row_number() - 1L)
      } else {
        data_frame(choice_value = NA, choice_label = NA, choice_number = 0L)
      }
    }) %>%
    mutate(input_id = paste(sec_number, question_number, choice_number, sep = "_"),
           output_field_name = paste0(field, ifelse(type == "checkbox",
                                                    sprintf("___%s", choice_value), ""))) %>%
    select(-choices)
  
  fil <- file(out_tex)
  open(fil, open = "wb")
  writeLines(sprintf("\\documentclass[english, print_questionnaire_id, oneside, pagemark, stamp]{sdaps}
\\usepackage[utf8]{inputenc}
\\author{Exeter IBD team}
\\title{%s}
\\begin{document}
\\begin{questionnaire}
\\setcounter{markcheckboxcount}{11}", title), fil)
  
  writeLines(tex_data$latex, fil)
  
  writeLines("\\end{questionnaire}
             \\end{document}", fil)
  close(fil)
  
  write.csv(import_spec, out_spec)
}
