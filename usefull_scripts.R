required_packages = c("magrittr",
                      "purrr")

for (package in required_packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

is_coerced_to_numeric = function(input) {
  stopifnot(is_atomic(input))

  map_lgl(input,
          ~ tryCatch(
            {
              as.numeric(.x)
              TRUE
            },
            error = function(error)
              FALSE,
            warning = function(warning)
              FALSE
          )
  )
}

describe_variable = function(input_var, grouping_var, skip_tests = FALSE) {
  stopifnot(is.factor(grouping_var),
            length(input_var) == length(grouping_var))

  grouping_name = deparse(substitute(grouping_var)) %>%
    strsplit(., "\\$") %>%
    unlist() %>%
    extract(2)

  output_stats = list(NULL, NULL, NULL, NULL)

  output_stats = set_names(output_stats,
                           c("shapiro_test",
                             paste0(grouping_name, "_", unique(grouping_var)[1], collapse = ""),
                             paste0(grouping_name, "_", unique(grouping_var)[2], collapse = ""),
                             "p_value"))

  if (is.numeric(input_var)) {
    output_stats[[1]] = shapiro.test(input_var)$p.value %>% round(3) %>% as.character()

    if (output_stats[[1]] > 0.05) {
      output_stats[[2]] = sprintf("%.2f±%.2f",
                                  mean(input_var[grouping_var == unique(grouping_var)[1]], na.rm = T),
                                  sd(input_var[grouping_var == unique(grouping_var)[1]], na.rm = T))

      output_stats[[3]] = sprintf("%.2f±%.2f",
                                  mean(input_var[grouping_var == unique(grouping_var)[2]], na.rm = T),
                                  sd(input_var[grouping_var == unique(grouping_var)[2]], na.rm = T))

    } else{
      output_stats[[2]] = sprintf(
        "%.2f [%.2f; %.2f]",
        quantile(input_var[grouping_var == unique(grouping_var)[1]], na.rm = T)[3],
        quantile(input_var[grouping_var == unique(grouping_var)[1]], na.rm = T)[2],
        quantile(input_var[grouping_var == unique(grouping_var)[1]], na.rm = T)[4]
      )
      output_stats[[3]] = sprintf(
        "%.2f [%.2f; %.2f]",
        quantile(input_var[grouping_var == unique(grouping_var)[2]], na.rm = T)[3],
        quantile(input_var[grouping_var == unique(grouping_var)[2]], na.rm = T)[2],
        quantile(input_var[grouping_var == unique(grouping_var)[2]], na.rm = T)[4]
      )


    }

    if(skip_tests){
      output_stats[[4]] = NA_real_
    }else{
      output_stats[[4]] = wilcox.test(input_var ~ grouping_var)$p.value %>% round(3)
    }




  } else if (is.factor(input_var) | is.character(input_var) | is.list(input_var)) {

    output_stats[[1]] = "Not applicable"

    output_stats[[2]] = cbind(
      table(input_var[grouping_var == unique(grouping_var)[1]]
            %>% unlist()
      ) %>%
        as.data.frame(),
      prop.table(table(input_var[grouping_var == unique(grouping_var)[1]] %>%
                         unlist())) %>%
        round(2)
    ) %>%
      extract(,c(1,2,4)) %>%
      capture.output() %>%
      paste0(collapse = "<br/>")

    output_stats[[3]] = cbind(
      table(input_var[grouping_var == unique(grouping_var)[2]]
            %>% unlist()
      ) %>%
        as.data.frame(),
      prop.table(table(input_var[grouping_var == unique(grouping_var)[2]] %>%
                         unlist())) %>%
        round(2)
    ) %>%
      extract(,c(1,2,4)) %>%
      capture.output() %>%
      paste0(collapse = "<br/>")

    if (is.list(input_var)){
      tmp = merge(input_var[grouping_var == unique(grouping_var)[1]] %>%
                    unlist() %>%
                    table() %>%
                    as.data.frame(),
                  input_var[grouping_var == unique(grouping_var)[2]] %>%
                    unlist() %>%
                    table() %>%
                    as.data.frame(),
                  by = ".",
                  all =T)

      tmp[is.na(tmp)] = 0

      tmp = as.matrix(tmp[,c(2,3)])

      output_stats[[4]] = fisher.test(tmp)$p.value %>% round(3)
    }else{
      output_stats[[4]] = fisher.test(input_var, grouping_var)$p.value %>% round(3)
    }
  }

  return(output_stats)

}


make_dummies_from_list = function(list_var){
  var_variants = list_var %>% unlist() %>% unique() %>% extract(order(.))

  dummy_return = matrix(nrow = length(list_var), ncol = length(var_variants))

  colnames(dummy_return) = var_variants

  for(x in seq_along(list_var)){
    for(y in seq_along(var_variants)){
      if(var_variants[y] %in% list_var[[x]]){
        dummy_return[x,y] = 1
      }
    }
  }

  dummy_return[is.na(dummy_return)] = 0

  dummy_return %<>% as.data.frame()

  return(dummy_return)
}






