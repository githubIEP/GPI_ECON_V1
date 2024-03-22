f_ClassTable <- function(df) {
  df.table <- df %>%
    summarise_all(list(~unique(class(.)))) %>%
    gather(variable, data_type) %>%
    arrange(variable)
  return(df.table)
}

ml_classes = f_ClassTable(ml.df)