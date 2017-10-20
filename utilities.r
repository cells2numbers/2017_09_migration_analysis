read_single_cell_data <- function(analysis_folder, csv_single_cells){
  df <- data.frame()
  for (iDir in list.dirs(analysis_folder)) {
    file_name <-  file.path(iDir, csv_single_cells)
    if (file.exists(file_name)) {
      df <- rbind(df,suppressMessages(read_csv(file_name)))
    }
  }
  return(df)
}


add_metadata_matrix <- function(df1, strata){
    Metadata_matrix <- data.frame(
      Metadata_id =  c("bd0036", "bd0037", "bd0067", "bd0068", "ej0003", "gn0018", "gn0026", "gn0028", "gn0048", "gn0049"),
      Metadata_matrix = c("FN","FN","HEM","HEM","HEM","FN","FN","FN","HEM","HEM")
    ) 
  return(
    inner_join(df1, Metadata_matrix, by = "Metadata_id")
    )
}



fraction_strong_affinity <- function(W, metadata_rows, strata) {
  
  Wt <-  W %>% 
    cbind(.,metadata_rows) %>%
    group_by_(.dots = strata) %>%
    summarise_all(funs(median)) %>%
    ungroup() %>%
    select_(.dots = paste('-', strata)) %>%
    t 
  
  M <- cbind(metadata_rows, Wt) %>%
    group_by_(.dots = strata ) %>%
    summarise_all(funs(median)) %>%
    ungroup() %>%
    select_(.dots = paste('-', strata)) %>%
    as.matrix()
  
  threshold <- M[lower.tri(M)] %>%
    quantile(0.95)
  
  tibble(threshold = threshold, fraction_strong = sum(diag(M) > threshold) / nrow(M), mean_affinity = mean(diag(M)))
}
