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