f_ProjectExport <- function(section, workbook, spreadsheet, chartlist) {
  
  #Initialise Counters
  figure_counter = NULL
  table_counter = NULL
  
  for (project_chart in chartlist) {
    if (project_chart['location'] == "Standard"){
      figure_counter = figure_counter + 1
      table_counter = table_counter + 1
      figure_xx = paste0(section,".",figure_counter)
      table_xx = paste0(section,".",table_counter)
    if (project_chart['type'] == "Table") {
        project_chart['counter'] == table_xx
    else {
        project_chart['counter'] == figure_xx
      }
      }
    }
    
  if (project_chart['type'])
  }
}