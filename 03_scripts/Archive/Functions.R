##### ----- Helper Functions for the GTI project

#-------------------------------------------------------------------------------
# - f_ConflictPriorities: Gives priority to tidyverse functions
f_ConflictPriorities <- function() {
  # Check if 'conflicted' is installed, install if not
  if (!requireNamespace("conflicted", quietly = TRUE)) {
    install.packages("conflicted")
  }
  library(conflicted)
  
  # Set conflict preferences using the global CONFLICT_PRIORITY
  for (conflict_func in names(CONFLICT_PRIORITY)) {
    conflict_prefer(conflict_func, CONFLICT_PRIORITY[[conflict_func]])
  }
}
#-------------------------------------------------------------------------------
# - f_LibraryLoader: Checks to see if packages are loaded before loading them
f_LibraryLoader <- function(...) {
  args <- substitute(list(...))[-1] # Capture the unquoted arguments
  package_names <- sapply(args, function(arg) {
    if (is.character(arg)) {
      return(arg) # Return the argument if it's a string
    } else {
      return(as.character(arg)) # Convert to string if it's a symbol
    }
  })
  
  for (package in package_names) {
    if (!package %in% rownames(installed.packages())) {
      stop(paste("Package not installed:", package))
    }
    
    if (!package %in% .packages()) {
      library(package, character.only = TRUE)
    }
  }
}

#-------------------------------------------------------------------------------
# - f_DownloadCandidate: Downloads the UCDP Candidate Dataset and Combines
f_DownloadCandidate <- function() {
  base_url <- paste0("https://ucdp.uu.se/downloads/candidateged/GEDEvent_v", VER_CANDIDATE, "_0_")
  save_dir <- paste0(ONEDRIVE, "/Data")
  combined_data <- NULL
  
  # Helper Functions
  download_file <- function(url, file_path) {
    tryCatch({
      download.file(url, destfile = file_path, mode = "wb")
    }, warning = function(w) {
      message("Warning for URL: ", url, "\n", w)
    }, error = function(e) {
      message("Failed to download data for URL: ", url, ": ", e$message)
    })
  }
  
  is_valid_file <- function(file_path) {
    data <- read.csv(file_path, nrows = 10) 
    expected_columns = GED_KEEP
    all(expected_columns %in% names(data))
  }
  
  process_file <- function(file_path) {
    data <- rio::import(file_path)
    data %>% 
      dplyr::filter(code_status == "Clear") %>%
      dplyr::select(all_of(GED_KEEP))
  }
  
  combine_data <- function(combined_data, data) {
    if (is.null(combined_data)) {
      data
    } else {
      dplyr::bind_rows(combined_data, data)
    }
  } # Closing brace for combine_data function
  
  # Create file directory if necessary
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  # Data Process Loop
  for (month in 1:12) {
    file_name <- paste0("GEDEvent_v23_0_", month, ".csv")
    file_path <- file.path(save_dir, file_name)
    url <- paste0(base_url, month, ".csv")
    
    if (!file.exists(file_path)) {
      download_file(url, file_path)
    }
    
    if (file.exists(file_path) && is_valid_file(file_path)) {
      data <- process_file(file_path)
      combined_data <- combine_data(combined_data, data)
    }
  }
  
  return(combined_data)
}

#-------------------------------------------------------------------------------
# f_DownloadGED: Downloads the UCDP GED Dataset
f_DownloadGED <- function(){
  
  #filepaths
  file_name <- paste0("ged",VER_GED,"-rds.zip")
  base_url <- paste0("https://ucdp.uu.se/downloads/ged/",file_name)
  save_dir <- paste0(ONEDRIVE, "/Data")
  file_path <- file.path(save_dir, file_name)

  # Create file directory if necessary
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  # Check to see if file already exists, download it if it doesn't
  if (!file.exists(file_path)) {
    download.file(base_url, file_path)
  }
  
  # If file already exists, tidy it and save it
  if (file.exists(file_path)) {
    data <- rio::import(file_path) %>%
      dplyr::select(all_of(GED_KEEP))
  }
  
  return(data)
}

#-------------------------------------------------------------------------------
f_WebBands <- function(value, bands) {
  case_when(
    value == bands[1] ~ 1,
    value > bands[1] & value <= bands[2] ~ 2,
    value > bands[2] & value <= bands[3] ~ 3,
    value > bands[3] & value <= bands[4] ~ 4,
    value > bands[4] & value <= bands[5] ~ 5,
    TRUE ~ 6  # Assumes value is greater than the last band
  )
}

#-------------------------------------------------------------------------------
f_ThemeGTI <- function(plot, 
                       chart_info,
                       plottitle,
                       xaxis, 
                       yaxis, 
                       xgridline, 
                       ygridline) {
  
  finalcaption <- paste0("Source: ", chart_info[["source"]])
  
  plot_labels =labs(title = chart_info[["title"]],
                    x = chart_info[["xtext"]],
                    y = chart_info[["ytext"]],
                    caption = finalcaption)
  
  plot_base <- theme_minimal()
  
  plot_theme <- plot_base +
    theme(plot.title.position = "plot",
          plot.subtitle = element_text(size = 12),
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0, colour = "#888686", size = 7),
          axis.text = element_text(colour = "#444444", size = 9),
          axis.title = element_text(face = "bold", size = 10),
          panel.grid.minor = element_blank(),
          legend.title = element_blank()
    )

  if (plottitle == "Include") {
    plot_theme <- plot_theme + theme(plot.title = element_text(face = "bold", size = 16))
  } else {
    plot_theme <- plot_theme + theme(plot.title = element_blank())
  }
  
  if (xaxis == "Include") {
    plot_theme <- plot_theme + theme(axis.line.x = element_line(colour = "#444444"))
  } else {
    plot_theme <- plot_theme + theme(axis.line.x = element_blank())
  }
  
  if (yaxis == "Include") {
    plot_theme <- plot_theme + theme(axis.line.y = element_line(colour = "#444444"))
  } else {
    plot_theme <- plot_theme + theme(axis.line.y = element_blank())
  }
  
  if (ygridline == "Include") {
    plot_theme <- plot_theme + theme(panel.grid.major.y = element_line(colour = "lightgrey"))
  } else {
    plot_theme <- plot_theme + theme(panel.grid.major.y = element_blank())
  }
  
  if (xgridline == "Include") {
    plot_theme <- plot_theme + theme(panel.grid.major.x = element_line(colour = "lightgrey"))
  } else {
    plot_theme <- plot_theme + theme(panel.grid.major.x = element_blank())
  }
  return(plot + plot_labels + plot_theme) 
}
#-------------------------------------------------------------------------------
f_GTISavePlots <- function(chart_title,plot_name) {
  
  # Looping through the three chart sizes
  for (size_name in names(GTI_CHARTS)) {
    size <- GTI_CHARTS[[size_name]]
    file_base_name <- paste0(chart_title["sheet"], "_", size_name)
    
    # Save as JPEG in CHARTS_PATH
    ggsave(paste0(o_CHARTS, file_base_name, ".jpg"), plot_name, 
           device = "jpeg", width = size["width"], height = size["height"], units = CHART_UNIT)
    
    # Save as PDF in ONEDRIVE_PATH
    ggsave(paste0(gd_CHARTS, "/", file_base_name, ".pdf"), plot_name, 
           device = "pdf", width = size["width"], height = size["height"], units = CHART_UNIT)
    
  }
}

#-------------------------------------------------------------------------------
f_LabelFormatter <- function(x) {
  ifelse(x < 0, paste0("-", abs(x)), as.character(x))
}

#-------------------------------------------------------------------------------
# f_TopNCountries: Returns the top N countries for a given year and indicator
f_TopNCountries <- function(df, N, gtiyear, indicator) {
  
  # Convert indicator to symbol
  indicator <- sym(indicator)
  
  # Filter for selected year
  df <- df %>% 
    filter(year == gtiyear) 
  
  # Get top N countries
  df1 <- df %>%
    select(country, !!indicator) %>%
    filter(!is.na(!!indicator)) %>%
    top_n(N, !!indicator) %>%
    arrange(desc(!!indicator))
  
  # Get sum of all other countries
  df2 <- df %>%
    select(country, !!indicator) %>%
    filter(!is.na(!!indicator)) %>%
    anti_join(df1, by = "country") %>%
    summarise(country = "All other countries", !!indicator := sum(!!indicator))
  
  # Combine the two dataframes
  df3 <- df1 %>%
    bind_rows(df2) %>%
    mutate(year = gtiyear)
  
  return(df3)
}

#-------------------------------------------------------------------------------

f_GTIChartbook <- function(filepath) {
   {
    wb <- createWorkbook()
    addWorksheet(wb, "default")
    saveWorkbook(wb, filepath, overwrite = TRUE)
  }
}

#-------------------------------------------------------------------------------


f_GTISheet <- function(workbook, chart_name) {
  
  sheet_name = chart_name["sheet"]
  addWorksheet(workbook, sheet_name)
  
  # Define and apply styles (as per your previous requirement)
  font_style <- createStyle(fontSize = 8, fontName = "Arial")
  addStyle(workbook, sheet = sheet_name, style = font_style, rows = 1:1000, cols = 1:100, gridExpand = TRUE)
  
  setColWidths(workbook, sheet = sheet_name, cols = 1, widths = 1)      # Column 1 width
  setColWidths(workbook, sheet = sheet_name, cols = 2, widths = "auto")  # Auto width for Column B
  
  bold_style <- createStyle(fontName = "Arial", fontSize = 8, textDecoration = "bold")
  addStyle(workbook, sheet = sheet_name, style = bold_style, rows = 1:1000, cols = 2, gridExpand = TRUE)
  
  # Add specific cell text
  writeData(workbook, sheet = sheet_name, x = "Title", startCol = 2, startRow = 2)
  writeData(workbook, sheet = sheet_name, x = "sub-title", startCol = 2, startRow = 3)
  writeData(workbook, sheet = sheet_name, x = "x-axis title", startCol = 2, startRow = 4)
  writeData(workbook, sheet = sheet_name, x = "y-axis title", startCol = 2, startRow = 5)
  writeData(workbook, sheet = sheet_name, x = "source", startCol = 2, startRow = 6)
  writeData(workbook, sheet = sheet_name, x = "Notes", startCol = 2, startRow = 7)
  writeData(workbook, sheet = sheet_name, x = chart_name["title"], startCol = 3, startRow = 2)
  writeData(workbook, sheet = sheet_name, x = chart_name["xtext"], startCol = 3, startRow = 4)
  writeData(workbook, sheet = sheet_name, x = chart_name["ytext"], startCol = 3, startRow = 5)
  writeData(workbook, sheet = sheet_name, x = chart_name["source"], startCol = 3, startRow = 6)
}

#-------------------------------------------------------------------------------
f_GTISheetImage <- function() {
  insertImage(tc_wb,CHART_ACTIVE[["sheet"]],paste0(o_CHARTS,CHART_ACTIVE[["sheet"]],"_small.jpg"),
              startRow = 9, startCol = 5)
}

