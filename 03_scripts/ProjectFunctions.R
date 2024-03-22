##### ----- Functions for the GTI project

f_LibraryLoader(tidyverse, 
                rlang,
                extrafont)

#------------------------------------------------------------------------------
# f_GetTT: Gets the latest TT data from the database, making sure to not pull data after the cutoff date
f_GetTT <- function(datadate,pulldate) {
  df = iepg_tt() %>%
    filter(start_date <= datadate,
           date_added_by_iep <= pulldate,
           last_appeared_in_tt >= pulldate) %>%
    group_by(id) %>%
    filter(date_added_by_iep == max(date_added_by_iep)) %>%
    ungroup()
  return(df)
}

#-------------------------------------------------------------------------------
# - f_ConflictPriorities: Gives priority to tidyverse functions. Will remove issues with filter(), select() etc.
f_ConflictPriorities <- function() {
  
  # Check if 'conflicted' package is installed, install if not
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
# - f_LibraryLoader: Checks to see if packages are loaded before loading them. Use at the start of every script.
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
# - f_DownloadCandidate: Downloads the UCDP monthly Candidate Datasets and Combines into a single dataframe
f_DownloadCandidate <- function() {
  base_url <- paste0("https://ucdp.uu.se/downloads/candidateged/GEDEvent_v", VER_CANDIDATE, "_0_")
  save_dir <- paste0(ONEDRIVE, "/Data/Conflict")
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
  
  # Checks to see if the file already exists
  is_valid_file <- function(file_path) {
    data <- read.csv(file_path, nrows = 10) 
    expected_columns = GED_KEEP
    all(expected_columns %in% names(data))
  }
  
  # If the file already exists, import and clean
  process_file <- function(file_path) {
    data <- rio::import(file_path)
    data %>% 
      dplyr::filter(code_status == "Clear") %>%
      dplyr::select(all_of(GED_KEEP))
  }
  
  # Combine with existing data
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
  save_dir <- paste0(ONEDRIVE, "/Data/Conflict")
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
# f_ScoringBands: Creates the scoring bands for the web data
f_ScoringBands <- function(value, bands) {
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
# f_ThemeGTI: Sets themes for GTI charts. Allows for easy customisation
f_ThemeGTI <- function(plot, chart_info, plottitle, xaxis, yaxis, xgridline, ygridline) {
  finalcaption <- paste0("Source: ", chart_info[["source"]])
  
  plot_labels <- labs(
    title = chart_info[["title"]],
    x = chart_info[["xtext"]],
    y = chart_info[["ytext"]],
    caption = finalcaption
  )
  
  plot_base <- theme_minimal()
  
  plot_theme <- plot_base +
    theme(text = element_text(family = HEAVY_FONT),
          plot.title.position = "plot",
          plot.subtitle = element_text(size = 9),
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0, colour = "#888686", size = 7),
          axis.text = element_text(colour = "#444444", size = 6.5, family = LIGHT_FONT),
          axis.title = element_text(face = "bold", size = 7, family = HEAVY_FONT),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 7, family = LIGHT_FONT),
          axis.line.x = element_blank(),
          axis.line.y = element_blank()
    )
  
  if (plottitle == "Include") {
    plot_theme <- plot_theme + theme(plot.title = element_text(size = 13, family = HEAVY_FONT))
  } else {
    plot_theme <- plot_theme + theme(plot.title = element_blank())
  }
  
  if (xaxis == "Include") {
    plot_theme <- plot_theme + theme(axis.line.x.bottom = element_line(colour = "#444444"))
  }
  
  if (yaxis == "Include") {
    plot_theme <- plot_theme + theme(axis.line.y = element_line(colour = "#444444"))
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
  
  # Apply themes and labels to the plot
  plot <- plot + plot_labels + plot_theme
  
  # Adjust y-axis to position x-axis line at y=0
  if (xaxis == "Include") {
    plot <- plot + scale_y_continuous(expand = c(0,0))
  }
  
  return(plot)
}

#-------------------------------------------------------------------------------
# f_GTISavePlots: Saves the GTI charts in the correct format in three different sizes
f_GTISavePlots <- function(chart_title, plot_name) {
  
  if (chart_title["type"] == "Chart") {
  # Looping through the three chart sizes
    for (size_name in names(GTI_CHARTS)) {
      size <- GTI_CHARTS[[size_name]]
      file_base_name <- paste0(chart_title["sheet"], "_", size_name)
      
      # Save as PNG in CHARTS_PATH with transparency
      png_file <- paste0(IMAGE_FILES, "/", file_base_name, ".png")
      ggsave(png_file, plot_name, device = "png", width = size["width"], height = size["height"], units = CHART_UNIT, bg = "transparent")
      
      # Save as SVG in ONEDRIVE_PATH
      svg_file <- paste0(CHART_FILES, "/", file_base_name, ".svg")
      ggsave(svg_file, plot_name, device = "svg", width = size["width"], height = size["height"], units = CHART_UNIT)
    }
  }
  
  if (chart_title["type"] == "Map") {
    # Looping through the three chart sizes
    for (size_name in names(GTI_MAPS)) {
      size <- GTI_MAPS[[size_name]]
      file_base_name <- paste0(chart_title["sheet"], "_", size_name)
      
      # Save as PNG in CHARTS_PATH with transparency
      png_file <- paste0(IMAGE_FILES, "/", file_base_name, ".png")
      ggsave(png_file, plot_name, device = "png", width = size["width"], height = size["height"], units = CHART_UNIT, bg = "transparent")
      
      # Save as SVG in ONEDRIVE_PATH
      svg_file <- paste0(MAP_FILES, "/", file_base_name, ".svg")
      ggsave(svg_file, plot_name, device = "svg", width = size["width"], height = size["height"], units = CHART_UNIT)
    }
  }
  
  if (chart_title["type"] == "Diagram") {
    # Looping through the three chart sizes
    for (size_name in names(GTI_CHARTS)) {
      size <- GTI_CHARTS[[size_name]]
      file_base_name <- paste0(chart_title["sheet"], "_", size_name)
      
      # Save as SVG in ONEDRIVE_PATH
      svg_file <- paste0(CHART_FILES, "/", file_base_name, ".svg")
      svg_temp = DiagrammeRsvg::export_svg(plot_name)
      write_lines(svg_temp,svg_file)
      
      #convert to png
      rsvg_png(svg_file, paste0(IMAGE_FILES, "/", file_base_name, ".png"))
      
      
    }
  }
  
}

#-------------------------------------------------------------------------------
# f_LabelFormatter: Helper function for chart labelling
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
# f_ShortenName: Shortens long names in TT variables, eg. "Law Enforcement/Internal Security" would become "Police
f_ShortenName <- function(name,dict) {
    if (name %in% names(dict)) {
      return(dict[name])
    } else {
      return(name)  # return the original name if not found in the dictionary
    }
}

#-------------------------------------------------------------------------------  
#f_LatLongShape: Takes lat long points and generates variable with admin 0, 1, or 2
f_LatLongShape = function(df, admin){
  
  message("GEO MATCHING CAN TAKE SOME TIME IF YOUR DATASET HAS A LOT OF ROWS.")
  message("IF THIS FREEZES YOUR COMPUTER YOU MAY WANT TO CHUNK UP YOUR DATA FRAME INTO SMALLER TABLES AND LOOP THROUGH.")
  missing = is.na(df$longitude) | is.na(df$latitude)
  df_missing_geo <- NULL # Initialize df_missing_geo as NULL
  
  if (sum(missing) > 0) {
    message(paste("There are", sum(missing), "rows that are missing geolocations...."))
    message("These rows will still be included in the returned df, but with NA values for admin ID variables")
    
    # Subset rows with missing geolocations
    df_missing_geo <- df[missing, ]
    df_missing_geo$admin_ID <- NA
    df_missing_geo$admin_Name <- NA
    df_missing_geo$admin_level <- NA
    
    # Process rows with complete geolocations
    df <- df[!missing, ]
  }
  
  gadm = iepg_get_gadm(admin)
  gadm = st_make_valid(gadm) # fix issues with overlapping polygons
  invalid_indices <- which(!st_is_valid(gadm)) # remove any polygons that are still invalid
  gadm = gadm[-invalid_indices,]
  
  df2 = st_as_sf(df, coords=c("longitude","latitude"), crs=st_crs(gadm))
  key <- st_nearest_feature(df2, gadm)
  gadm = gadm %>% st_drop_geometry()
  df$admin_ID = gadm[as.numeric(key), "geocode"]
  df$admin_Name = gadm[as.numeric(key), "geoname"]
  df$admin_level = admin
  
  # Conditionally combine the dataframes
  if (!is.null(df_missing_geo)) {
    df <- rbind(df, df_missing_geo)
  }
  
  return(df)
}


#-------------------------------------------------------------------------------
#f_dbIEPCombine: Combines multiple IEP datasets into one dataframe
f_dbIEPCombine <- function(id_range, index_name) {
  # List to store each dataframe
  list_of_dfs <- lapply(id_range, function(id) {
    iepg_get(id)
  })
  
  # Combine all dataframes into one and then perform the additional manipulations
  combined_df <- bind_rows(list_of_dfs) %>%
    ungroup() %>%
    select(
      geocode,
      country = geoname,
      year,
      indicator = variablename,
      type = disaggregation,
      value
    ) %>%
    mutate(index = index_name)
  
  return(combined_df)
}

#-------------------------------------------------------------------------------
#' f_PowerPlot: Makes a log-log plot of deaths vs cumulative count for a specific
#' value of a variable

f_PowerPlot <- function(df, variable, selection) {
  variable_name <- as_label(enquo(variable))
  
  df <- df %>%
    filter(!!enquo(variable) == selection)
  
  # Define chart_info
  CHART_PowerPlotF <- list(
    title = paste0("Cumulative Distribution of Deaths from Terrorism: ", variable_name, " = ", selection),
    sheet = "Power_Plot",
    source = "Your Source Here",  # Replace with your source
    xtext = "DEATHS PER ATTACK",
    ytext = "CUMULATIVE COUNT OF EVENTS"
  )
  
  df <- df %>%
    group_by(deaths_total) %>%
    summarise(count = n(), .groups = 'drop') %>%
    arrange(desc(deaths_total))
  
  df$cumulative_count <- cumsum(df$count)
  
  p <- ggplot(df, aes(x = deaths_total, y = cumulative_count)) +
    geom_line() +
    geom_point()
  
  pCHART_ACTIVE <- f_ThemeGTI(p,
                              CHART_PowerPlotF,
                              "Include",
                              "Include",
                              "Include",
                              "Include",
                              "Include") +
    scale_x_log10() +
    scale_y_log10(labels = scales::comma)
  
  print(pCHART_ACTIVE)
}

## -- f_ScatterLabels ----------------------------------------------------------

f_ScatterLabels <- function(pScatter, xaxis = "Include", yaxis = "Include", 
                            left_text = "", right_text = "", up_text = "", down_text = "", xposition = 0.07, yposition = 0.045) {
  
  ARROW_LENGTH = 0.05
  LEFT_ARROW_START = 0.12
  LEFT_ARROW_END = LEFT_ARROW_START + ARROW_LENGTH
  RIGHT_ARROW_END = 0.97
  RIGHT_ARROW_START = RIGHT_ARROW_END - ARROW_LENGTH
  UP_ARROW_END = 0.96
  UP_ARROW_START = UP_ARROW_END - ARROW_LENGTH
  DOWN_ARROW_START = 0.12
  DOWN_ARROW_END = DOWN_ARROW_START + ARROW_LENGTH
  LEFT_TEXT = LEFT_ARROW_END + 0.01
  RIGHT_TEXT = RIGHT_ARROW_START - 0.01
  UP_TEXT = UP_ARROW_START - 0.01
  DOWN_TEXT = DOWN_ARROW_END + 0.01

  
  # Using ggdraw to add annotations conditionally
  p <- ggdraw(pScatter)
  
  if (xaxis == "Include") {
    right_arrow <- linesGrob(x = unit(c(RIGHT_ARROW_START, RIGHT_ARROW_END), "npc"), y = unit(c(xposition, xposition), "npc"),
                             arrow = arrow(ends = "last", type = "closed", length = unit(2, "mm")))
    left_arrow <- linesGrob(x = unit(c(LEFT_ARROW_START, LEFT_ARROW_END), "npc"), y = unit(c(xposition, xposition), "npc"),
                            arrow = arrow(ends = "first", type = "closed", length = unit(2, "mm")))
    p <- p +
      draw_grob(right_arrow) +
      draw_grob(left_arrow) +
      draw_label(left_text, x = LEFT_TEXT, y = xposition, hjust = 0,vjust = 0.25, fontface = "bold", size = 6, fontfamily = HEAVY_FONT) +
      draw_label(right_text, x = RIGHT_TEXT, y = xposition, hjust = 1, vjust = 0.25, fontface = "bold", size = 6, fontfamily = HEAVY_FONT)
  }
  
  if (yaxis == "Include") {
    up_arrow <- linesGrob(x = unit(c(yposition, yposition), "npc"), y = unit(c(UP_ARROW_START, UP_ARROW_END), "npc"),
                          arrow = arrow(ends = "last", type = "closed", length = unit(2, "mm")))
    down_arrow <- linesGrob(x = unit(c(yposition, yposition), "npc"), y = unit(c(DOWN_ARROW_START, DOWN_ARROW_END), "npc"),
                            arrow = arrow(ends = "first", type = "closed", length = unit(2, "mm")))
    p <- p +
      draw_grob(up_arrow) +
      draw_grob(down_arrow) +
      draw_label(down_text, x = yposition, y = DOWN_TEXT, hjust = 0, vjust = 0.25, angle = 90, fontface = "bold", size = 6, fontfamily = HEAVY_FONT) +
      draw_label(up_text, x = yposition, y = UP_TEXT, hjust = 1, vjust = 0.25, angle = 90, fontface = "bold", size = 6, fontfamily = HEAVY_FONT)
  }
  
  return(p)
}

## -- f_CalculateCorrelations --------------------------------------------------
f_CalculateCorrelations <- function(df) {
  df %>%
    pivot_wider(names_from = variablename, values_from = value) %>%
    select(where(is.numeric)) %>%
    select(-year) %>%
    cor(use = "complete.obs") %>%
    .[, "GTI", drop = FALSE] %>%
    as_tibble(rownames = "Indicator") %>%
    rename(Correlation = GTI)
}

#-------------------------------------------------------------------------------
# f_GTIChartbook: Creates the GTI chartbook
f_GTIChartbook <- function(filepath) {
  {
    wb <- createWorkbook()
    addWorksheet(wb, "default")
    saveWorkbook(wb, filepath, overwrite = TRUE)
  }
}

#-------------------------------------------------------------------------------
# f_GTISheet: Creates a new sheet in the GTI chartbook in the appropriate format
f_GTISheet <- function(workbook, chart_name) {
  
  sheet_name = chart_name["sheet"]
  
  # Check if the sheet already exists
  existing_sheets <- sheets(workbook)
  if (!(sheet_name %in% existing_sheets)) {
    addWorksheet(workbook, sheet_name)
  }
  
  # Define and apply styles
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
# f_GTISheetData: Add data to the GTI chart book
f_GTISheetData <- function(df, workbook, chart_info) {
  writeData(workbook, sheet = chart_info["sheet"], 
            x = df, startCol = 3, startRow = 9)
}

#-------------------------------------------------------------------------------
# f_GTISheetImage: Copies an image of the chart to the worksheet as a reference
f_GTISheetImage <- function(workbook, chart_info) {
  insertImage(workbook,chart_info[["sheet"]],paste0(IMAGE_FILES,"/",chart_info[["sheet"]],"_small.png"),
              startRow = 9, startCol = 10)
}

## -- f_ChartExport ----------------------------------------------------------
f_ProjectExport <- function(section, workbook, spreadsheet, chartlist) {
  
  for (chart_info in chartlist) {
    
    # Transform string into variable
    chart_var = get(chart_info, envir = .GlobalEnv)
    
    if (chart_var['position'] == "Normal") {
      if (chart_var['type'] %in% c("Chart", "Diagram", "Map")) {
        figure_count = figure_count + 1
        figure_xx = paste0(section, ".", figure_count)
        chart_var['counter'] = figure_xx
        chart_var['sheet'] = paste0(figure_xx, "_", chart_var['sheet'])
      } else if (chart_var['type'] == "Table") {
        table_count = table_count + 1
        table_xx = paste0(section, ".", table_count)
        chart_var['counter'] = table_xx
        chart_var['sheet'] = paste0(table_xx, "_", chart_var['sheet'])
      } else {
        chart_var['counter'] = section
      }
    }
    
    if (nchar(chart_var['sheet']) > 31) {
      chart_var['sheet'] = substr(chart_var['sheet'], 1, 31)
    }
    
    if (chart_var["type"] == "Chart") {
      chart_name = paste0("p",chart_info)
      chart_plot = get(chart_name, envir = .GlobalEnv)
      chart_data = paste0(chart_info,".df")
      chart_df = get(chart_data, envir = .GlobalEnv)
      f_GTISheet(workbook,chart_var)
      f_GTISheetData(chart_df, workbook, chart_var)
      f_GTISavePlots(chart_var, chart_plot)
      Sys.sleep(1)
      f_GTISheetImage(workbook = workbook, chart_info = chart_var)
    }
    
    # If type is table
    if (chart_var["type"] == "Table") {
      chart_data = paste0(chart_info,".df")
      chart_df = get(chart_data, envir = .GlobalEnv)
      f_GTISheet(workbook,chart_var)
      f_GTISheetData(chart_df, workbook, chart_var)
      rio::export(chart_df,paste0(TABLE_FILES,"/",chart_var[["sheet"]],".csv"), row.names = FALSE)
    }
    
    # If type is Map or Diagram
    if (chart_var["type"] == "Map" | chart_var["type"] == "Diagram") {
      chart_name = paste0("p",chart_info)
      chart_plot = get(chart_name, envir = .GlobalEnv)
      f_GTISheet(workbook,chart_var)
      f_GTISavePlots(chart_var, chart_plot)
      Sys.sleep(1)
      f_GTISheetImage(workbook = workbook, chart_info = chart_var)
    }
  }
  
  # save the workbook
  saveWorkbook(workbook, spreadsheet, overwrite = TRUE)
  
  # Assign current count for this section to a variable in the global environment
  assign(paste0("figure_count"), figure_count, envir = .GlobalEnv)
  assign(paste0("table_count"), table_count, envir = .GlobalEnv)
}

#-------------------------------------------------------------------------------
f_top10ShapeFiles <- function(country_codes) {
  base_url <- "https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41_"
  destination_folder <- "02_data/raw/"
  file_first <- "gadm41_"
  file_last <- "_shp.zip"
  
  
  if (!dir.exists(destination_folder)) {
    dir.create(destination_folder, recursive = TRUE)
  }
  
  for (code in country_codes) {
    file_name <- paste0("gadm41_", code, "_shp.zip")
    full_url <- paste0(base_url, code, "_shp.zip")
    destination_path <- paste0(destination_folder, file_name)
    
    if (!file.exists(destination_path)) {
      message(paste("Downloading:", file_name))
      download.file(full_url, destfile = destination_path, mode = "wb")
      message(paste("Download completed:", file_name))
      unzip(zipfile = paste0(destination_folder,file_first,code,file_last), exdir = paste0(destination_folder,"/Shapefiles"))
    } else {
      message(paste("File already exists:", file_name))
    }
  }
}

# -- REGIONAL TABLES -----------------------------------------------------------

f_RegionalTable <- function(df,REGION) {
  df_region = df %>%
    filter(region == REGION) %>%
    select(-region)
  
  # Calculate the averages for Change10 and Change1
  avg_change10 <- round(mean(df$change_10, na.rm = TRUE), 3)
  avg_change1 <- round(mean(df$change_1, na.rm = TRUE), 3)
  
  # Create new row with averages
  bottom_row <- data.frame(Country = "Regional Average", 
                           `Overall Score` = NA, 
                           `Overall Rank` = NA, 
                           change_10 = avg_change10, 
                           change_1 = avg_change1,
                           check.names = FALSE)
  
  # Add the new row to the dataframe
  df_region <- rbind(df_region, bottom_row) %>%
    select(Country,`Overall Score`,`Overall Rank`, change_10, change_1) %>%
    rename(!!sym(paste0("Change ", GTI_YEAR - 10, "-", GTI_YEAR)) := change_10,
           !!sym(paste0("Change ", GTI_YEAR - 1, "-", GTI_YEAR)) := change_1) %>%
    arrange(`Overall Rank`)
  
  return(df_region)
}

## -- f_ChangeMapTheme ---------------------------------------------------------

f_ChangeMapTheme <- function (base_size = 9, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.line = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(), axis.title = element_blank(),
          panel.grid = element_blank(), panel.border = element_blank(),
          panel.spacing = unit(0, "lines"),  strip.background = element_blank(),
          legend.justification = c(0, 0), legend.position = c(0,0))
}


## -- f_FullChartList ----------------------------------------------------------

f_FullChartList <- function() {
  chart_objects <- ls(pattern = "^CHART_") # List objects starting with "CHART_"
  return(chart_objects)
}


# -- SETUP FUNCTIONS -----------------------------------------------------------
f_ConflictPriorities() 
