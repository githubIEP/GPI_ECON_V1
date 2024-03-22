
###  Country Profile Headers  
tmp_name = focus_country

tmp_rank = gti_national %>% 
  dplyr::filter(country %in% focus_country, year==CURRENT_YEAR) %>%
  pull(rank)

tmp_deaths = gti_national %>%
  dplyr::filter(country %in% focus_country,
                year == CURRENT_YEAR) %>%
  pull(deaths_total)

tmp_injured = gti_national %>%
  dplyr::filter(country %in% focus_country,
                year == CURRENT_YEAR) %>%
  pull(injured_total)

tmp_incidents = gti_national %>%
  dplyr::filter(country %in% focus_country,
                year == CURRENT_YEAR) %>%
  pull(incidents_total)

tmp_banded = gti_national %>%
  dplyr::filter(country %in% focus_country,
                year == CURRENT_YEAR) %>%
  pull(banded_score)

# Generate table to go with the country plots 
tmp = data.frame(" " = c("","",tmp_name,""),
                 " " =c("","GTI RANK",tmp_rank,""),
                 " " =c("","GTI SCORE",round(tmp_banded,3),""),
                 " " =c("", tmp_deaths,tmp_injured,tmp_incidents),
                 " " =c("", "DEAD","INJURED","INCIDENTS"))

writeDataTable(wb, sheet = as.character(focus_code), x = tmp, startCol = 13, startRow = 1)

names(tmp)= NULL

text_table_tmp = ggtexttable(tmp, rows = NULL, theme = ttheme("blank", base_size = 30))

# Generate specific cells 
tab <- table_cell_font(text_table_tmp, row = 3, column = 1,
                       face = "bold",
                       size=30) 

tab <- table_cell_font(tab, row = 3, column = 2,
                       face = "bold", color = "red",
                       size=20)

tab <- table_cell_font(tab, row = 3, column = 3,
                       face = "bold", color = "black",
                       size=20)

tab <- table_cell_font(tab, row = c(2,3,4), column = c(4,5),
                       face = "bold", color = "grey55",
                       size=20)

tab <- table_cell_font(tab, row = c(2), column = c(2,3),
                       face = "bold", color = "grey55",
                       size=20)

text_table_tmp = tab
text_table_tmp


# Generate the full graph 
p4 = plot_spacer() + text_table_tmp +  plot_spacer()
print(p4)
insertPlot(wb, as.character(focus_code), height = HEIGHT, width = WIDTH, fileType = "jpg",
           startRow = 20,
           startCol = 1)

print(p)
insertPlot(wb, as.character(focus_code), height = HEIGHT, width = WIDTH, fileType = "jpg",
           startRow = 20,
           startCol = 1)

}

# Save the workbook
saveWorkbook(wb, file = paste0(CHARTS_PATH, "GTI-Section-1-chart-file.xlsx"), overwrite = TRUE)
  
  

  
  ###  Country Profile Headers  
  tmp_name = focus_country
  
  tmp_rank = gti_national %>% 
    dplyr::filter(country %in% focus_country, year==CURRENT_YEAR) %>%
    pull(rank)
  
  tmp_deaths = gti_national %>%
    dplyr::filter(country %in% focus_country,
                  year == CURRENT_YEAR) %>%
    pull(deaths_total)
  
  tmp_injured = gti_national %>%
    dplyr::filter(country %in% focus_country,
                  year == CURRENT_YEAR) %>%
    pull(injured_total)
  
  tmp_incidents = gti_national %>%
    dplyr::filter(country %in% focus_country,
                  year == CURRENT_YEAR) %>%
    pull(incidents_total)
  
  tmp_banded = gti_national %>%
    dplyr::filter(country %in% focus_country,
                  year == CURRENT_YEAR) %>%
    pull(banded_score)
  
  # Generate table to go with the country plots 
  tmp = data.frame(" " = c("","",tmp_name,""),
                   " " =c("","GTI RANK",tmp_rank,""),
                   " " =c("","GTI SCORE",round(tmp_banded,3),""),
                   " " =c("", tmp_deaths,tmp_injured,tmp_incidents),
                   " " =c("", "DEAD","INJURED","INCIDENTS"))
  
  writeDataTable(wb, sheet = as.character(focus_code), x = tmp, startCol = 13, startRow = 1)
  
  names(tmp)= NULL
  
  text_table_tmp = ggtexttable(tmp, rows = NULL, theme = ttheme("blank", base_size = 30))
  
  # Generate specific cells 
  tab <- table_cell_font(text_table_tmp, row = 3, column = 1,
                         face = "bold",
                         size=30) 
  
  tab <- table_cell_font(tab, row = 3, column = 2,
                         face = "bold", color = "red",
                         size=20)
  
  tab <- table_cell_font(tab, row = 3, column = 3,
                         face = "bold", color = "black",
                         size=20)
  
  tab <- table_cell_font(tab, row = c(2,3,4), column = c(4,5),
                         face = "bold", color = "grey55",
                         size=20)
  
  tab <- table_cell_font(tab, row = c(2), column = c(2,3),
                         face = "bold", color = "grey55",
                         size=20)
  
  text_table_tmp = tab
  text_table_tmp
  
  
  # Generate the full graph 
  p4 = plot_spacer() + text_table_tmp +  plot_spacer()
  print(p4)
  insertPlot(wb, as.character(focus_code), height = HEIGHT, width = WIDTH, fileType = "jpg",
             startRow = 20,
             startCol = 1)
  
  print(p)
  insertPlot(wb, as.character(focus_code), height = HEIGHT, width = WIDTH, fileType = "jpg",
             startRow = 20,
             startCol = 1)
  
