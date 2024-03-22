testcounter = 1.0

f_testcounterplus = function(current_count,range){
  for (number in range) {
    print(current_count)
    current_count = current_count + 0.1
  }
}

f_testcounterplus(testcounter,1:100)

SIGNIFICANCE_CHANGEYEARS = 3

mapchanges.df <- tt_incidents.df %>%
  filter(complete.cases(longitude, latitude)) %>%
  filter(year >= GTI_YEAR - SIGNIFICANCE_CHANGEYEARS & year <= GTI_YEAR) %>%
  f_LatLongShape("level1")