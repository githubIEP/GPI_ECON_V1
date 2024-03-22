library(dplyr)
library(tidyverse)

gti_national = rio::import("./02_data/processed/gti_banded_national.rds")
min_banded = min(gti_national$banded_score) 
max_banded = max(gti_national$banded_score) 

#  Subtract the minimum from the maximum gti scores and calculate by 

# a) root = 2*(highest GTI banded score - lowest gti banded score)
root = 2 * (max_banded-min_banded)

# b) range = 2 * highest gti raw score - lowest gti raw score 
max_score = max(gti_national$weighted_score)
min_score = min(gti_national$weighted_score)
range = max_score - min_score

# c) calculate the inverval step by taking the 20th root of range
interval_step <- range ^ (1 / root)

# c.1 make the minimum banded score interval 0
band_ints = min_score

# generate futoffs for each band by takeing the (i-1) root of the interval step
# This will create 21 bands from 0 to 10 in 05 increments. 
for (i in 2:(root + 1)) {
  band_ints[i] <- min_score + interval_step ^ (i - 1)
}

# add the banding number, the resoective bands and the scores
options(scipen=999)
result = as.data.frame(cbind("Banded number" = seq(1,21,1),
                             "Bands" = seq(0,10,0.5),
                             "Band cutoff values" = round(band_ints,2)))

rio::export(result, "./02_data/processed/banded-scores-methodology.csv")
