library(tidyverse)
library(stringr)
library(readr)

topdir <- getwd()
datadir <- paste0(topdir, "/data/")

# load caption data ---------------

caps <- read_csv(paste0(datadir, "captions.csv"))

# process caption data ------------

caps <- caps %>%
  mutate(wordcount = length(Caption),
         question = ifelse(str_detect(Caption, "\\?"), 1, 0),
         exclamation = ifelse(str_detect(Caption, "\\!"), 1, 0))
