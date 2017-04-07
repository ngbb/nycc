library(tidyverse)
library(stringr)
library(readr)
library(ngram)

topdir <- getwd()
datadir <- paste0(topdir, "/data/")

# load data ------------------

caps1 <- read_csv(paste0(datadir, "contestcaptions.csv"))
caps2 <- read_csv(paste0(datadir, "issuecaptions.csv"))

caps <- bind_rows(caps1, caps2)


# process data ---------------
caps <- caps %>%
  mutate(caplength = str_count(Caption, "\\S+"))

# concatenate all captions into single string
all_text <- str_c(caps$Caption, collapse = " ")

captionNgram <- ngram(all_text)


tt <- babble(captionNgram, genlen = 150, seed = 12345)

rr <- str_split(tt, "[[:punct:]]")

split_by_sentence <- function (text) {

  result <- strsplit(text, '(?<=[!?.])[[:space:]]*', perl = TRUE)

  return(result)
}
