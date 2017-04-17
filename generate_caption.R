library(tidyverse)
library(stringr)
library(readr)
library(ngram)
library(twitteR)

# establish directory
ifelse(Sys.info()["nodename"] == "localhost",
       topdir <- "/home/baron/projects/nycc/",
       topdir <- "/ua/baron/scripts/nycc/")
datadir <- paste0(topdir, "/data/")

# get twitter authorization
source(paste0(topdir, "twitter_info.R"))

# load data ------------------

caps1 <- read_csv(paste0(datadir, "contestcaptions.csv"))
caps2 <- read_csv(paste0(datadir, "issuecaptions.csv"))
imgs <- read_csv(paste0(datadir, "issueimages.csv"))

caps <- bind_rows(caps1, caps2)

# process data ---------------


# concatenate all captions into single string
all_text <- str_c(caps$Caption, collapse = " ")

# generate n-gram (n=2)
captionNgram <- ngram(all_text)

# split a babble into separate sentences, remove first and last partial sentences
split_by_sentence <- function (text) {
  result <- unlist(strsplit(text, '(?<=[!?.])[[:space:]]*', perl = TRUE))
  result <- result[!str_detect(result, "^(\\&|\\'|\\`)")]
  return(result[2:(length(result) - 1)])
}

# pick a sentence from the split babble; pick 2 if first is short
makeCaption <- function(caplist, cutofflength = 7) {
  captionsample <- sample(caplist, 2)
  
  if(str_count(captionsample[1], "\\S+") >= cutofflength) {
    caption <- captionsample[1]
  } else {
    caption <- paste(captionsample[1], captionsample[2])
  }
  return(caption)
}

# tweet image + caption
sendTweet <- function() {
  gencap1 <- makeCaption(split_by_sentence(babble(captionNgram, genlen = 1000)))
  img <- sample(imgs$Image, 1)
  download.file(img, destfile = "temp.jpg")

  # add quotes around caption
  tweet(paste0("\"", gencap1, "\""), mediaPath = "temp.jpg")

  file.remove("temp.jpg")
}

# run sendTweet function
sendTweet()
