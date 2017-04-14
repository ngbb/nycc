library(tidyverse)
library(stringr)
library(readr)
library(ngram)
library(twitteR)

source("./twitter_info.R")

topdir <- getwd()
datadir <- paste0(topdir, "/data/")

# load data ------------------

caps1 <- read_csv(paste0(datadir, "contestcaptions.csv"))
caps2 <- read_csv(paste0(datadir, "issuecaptions.csv"))
imgs <- read_csv(paste0(datadir, "issueimages.csv"))

caps <- bind_rows(caps1, caps2)


# process data ---------------
caps <- caps %>%
  mutate(caplength = str_count(Caption, "\\S+"))

# concatenate all captions into single string
all_text <- str_c(caps$Caption, collapse = " ")

captionNgram <- ngram(all_text)

tt <- babble(captionNgram, genlen = 1000, seed = 12346)

#rr <- str_split(tt, "[[:punct:]]")

split_by_sentence <- function (text) {
  result <- unlist(strsplit(text, '(?<=[!?.])[[:space:]]*', perl = TRUE))
  result <- result[!str_detect(result, "^(\\&|\\'|\\`)")]
  return(result[2:(length(result) - 1)])
}


makeCaption <- function(caplist) {
  captionsample <- sample(caplist, 2)
  
  if(str_count(captionsample[1], "\\S+") >= 7) {
    caption <- captionsample[1]
  } else {
    caption <- paste(captionsample[1], captionsample[2])
  }
  return(caption)
}

sendTweet <- function() {
  gencap1 <- makeCaption(split_by_sentence(babble(captionNgram, genlen = 1000)))
  img <- sample(imgs$Image, 1)
  download.file(img, destfile = "temp.jpg")

  #tweet(gencap1, mediaPath = "temp.jpg")
  tweet(paste0("\"", gencap1, "\""), mediaPath = "temp.jpg")

  file.remove("temp.jpg")
}

## it works!
