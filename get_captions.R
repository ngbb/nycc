library(tidyverse)
library(rvest)
library(stringr)
library(readr)

topdir <- getwd()
datadir <- paste0(topdir, "/data/")

## Use rvest to scrape pages for captions -------------------

getCaps <- function(contestNumber) {

  cap <- read_html(paste0("http://contest.newyorker.com/CaptionContest.aspx?id=", contestNumber))
  caps <- html_nodes(cap, ".cap em")

  caps <- as.character(caps)

  # clean bad quotation marks and html tags
  for(i in 1:length(caps)) {
    caps[i] <- gsub("\"", "", caps[i])
    caps[i] <- gsub("<em>“", "", caps[i])
    caps[i] <- gsub("<em>", "", caps[i])
    caps[i] <- gsub("\\\\", "", caps[i])
    
    caps[i] <- gsub("”</em>", "", caps[i])
    caps[i] <- gsub("</em>", "", caps[i])
  }

  return(caps)
}

# define caption number range, prepare to scrape ------------------
captionNumbers <- c(1:560)

captions <- NULL

# scrape caption loop -------------------------
for(i in captionNumbers) {
  temp <- getCaps(i)

  df <- data.frame(temp, 1:3, stringsAsFactors = FALSE)
  captions <- bind_rows(captions, df)
}

# clean up caption dataset
names(captions) <- c("Caption", "Rank")

# Save scraped data ---------------------------

write_csv(captions, paste0(datadir, "captions.csv"))
