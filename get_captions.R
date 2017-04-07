library(tidyverse)
library(rvest)
library(stringr)
library(readr)

topdir <- getwd()
datadir <- paste0(topdir, "/data/")

## Use rvest to scrape pages for captions -------------------

getContestCaps <- function(contestNumber) {

  cap <- read_html(paste0("http://contest.newyorker.com/CaptionContest.aspx?id=", contestNumber))
  caps <- html_nodes(cap, ".cap em")

  caps <- as.character(caps)

  # clean bad quotation marks and html tags
  for(i in 1:length(caps)) {
    caps[i] <- gsub("\"", "", caps[i])
    caps[i] <- gsub("“", "", caps[i])
    caps[i] <- gsub("”", "", caps[i])
    caps[i] <- gsub("\\\\", "", caps[i])
    caps[i] <- gsub("<.*?>", "", caps[i])
  }

  return(caps)
}

getIssueCaptions <- function(pageNumber) {

  cap <- read_html(paste0("http://www.newyorker.com/cartoons/daily-cartoon/page/", pageNumber))
  caps <- html_nodes(cap, ".p-summary")
  caps <- as.character(caps)

  for(i in 1:length(caps)) {
    caps[i] <- gsub("“", "", caps[i])
    caps[i] <- gsub("”", "", caps[i])
    caps[i] <- gsub("<.*?>", "", caps[i])
  }

  return(caps)
}

# -----------------------------------------------------------------
# run loops to scrape captions ------------------------------------
# -----------------------------------------------------------------

# define contest caption number range, prepare to scrape ----------
captionNumbers <- 1:560

contestCaptions <- NULL

# scrape contest caption loop -------------------------
for(i in captionNumbers) {
  temp <- getContestCaps(i)

  df <- data.frame(temp, 1:3, stringsAsFactors = FALSE)
  contestCaptions <- bind_rows(contestCaptions, df)
}

# clean up caption dataset
names(contestCaptions) <- c("Caption", "Rank")

# Save scraped data ---------------------------

write_csv(contestCaptions, paste0(datadir, "contestcaptions.csv"))

# -----------------------------------------------------------------
# scrape regular captions from daily caption archive
# -----------------------------------------------------------------

# define page range
dailyPageNums <- 1:81

issueCaptions <- NULL

# scrape issue captions loop --------------------------
for(i in dailyPageNums) {
  temp <- getIssueCaptions(i)

  # not perfect, but treat published captions as contest winners
  df <- data.frame(temp, 1, stringsAsFactors = FALSE)
  issueCaptions <- bind_rows(issueCaptions, df)
}

names(issueCaptions) <- c("Caption", "Rank")

# Save issue caption dataset --------------------------

write_csv(issueCaptions, paste0(datadir, "issuecaptions.csv"))
