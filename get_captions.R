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
    caps[i] <- gsub("\"", "", caps[i])
  }
  
  sess <- html_session(paste0("http://www.newyorker.com/cartoons/daily-cartoon/page/", pageNumber))
  imgsrc <- sess %>%
    read_html() %>%
    html_nodes("img")

  imgsrc <- unlist(str_split(imgsrc, "src\\=\""))
  imgsrc <- imgsrc[str_detect(imgsrc, "http\\://www.newyorker.com/wp-content/uploads")]
  imgs <- unlist(str_split(imgsrc, "\""))
  imgs <- imgs[str_detect(imgs, "http\\://www.newyorker.com/wp-content/uploads")]
  
  imgs <- imgs[str_sub(imgs, start = -3) == "jpg" & str_sub(imgs, start = 1, end = 4) == "http"]

  return(list(caps, imgs))
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

  issueCaptions <- if(i == 1) {
    temp
  } else {
    Map(c, issueCaptions, temp)
    }
}

names(issueCaptions) <- c("Caption", "Image")

# Save issue caption dataset --------------------------

write_csv(data.frame(issueCaptions$Caption, stringsAsFactors = FALSE), paste0(datadir, "issuecaptions.csv"))
write_csv(data.frame(issueCaptions$Image, stringsAsFactors = FALSE), paste0(datadir, "issueimages.csv"))
