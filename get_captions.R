library(tidyverse)
library(rvest)
library(stringr)
library(readr)

topdir <- getwd()
datadir <- paste0(topdir, "/data/")

## set contest caption limit and daily cartoon page limit
captionNumbers <- 1:572
dailyPageNums <- 1:88

## Use rvest to scrape pages for captions -------------------

getContestCaps <- function(contestNumber) {

  cap <- read_html(paste0("http://contest.newyorker.com/CaptionContest.aspx?id=", contestNumber))
  caps <- html_nodes(cap, ".cap em")
  caps <- as.character(caps)
  
  # clean up strings
  caps <- str_replace_all(caps, "“", "")
  caps <- str_replace_all(caps, "”", "")
  caps <- str_replace_all(caps, "<.*?>", "")
  caps <- str_replace_all(caps, "\"", "")
  caps <- str_replace_all(caps, "’", "'")
  caps <- str_replace_all(caps, "‘", "'")
  caps <- str_replace_all(caps, "—", ", ")
  caps <- str_replace_all(caps, "…", "")
  caps <- str_replace_all(caps, "\U2011", "-")
  caps <- str_replace_all(caps, "\U00A0", "")
  caps <- str_replace_all(caps, "\U00E9", "e")
  caps <- str_replace_all(caps, "(?<=\\b[A-Z])[.](?=[A-Z]|[ a-z]|[,])", "")
  caps <- str_replace_all(caps, " . . .", "...")
  caps <- str_replace_all(caps, "&#13;", "")
  caps <- str_trim(caps)

  return(caps)
}

getIssueCaptions <- function(pageNumber) {
  
  cap <- read_html(paste0("http://www.newyorker.com/cartoons/daily-cartoon/page/", pageNumber))
  caps <- html_nodes(cap, ".River__dek___CayIg")
  caps <- as.character(caps)

  # clean up strings
  caps <- str_replace_all(caps, "“", "")
  caps <- str_replace_all(caps, "”", "")
  caps <- str_replace_all(caps, "<.*?>", "")
  caps <- str_replace_all(caps, "\"", "")
  caps <- str_replace_all(caps, "’", "'")
  caps <- str_replace_all(caps, "‘", "'")
  caps <- str_replace_all(caps, "—", ", ")
  caps <- str_replace_all(caps, "…", "")
  caps <- str_replace_all(caps, "\U2011", "-")
  caps <- str_replace_all(caps, "\U00A0", "")
  caps <- str_replace_all(caps, "\U00E9", "e")
  caps <- str_replace_all(caps, "(?<=\\b[A-Z])[.](?=[A-Z]|[ a-z]|[,])", "")
  caps <- str_replace_all(caps, " . . .", "...")
  caps <- str_replace_all(caps, "&#13;", "")
  caps <- str_trim(caps)
  
  sess <- html_session(paste0("http://www.newyorker.com/cartoons/daily-cartoon/page/", pageNumber))
  imgsrc <- sess %>%
    read_html() %>%
    html_nodes("img")

  imgsrc <- unlist(str_split(imgsrc, "src\\=\""))
  imgsrc <- imgsrc[str_detect(imgsrc, "https\\://media.newyorker.com/photos")]
  imgs <- unlist(str_split(imgsrc, "\""))
  imgs <- imgs[str_detect(imgs, "https\\://media.newyorker.com/photos")]
  
 # imgs <- imgs[str_sub(imgs, start = -3) == "jpg" & str_sub(imgs, start = 1, end = 4) == "http"]

  return(list(caps, imgs))
}

# -----------------------------------------------------------------
# run loops to scrape captions ------------------------------------
# -----------------------------------------------------------------

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

issuecaps <- data.frame(issueCaptions$Caption, stringsAsFactors = FALSE)
names(issuecaps) <- c("Caption")
issueimgs <- data.frame(issueCaptions$Image, stringsAsFactors = FALSE)
names(issueimgs) <- c("Image")

write_csv(issuecaps, paste0(datadir, "issuecaptions.csv"))
write_csv(issueimgs, paste0(datadir, "issueimages.csv"))
