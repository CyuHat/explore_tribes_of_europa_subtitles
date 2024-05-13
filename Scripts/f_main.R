# Libraries----
library(stringr)
library(tidytext)
library(srt)
library(tidyr)
library(dplyr)
library(udpipe)
library(rlang)
library(purrr)

# Functions to import srt----
srt <- function (path, collapse = "\n"){
  
  newline <- utils::getFromNamespace("newline", "srt")
  
  x <- str_conv(readLines(con = path), "UTF-8")
  nl <- newline(x, rm.last = FALSE)
  if (any(diff(nl) == 1)) {
    x <- x[-nl[diff(nl) == 1]]
  }
  t <- srt_seconds(x)
  y <- data.frame(stringsAsFactors = FALSE,
                  n = srt_index(x), 
                  start = t$start,
                  end = t$end,
                  subtitle = srt_text(x, collapse = collapse))
  y <- as_tibble(y)
  y[1,1] <- 1
  return(y)  
}

# Load srt----
load_srt <- function(path) {
  episode <-
    str_extract(path, "S\\d{2}E\\d{2}")
  
  df <- 
    srt(path)
  
  df$episode <- episode
  
  return(df)
}

# Load all srt----
load_all_srt <- function(folder = "Data/", regex = "\\.srt") {
  path <- list.files(folder,
                     pattern = regex,
                     full.names = TRUE)
  
  df <- 
    map_dfr(path, load_srt)
  
  return(df)
}

# Functions for Part of Speech----
# Add annotation
annotate <- function(text){
  pos <- 
    udpipe_annotate(de, text) %>% 
    as_tibble() %>% 
    select(lemma, upos)
  
  return(pos)
}

# The whole part of speech
full_pos <- function(data, text){
  res <- 
    data %>% 
    mutate(pos = map({{text}}, annotate)) %>% 
    unnest(pos)
  
  return(res)
}