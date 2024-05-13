# Librairies----
library(udpipe)
library(deeplr)

# Modèle----
udpipe_download_model("german-gsd", "MyData/udpipe/")
de <- udpipe_load_model("MyData/udpipe/german-gsd-ud-2.5-191206.udpipe")

# Functions
source("Scripts/f_main.R")

# Données----
tribe <- load_all_srt()

tribe_pos <- 
  tribe %>% 
  select(episode, subtitle) %>% 
  mutate(pos = map(subtitle, annotate)) %>% 
  unnest(pos) %>% 
  select(-subtitle)

top300 <- 
  tribe_pos %>% 
  filter(!lemma %in% stopwords::data_stopwords_stopwordsiso$de,
         !upos %in% c("PUNCT", "NUM")) %>% 
  count(lemma, upos, sort = TRUE) %>% 
  drop_na() %>% 
  top_n(n, n = 300) %>% 
  select(Deutsch = lemma)

readr::write_csv(top300, "MyData/Voc_tribe_of_europa.csv")


