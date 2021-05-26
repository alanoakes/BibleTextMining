# + ------------------------------------------------------------------- +
# Word Input ----
# + ------------------------------------------------------------------- +
WordConcept <- "forgive"

# + ------------------------------------------------------------------- +
# libraries ----
# + ------------------------------------------------------------------- +
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidytext)
library(forcats)
library(DT)
library(plotly)
data("stop_words")

# + ------------------------------------------------------------------- +
# Read Data ----
# + ------------------------------------------------------------------- +
setwd('C:/Users/oakespar/Documents/2021-BibleStudies')
Scrip       <- read_csv('data/ScriptureStrongsKjv.csv', col_names = TRUE)
Concordance <- read_csv('data/Concordance.csv', col_names = TRUE)
BibleOrder  <- read_csv('data/BibleBkOrder.csv', col_names = TRUE)

# + ------------------------------------------------------------------- +
# Build Lexicon Table ----
# + ------------------------------------------------------------------- +
Concordance %>% 
  filter(grepl(paste0(WordConcept, "*"), Meaning)) %>%
  select(Strongs, Pos_Long, Transliteration, Meaning) %>%
  datatable(class = 'cell-border stripe', rownames = FALSE)

# + ------------------------------------------------------------------- +
# Subset Bible By Strongs ----
# + ------------------------------------------------------------------- +
#names(Concordance) <- c('Strongs', 'Lgg', 'X', 'Word', 'Pos_Long', 'Pos_Abbv', 'Transliteration', 'Meaning')
#Conc_StpWrd <- Concordance[,c(1,5,6)]
StrongsArray       <- Concordance %>% filter(grepl(paste0(WordConcept, "*"), Meaning)) %>% select(Strongs) %>% pull(Strongs)
StrongsArray       <- str_c(StrongsArray, collapse = "|")
SubScrip           <- Scrip %>% filter(grepl(StrongsArray, Text))
SubScrip           <- SubScrip %>% filter(grepl(paste0(WordConcept, "*"), Text))

# + ------------------------------------------------------------------- +
# Unnest Scriptures By Strongs Numbers ----
# + ------------------------------------------------------------------- +
TknsUnst <- SubScrip %>%
  separate_rows(Text, sep = "\\^\\s?") %>%
  mutate(Strongs = str_trim(str_extract(Text,'\\s?[GH]\\d+'), side = 'both'),
         Text = str_trim( str_sub(Text, 1, str_length(Text) - str_length(Strongs)), side = 'both')) %>%
  mutate_all(na_if, "") %>% 
  drop_na(Strongs)

TknsUnst$Text <- str_replace(TknsUnst$Text, "[:punct:]", "")
TknsUnst$Text <- str_to_lower(TknsUnst$Text)
TknsUnst      <- left_join(TknsUnst, BibleOrder)
TknsUnst$Id   <- with( TknsUnst, paste(Book, Chapter, Verse))
TknsUnst$Id_s <- with( TknsUnst, paste(Book, Chapter, Verse, Strongs))

# + ------------------------------------------------------------------- +
# Strongs Inflection Count ----
# + ------------------------------------------------------------------- +
# Clean the inflections uses
ClnInflection <- TknsUnst %>%
  filter(grepl(StrongsArray, Strongs)) %>%
  distinct(Strongs, Text) %>%
  mutate(StrIncr = paste0(Strongs, '-', row_number())) %>%
  select(StrIncr, Text) %>%
  group_by(StrIncr) %>%
  unnest_tokens(word, Text) %>%
  filter(grepl(paste0(WordConcept, "*"), word)) %>%
  mutate(Strongs = str_remove_all(StrIncr, '-\\d+')) %>%
  ungroup() %>% group_by(Strongs) %>%
  count(Strongs, word, sort = TRUE)

# make a list of my stop words
#split(ClnInflection_1$word, ClnInflection_1$Strongs)
#mystopwords <- tibble(
#  word = c('hast' ,'hath', 'thou', 'thee', 'ye', 'maketh', 'peradventure', 'shalt', 
#           'thereof', 'art', 'mayest', 'wherewith')
#) 

# make a misc word category per strongs word
#ClnInflSum <- ClnInflection %>%
#  group_by(Strongs) %>%
#  summarize(TotalN = sum(n))
#
#ClnInflection <- left_join(ClnInflection, ClnInflSum) %>%
#  mutate(PrctN = n / TotalN) %>%
#  mutate(WordN = ifelse(PrctN > quantile(PrctN, 0.50), Word, 'MISC')) %>%
#  ungroup()

# + ------------------------------------------------------------------- +
# Strongs Inflection Sankey ----
# + ------------------------------------------------------------------- +
links <- ClnInflection
  #select(Strongs, WordN) %>%
  #group_by(Strongs) %>%
  #count(Strongs, WordN)

colnames(links) <- c('source', 'target', 'value')

# From these flows we need to create a node data frame: it lists every 
# entities involved in the flow
nodes <- data.frame(
  name = c(as.character(links$source), as.character(links$target)) %>%
    unique()
)
# With networkD3, connection must be provided using id, not using real name 
# like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network. I call my colour scale with the colourScale argument
fig <- plot_ly(
  type = 'sankey',
  orientation = 'h',
  node = list(
    label = nodes$name,
    pad = 15,
    thickness = 10
  ),
  link = list(
    source = links$IDsource,
    target = links$IDtarget,
    value = links$value
  )
)
fig %>% layout(title = 'Inflection Uses Per Strongs Number')

# + ------------------------------------------------------------------- +
# View Scripture Locations ----
# + ------------------------------------------------------------------- +
TknsUnst %>%
  filter(grepl(StrongsArray, Strongs)) %>%
  group_by(Strongs) %>%
  count(Strongs, Text, sort = TRUE) %>%
  ungroup() %>%
  left_join(TknsUnst) %>%
  ggplot(aes(fill=Strongs, y=n, x=reorder(Book, -OrderBk))) +
    geom_bar(position="stack", stat="identity") +
  facet_grid(rows = vars(reorder(Section, Section_Ord)), scales = 'free') +
  coord_flip() +
  xlab('Books By Section') + ylab('Strongs Counts')

# + ------------------------------------------------------------------- +
# Find POS: Nouns ----
# + ------------------------------------------------------------------- +
data.frame(sort(table(Concordance$Pos_Long)))
#lapply(split(Concordance$Text, Concordance$Pos_Long), function(x) head(x))

Concordance$Pos_Long <- str_replace(Concordance$Pos_Long, 'name/g.', 'name group')
Concordance$Pos_Long <- str_replace(Concordance$Pos_Long, 'name/l.', 'name location')
Concordance$Pos_Long <- str_replace(Concordance$Pos_Long, 'noun fem', 'name feminine')
Concordance$Pos_Long <- str_replace(Concordance$Pos_Long, 'noun masc', 'name masculine')
Concordance$Pos_Long <- tolower(Concordance$Pos_Long)

# View Total pos across bible
TknsUnst %>%
  left_join(Concordance[,c(1,5,6)]) %>%
  filter(grepl('name*|noun*', Pos_Long)) %>%
  group_by(Book) %>%
  count(Pos_Long, Text, sort = TRUE) %>%
  left_join(BibleOrder[,c(5,6)]) %>%
  drop_na() %>%
  group_by(Pos_Long) %>%
  slice_max(Text, n = 5) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  ggplot(aes(n, Text, fill = Pos_Long)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~Pos_Long, ncol = 2, scales = 'free')

# view pos counts by book
TknsUnst %>%
  left_join(Concordance[,c(1,5,6)]) %>%
  filter(grepl('name*|noun*', Pos_Long)) %>%
  count(Book, Pos_Long, Text, sort = TRUE) %>%
  left_join(BibleOrder[,c(5,6)]) %>%
  drop_na() %>%
  ggplot() +
    geom_bar(mapping = aes(x = fct_reorder(Book, OrderBk, .desc = TRUE), fill = Pos_Long)) +
    coord_flip()

# + ------------------------------------------------------------------- +
# Calc Bigram Strongs ----
# + ------------------------------------------------------------------- +
StrongsStrng <- TknsUnst %>%
  select(Id, Strongs) %>%
  group_by(Id) %>%
  nest() %>%
  unnest_wider(data) %>%
  unnest_wider(Strongs, names_sep = '_') %>%
  unite('StrongsStr', Strongs_1:last_col(), na.rm = TRUE, remove = TRUE) %>%
  select(Id, StrongsStr)

StrongsStrng$StrongsStr <- str_replace_all(StrongsStrng$StrongsStr, "_", " ")

BigramStrong <- StrongsStrng %>%
  unnest_tokens(bigram, StrongsStr, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  count(word1, word2, sort = TRUE)

BigramStrong$word1 <- paste(BigramStrong$Id, str_to_upper(BigramStrong$word1))
BigramStrong$word2 <- paste(BigramStrong$Id, str_to_upper(BigramStrong$word2))

BigramStrong <- tibble(
  merge(
    x = BigramStrong, 
    y = TknsUnst[,c('Id_s', 'Text')], 
    by.x = 'word1', by.y = 'Id_s'
  )
)
BigramStrong <- tibble(
  merge(
    x = BigramStrong, 
    y = TknsUnst[,c('Id_s', 'Text')], 
    by.x = 'word2', by.y = 'Id_s'
  )
)
colnames(BigramStrong) <- c('Id_1', 'Id_2', 'Id', 'n', 'word1', 'word2')
BigramStrong <- BigramStrong %>% select(Id_1, Id_2, word1, word2, n)

# + ------------------------------------------------------------------- +
# View Bigram Strongs As Source ----
# + ------------------------------------------------------------------- +
library(igraph)

BigramSrc_tf <- BigramStrong %>%
  separate(Id_1, c('Book', 'Ch', 'Vs', 'Strong1'), sep = ' ') %>%
  separate(Id_2, c('BK', 'CH', 'VS', 'Strong2'), sep = ' ') %>%
  select(Book, Ch, Vs, Strong1, Strong2, word1, word2, n) %>%
  filter(grepl(StrongsArray, Strong2)) %>%
  left_join(BibleOrder, by = 'Book') %>%
  count(Section, word1, word2, sort = TRUE)
  
  
BigramSrc_tfidf <- BigramSrc_tf %>%
  unite(bigram, word1, word2, sep = ' ') %>%
  bind_tf_idf(Section, bigram, n) %>%
  arrange(desc(tf_idf))
  
BigramSrc_tfidf %>%
  group_by(Section) %>%
  slice_max(tf_idf, n = 2) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = Section)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Section, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

Bigram_Src <- BigramSrc_tfidf %>%
  filter(n > 3) %>%
  graph_from_data_frame()

library(ggraph)
set.seed(20210511)

ggraph(Bigram_Src, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# + ------------------------------------------------------------------- +
# View Bigram Strongs As Target ----
# + ------------------------------------------------------------------- +

Bigram_Tgt <- BigramStrong %>%
  separate(Id_1, c('Bk', 'Ch', 'Vs', 'Strong1'), sep = ' ') %>%
  separate(Id_2, c('BK', 'CH', 'VS', 'Strong2'), sep = ' ') %>%
  select(Bk, Ch, Vs, Strong1, Strong2, word1, word2, n) %>%
  filter(grepl(StrongsArray, Strong1)) %>%
  count(word1, word2, sort = TRUE) %>%
  filter(n > 3) %>%
  graph_from_data_frame()

set.seed(20210511)
ggraph(Bigram_Tgt, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# + ------------------------------------------------------------------- +
# Listing and concatenating token variations of strongs words ----
# + ------------------------------------------------------------------- +
TknsLs <- TknsUnst %>% 
  group_by(Strongs) %>% 
  distinct(Text) %>%
  nest() %>% 
  unnest_wider(data) %>% 
  unnest_wider(Text, names_sep = '_') %>% 
  unite('TextVars', Text_1:last_col(), na.rm = TRUE, remove = TRUE) %>%
  select(Strongs, TextVars)

TknsLs$TextVars <- str_replace_all(TknsLs$TextVars, "_", ", ")

# Build Relevant Stop Words List & remove
TknsUnst <- inner_join(TknsUnst, Conc_StpWrd)
TknsUnst <- inner_join(TknsUnst, BibleOrder)
StopPos  <- c("", "Aramaic", "article", "conditional", "conjuction", "conjunction", "expression", "Hebrew", "injunction", "interrogative", "n-e.", "n/g.", "p/c", "p/d.", "p/f", "p/i.", "p/k", "p/p.", "p/q.", "p/r.", "p/s.", "particle", "preposition", "Preposition", "prt-d.", "prt-n.", "prt-x.", "prt.", "subv.", "v-xxp.")
TknsUnst <- TknsUnst %>% filter(!Pos_Long %in% StopPos)

# TF = term refrequency
#Book_Words         <- TknsUnst %>% count(Book, Strongs, sort = TRUE)
Book_Words          <- TknsUnst %>% count(Book, Text, sort = TRUE)
TotalWords_bk       <- Book_Words %>% group_by(Book) %>% summarise(total = sum(n))
Book_Words          <- left_join(Book_Words, TotalWords_bk)

# testament words
Testm_Words          <- TknsUnst %>% count(Testament, Text, sort = TRUE)
TotalWords_tm        <- Book_Words %>% group_by(Testament) %>% summarise(total = sum(n))
Testm_Words          <- left_join(Testm_Words, TotalWords_tm)

# testament words
Sctn_Words           <- TknsUnst %>% count(Section, Text, sort = TRUE)
TotalWords_sm        <- Book_Words %>% group_by(Section) %>% summarise(total = sum(n))
Sctn_Words           <- left_join(Sctn_Words, TotalWords_sm)

#Book_Words          <- left_join(Book_Words, TknsLs)
#Book_Words$TextVars <- str_trunc(Book_Words$TextVars, 20, side = 'right')
#Book_Words$StrText  <- paste0(Book_Words$TextVars, '[', Book_Words$Strongs, ']')

# tidytext preprocess
Scrip1 <- inner_join(Scrip1, BibleOrder)
Testm_Scrip1 <- Scrip1 %>% 
  unnest_tokens(word, Text) %>% 
  anti_join(stop_words) %>%
  group_by(Testament) %>%
  count(Testament, word, sort = TRUE)



freq_by_rank_bw <- Book_Words %>%
  group_by(Book) %>%
  mutate(rank = row_number(), TermFreq = n/total) %>%
  ungroup()

freq_by_rank_tw <- Testm_Words %>%
  group_by(Testament) %>%
  mutate(rank = row_number(), TermFreq = n/total) %>%
  ungroup()

# TF-IDF
Ch_tf_idf_bw <- Book_Words %>%
  bind_tf_idf(Strongs, Book, n) %>%
  select(-total) %>%
  arrange(desc(tf_idf))

Ch_tf_idf_tw <- Testm_Words %>%
  bind_tf_idf(Text, Testament, n) %>%
  select(-total) %>%
  arrange(desc(tf_idf))

Ch_tf_idf_sw <- Sctn_Words %>%
  bind_tf_idf(Text, Section, n) %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# View tf-idf
Ch_tf_idf %>%
  group_by(Book) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(StrText, tf_idf), fill = Book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Testament, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

Ch_tf_idf_tw %>%
  group_by(Testament) %>%
  slice_max(tf_idf, n = 20) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(Text, tf_idf), fill = Testament)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Testament, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

Ch_tf_idf_sw %>%
  group_by(Section) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(Text, tf_idf), fill = Section)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Section, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)