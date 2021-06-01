# + ------------------------------------------------------------------------- +
# Make Dynamic Date ----
# + ------------------------------------------------------------------------- +
TmStmp     <- gsub('[:-]', '', substr(Sys.time(), 1, 19))
TmStmp     <- gsub(' ', '-', TmStmp)
DailyDevo  <- paste0('studies/daily-devo/', Sys.Date())

# + ------------------------------------------------------------------------- +
# Libraries ----
# + ------------------------------------------------------------------------- +
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidytext)
library(textdata)
library(forcats)
data("stop_words")

# + ------------------------------------------------------------------------- +
# Read In Data ----
# + ------------------------------------------------------------------------- +
setwd('C:/Users/oakespar/Documents/2021-BibleStudies')
dir.create(DailyDevo)
Scripture   <- read_csv('data/ScriptureKjv.csv', col_names = TRUE)
BibleOrder  <- read_csv('data/BibleBkOrder.csv', col_names = TRUE)

# + ------------------------------------------------------------------------- +
# List Book Names ----
# + ------------------------------------------------------------------------- +
data.frame(bk = BibleOrder$BookName, bks = BibleOrder$Book)

# + ------------------------------------------------------------------------- +
# Set Study Area ----
# + ------------------------------------------------------------------------- +
OldT_Books    <- c("2Ch")
OldT_Chapters <- 15:16
NewT_Books    <- c("Jhn")
NewT_Chapters <- c(12)

# subtitle for charts
ttlCh <- paste0(c(paste(OldT_Books, OldT_Chapters), paste(NewT_Books, NewT_Chapters)), collapse = ', ')

# + ------------------------------------------------------------------------- +
# Subset Scripture ----
# + ------------------------------------------------------------------------- +
OldT_Subset        <- Scripture %>% filter(Book %in% OldT_Books & Chapter %in% OldT_Chapters)
NewT_Subset        <- Scripture %>% filter(Book %in% NewT_Books & Chapter %in% NewT_Chapters)
Comb_Subset        <- rbind(OldT_Subset, NewT_Subset)
Comb_Subset$BkCh   <- paste(Comb_Subset$Book,Comb_Subset$Chapter)
Comb_Subset$BkChVs <- paste(Comb_Subset$Book,Comb_Subset$Chapter, Comb_Subset$Verse)

# + ------------------------------------------------------------------------- +
# Preprocess Text ----
# + ------------------------------------------------------------------------- +
Tidy_Scriptures <- Comb_Subset %>%
  unnest_tokens(word, Text) %>%
  anti_join(stop_words)

Words_Chapter <- Tidy_Scriptures %>% count(BkCh, word, sort = TRUE)
Words_Total   <- Words_Chapter %>% group_by(BkCh) %>% summarise(total = sum(n))
Words_Chapter <- left_join(Words_Chapter, Words_Total)

# + ------------------------------------------------------------------------- +
# Visualize Content Scope ----
# + ------------------------------------------------------------------------- +
# make counts of scripture and words per BkCh
ScripStats_BkChVs <- Comb_Subset %>% count(BkCh, length(Verse)) %>% rename(Scriptures = n)
ScripStats_BkChWd <- Words_Chapter %>% count(BkCh, length(word)) %>% rename(Words = n)

# combine both, prep for column plot
ScripStats <- left_join(ScripStats_BkChVs, ScripStats_BkChWd) %>%
  select(BkCh, Scriptures, Words) %>%
  gather('Variables', 'Values', -BkCh)

# View in ggplot
ggplot(ScripStats, aes(fill = Variables, y = Values, x = BkCh)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_text(aes(label = Values), position = position_dodge(width = 0.9), vjust = -0.25) +
  labs(
    title = 'Scripture and Distinct Words Counts Per Chapter',
    subtitle = 'Note: Stop Words Removed'
  ) + xlab('Books of Study')
  
ggsave(paste0(DailyDevo,'/',TmStmp, '_00_','ScriptureStats.png'))
# + ------------------------------------------------------------------------- +
# Visualize n Cutoff ----
# + ------------------------------------------------------------------------- +
ggplot(Words_Chapter, aes(x = as.factor(BkCh), y = n)) +
  geom_boxplot(fill = "slateblue", alpha = 0.2) + 
  xlab(paste("Book/Chapter\nRan on", Sys.time())) + ylab("Occurence Per Chapter") +
  ggtitle('Term Count Per Chapter')

ggsave(paste0(DailyDevo, '/',TmStmp, '_01_','tfidf_cutoff.png'))
# + ------------------------------------------------------------------------- +
# Calculate TF-IDF ----
# + ------------------------------------------------------------------------- +
Words_tf_idf <- Words_Chapter %>% 
  bind_tf_idf(word, BkCh, n) %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# + ------------------------------------------------------------------------- +
# View Top TF-IDF ----
# + ------------------------------------------------------------------------- +
slicemx <- 5  # choosing top % tokens of tf-idf per chapter

Words_tfidf_g <- Words_tf_idf %>%
  group_by(BkCh) %>%
  slice_max(tf_idf, prop = slicemx / 100) %>%
  ungroup() 

Words_tfidf_g %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = BkCh)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~BkCh, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  ggtitle(paste('Top', slicemx, '% of Tokens Per Chapter with Highest TF-IDF'))

ggsave(paste0(DailyDevo, '/',TmStmp, '_02_','tfidf_top', slicemx, 'pct.png'))
# + ------------------------------------------------------------------------- +
# Calc Sentiment ----
# re: sentiment per verse
# + ------------------------------------------------------------------------- +
SentmtType <- 'bing'  # choose sentiment type bing

Words_Sentiment <- Comb_Subset %>% 
  unnest_tokens(word, Text) %>%
  anti_join(stop_words) %>% 
  inner_join(get_sentiments(SentmtType)) %>%
  mutate(linenumber = row_number()) %>% 
  count(BkCh, Verse, sentiment) %>%
  arrange(desc(BkCh)) %>% 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

# + ------------------------------------------------------------------------- +
# View Sentiment ----
# + ------------------------------------------------------------------------- +
ggplot(Words_Sentiment, aes(Verse, sentiment, fill = BkCh)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~BkCh, ncol = 2, scales = "free_x") +
  ggtitle(paste0('Sentiment Analysis Per Verse\nusing ', SentmtType, ' dictionary'))

ggsave(paste0(DailyDevo, '/',TmStmp, '_03_','Sentiment_', SentmtType, '.png'))
# + ------------------------------------------------------------------------- +
# Calc Bigram TF-IDF ----
# re: per book
# + ------------------------------------------------------------------------- +
Bigram_tf_idf <- Comb_Subset %>%
  unnest_tokens(bigram, Text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(Book, bigram) %>%
  bind_tf_idf(bigram, Book, n) %>%
  arrange(desc(tf_idf))


# + ------------------------------------------------------------------------- +
# View n bigram Cutoff ----
# re: for bigrams per book
# + ------------------------------------------------------------------------- +
Bigram_tf_idf %>%
  ggplot(aes(x = as.factor(Book), y = n)) +
    geom_boxplot(fill = "slateblue", alpha = 0.2) + 
    xlab("Books") + ylab("Occurence Per Book") +
  ggtitle(paste('Bigram Counts Per Book\nRe:', 
                ttlCh))

ggsave(paste0(DailyDevo, '/',TmStmp, '_04_','bigram_counts', '.png'))
# + ------------------------------------------------------------------------- +
# View Top TF-IDF Bigrams ----
# re: per book
# + ------------------------------------------------------------------------- +
BigrmPct <- 10 # determine pct cutoff for top tf-idf

Bigram_tf_idf %>%
  group_by(Book) %>%
  slice_max(tf_idf, prop = BigrmPct / 100) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = Book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  ggtitle(paste('Top', BigrmPct, '% of Top TF-IDF Per Book\nRe:',
                ttlCh))

ggsave(paste0(DailyDevo, '/',TmStmp, '_05_','bigram_tfidf_top', BigrmPct, 'pct.png'))
# + ------------------------------------------------------------------------- +
# Develop Word Relationships ----
# + ------------------------------------------------------------------------- +
Bigram_Data <- Comb_Subset %>%
  unnest_tokens(bigram, Text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)

# + ------------------------------------------------------------------------- +
# View Word Relats GGraph ----
# + ------------------------------------------------------------------------- +
library(igraph)
library(ggraph)

Bigram_Rel <- Bigram_Data %>%
  graph_from_data_frame()

# change the size of the nodes and node labels so that they match their importance
# taking the log to improve it
V(Bigram_Rel)$size <- log(strength(Bigram_Rel)) * 4 + 3

# Set colors for highest tf-idf words
V(Bigram_Rel)$color <- NA          # remove any colors
Tfidf_Words <- Words_tfidf_g$word  # grab top tf-idf words shown earlier
V(Bigram_Rel)$color[V(Bigram_Rel)$name %in% Tfidf_Words] <- 'yellow' # set the colors found
V(Bigram_Rel)$color[!V(Bigram_Rel)$name %in% Tfidf_Words] <- 'darkgrey' # set the colors not found

# mark the groupings
ComWalk <- walktrap.community(Bigram_Rel)


# open png file
png(paste0(DailyDevo, '/',TmStmp, '_06_','BigramNetworks.png'),
    width = 2906, height = 2581)

# run the plot into png stream
par(mar=c(0,0,6,0), bg = '#d8dee9')

plot(Bigram_Rel,
    mark.groups = ComWalk,
    mark.col = "#5e81ac", 
    mark.border = NA,
    edge.arrow.size = 0.05,        # change arrow size
    vertex.label.color = "black",  # change color of labels
    vertex.label.cex = 3.00,       # change size of labels to 75% of original size
    vertex.frame.color="white",  # remove black edges of node circles
    edge.color="white",           # change edge color to grey
    
)
legend(
  x = -1.1, y = -0.9, 
  c('Top 5% tf-idf Words', 'Other Words', 'Word Groups'), 
  pt.bg = c('yellow', 'darkgrey', '#5e81ac'),
  pch = 21, col = '#777777', pt.cex = 5, cex = 3, bty = 'n', ncol = 1
)
title(main = 'Word Relationships with Highest TF-IDF', cex.main = 6, col = '#2e3440')

# close the png file
dev.off()

# + ------------------------------------------------------------------------- +
# Calculate Topic Models ----
# + ------------------------------------------------------------------------- +
library(topicmodels)
library(reshape2)

# Right now our data frame word_counts is in a tidy form, with 
# one-term-per-document-per-row, but the topicmodels package requires a 
# DocumentTermMatrix. As described in Chapter 5.2, we can cast a one-token-per-row 
# table into a DocumentTermMatrix with tidytext’s cast_dtm()
chapters_dtm <- Words_Chapter %>% cast_dtm(BkCh, word, n)

# We can then use the LDA() function to create a four-topic model. In this case
# we know we’re looking for four topics because there are four books; in other
# problems we may need to try a few different values of k.
chapters_lda <- LDA(chapters_dtm, k = 3, control = list(seed = 1234))

# examine per-topic-per-word probabilities.
chapter_topics <- tidy(chapters_lda, matrix = "beta")

# use dplyr’s slice_max() to find the top 5 terms within each topic.
top_terms <- chapter_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>% 
  ungroup() %>%
  arrange(topic, -beta)

# + ------------------------------------------------------------------------- +
# View Topic Models ----
# + ------------------------------------------------------------------------- +

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  ggtitle(paste('Topic Model (k = 3, n = 5)\nChapters:', ttlCh))

ggsave(paste0(DailyDevo, '/',TmStmp, '_07_','TopicModels', '.png'))

# + ------------------------------------------------------------------------- +
# Make Daily Devo md file ----
# + ------------------------------------------------------------------------- +
library(rvest)

utmost       <- read_html('https://utmost.org/classic/today/')
utmost_title <- utmost %>% html_elements('.entry-title') %>% html_text(trim = TRUE)
utmost_keyvs <- utmost %>% html_elements('#key-verse-box') %>% html_text(trim = TRUE)
utmost_contt <- utmost %>% html_elements('.post-content') %>% html_text(trim = TRUE) %>% str_replace_all('\n', '')
utmost_wisdm <- utmost %>% html_elements('.wisdom-content') %>% html_text(trim = TRUE)#%>% str_replace_all('\n', '')

utmost_dlybd <- utmost %>% html_elements('#bible-in-a-year-box') %>% html_text(trim = TRUE) %>% 
  str_replace_all('Bible in a Year\\: ', '')
#  str_split_fixed('; ', n = 2) %>%
#  as.data.frame()
# OldT <- utmost_dlybd$V1 %>% str_split_fixed('-', n = 2) %>% as.data.frame()
# OldT_Book     <- OldT$V1 %>% str_sub(1, str_length(OldT$V1) - str_length(str_extract(OldT$V1, '\\s\\d+')))
# OldT_Ch1      <- OldT$V1 %>% str_extract_all('\\s\\d+') %>% str_trim() %>% as.numeric()
# OldT_Ch2      <- OldT$V2 %>% as.numeric()
# OldT_Chapters <- OldT_Ch1:OldT_Ch2
# NewT <- utmost_dlybd$V2

fileConn <- file(paste0(DailyDevo, '/', TmStmp, '.md'))
writeLines(
  str_wrap(
    c( 
      paste("# Daily Devotion"),
      paste("* Time Stamp: ", Sys.time()),
      cat('\n'), paste('\n'),
      paste('##', utmost_title),
      paste('*', utmost_keyvs),
      cat('\n'), paste('\n'),
      utmost_contt,
      cat('\n'), paste('\n'),
      paste('## Daily Wisdom'),
      utmost_wisdm,
      cat('\n'), paste('\n'),
      paste('## Scripture Reading'),
      paste('*', utmost_dlybd),
      cat('\n'), paste('\n'),
      paste('###', OldT_Books, OldT_Chapters[1]),
      unlist(split(Comb_Subset$Text, Comb_Subset$BkCh)[1]),
      cat('\n'), paste('\n'),
      paste('###', OldT_Books, OldT_Chapters[2]),
      unlist(split(Comb_Subset$Text, Comb_Subset$BkCh)[2]),
      cat('\n'), paste('\n'),
      paste('###', NewT_Books, NewT_Chapters[1]),
      unlist(split(Comb_Subset$Text, Comb_Subset$BkCh)[3]),
      cat('\n'), paste('\n'),
      paste('## Scripture Stats'),
      cat('\n'), paste('\n'),
      paste(paste0('![]', '(', TmStmp, '_00_','ScriptureStats.png', ')')),
      cat('\n'), paste('\n'),
      paste('## TF-IDF Cut Off'),
      cat('\n'), paste('\n'),
      paste(paste0('![]', '(', TmStmp, '_01_','tfidf_cutoff.png', ')')),
      cat('\n'), paste('\n'),
      paste('## Top TF-IDF'),
      cat('\n'), paste('\n'),
      paste(paste0('![]', '(', TmStmp, '_02_','tfidf_top', slicemx, 'pct.png', ')')),
      cat('\n'), paste('\n'),
      paste('## Sentiment'),
      cat('\n'), paste('\n'),
      paste(paste0('![]', '(', TmStmp, '_03_','Sentiment_', SentmtType, '.png', ')')),
      cat('\n'), paste('\n'),
      paste('## Bigram Counts'),
      cat('\n'), paste('\n'),
      paste(paste0('![]', '(', TmStmp, '_04_','bigram_counts', '.png', ')')),
      cat('\n'), paste('\n'),
      paste('## Top Bigram TF-IDF'),
      cat('\n'), paste('\n'),
      paste(paste0('![]', '(', TmStmp, '_05_','bigram_tfidf_top', BigrmPct, 'pct.png', ')')),
      cat('\n'), paste('\n'),
      paste('## Bigram Networks'),
      cat('\n'), paste('\n'),
      paste(paste0('![]', '(', TmStmp, '_06_','BigramNetworks.png', ')')),
      cat('\n'), paste('\n'),
      paste('## LDA Topic Model'),
      cat('\n'), paste('\n'),
      paste(paste0('![]', '(', TmStmp, '_07_','TopicModels', '.png', ')'))
    ), 
    width = 80
  ),
  fileConn
)
close(fileConn)

# + ------------------------------------------------------------------------- +
# Review and Clean Environment ----
# + ------------------------------------------------------------------------- +
fileConnI <- file.info(paste0(DailyDevo, '/', TmStmp, '.md'), extra_cols = FALSE)
row.names(fileConnI) <- 'File Info'
colnames(fileConnI)  <- c('size', 'isdir', 'mode', 'file modification', 'last status change', 'last access time')
fileConnI$`Local Memory Usage` <- paste(memory.size(), 'MB')
t(fileConnI)

rm(list = ls()) # remove all environment objects
ls()