# Bible Text Mining

This is a text mining software made on top of "tidytext" in R to suppliment the following Biblical methods:

1. Word/Concept Studies
2. Passage/Content Studies

This software is currently under development. For questions or concerns, email me at alan.p.oakes@gmail.com.
The text mining process is as follows:

## Word Study Process
01. word input
02. read data
03. build lexicon
04. subset bible by strongs numbers
05. unnest scriptures by all strongs numbers
06. make sankey of strongs inflection counts
07. make bar chart of strongs number locations per c(chapter,section,testament)
08. choose correct inflections list
09. subset by correct inflections from strongs numbers
10. count all distinct parts of speech
11. make sankey of parts of speech throughout scripture
12. make distribution plot of all words counts and boxplot per part of speech
13. remove stop words and inflated words
14. make relevant book, chapter and verse subsets for content analysis

## Content Study Process
1. take word study subset of c(testament, section, book, chapter, verse, text)
2. unnest by tidytext and calculate tf-idf by token per section
3. calculate sentimnent
4. calculate bi-gram tf-idf
5. calculate network analysis
6. calculate topic model

Thanks,
Parker Oakes
