# Text Analysis
setwd("C:/Users/oakespar/Documents/forgive/forgive_analysis")

# Load Libraries
#library(tidytext)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
#library(networkD3)

# Read Data Files
LexCount <- read.csv('data/LexCountOut.csv', header = TRUE, sep = ',')
TextOut  <- read.csv('data/TextOutput.csv', header = TRUE, sep = ',')

# H3722 H5375 H5545 H5546 H5547 G859 G863 G5483 
# pre-processing data
TextOut_H3722 <- TextOut[TextOut$Strongs == 'H3722',]
TextOut_H5375 <- TextOut[TextOut$Strongs == 'H5375',]
TextOut_H5545 <- TextOut[TextOut$Strongs == 'H5545',]
TextOut_H5546 <- TextOut[TextOut$Strongs == 'H5546',]
TextOut_H5547 <- TextOut[TextOut$Strongs == 'H5547',]
TextOut_G0859 <- TextOut[TextOut$Strongs == 'G859',]
TextOut_G0863 <- TextOut[TextOut$Strongs == 'G863',]
TextOut_G5483 <- TextOut[TextOut$Strongs == 'G5483',]

# Create strongs counts per record
TextOut_H3722$StrCnt <- str_count(TextOut_H3722$Text, 'H3722')
TextOut_H5375$StrCnt <- str_count(TextOut_H5375$Text, 'H5375')
TextOut_H5545$StrCnt <- str_count(TextOut_H5545$Text, 'H5545')
TextOut_H5546$StrCnt <- str_count(TextOut_H5546$Text, 'H5546')
TextOut_H5547$StrCnt <- str_count(TextOut_H5547$Text, 'H5547')
TextOut_G0859$StrCnt <- str_count(TextOut_G0859$Text, 'G859')
TextOut_G0863$StrCnt <- str_count(TextOut_G0863$Text, 'G863')
TextOut_G5483$StrCnt <- str_count(TextOut_G5483$Text, 'G5483')
TextOut1 <- rbind(TextOut_H3722,TextOut_H5375,TextOut_H5545,TextOut_H5546,TextOut_H5547,TextOut_G0859,TextOut_G0863,TextOut_G5483)

# Make df for all instances across scripture
str1_H3722 <- TextOut_H3722$Text %>% strsplit(split = '\\^ ')
str1_H5375 <- TextOut_H5375$Text %>% strsplit(split = '\\^ ')
str1_H5545 <- TextOut_H5545$Text %>% strsplit(split = '\\^ ')
str1_H5546 <- TextOut_H5546$Text %>% strsplit(split = '\\^ ')
str1_H5547 <- TextOut_H5547$Text %>% strsplit(split = '\\^ ')
str1_G0859 <- TextOut_G0859$Text %>% strsplit(split = '\\^ ')
str1_G0863 <- TextOut_G0863$Text %>% strsplit(split = '\\^ ')
str1_G5483 <- TextOut_G5483$Text %>% strsplit(split = '\\^ ')
str2_H3722 <- lapply(str1_H3722, function(x) x[grepl('H3722', x)])
str2_H5375 <- lapply(str1_H5375, function(x) x[grepl('H5375', x)])
str2_H5545 <- lapply(str1_H5545, function(x) x[grepl('H5545', x)])
str2_H5546 <- lapply(str1_H5546, function(x) x[grepl('H5546', x)])
str2_H5547 <- lapply(str1_H5547, function(x) x[grepl('H5547', x)])
str2_G0859 <- lapply(str1_G0859, function(x) x[grepl('G859', x)])
str2_G0863 <- lapply(str1_G0863, function(x) x[grepl('G863', x)])
str2_G5483 <- lapply(str1_G5483, function(x) x[grepl('G5483', x)])
names(str2_H3722) <- paste(TextOut_H3722$Book, TextOut_H3722$Chapter, TextOut_H3722$Verse, "x")
names(str2_H5375) <- paste(TextOut_H5375$Book, TextOut_H5375$Chapter, TextOut_H5375$Verse, "x")
names(str2_H5545) <- paste(TextOut_H5545$Book, TextOut_H5545$Chapter, TextOut_H5545$Verse, "x")
names(str2_H5546) <- paste(TextOut_H5546$Book, TextOut_H5546$Chapter, TextOut_H5546$Verse, "x")
names(str2_H5547) <- paste(TextOut_H5547$Book, TextOut_H5547$Chapter, TextOut_H5547$Verse, "x")
names(str2_G0859) <- paste(TextOut_G0859$Book, TextOut_G0859$Chapter, TextOut_G0859$Verse, "x")
names(str2_G0863) <- paste(TextOut_G0863$Book, TextOut_G0863$Chapter, TextOut_G0863$Verse, "x")
names(str2_G5483) <- paste(TextOut_G5483$Book, TextOut_G5483$Chapter, TextOut_G5483$Verse, "x")
str2_H3722 <- unlist(str2_H3722, use.names = TRUE, recursive = TRUE)
str2_H5375 <- unlist(str2_H5375, use.names = TRUE, recursive = TRUE)
str2_H5545 <- unlist(str2_H5545, use.names = TRUE, recursive = TRUE)
str2_H5546 <- unlist(str2_H5546, use.names = TRUE, recursive = TRUE)
str2_H5547 <- unlist(str2_H5547, use.names = TRUE, recursive = TRUE)
str2_G0859 <- unlist(str2_G0859, use.names = TRUE, recursive = TRUE)
str2_G0863 <- unlist(str2_G0863, use.names = TRUE, recursive = TRUE)
str2_G5483 <- unlist(str2_G5483, use.names = TRUE, recursive = TRUE)
df_H3722 <- as.data.frame(str_split_fixed(names(str2_H3722), pattern = ' ', n = Inf))
df_H5375 <- as.data.frame(str_split_fixed(names(str2_H5375), pattern = ' ', n = Inf))
df_H5545 <- as.data.frame(str_split_fixed(names(str2_H5545), pattern = ' ', n = Inf))
df_H5546 <- as.data.frame(str_split_fixed(names(str2_H5546), pattern = ' ', n = Inf))
df_H5547 <- as.data.frame(str_split_fixed(names(str2_H5547), pattern = ' ', n = Inf))
df_G0859 <- as.data.frame(str_split_fixed(names(str2_G0859), pattern = ' ', n = Inf))
df_G0863 <- as.data.frame(str_split_fixed(names(str2_G0863), pattern = ' ', n = Inf))
df_G5483 <- as.data.frame(str_split_fixed(names(str2_G5483), pattern = ' ', n = Inf))
df_H3722$Transl <- str_replace(str2_H3722, ' H3722', ''); df_H3722$Strongs <- 'H3722'
df_H5375$Transl <- str_replace(str2_H5375, ' H5375', ''); df_H5375$Strongs <- 'H5375'
df_H5545$Transl <- str_replace(str2_H5545, ' H5545', ''); df_H5545$Strongs <- 'H5545'
df_H5546$Transl <- str_replace(str2_H5546, ' H5546', ''); df_H5546$Strongs <- 'H5546'
df_H5547$Transl <- str_replace(str2_H5547, ' H5547', ''); df_H5547$Strongs <- 'H5547'
df_G0859$Transl <- str_replace(str2_G0859, ' G859', '');  df_G0859$Strongs <- 'G859'
df_G0863$Transl <- str_replace(str2_G0863, ' G863', '');  df_G0863$Strongs <- 'G863'
df_G5483$Transl <- str_replace(str2_G5483, ' G5483', ''); df_G5483$Strongs <- 'G5483'
StrongsTranslit <- rbind(df_H3722, df_H5375, df_H5545, df_H5546, df_H5547, df_G0859, df_G0863, df_G5483)
colnames(StrongsTranslit) <- c('Book', 'Ch', 'Vs', 'Incr','Transl', 'Strongs')
StrongsTranslit$Incr <- str_replace(StrongsTranslit$Incr, 'x4', '5')
StrongsTranslit$Incr <- str_replace(StrongsTranslit$Incr, 'x3', '4')
StrongsTranslit$Incr <- str_replace(StrongsTranslit$Incr, 'x2', '3')
StrongsTranslit$Incr <- str_replace(StrongsTranslit$Incr, 'x1', '2')
StrongsTranslit$Incr <- str_replace(StrongsTranslit$Incr, 'x',  '1')

# write the data to dir
write.csv(x = StrongsTranslit, file = "data/Analysis_StrongsTransl.csv")
write.csv(x = TextOut1, file = 'data/Analysis_StrongsDB.csv')


# visualize strongs usage per book
with(StrongsTranslit, table(Book, Strongs))
#with(StrongsTranslit, barplot(table(Strongs, Book), horiz = TRUE))

# remove strongs numbers from text to prepare for tidytext
TextOut$Text %>% str_replace_all('[GH][:digit:]{1,4}\\^ ', '')
