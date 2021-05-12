library(networkD3)
library(dplyr)

# Make a connection data frame
links <- read.csv(file = "C:/Users/oakespar/Documents/forgive/forgive_sankey.csv", 
         header = TRUE,
         sep = ',',
         encoding = 'UTF-8')

colnames(links) <- c('source', 'target', 'value')
links <- sort(links, decreasing = FALSE)
head(links)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network. I call my colour scale with the colourScale argument
p <- sankeyNetwork(Links = links, 
                   Nodes = nodes, 
                   Source = "IDsource", 
                   Target = "IDtarget", 
                   Value = "value", 
                   NodeID = "name",
                   fontSize = 14,
                   margin = c(15, 0, 0, 0))

p