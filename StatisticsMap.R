library(networkD3)
library(dplyr)
library(openxlsx)

# Import 
DATA <- read.xlsx("C:/Users/hugo.marthinet/Downloads/StatisticsMap.xlsx")

data(MisLinks)
data(MisNodes)
head(MisLinks)
head(MisNodes)

Nodes <- data.frame(
  id = 0:(nrow(DATA)-1),
  name = apply(DATA[, 1:6], 1, function(x) {x[!is.na(x)]}),
  group = rep(DATA$group[!is.na(DATA$group)], diff(c(which(!is.na(DATA$group)), nrow(DATA)+1))),
  size = 1
)
Nodes$size[!is.na(DATA$group)] <- diff(c(which(!is.na(DATA$group)), nrow(DATA)+1))
Nodes$size[!is.na(DATA$name1)] <- diff(c(which(!is.na(DATA$name1)), nrow(DATA)+1))
Nodes$size[!is.na(DATA$name2)] <- diff(c(which(!is.na(DATA$name2)), nrow(DATA)+1))
Nodes$size[!is.na(DATA$name3)] <- diff(c(which(!is.na(DATA$name3)), nrow(DATA)+1))
Nodes$size[!is.na(DATA$name4)] <- diff(c(which(!is.na(DATA$name4)), nrow(DATA)+1))


getParentNode <- function(child, parent) {
  sapply(child, function(x) {max(parent[parent < x])})
}

Links <- data.frame(
  source = Nodes$id[!is.na(DATA$name5)],
  target = getParentNode(
    child = Nodes$id[!is.na(DATA$name5)],
    parent = Nodes$id[!is.na(DATA$name4)]
  ),
  value = 1
) 
Links <- rbind(Links,
               data.frame(
                 source = Nodes$id[!is.na(DATA$name4)],
                 target = getParentNode(
                   child = Nodes$id[!is.na(DATA$name4)],
                   parent = Nodes$id[!is.na(DATA$name3)]
                 ),
                 value = 1
               ))
Links <- rbind(Links,
               data.frame(
                 source = Nodes$id[!is.na(DATA$name3)],
                 target = getParentNode(
                   child = Nodes$id[!is.na(DATA$name3)],
                   parent = Nodes$id[!is.na(DATA$name2)]
                 ),
                 value = 1
               ))
Links <- rbind(Links,
               data.frame(
                 source = Nodes$id[!is.na(DATA$name2)],
                 target = getParentNode(
                   child = Nodes$id[!is.na(DATA$name2)],
                   parent = Nodes$id[!is.na(DATA$name1)]
                 ),
                 value = 1
               ))
Links <- rbind(Links,
               data.frame(
                 source = Nodes$id[!is.na(DATA$name1)],
                 target = getParentNode(
                   child = Nodes$id[!is.na(DATA$name1)],
                   parent = Nodes$id[!is.na(DATA$group)]
                 ),
                 value = 1
               ))
