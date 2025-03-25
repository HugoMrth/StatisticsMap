library(networkD3)
library(tidyverse)
library(openxlsx)

# Import 
DATA <- read.xlsx("C:/Users/hugo.marthinet/Downloads/index.xlsx")



Nodes <- data.frame(
  id = 0:(nrow(DATA)-1),
  name = apply(DATA[, 1:6], 1, function(x) {x[!is.na(x)]}),
  group = rep(DATA$group[!is.na(DATA$group)], diff(c(which(!is.na(DATA$group)), nrow(DATA)+1))),
  size = 1
)
Nodes$size[!is.na(DATA$group)] <- diff(c(which(!is.na(DATA$group)), nrow(DATA)+1))

condition <- is.na(DATA$group)
values <- diff(c(which(!is.na(DATA$name1[condition])),nrow(DATA[condition,])+1))
Nodes$size[!is.na(DATA$name1)] <- ifelse(values == 1, 1, values - 1)

condition <- is.na(DATA$group) & is.na(DATA$name1)
values <- diff(c(which(!is.na(DATA$name2[condition])), nrow(DATA[condition,])+1))
Nodes$size[!is.na(DATA$name2)] <- ifelse(values == 1, 1, values - 1)

condition <- is.na(DATA$group) & is.na(DATA$name1) & is.na(DATA$name2)
values <- diff(c(which(!is.na(DATA$name3[condition])), nrow(DATA[condition,])+1))
Nodes$size[!is.na(DATA$name3)] <- ifelse(values == 1, 1, values - 1)

condition <- is.na(DATA$group) & is.na(DATA$name1) & is.na(DATA$name2) & is.na(DATA$name3)
values <- diff(c(which(!is.na(DATA$name4[condition])), nrow(DATA[condition,])+1))
Nodes$size[!is.na(DATA$name4)] <- ifelse(values == 1, 1, values - 1)


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



interLinks <- cbind(
  id = Nodes$id[!is.na(DATA$links)], 
  links = DATA$links[!is.na(DATA$links)]
)
interLinks <- rbind(
  list_rbind(apply(interLinks[str_detect(interLinks[, 2], ", "),], 1, function(x) {
    data.frame(id = x[1],
               links = unlist(str_split(x[2], ", ")))
  })),
  interLinks[!str_detect(interLinks[, 2], ", "),]
)
interLinks <- left_join(
  interLinks,
  Nodes[, c("name", "id")],
  by = c("links" = "name")
)[, c(1, 3)]
interLinks$value <- 1
colnames(interLinks) <- c("source", "target", "value")
interLinks$source <- as.numeric(interLinks$source)

Links <- rbind(Links, interLinks) 


forceNetwork(Links = Links, 
             Nodes = Nodes, 
             Source = "source",
             Target = "target", 
             Value = "value", 
             NodeID = "name",
             Group = "group", 
             Nodesize = "size",
             colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"), 
             radiusCalculation = "d.nodesize*2+6",
             opacity = 1, 
             opacityNoHover = 0.85,
             charge = -1000,
             fontSize = 100,
             zoom = TRUE)