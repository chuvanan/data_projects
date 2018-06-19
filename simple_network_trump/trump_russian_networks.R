



library(igraph)


g <- graph.formula(Stone-Putin,
                   Tillerson-Putin,
                   Tillerson-business,
                   Trump.Jr-business,
                   Ross-business,
                   Kushner-business,
                   Kushner-Kislyak,
                   Trump-Putin,
                   Trump-Kislyak,
                   Trump-business,
                   Sessions-Kislyak,
                   Flynn-Kislyak,
                   Flynn-Putin,
                   Gordon-Kislyak,
                   Manafort-Putin,
                   Page-Kislyak,
                   Page-business)


plot(g, edge.width=1.2, vertex.color= g$color,
     vertex.size=8, edge.curved=0.2, vertex.label.family = "Lato Light",
     vertex.label.font = 2, vertex.label.cex = 1.1,
     vertex.frame.color="gray", vertex.label.color="black",
     frame = T, vertex.shape = g$pch)
legend("topleft", legend = c("US gov", ""))


g$country <- c("usa", "rus", "usa", "rus", "usa", "usa", "usa",
               "rus", "usa", "usa", "usa", "usa", "usa", "usa")
g$conn <- c("out", "in", "in", "out", "out", "in", "in", "in", "in",
            "in", "out", "out", "out", "out")
g$color <- ifelse(g$country == "usa", "yellow", "lightblue")
g$pch <- ifelse(g$conn == "out", "none", "circle")

V(g)




