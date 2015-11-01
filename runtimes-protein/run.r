## igraph (current: v1.0.1)
if (!require(igraph, quietly = TRUE)) {
  install.package("igraph")
}
library(igraph)

## network (current: v1.13.0)
if (!require(network, quietly = TRUE)) {
  install.package("network")
}
library(network)

## ggplot2
if (!require(ggnetwork, quietly = TRUE) ||
    packageVersion("ggplot2") < "1.0.1.9003") {
  devtools::install_github("hadley/ggplot2")
}
library(ggplot2)

## ggnet2
if (!require(ggnet, quietly = TRUE)) {
  devtools::install_github("briatte/ggnet")
}
library(ggnet)

## geom_net
if (!require(geomnet, quietly = TRUE)) {
  devtools::install_github("sctyner/geomnet")
}
library(geomnet) # also currently requires dplyr

## ggnetwork
if (!require(ggnetwork, quietly = TRUE) ||
    packageVersion("ggnetwork") < "0.2.0") {
  devtools::install_github("briatte/ggnetwork")
}
library(ggnetwork)

## other packages
library(dplyr)
library(tidyr)

## pre-loading
library(scales)
library(sna)

d = data.frame()

if (!file.exists("runtimes-protein-100.csv")) {
  
  for (i in 1:100) {
  
    cat("Iteration", sprintf("%3.0f", i), "/ 100\n")
  
    n = as.matrix(protein$edges[, 1:2])
    n = igraph::graph_from_edgelist(n, directed = FALSE)
  
    t1 = system.time({
      plot(n, vertex.label = NA, layout = layout_randomly)
    })[1]
  
    n = network(protein$edges[, 1:2], directed = FALSE)

    t2 = system.time({
     plot(n, coord = gplot.layout.random(n, NULL))
    })[1]

    t3 = system.time({
      print(ggnet2(n, mode = "random"))
    })[1]

    t4 = system.time({
      print(ggplot(data = protein$edges, aes(from_id = from, to_id = to)) +
              geom_net(layout = "random"))
    })[1]

    t5 = system.time({
      print(ggplot(ggnetwork(n, layout = "random"),
                   aes(x, y, xend = xend, yend = yend)) +
              geom_edges() +
              geom_nodes())
    })[1]
  
    d = rbind(d, data.frame(
      iteration = i,
      igraph = t1,
      network = t2,
      ggnet2 = t3,
      geomnet = t4,
      ggnetwork = t5,
      row.names = NULL
    ))

  }

  write.csv(d, file = "runtimes-protein-100.csv", row.names = FALSE)
  
}

g = read.csv("runtimes-protein-100.csv", stringsAsFactors = FALSE) %>%
      gather(`Visualization approach`, time, -iteration)

ggplot(g, aes(x = `Visualization approach`, y = time,
              fill = `Visualization approach`)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  labs(y = "\nPlotting time (seconds)", x = "Visualization approach\n") +
  guides(fill = FALSE) +
  theme_classic(18) +
  theme(axis.text = element_text(size = rel(1)))
