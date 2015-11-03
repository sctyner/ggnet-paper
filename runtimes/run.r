## igraph (current: v1.0.1)
if (!require(igraph, quietly = TRUE)) {
  install.packages("igraph")
}
library(igraph)

## network (current: v1.13.0)
if (!require(network, quietly = TRUE)) {
  install.packages("network")
}
library(network)

## ggplot2
if (!require(ggplot2, quietly = TRUE) ||
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

# note: plot.network might be slower because it actually prints the plots, so to
# compare the methods, we need to evaluate the ggplot plots by printing them too

for (k in 0:9) {

  for (i in seq(250, 25, -25)) {
    
    f = paste0("runtimes-", sprintf("%04.0f", i), "-", k, ".csv")
    
    if (!file.exists(f)) {

      d = data.frame()

      for (j in 1:10) {

        r = sna::rgraph(i, tprob = 0.2)
      
        cat("Network size", i,
            "iteration", sprintf("%3.0f", 10 * k + j), "/ 100\n")

        n = igraph::graph_from_adjacency_matrix(r, mode = "undirected")

        t1 = system.time({
          plot(n, vertex.label = NA)
        })[1]

        n = network::network(r, directed = FALSE)

        t2 = system.time({
          plot.network(n)
        })[1]

        t3 = system.time({
          print(ggnet2(n))
        })[1]

        e = data.frame(sna::as.edgelist.sna(n))

        t4 = system.time({
          print(ggplot(data = e) +
                  geom_net(aes(from_id = X1, to_id = X2)))
        })[1]

        t5 = system.time({
          print(ggplot(ggnetwork(n),
                       aes(x, y, xend = xend, yend = yend)) +
                  geom_edges() +
                  geom_nodes())
        })[1]

        d = rbind(d, data.frame(
          network_size = i,
          iteration = 10 * k + j,
          igraph = t1,
          network = t2,
          ggnet2 = t3,
          geomnet = t4,
          ggnetwork = t5,
          row.names = NULL
        ))

        dev.off()

      }

      write.csv(d, f, row.names = FALSE)

    }
        
  }
  
  g = list.files(pattern = "runtimes-(.*)csv$") %>%
        lapply(read.csv, stringsAsFactors = FALSE) %>%
        bind_rows %>%
        gather(`Visualization approach`, time, -network_size, -iteration) %>%
        group_by(network_size, `Visualization approach`) %>%
        summarise(mean_time = mean(time),
                  q05 = quantile(time, 0.05),
                  q95 = quantile(time, 0.95))

  g = ggplot(g, aes(x = network_size, y = mean_time,
                    fill = `Visualization approach`)) +
    geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 1/3) +
    geom_line(aes(color = `Visualization approach`)) +
    scale_y_continuous(breaks = seq(0, max(g$q95), 5)) +
    scale_x_continuous(breaks = seq(25, 250, 25)) +
    labs(y = "Average plotting time (seconds)\n", x = "\nNetwork size") +
    theme_classic(18) +
    theme(
      axis.text = element_text(size = rel(1)),
      legend.text = element_text(size = rel(1)),
      legend.justification = c(0,1),
      legend.position = c(0,1)
    )
  
  ggsave("runtimes.pdf", g, width = 7, height = 7)
  
}
