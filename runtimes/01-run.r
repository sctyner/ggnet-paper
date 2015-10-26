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
library(geomnet)
library(dplyr) # currently required by geomnet

## ggnetwork
if (!require(ggnetwork, quietly = TRUE) ||
    packageVersion("ggnetwork") < "0.2.0") {
  devtools::install_github("briatte/ggnetwork")
}
library(ggnetwork)

# note: plot.network might be slower because it actually prints the plots, so to
# compare the methods, we need to evaluate the ggplot plots by printing them too

for (i in seq(20, 1000, 20)) {

  f = paste0("runtimes-", sprintf("%04.0f", i), ".csv")
  if (!file.exists(f)) {

    r = sna::rgraph(i)
    d = data.frame()

    for (j in 1:100) {

      cat("Timing networks of size", i,
          "iteration", sprintf("%3.0f", j), "/ 100\n")

      n = graph.adjacency(r)

      t1 = system.time({
        plot(n)
      })[1]

      n = network(r)

      t2 = system.time({
        plot.network(n)
      })[1]

      t3 = system.time({
        print(ggnet2(n))
      })[1]

      t4 = system.time({
        print(ggplot(data = data.frame(sna::as.edgelist.sna(n))) +
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
        iteration = j,
        igraph = t1,
        network = t2,
        ggnet2 = t3,
        geomnet = t4,
        ggnetwork = t5,
        row.names = NULL
      ))

      dev.off()

    }

    cat("\n")
    write.csv(d, f, row.names = FALSE)

  }

}
