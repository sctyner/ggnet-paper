## network
if (!require(ggnetwork, quietly = TRUE)) {
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

## ggnetwork
if (!require(ggnetwork, quietly = TRUE) ||
    packageVersion("ggnetwork") < "0.2.0") {
  devtools::install_github("briatte/ggnetwork")
}
library(ggnetwork)

# note: plot.network might be slower because it actually prints the plots, so to
# compare the methods, we need to evaluate the ggplot plots by printing them too

for (i in seq(20, 1000, 20)) {

  f = paste0("runtimes", sprintf("%04.0f", i), ".csv")
  if (!file.exists(f)) {

    n = network(sna::rgraph(i))

    cat("Timing functions with networks of size", i, "...\n")

    d = data.frame()
    p = txtProgressBar(0, 100, style = 3)

    for (j in 1:100) {

      t0 = system.time({
        plot.network(n)
      })[1]

      t1 = system.time({
        print(ggnet2(n))
      })[1]

      t2 = system.time({
        print(ggplot(data = data.frame(sna::as.edgelist.sna(n))) +
                geom_net(aes(from_id = X1, to_id = X2)))
      })[1]

      t3 = system.time({
        print(ggplot(ggnetwork(n),
                     aes(x, y, xend = xend, yend = yend)) +
                geom_edges() +
                geom_nodes())
      })[1]

      d = rbind(d, data.frame(
        network_size = i,
        iteration = j,
        network = t0,
        ggnet2 = t1,
        geomnet = t2,
        ggnetwork = t3,
        row.names = NULL
      ))

      setTxtProgressBar(p, j)
      dev.off()

    }

    write.csv(d, f, row.names = FALSE)

  }

}
