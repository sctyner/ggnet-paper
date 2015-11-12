

## ----load_packages, results='hide'---------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2) # needs to be version ≥ 1.0.1.9003
library(scales)

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
    packageVersion("ggnetwork") < "0.2.1") {
  devtools::install_github("briatte/ggnetwork")
}
library(ggnetwork)

## pre-load
library(network)
library(sna)

## ----madmen_geom_net, fig.width=7, fig.height=7.5------------------------
# code for geom_net
# data step: merge edges and nodes
MMnet <- merge(madmen$edges, madmen$vertices,
               by.x = "Name1", by.y = "label", all = TRUE)
# create plot
ggplot(data = MMnet, aes(from_id = Name1, to_id = Name2)) +
  geom_net(aes(colour = Gender), size = 4, label = TRUE,
           vjust = -0.6, ecolour = "grey60") +
  scale_colour_manual(values = c("#FF69B4", "#0099ff")) +
  xlim(c(-0.05, 1.05)) +
  theme_net() +
  theme(legend.position = "bottom")

## ----blood_common--------------------------------------------------------
data(blood, package = "geomnet")

## ----blood_ggnet2,  echo=TRUE, fig.width=6, fig.height=6-----------------
# plot with ggnet2
ggnet2(network(blood$edges[, 1:2]), mode = "circle", size = 15,
       label = TRUE, arrow.size = 10, arrow.gap = 0.05, vjust = 0.5,
       node.color = "darkred", label.color = "grey80")

## ----blood_geom_net, echo=TRUE, fig.width=6, fig.height=6----------------
# plot with geomnet
ggplot(data = blood$edges, aes(from_id = from, to_id = to)) +
  geom_net(colour = "darkred", layout = "circle", label = TRUE, size = 15,
           directed = TRUE, vjust = 0.5, labelcolour = "grey80",
           arrowsize = 1.5, linewidth = 0.5, arrowgap = 0.05,
           selfies = TRUE, ecolour = "grey40") +
  theme_net()

## ----blood_ggnetwork, echo=TRUE, fig.width=6, fig.height=6---------------
# plot with ggnetwork
ggplot(ggnetwork(network(blood$edges[, 1:2]),
                 layout = "circle", arrow.gap = 0.05),
       aes(x, y, xend = xend, yend = yend)) +
  geom_edges(color = "grey50",
             arrow = arrow(length = unit(10, "pt"), type = "closed")) +
  geom_nodes(size = 15, color = "darkred") +
  geom_nodetext(aes(label = vertex.names), color = "grey80") +
  theme_blank()

## ----email_ggnet2, size="footnotesize", opts.label="codefig", echo=TRUE, out.width='\\textwidth'----
em.cet <- as.character(
  email$nodes$CurrentEmploymentType)
names(em.cet) = email$nodes$label

edges <- subset(email$edges, nrecipients < 54)
em.net <- edges[, c("From", "to") ]
em.net <- network(em.net)
em.net %v% "curr_empl_type" <-
  em.cet[ network.vertex.names(em.net) ]

ggnet2(em.net, color = "curr_empl_type",
       size = 4, palette = "Set1",
       arrow.size = 5, arrow.gap = 0.02,
       edge.alpha = 0.25,
       edge.color = c("color", "grey50"),
       color.legend = "Employment Type") +
  theme(legend.position = "bottom")

## ----email_geom_net, size="footnotesize", opts.label="codefig", echo=TRUE, out.width='\\textwidth'----
emailnet <- merge(
  subset(email$edges, nrecipients < 54),
  email$nodes,
  by.x = "From", by.y = "label", all = TRUE)

ggplot(data = emailnet,
       aes(from_id = From, to_id = to)) +
  geom_net(
    aes(colour = CurrentEmploymentType,
        group = CurrentEmploymentType,
        linewidth = 3 * (...samegroup.. / 8 + .125)),
    ealpha = 0.25,
    size = 4, curvature = 0.05,
    directed = TRUE, arrowsize = 0.5) +
  scale_colour_brewer("Employment Type", palette = "Set1") +
  theme_net() +
  theme(legend.position = "bottom")

## ----email_ggnetwork, size="footnotesize", opts.label="codefig", echo=TRUE, out.width='\\textwidth'----
ggplot(ggnetwork(em.net, arrow.gap = 0.02),
       aes(x, y, xend = xend, yend = yend)) +
  geom_edges(
    aes(color = curr_empl_type),
    alpha = 0.25,
    arrow = arrow(length = unit(5, "pt"),
                  type = "closed"),
    curvature = 0.05) +
  geom_nodes(aes(color = curr_empl_type),
             size = 4) +
  scale_color_brewer("Employment Type",
                     palette = "Set1") +
  theme_blank() +
  theme(legend.position = "bottom")

## ----email_facet_ggnet2, fig.height=4, fig.width=8, echo=TRUE, out.width='\\textwidth'----
# ggnet2 code for the email network facetted by day as shown in fig.4a

# data preparation
em.day <- subset(email$edges, nrecipients < 54)[, c("From", "to", "day") ]
em.day <- lapply(unique(em.day$day),
                 function(x) subset(em.day, day == x)[, 1:2 ])
em.day <- lapply(em.day, network, directed = TRUE)
for (i in 1:length(em.day)) {
  em.day[[ i ]] %v% "curr_empl_type" <-
    em.cet[ network.vertex.names(em.day[[ i ]]) ]
  em.day[[ i ]] %n% "day" <- unique(email$edges$day)[ i ]
}

# plot ggnet2
g <- list(length(em.day))
for (i in 1:length(em.day)) {
  g[[ i ]] <- ggnet2(em.day[[ i ]], size = 2, color = "curr_empl_type",
                     palette = "Set1", arrow.size = 0, arrow.gap = 0.01,
                     edge.alpha = 0.1, legend.position = "none") +
    ggtitle(paste("Day", em.day[[ i ]] %n% "day")) +
    theme(panel.border = element_rect(color = "grey50", fill = NA),
          aspect.ratio = 1)
}
gridExtra::grid.arrange(grobs = g, nrow = 2)

## ----email_facet_geom_net, fig.height=4.5, fig.width=8, echo=TRUE, out.width='\\textwidth'----
# geomnet code for the  email network facetted by day as shown in fig.4b

# data step: making sure that there is one entry for each person on each day
employee <- data.frame(expand.grid(label = unique(email$nodes$label),
                                   day = unique(email$edges$day)))
employee <- merge(employee, email$nodes, by = "label")
emailnet <- merge(subset(email$edges, nrecipients < 54), employee,
                  by.x = c("From", "day"), by.y = c("label", "day"),
                  all = TRUE)

# creating the plot
ggplot(data = emailnet, aes(from_id = From, to_id = to)) +
  geom_net(aes(colour = CurrentEmploymentType,
               group = CurrentEmploymentType,
               linewidth = 2 * (...samegroup.. / 8 + .125)),
           fiteach = TRUE, ealpha = 0.5, size = 1.5) +
  scale_colour_brewer("Employment Type", palette = "Set1") +
  theme_net() +
  facet_wrap(~day, nrow = 2, labeller = "label_both") +
  theme(legend.position = "bottom",
        panel.border = element_rect(fill = NA, colour = "grey60"),
        plot.margin = unit(c(0, 0, 0, 0), "mm"))

## ----email_facet_ggnetwork, fig.height=4.5, fig.width=8,echo=TRUE, out.width='\\textwidth'----
# ggnetwork code for the  email network facetted by day as shown in fig.4c

# create the network and aesthetics
edges <- subset(email$edges, nrecipients < 54)
edges <- edges[, c("From", "to", "day") ]
em.net <- network(edges[, 1:2])
set.edge.attribute(em.net, "day", edges[, 3])
em.net %v% "curr_empl_type" <- em.cet[ network.vertex.names(em.net) ]

# create the plot
ggplot(ggnetwork(em.net, arrow.gap = 0.02, by = "day"),
       aes(x, y, xend = xend, yend = yend)) +
  geom_edges(
    aes(color = curr_empl_type),
    alpha = 0.25,
    arrow = arrow(length = unit(5, "pt"),
                  type = "closed")) +
  geom_nodes(aes(color = curr_empl_type),
             size = 1.5) +
  scale_color_brewer("Employment Type",
                     palette = "Set1") +
  facet_wrap(~day, nrow = 2, labeller = "label_both") +
  theme_facet(legend.position = "bottom")

## ----theme_ggnet2, size="footnotesize", opts.label="codefig", echo=TRUE----
te.net <- network(theme_elements$edges)
te.net %v% "size" <-
  sqrt(10 * (sna::degree(te.net) + 1))

ggnet2(te.net, label = TRUE, color = "white",
       label.size = "size", layout.exp = 0.15)

## ----theme_geom_net, size="footnotesize", opts.label="codefig", echo=TRUE, fig.width=7, fig.height = 7----
# data step: merge nodes and edges and
# introduce a degree-out variable
TEnet <- merge(
  theme_elements$edges,
  theme_elements$vertices,
  by.x = "parent", by.y = "name", all = TRUE)
TEnet <- TEnet %>%
  group_by(parent) %>%
  mutate(degree = sqrt(10 * n() + 1))

# create plot:
ggplot(data = TEnet,
       aes(from_id = parent, to_id = child)) +
  geom_net(
    aes(fontsize = degree), directed = TRUE,
    label = TRUE, vjust = -.5, size = 3,
    ecolour = "grey70", arrowsize = 0.5,
    linewidth = 0.5) +
  theme_net() +
  xlim(c(-0.05, 1.05))

## ----theme_ggnetwork, size="footnotesize", opts.label="codefig", echo=TRUE----
ggplot(ggnetwork(te.net),
       aes(x, y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_nodes(size = 12, color = "white") +
  geom_nodetext(
    aes(size = size, label = vertex.names)) +
  scale_size_continuous(range = c(4, 8)) +
  guides(size = FALSE) +
  theme_blank()

## ----madmen_prepare, echo=TRUE-------------------------------------------
# data step for both ggnet2 and ggnetwork
# create undirected network
mm.net <- network(madmen$edges[, 1:2], directed = FALSE)
# gender vertex attribute
rownames(madmen$vertices) <- madmen$vertices$label
mm.net %v% "gender" <- as.character(
  madmen$vertices[ network.vertex.names(mm.net), "Gender"]
)
# gender color palette
mm.col <- c("female" = "#ff69b4", "male" = "#0099ff")

## ----madmen_ggnet2, echo=TRUE, out.width='\\textwidth', fig.width=9, fig.height=9----
ggnet2(mm.net, color = mm.col[ mm.net %v% "gender" ],
       label = TRUE, label.color = mm.col[ mm.net %v% "gender" ],
       size = 4, vjust = -0.6)

## ----madmen_ggnetwork, echo=TRUE,  out.width='\\textwidth', fig.width=9, fig.height=9----
ggplot(data = ggnetwork(mm.net), aes(x, y, xend = xend, yend = yend)) +
  geom_edges(color = "grey50") +
  geom_nodes(aes(colour = gender), size = 4) +
  geom_nodetext(aes(colour = gender, label = vertex.names),
                size = 4, vjust = -0.6) +
  scale_colour_manual(values = mm.col) +
  xlim(c(-0.05, 1.05)) +
  theme_blank() +
  theme(legend.position = "bottom")

## ----madmen2_prepare, echo = TRUE----------------------------------------
# data step for ggnet2 and ggnetwork
# create directed network
mm.dir <- network(mm.directed$edges, directed = TRUE)
# gender vertex attribute
rownames(mm.directed$vertices) <- mm.directed$vertices$label
mm.dir %v% "gender" <- as.character(
  mm.directed$vertices[ network.vertex.names(mm.dir), "Gender" ]
)

## ----madmen2_ggnet2, size="footnotesize", opts.label="codefig", echo=TRUE----
# see fig.7 for data preparation

ggnet2(
  mm.dir, mode = "fruchtermanreingold", size = 3,
  color = mm.col[ mm.dir %v% "gender" ],
  label = TRUE,
  label.color = mm.col[ mm.dir %v% "gender" ],
  hjust = -0.1, legend.position = "bottom",
  layout.exp = 0.15, arrow.size = 7.5,
  arrow.gap = 0.02
)

## ----madmen2_geom_net, size="footnotesize", opts.label="codefig", echo=TRUE----
MM2net <- merge(
  mm.directed$edges,
  mm.directed$vertices,
  by.x = "Name1", by.y = "label", all = TRUE)

ggplot(data = MM2net,
       aes(from_id = Name1, to_id = Name2)) +
  geom_net(
    aes(colour = Gender),  directed = TRUE,
    label = TRUE, ecolour = "grey50",
    linewidth = 0.5, size = 2.5, vjust = -.5,
    layout = "fruchtermanreingold") +
  scale_colour_manual(
    values = c("#ff69b4", "#0099ff")) +
  xlim(c(-0.1, 1.1)) +
   theme_net() +
  theme(legend.position = "bottom")

## ----madmen2_ggnetwork, size="footnotesize", opts.label="codefig", echo=TRUE, dependson="madmen2_ggnet2"----
# see fig.7 for data preparation

ggplot(
  ggnetwork(mm.dir,
            layout = "fruchtermanreingold"),
       aes(x, y, xend = xend, yend = yend)) +
  geom_edges(
    color = "grey50",
    arrow = arrow(length = unit(10, "pt"),
                  type = "closed")) +
  geom_nodes(size = 3, aes(color = gender)) +
  geom_nodetext(aes(label = vertex.names,
                    color = gender),
                hjust = -0.1) +
  scale_color_manual(values = mm.col) +
  theme_blank() +
  theme(legend.position = "bottom")

## ----football_ggnet2, size="footnotesize", opts.label="codefig", echo=TRUE----
rownames(football$vertices) <-
  football$vertices$label

fb.net <- network(football$edges[, 1:2],
                  directed = FALSE)
set.edge.attribute(
  fb.net, "same.conf",
  football$edges$same.conf)
fb.net %v% "conf" <-
  football$vertices[
    network.vertex.names(fb.net), "value"
    ]

ggnet2(fb.net, mode = "fruchtermanreingold",
       color = "conf",  palette = "Paired",
       color.legend = "Conference",
       edge.color = c("color", "grey75"))

## ----football_geom_net, size="footnotesize", fig.height=10, opts.label="codefig", echo=TRUE----
# data step: merge vertices and edges
ftnet <- merge(
  football$edges, football$vertices,
  by.x = "from", by.y = "label", all = TRUE)

# label independent schools
ftnet$schools <- ifelse(
  ftnet$value == "Independents", ftnet$from, "")

# create data plot
ggplot(data = ftnet,
       aes(from_id = from, to_id = to)) +
  geom_net(
    aes(colour = value, group = value,
        linetype = factor(same.conf != 1),
        label = schools),
    linewidth = 0.5,
    size = 5, vjust = -0.75, alpha = 0.3,
    layout = 'fruchtermanreingold') +
  theme_net() +
  theme(legend.position = "bottom") +
  scale_colour_brewer("Conference", palette = "Paired")  +
  guides(linetype = FALSE)

## ----football_ggnetwork, size="footnotesize", opts.label="codefig", echo=TRUE----
ggplot(
  ggnetwork(fb.net, layout = "fruchtermanreingold"),
  aes(x, y, xend = xend, yend = yend)) +
  geom_edges(
    aes(linetype = as.factor(same.conf)),
    color = "grey50") +
  geom_nodes(aes(color = conf), size = 4) +
  scale_color_brewer("Conference",
                     palette = "Paired") +
    scale_linetype_manual(values = c(2,1)) +
  guides(linetype = FALSE) +
  theme_blank()

## ----bikes_geom_net, echo=TRUE, warning=FALSE, fig.keep='all'------------
# code for geomnet
# data preparation
tripnet <- merge(bikes$trips, bikes$stations, by.x = "Start.station",
                 by.y = "name", all = TRUE)

tripnet$Metro = FALSE
idx <- grep("Metro", tripnet$Start.station)
tripnet$Metro[idx] <- TRUE

# plot the bike sharing network
ggplot(aes(from_id = Start.station, to_id = End.station), data = tripnet) +
  geom_net(aes(linewidth = n / 15, colour = Metro),
           label = TRUE, vjust = -0.5) +
  theme_net() +
  xlim(c(-0.1, 1.1)) +
  scale_colour_manual("Metro Station", values = c("grey40", "darkorange")) +
  theme(legend.position = "bottom")


## ----bikes_prepare, echo = TRUE------------------------------------------
# data preparation for ggnet2 and ggnetwork
bikes.net <- network(bikes$trips[, 1:2 ], directed = FALSE)
set.edge.attribute(bikes.net, "n", bikes$trips[, 3 ] / 15)
bikes.net %v% "station" <-
  grepl("Metro", network.vertex.names(bikes.net))
bikes.net %v% "station" <-
  1 + as.integer(bikes.net %v% "station")
rownames(bikes$stations) <- bikes$stations$name
bikes.net %v% "lon" <-
  bikes$stations[ network.vertex.names(bikes.net), "long" ]
bikes.net %v% "lat" <-
  bikes$stations[ network.vertex.names(bikes.net), "lat" ]
bikes.col <- c("grey40", "darkorange")

## ----bikes_ggnet2, echo = TRUE, fig.keep='all'---------------------------
# Fruchterman-Reingold placement
ggnet2(bikes.net, size = 4, label = TRUE, vjust = -0.5,
       edge.size = "n", color = bikes.col[ bikes.net %v% "station" ],
       label.color = bikes.col[ bikes.net %v% "station" ],
       layout.exp = 1.1)

## ----bikes_ggnetwork, echo = TRUE, fig.keep='all'------------------------
# Fruchterman-Reingold placement
ggplot(data = ggnetwork(bikes.net),
       aes(x, y, xend = xend, yend = yend)) +
  geom_edges(aes(size = n), color = "grey40") +
  geom_nodes(aes(color = factor(station)), size = 4) +
  geom_nodetext(aes(label = vertex.names, color = factor(station)),
                vjust = -0.5) +
  scale_size_continuous("Trips", breaks = c(2, 4, 6), labels = c(30, 60, 90)) +
  scale_colour_manual("Metro station", labels = c("FALSE", "TRUE"),
                      values = c("grey40", "darkorange")) +
  theme_blank() +
  theme(legend.position = "bottom", legend.box = "horizontal")

## ----geographic_common, echo=TRUE----------------------------------------
data(metro_map)

## ----geographic_geomnet, echo=TRUE---------------------------------------
# geomnet: overlay bike sharing network on geographic map
ggplot(data = metro_map, aes(x = x, y = y)) +
  geom_raster(fill = metro_map$fill, alpha = 0.75) +
  geom_net(data = tripnet, layout = NULL, label = TRUE,
           vjust = -0.5, ealpha = 0.5,
           aes(from_id = Start.station,
               to_id = End.station,
               x = long, y = lat,
               linewidth = n / 15,
               colour = Metro)) +
  scale_colour_manual("Metro Station", values = c("grey40", "darkorange")) +
  theme_net() %+replace% theme(aspect.ratio=NULL) +
  theme(legend.position = "bottom")

## ----geographic_ggnet2, echo=TRUE----------------------------------------
# geographic placement using ggnet2
ggnet2(bikes.net, size = 4, label = TRUE, vjust = -0.5,
       mode = c("lon", "lat"),
       edge.size = "n",
       color = bikes.col[ bikes.net %v% "station" ],
       label.color = bikes.col[ bikes.net %v% "station" ],
       layout.exp = 1.1)

## ----geographic_ggnetwork, echo=TRUE-------------------------------------
# geographic placement using ggnetwork
coords <- c(bikes.net %v% "lon", bikes.net %v% "lat")
map_01 <- metro_map
map_01$x <- scales::rescale(map_01$x)
map_01$y <- scales::rescale(map_01$y)
ggplot(data = ggnetwork(bikes.net, layout = matrix(coords, ncol = 2)),
       aes(x, y, xend = xend, yend = yend)) +
  geom_raster(data = map_01, aes(x = x, y = y, xend = NULL, yend = NULL),
              fill = map_01$fill, alpha = 0.75) +
  geom_edges(aes(size = n), color = "grey40") +
  geom_nodes(aes(color = factor(station)), size = 4) +
  geom_nodetext(aes(label = vertex.names, color = factor(station)),
                vjust = -0.5) +
  scale_size_continuous("Trips", breaks = c(2, 4, 6), labels = c(30, 60, 90)) +
  scale_colour_manual("Metro station", labels = c("FALSE", "TRUE"),
                      values = c("grey40", "darkorange")) +
  scale_x_continuous(limits = c(-0.1, 1.1)) +
  scale_y_continuous(limits = c(-0.1, 1.1)) +
  theme_blank() +
  theme(legend.position = "bottom", legend.box = "horizontal")

## ----yeast_ggnet2, echo=TRUE, eval = FALSE, cache=FALSE------------------
# plot with ggnet2
ggnet2(network(protein$edges[,1:2]), size = 2, color = "magenta",
     mode = "random", layout.par = list(dist = "uniang"),
     edge.alpha = 0.05)

## ----yeast_geom_net, echo=TRUE, cache=FALSE------------------------------
# plot with geom_net
ggplot(data = protein$edges, aes(from_id = from, to_id = to)) +
  geom_net(alpha = 0.25, ealpha = 0.05, size = 2, colour = "magenta",
           ecolour = "grey70", linewidth = 0.5,
           layout = "random", layout.par = list(dist = "uniang")) +
  theme_net()

## ----yeast_ggnetwork, echo=TRUE, eval = FALSE,  cache=FALSE--------------
# plot with ggnetwork
ggplot(ggnetwork(protein$edges[, 1:2], layout = "random", dist = "uniang"),
      aes(x, y, xend = xend, yend = yend)) +
 geom_edges(color = "grey70", lwd = 0.5, alpha = 0.05) +
 geom_nodes(alpha = 0.25, color = "magenta", size = 2) +
 theme_blank()

