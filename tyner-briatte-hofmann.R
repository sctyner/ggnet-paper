## ----setup_knitr, echo=FALSE---------------------------------------------
library(knitr)
opts_chunk$set(
  tidy = FALSE, echo = FALSE, cache = FALSE, eval = TRUE,
  message = FALSE, warning = FALSE, highlight = FALSE, 
  background = '#FFFFFF', dev = 'pdf', 
  fig.height = 8, fig.width = 8, fig.align = 'center', fig.show = 'hide'
)

## From Josh O'Brien's stackoverflow answer:
## http://stackoverflow.com/questions/11030898/knitr-how-to-align-code-and-plot-side-by-side
## These two settings control text width in codefig vs. usual code blocks
partWidth <- 35
fullWidth <- 60
options(width = fullWidth)

##  (1) CHUNK HOOK FUNCTION
##   First, to set R's textual output width on a per-chunk basis, we
## need to define a hook function which temporarily resets global R's
## option() settings, just for the current chunk
knit_hooks$set(r.opts = local({
    ropts <- NA
    function(before, options, envir) {
        if (before) {
            ropts <<- options(options$r.opts)
        } else {
            options(ropts)
        }
    }
}))

## (2) OUTPUT HOOK FUNCTION

##   Define a custom output hook function. This function processes _all_
## evaluated chunks, but will return the same output as the usual one,
## UNLESS a 'codefig' argument appeared in the chunk's header.  In that
## case, wrap the usual textual output in LaTeX code placing it in a
## narrower adjustbox environment and setting the graphics that it
## produced in another box beside it.

defaultChunkHook <- environment(knit_hooks[["get"]])$defaults$chunk

codefigChunkHook <- function(x, options) {
        main <-  defaultChunkHook(x, options)
        before <-
            "\\vspace{1em}\n
             \\begin{adjustbox}{valign=t}\n
             \\begin{minipage}{.49\\textwidth}\n"
        after <-
            paste("\\vspace{1em}\n
                   \\end{minipage}\n
                  \\begin{minipage}{.49\\textwidth}\n",
                   paste0("\\includegraphics[width=\\textwidth]{figure/",
                          options[["label"]], "-1.pdf}\n
                          \\end{minipage}\n
                          \\end{adjustbox}"),
                          sep = "\n")
    ## Was a codefig option supplied in chunk header?
    ## If so, wrap code block and graphical output with needed LaTeX code.
    if (!is.null(options$codefig)) {
      return(sprintf("%s %s %s", before, main, after))
    } else {
      return(main)
    }
}

knit_hooks[["set"]](chunk = codefigChunkHook)


## (3) TEMPLATE
##   codefig=TRUE is just one of several options needed for the
## side-by-side code block and a figure to come out right. Rather
## than typing out each of them in every single chunk header, we
## define a _template_ which bundles them all together. Then we can
## set all of those options simply by typing opts.label="codefig".

opts_template[["set"]](
codefig = list(codefig = TRUE, fig.show = "hide",
               r.opts = list(width = partWidth),
               tidy.opts = list(width.cutoff = partWidth)))

## ----load_packages, echo=FALSE, results='hide'---------------------------
library(dplyr)
library(tidyr)
library(ggplot2) # needs to be version ≥ 1.0.1.9003
library(scales)

## ggnet2
# if (!require(ggnet, quietly = TRUE)) {
#   devtools::install_github("briatte/ggnet")
# }
# library(ggnet)
if (!require(GGally, quietly = TRUE)) {
  devtools::install_github("ggobi/ggally")
}

## geom_net
if (!require(geomnet, quietly = TRUE)) {
  devtools::install_github("sctyner/geomnet")
}

## ggnetwork
if (!require(ggnetwork, quietly = TRUE) ||
    packageVersion("ggnetwork") < "0.2.1") {
  devtools::install_github("briatte/ggnetwork")
}

## pre-load
library(network)
library(sna)

## ----madmen_geom_net, fig.width=7, fig.height=7.5------------------------
# make data accessible 
data(madmen, package = "geomnet")

# code for geom_net
# data step: join the edge and node data with a fortify call
MMnet <- fortify(as.edgedf(madmen$edges), madmen$vertices)
# create plot
set.seed(10052016)
ggplot(data = MMnet, aes(from_id = from_id, to_id = to_id)) +
  geom_net(aes(colour = Gender), layout.alg = "kamadakawai", 
           size = 2, labelon = TRUE, vjust = -0.6, ecolour = "grey60",
           directed =FALSE, fontsize = 3, ealpha = 0.5) +
  scale_colour_manual(values = c("#FF69B4", "#0099ff")) +
  xlim(c(-0.05, 1.05)) +
  theme_net() +
  theme(legend.position = "bottom")

## ----madmen_ggnet2, eval=FALSE, echo=TRUE, fig.cap="The code required to generate Figure~\\ref{fig.cap:madmen} using the \\code{ggnet2} function in the \\pkg{GGally} package."----
## library(GGally)
## library(network)
## # make the data available
## data(madmen, package = 'geomnet')
## # data step for both ggnet2 and ggnetwork
## # create undirected network
## mm.net <- network(madmen$edges[, 1:2], directed = FALSE)
## # create node attribute (gender)
## rownames(madmen$vertices) <- madmen$vertices$label
## mm.net %v% "gender" <- as.character(
##   madmen$vertices[ network.vertex.names(mm.net), "Gender"]
## )
## # gender color palette
## mm.col <- c("female" = "#ff69b4", "male" = "#0099ff")
## # create plot for ggnet2
## set.seed(10052016)
## ggnet2(mm.net, color = mm.col[ mm.net %v% "gender" ],
##        labelon = TRUE, label.color = mm.col[ mm.net %v% "gender" ],
##        size = 2, vjust = -0.6, mode = "kamadakawai", label.size = 3)

## ----madmen_geom_net_code, echo=TRUE, eval=FALSE, fig.cap="The code required to generate Figure~\\ref{fig.cap:madmen} using the \\code{geom\\_net} function in the \\pkg{geomnet} package."----
## # also loads ggplot2
## library(geomnet)
## 
## # data step: join the edge and node data with a fortify call
## MMnet <- fortify(as.edgedf(madmen$edges), madmen$vertices)
## # create plot
## set.seed(10052016)
## ggplot(data = MMnet, aes(from_id = from_id, to_id = to_id)) +
##   geom_net(aes(colour = Gender), layout.alg = "kamadakawai",
##            size = 2, labelon = TRUE, vjust = -0.6, ecolour = "grey60",
##            directed =FALSE, fontsize = 3, ealpha = 0.5) +
##   scale_colour_manual(values = c("#FF69B4", "#0099ff")) +
##   xlim(c(-0.05, 1.05)) +
##   theme_net() +
##   theme(legend.position = "bottom")

## ----data_peek, echo=TRUE------------------------------------------------
head(as.edgedf(madmen$edges), 3)
head(madmen$vertices, 3)
head(fortify(as.edgedf(madmen$edges), madmen$vertices), 3)

## ----madmen_ggnetwork, echo=TRUE, eval=FALSE, fig.cap="The code required to generate Figure~\\ref{fig.cap:madmen} using the \\pkg{ggnetwork} package."----
## # create plot for ggnetwork. uses same data created for ggnet2 function
## library(ggnetwork)
## set.seed(10052016)
## ggplot(data = ggnetwork(mm.net, layout = "kamadakawai"),
##        aes(x, y, xend = xend, yend = yend)) +
##   geom_edges(color = "grey50") + # draw edge layer
##   geom_nodes(aes(colour = gender), size = 2) + # draw node layer
##   geom_nodetext(aes(colour = gender, label = vertex.names),
##                 size = 3, vjust = -0.6) + # draw node label layer
##   scale_colour_manual(values = mm.col) +
##   xlim(c(-0.05, 1.05)) +
##   theme_blank() +
##   theme(legend.position = "bottom")

## ----examples-setup, echo = TRUE-----------------------------------------
library(ggplot2)
library(GGally)
library(geomnet)
library(ggnetwork)

## ----blood_ggnet2,  echo=TRUE, fig.width=6, fig.height=6, warning=FALSE----
# make data accessible
data(blood, package = "geomnet")

# plot with ggnet2 (Figure 2a)
set.seed(12252016)
ggnet2(network(blood$edges[, 1:2], directed=TRUE), 
       mode = "circle", size = 15, label = TRUE, 
       arrow.size = 10, arrow.gap = 0.05, vjust = 0.5,
       node.color = "darkred", label.color = "grey80")

## ----blood_geom_net, echo=TRUE, fig.width=6, fig.height=6----------------
# plot with geomnet (Figure 2b)
set.seed(12252016)
ggplot(data = blood$edges, aes(from_id = from, to_id = to)) +
  geom_net(colour = "darkred", layout.alg = "circle", labelon = TRUE, size = 15,
           directed = TRUE, vjust = 0.5, labelcolour = "grey80",
           arrowsize = 1.5, linewidth = 0.5, arrowgap = 0.05,
           selfloops = TRUE, ecolour = "grey40") + 
  theme_net() 

## ----blood_ggnetwork, echo=TRUE, fig.width=6, fig.height=6---------------
# plot with ggnetwork (Figure 2c)
set.seed(12252016)
ggplot(ggnetwork(network(blood$edges[, 1:2]),
                 layout = "circle", arrow.gap = 0.05),
       aes(x, y, xend = xend, yend = yend)) +
  geom_edges(color = "grey50",
             arrow = arrow(length = unit(10, "pt"), type = "closed")) +
  geom_nodes(size = 15, color = "darkred") +
  geom_nodetext(aes(label = vertex.names), color = "grey80") +
  theme_blank()

## ----email_ggnet2, size="footnotesize", opts.label="codefig", echo=TRUE, out.width='\\textwidth'----
# make data accessible
data(email, package = 'geomnet')

# create node attribute data
em.cet <- as.character(
  email$nodes$CurrentEmploymentType)
names(em.cet) = email$nodes$label

# remove the emails sent to all employees
edges <- subset(email$edges, nrecipients < 54)
# create network
em.net <- edges[, c("From", "to") ]
em.net <- network(em.net, directed = TRUE)
# create employee type node attribute
em.net %v% "curr_empl_type" <-
  em.cet[ network.vertex.names(em.net) ]
set.seed(10312016)
ggnet2(em.net, color = "curr_empl_type",
       size = 4, palette = "Set1",
       arrow.size = 5, arrow.gap = 0.02,
       edge.alpha = 0.25, mode = "fruchtermanreingold",
       edge.color = c("color", "grey50"),
       color.legend = "Employment Type") +
  theme(legend.position = "bottom")

## ----email_geom_net, size="footnotesize", opts.label="codefig", echo=TRUE, out.width='\\textwidth'----
# data step for the geomnet plot
email$edges <- email$edges[, c(1,5,2:4,6:9)]
emailnet <- fortify(
  as.edgedf(subset(email$edges, nrecipients < 54)),
  email$nodes)
set.seed(10312016)
ggplot(data = emailnet,
       aes(from_id = from_id, to_id = to_id)) +
  geom_net(layout.alg = "fruchtermanreingold",
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
# use em.net created in ggnet2step
set.seed(10312016)
ggplot(ggnetwork(em.net, arrow.gap = 0.02,layout = "fruchtermanreingold"),
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
# data preparation. first, remove emails sent to all employees
em.day <- subset(email$edges, nrecipients < 54)[, c("From", "to", "day") ]
# for small multiples by day, create one element in a list per day 
# (10 days, 10 elements in the list em.day)
em.day <- lapply(unique(em.day$day),
                 function(x) subset(em.day, day == x)[, 1:2 ])
# make the list of edgelists a list of network objects for plotting with ggnet2
em.day <- lapply(em.day, network, directed = TRUE)
# create vertex (employee type) and network (day) attributes for each element in list
for (i in 1:length(em.day)) {
  em.day[[ i ]] %v% "curr_empl_type" <-
    em.cet[ network.vertex.names(em.day[[ i ]]) ]
  em.day[[ i ]] %n% "day" <- unique(email$edges$day)[ i ]
}

# plot ggnet2
# first, make an empty list containing slots for the 10 days (one plot per day)
g <- list(length(em.day))
set.seed(7042016)
# create a ggnet2 plot for each element in the list of networks
for (i in 1:length(em.day)) {
  g[[ i ]] <- ggnet2(em.day[[ i ]], size = 2, 
                     color = "curr_empl_type",
                     palette = "Set1", arrow.size = 0,
                     arrow.gap = 0.01, edge.alpha = 0.1,
                     legend.position = "none", 
                     mode = "kamadakawai") +
    ggtitle(paste("Day", em.day[[ i ]] %n% "day")) +
    theme(panel.border = element_rect(color = "grey50", fill = NA),
          aspect.ratio = 1)
}
# arrange all of the network plots into one plot window
gridExtra::grid.arrange(grobs = g, nrow = 2)

## ----email_facet_geom_net, fig.height=4.5, fig.width=8, echo=TRUE, out.width='\\textwidth'----
# data step: use the fortify.edgedf group argument to 
# combine the edge and node data and allow all nodes to 
# show up on all days. Also, remove emails sent to all  
# employees
emailnet <- fortify(as.edgedf(subset(email$edges, nrecipients < 54)), email$nodes, group = "day")

# creating the plot
set.seed(7042016)
ggplot(data = emailnet, aes(from_id = from, to_id = to_id)) +
  geom_net(layout.alg = "kamadakawai", singletons = FALSE,
    aes(colour = CurrentEmploymentType,
        group = CurrentEmploymentType,
        linewidth = 2 * (...samegroup.. / 8 + .125)),
        arrowsize = .5,
        directed = TRUE, fiteach = TRUE, ealpha = 0.5, size = 1.5, na.rm = FALSE) +
  scale_colour_brewer("Employment Type", palette = "Set1") +
  theme_net() +
  facet_wrap(~day, nrow = 2, labeller = "label_both") +
  theme(legend.position = "bottom",
        panel.border = element_rect(fill = NA, colour = "grey60"),
        plot.margin = unit(c(0, 0, 0, 0), "mm"))

## ----email_facet_ggnetwork, fig.height=4.5, fig.width=8,echo=TRUE, out.width='\\textwidth'----
# create the network and aesthetics
# first, remove emails sent to all employees
edges <- subset(email$edges, nrecipients < 54)
edges <- edges[, c("From", "to", "day") ]
# Create network class object for plotting with ggnetwork
em.net <- network(edges[, 1:2])
# assign edge attributes (day)
set.edge.attribute(em.net, "day", edges[, 3])
# assign vertex attributes (employee type)
em.net %v% "curr_empl_type" <- em.cet[ network.vertex.names(em.net) ]

# create the plot
set.seed(7042016)
ggplot(ggnetwork(em.net, arrow.gap = 0.02, by = "day", 
                 layout = "kamadakawai"),
       aes(x, y, xend = xend, yend = yend)) +
  geom_edges(
    aes(color = curr_empl_type),
    alpha = 0.25,
    arrow = arrow(length = unit(5, "pt"), type = "closed")) +
  geom_nodes(aes(color = curr_empl_type), size = 1.5) +
  scale_color_brewer("Employment Type", palette = "Set1") +
  facet_wrap(~day, nrow = 2, labeller = "label_both") +
  theme_facet(legend.position = "bottom")

## ----theme_ggnet2, size="footnotesize", opts.label="codefig", echo=TRUE----
# make data accessible
data(theme_elements, package = "geomnet")

# create network object
te.net <- network(theme_elements$edges)
# assign node attribut (size based on node degree)
te.net %v% "size" <-
  sqrt(10 * (sna::degree(te.net) + 1))
set.seed(3272016)
ggnet2(te.net, label = TRUE, color = "white", label.size = "size",
       mode = "fruchtermanreingold", layout.exp = 0.15)

## ----theme_geom_net, size="footnotesize", opts.label="codefig", echo=TRUE, fig.width=7, fig.height = 7----
# data step: merge nodes and edges and
# introduce a degree-out variable
# data step: merge nodes and edges and
# introduce a degree-out variable
TEnet <- fortify(
  as.edgedf(theme_elements$edges[,c(2,1)]),
            theme_elements$vertices)
TEnet <- TEnet %>%
  group_by(from_id) %>%
  mutate(degree = sqrt(10 * n() + 1))

# create plot:
set.seed(3272016)
ggplot(data = TEnet,
       aes(from_id = from_id, to_id = to_id)) +
  geom_net(layout.alg = "fruchtermanreingold",
    aes(fontsize = degree), directed = TRUE,
    labelon = TRUE, size = 1, labelcolour = 'black',
    ecolour = "grey70", arrowsize = 0.5,
    linewidth = 0.5, repel = TRUE) +
  theme_net() +
  xlim(c(-0.05, 1.05))

## ----theme_ggnetwork, size="footnotesize", opts.label="codefig", echo=TRUE----
set.seed(3272016)
# use network created in ggnet2 data step
ggplot(ggnetwork(te.net, layout = "fruchtermanreingold"),
       aes(x, y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_nodes(size = 12, color = "white") +
  geom_nodetext(
    aes(size = size, label = vertex.names)) +
  scale_size_continuous(range = c(4, 8)) +
  guides(size = FALSE) +
  theme_blank()

## ----football_ggnet2, size="footnotesize", opts.label="codefig", echo=TRUE----
#make data accessible
data(football, package = 'geomnet')
rownames(football$vertices) <-
  football$vertices$label
# create network 
fb.net <- network(football$edges[, 1:2],
                  directed = TRUE)
# create node attribute (what conference is team in?)
fb.net %v% "conf" <-
  football$vertices[
    network.vertex.names(fb.net), "value"
    ]
# create edge attribute (between teams in same conference?)
set.edge.attribute(
  fb.net, "same.conf",
  football$edges$same.conf)
set.seed(5232011)
ggnet2(fb.net, mode = "fruchtermanreingold",
       color = "conf",  palette = "Paired",
       color.legend = "Conference",
       edge.color = c("color", "grey75"))

## ----football_geom_net, size="footnotesize", fig.height=10, opts.label="codefig", echo=TRUE----
# data step: merge vertices and edges
# data step: merge vertices and edges
ftnet <- fortify(as.edgedf(football$edges), football$vertices)

# create new label variable for viewing independent schools
ftnet$schools <- ifelse(
  ftnet$value == "Independents", ftnet$from_id, "")

# create data plot
set.seed(5232011)
ggplot(data = ftnet,
       aes(from_id = from_id, to_id = to_id)) +
  geom_net(layout.alg = 'fruchtermanreingold',
    aes(colour = value, group = value,
        linetype = factor(same.conf != 1),
        label = schools),
    linewidth = 0.5,
    size = 5, vjust = -0.75, alpha = 0.3) +
  theme_net() +
  theme(legend.position = "bottom") +
  scale_colour_brewer("Conference", palette = "Paired")  +
  guides(linetype = FALSE)

## ----football_ggnetwork, size="footnotesize", opts.label="codefig", echo=TRUE----
# use network from ggnet2 step
set.seed(5232011)
ggplot(
  ggnetwork(
    fb.net, 
    layout = "fruchtermanreingold"),
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

## ----davis_data_1, echo=TRUE---------------------------------------------
# access the data and rename it for convenience
library(tnet)

data(tnet)
elist <- data.frame(Davis.Southern.women.2mode)
names(elist) <- c("Lady", "Event")

## ----davis_packages------------------------------------------------------
detach(package:tnet)
detach(package:igraph)

## ----data_davis_list, dependson='davis_data_1', echo=TRUE----------------
head(elist)

## ----davis_data_2, dependson='davis_data_1', echo=TRUE-------------------
elist$Lady <- paste("L", elist$Lady, sep="")
elist$Event <- paste("E", elist$Event, sep="")

davis <- elist
names(davis) <- c("from", "to")
davis <- rbind(davis, data.frame(from=davis$to, to=davis$from))
davis$type <- factor(c(rep("Lady", nrow(elist)), rep("Event", nrow(elist))))

## ----davis_ggnet2, dependson='davis_data_2', size="footnotesize", opts.label="codefig", echo=TRUE, warning=FALSE----
# Southern women network in ggnet2
# create affiliation matrix
bip = xtabs(~Event+Lady, data=elist)

# weighted bipartite network
bip = network(bip,
              matrix.type = "bipartite",
              ignore.eval = FALSE,
              names.eval = "weights")

# detect and color the mode
set.seed(8262013)
ggnet2(bip, color = "mode", palette = "Set2", 
       shape = "mode", mode = "kamadakawai",
       size = 15, label = TRUE) +
  theme(legend.position="bottom")

## ----davis_geom_net, dependson='davis_data_2', size="footnotesize", opts.label="codefig", echo=TRUE----
# Southern women network in geomnet
# change labelcolour
davis$lcolour <- 
  c("white", "black")[as.numeric(davis$type)]

set.seed(8262013)
ggplot(data = davis) + 
  geom_net(layout.alg = "kamadakawai",
    aes(from_id = from, to_id = to, 
        colour = type, shape = type), 
    size = 15, labelon = TRUE, ealpha = 0.25,
    vjust = 0.5, hjust = 0.5,
    labelcolour = davis$lcolour) +
  theme_net() + 
  scale_colour_brewer("Type of node", palette = "Set2") +
  scale_shape("Type of node") +
  theme(legend.position = "bottom")

## ----davis_ggnetwork, dependson='davis_data_2', size="footnotesize", opts.label="codefig", echo=TRUE, warning=FALSE----
# Southern women network in ggnetwork. Use data from ggnet2 step
# assign vertex attributes (Node type and label)
set.vertex.attribute(bip, "mode", 
  c(rep("event", 14), rep("woman", 18)))
set.seed(8262013)
ggplot(data = ggnetwork(bip, layout = "kamadakawai"),
       aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_edges(colour = "grey80") +
  geom_nodes(aes(colour = mode, shape = mode), size = 15) +
  geom_nodetext(aes(label = vertex.names)) +
  scale_colour_brewer(palette = "Set2") +
  theme_blank() + 
  theme(legend.position = "bottom") 

## ----bikes_geom_net_data, echo=TRUE, warning=FALSE, fig.keep='all'-------
# make data accessible
data(bikes, package = 'geomnet')
# data step for geomnet
tripnet <- fortify(as.edgedf(bikes$trips), bikes$stations[,c(2,1,3:5)])
# create variable to identify Metro Stations
tripnet$Metro = FALSE
idx <- grep("Metro", tripnet$from_id)
tripnet$Metro[idx] <- TRUE

## ----bikes_geom_net_remove_singleton, echo=FALSE, warning=FALSE, message=FALSE----
tripnet <- tripnet[-which(tripnet$from_id == "Shady Grove Hospital"),]

## ----bikes_geom_net, echo=TRUE, warning=FALSE, fig.keep='all'------------
# plot the bike sharing network shown in Figure 7b
set.seed(1232016)
ggplot(aes(from_id = from_id, to_id = to_id), data = tripnet) +
  geom_net(aes(linewidth = n / 15, colour = Metro),
           labelon = TRUE, repel = TRUE) +
  theme_net() +
  xlim(c(-0.1, 1.1)) +
  scale_colour_manual("Metro Station", values = c("grey40", "darkorange")) +
  theme(legend.position = "bottom")

## ----bikes_prepare, echo = TRUE------------------------------------------
# data preparation for ggnet2 and ggnetwork
bikes.net <- network(bikes$trips[, 1:2 ], directed = FALSE)
# create edge attribute (number of trips)
network::set.edge.attribute(bikes.net, "n", bikes$trips[, 3 ] / 15)
# create vertex attribute for Metro Station
bikes.net %v% "station" <-  grepl("Metro", network.vertex.names(bikes.net))
bikes.net %v% "station" <-  1 + as.integer(bikes.net %v% "station")
rownames(bikes$stations) <- bikes$stations$name
# create node attributes (coordinates)
bikes.net %v% "lon" <-
  bikes$stations[ network.vertex.names(bikes.net), "long" ]
bikes.net %v% "lat" <-
  bikes$stations[ network.vertex.names(bikes.net), "lat" ]
bikes.col <- c("grey40", "darkorange")

## ----bikes_ggnet2, echo = TRUE, fig.keep='all'---------------------------
# Non-geographic placement
set.seed(1232016)
ggnet2(bikes.net, mode = "fruchtermanreingold", size = 4, label = TRUE,
       vjust = -0.5, edge.size = "n", layout.exp = 1.1,
       color = bikes.col[ bikes.net %v% "station" ],
       label.color = bikes.col[ bikes.net %v% "station" ])

## ----bikes_ggnetwork, echo = TRUE, fig.keep='all'------------------------
# Non-geographic placement. Use data from ggnet2 step.
set.seed(1232016)
ggplot(data = ggnetwork(bikes.net, layout = "fruchtermanreingold"),
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
  library(ggmap)
metro_map <- get_map(location = c(left = -77.22257, bottom = 39.05721, 
                                  right = -77.11271, top = 39.14247))

## ----geographic_geomnet, echo=TRUE---------------------------------------
# geomnet: overlay bike sharing network on geographic map
  ggmap(metro_map) + 
  geom_net(data = tripnet, layout.alg = NULL, labelon = TRUE,
           vjust = -0.5, ealpha = 0.5,
           aes(from_id = from_id,
               to_id = to_id,
               x = long, y = lat,
               linewidth = n / 15,
               colour = Metro)) +
  scale_colour_manual("Metro Station", values = c("grey40", "darkorange")) +
  theme_net() %+replace% theme(aspect.ratio=NULL, legend.position = "bottom") +
  coord_map() 

## ----yeast_ggnet2, echo=FALSE, eval = FALSE, cache=FALSE-----------------
## # plot with ggnet2
## data(protein, package = 'geomnet')
## set.seed(10052016)
## ggnet2(network(protein$edges[,1:2]), size = 2, color = "magenta",
##       mode = "random", layout.par = list(dist = "uniang"),
##       edge.alpha = 0.05)

## ----yeast_geom_net, echo=FALSE, cache=FALSE, eval=TRUE------------------
# plot with geom_net
yeast <- read.csv("data/protein-layout.csv", stringsAsFactors = FALSE)
ggplot(data = yeast) + 
  geom_net(layout.alg = NULL, 
           aes(from_id = from, to_id = to,
               x = x , y = y), size = 1, linewidth = .5, ealpha = .25, colour = 'black') + 
  theme_net()

## ----yeast_ggnetwork, echo=FALSE, eval = FALSE,  cache=FALSE-------------
## # plot with ggnetwork
## set.seed(10052016)
## ggplot(ggnetwork(protein$edges[, 1:2], layout = "random", dist = "uniang"),
##        aes(x, y, xend = xend, yend = yend)) +
##   geom_edges(color = "grey70", lwd = 0.5, alpha = 0.05) +
##   geom_nodes(alpha = 0.25, color = "magenta", size = 2) +
##   theme_blank()
## 

## ----compare, echo = FALSE, eval = TRUE, fig.height = 4, fig.width = 8, out.width = '\\textwidth'----
g = read.csv("runtimes-protein/runtimes-protein-100.csv", stringsAsFactors = FALSE) %>%
      gather(`Visualization approach`, time, -iteration)

ggplot(g, aes(x = reorder(`Visualization approach`, time, median), y = time,
              color = `Visualization approach`,
              fill = `Visualization approach`)) +
  geom_boxplot(alpha = 0.6) +
  scale_fill_brewer(palette = "Set1") +
  scale_colour_brewer(palette = "Set1") +
  labs(x = "Visualisation approach\n", y = "\nAverage plotting time (seconds)") +
  ylim(c(0, NA)) +
  coord_flip() +
  theme_bw() + # HH: gridlines help with comparisons. significantly.
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = rel(1)))

## ----runtimes, eval=FALSE, echo=FALSE------------------------------------
## g = list.files("runtimes", pattern = "runtimes-(.*)csv$", full.names = TRUE) %>%
##         lapply(read.csv, stringsAsFactors = FALSE) %>%
##         bind_rows %>%
##         gather(`Visualization approach`, time, -network_size, -iteration) %>%
##         group_by(network_size, `Visualization approach`) %>%
##         summarise(mean_time = mean(time),
##                   q05 = quantile(time, 0.05),
##                   q95 = quantile(time, 0.95))
## 
## ggplot(g, aes(x = network_size, y = mean_time,
##               fill = `Visualization approach`)) +
##   geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 1/3) +
##   geom_line(aes(color = `Visualization approach`)) +
##   scale_color_brewer(palette = "Set2") +
##   scale_fill_brewer(palette = "Set2") +
##   scale_y_continuous(breaks = seq(0, max(g$q95), 5)) +
##   scale_x_continuous(breaks = seq(25, 250, 25)) +
##   labs(y = "Average plotting time (seconds)\n", x = "\nNetwork size") +
##   theme_classic(18) +
##   theme(axis.text = element_text(size = rel(1)),
##         legend.text = element_text(size = rel(1)),
##         legend.justification = c(0,1),
##         legend.position = c(0,1))

## ----runtimes-all, echo=FALSE, fig.width=8, fig.height=7.5---------------
g <- "runtimes" %>%
  list.files(pattern = "runtimes-(.*)csv$", recursive = TRUE, full.names = TRUE)

g <- lapply(g, function(x) {
    cbind(setup = gsub("(.*)runtimes/(.*)-(.*)/(.*)", "\\2", x),
          read.csv(x, stringsAsFactors = FALSE))
    }) %>%
    bind_rows %>%
  gather(`Visualization approach`, time, -network_size, -iteration, -setup) %>%
  group_by(network_size, `Visualization approach`)

qplot(data = g, network_size, time, colour = `Visualization approach`, alpha = I(0.1)) +
  facet_wrap(facets = ~setup, scales = "free_y") +
  geom_smooth(fill="white") +
  scale_colour_brewer(palette = "Set1") +
  xlab("Network size (edge probability p = 0.2)") +
  ylab("Time (in seconds)") +
  theme_bw() +
  theme(legend.position = "bottom")

