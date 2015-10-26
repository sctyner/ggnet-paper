library(ggplot2)
library(tidyr)
library(dplyr)

d = lapply(list.files(pattern = "csv$"),
           read.csv, stringsAsFactors = FALSE)

d = do.call(rbind, d)

ggplot(gather(d, `Visualization method`, time, -network_size, -iteration) %>%
         group_by(network_size, `Visualization method`) %>%
         summarise(mean_time = mean(time)),
       aes(x = network_size, y = mean_time, linetype = `Visualization method`)) +
  geom_line() +
  geom_point(size = 1) +
  labs(y = "Average plotting time (seconds)", x = "Network size") +
  theme_classic() +
  theme(legend.justification = c(0,1), legend.position = c(0,1))
