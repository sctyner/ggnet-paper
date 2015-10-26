library(ggplot2)
library(tidyr)
library(dplyr)

d = lapply(list.files(pattern = "csv$"),
           read.csv, stringsAsFactors = FALSE)
d = do.call(rbind, d)

ggplot(gather(d, key, value, -network_size, -iteration) %>%
         group_by(network_size, key) %>%
         summarise(value = mean(value)),
       aes(x = network_size, y = value, linetype = key)) +
  geom_line() +
  geom_point(size = 1) +
  labs(y = "Average plotting time (seconds)", x = "Network size") +
  theme_classic()
