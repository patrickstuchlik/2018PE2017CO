library(tidyverse)

b17d1718 <- denom2017 %>%
  left_join(numer2017, by = c("CO_SEQNUM", "CO_DODYY")) %>%
  left_join(numer2018, by = c("CO_SEQNUM", "CO_DODYY"))