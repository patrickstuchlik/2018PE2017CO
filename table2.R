library(tidyverse)

#linked cohort dataset joins
b17d1718 <- denom2017 %>%
  left_join(numer2017, by = c("CO_SEQNUM", "CO_DODYY")) %>%
  left_join(numer2018, by = c("CO_SEQNUM", "CO_DODYY"))

#coalesce redundant columns and add numerator indicator
b17d1718 <- b17d1718 %>%
  mutate(DOB_YY = coalesce(DOB_YY.x,DOB_YY.y,DOB_YY),
         RESTATUS = coalesce(RESTATUS.x,RESTATUS.y,RESTATUS),
         SEX = coalesce(SEX.x,SEX.y,SEX),
         BWTR14 = coalesce(BWTR14.x,BWTR14.y,BWTR14),
         MRACEHISP = coalesce(MRACEHISP.x,MRACEHISP.y,MRACEHISP),
         OEGest_R10 = coalesce(OEGest_R10.x, OEGest_R10.y, OEGest_R10)) %>%
  select(-c(ends_with(".x"))) %>%
  select(-c(ends_with(".y"))) %>%
  mutate(death = !is.na(CO_DODYY))

#retrieve value definitions from data dictionary
mracehisp <- datadict %>%
  filter(Field == "MRACEHISP") %>%
  mutate(MRACEHISP = as.integer(Values)) %>%
  select(c(MRACEHISP,Definition))

#table 2 excludes "more than one race" and "origin unknown"
mracehisp_mod <- mracehisp %>%
  filter(str_detect(Definition,"more", negate = T)) %>%
  filter(str_detect(Definition, "unknown", negate = T)) %>%
  mutate(Race = str_remove(Definition," \\(only\\)")) %>%
  select(-c(Definition))


