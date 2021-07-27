library(tidyverse)
library(gt)

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

b17d1718 <- b17d1718 %>%
  full_join(mracehisp_mod)

#collapsed birthweight categories for table 2
bwt_tbl2 <- fct_collapse(b17d1718$BWTR14,
                        "<500" = "01",
                        "500-749" = "02",
                        "750-999" = "03",
                        "1000-1249" = "04",
                        "1250-1499" = "05",
                        "1500-1999" = "06",
                        "2000-2499" = "07",
                        "2500+" = c("08","09","10","11","12","13"),
                        "Not Stated" = "14"
)

b17d1718$bwt_tbl2 <- bwt_tbl2

#aggregate counts
#male and female, births only, by weight category
mf1 <- b17d1718 %>%
  group_by(Race,SEX,bwt_tbl2) %>%
  count() %>%
  mutate(bdi = "Live births")

#total births across all weights, separated by gender
mf2 <- b17d1718 %>%
  group_by(Race,SEX) %>%
  count() %>%
  mutate(bdi = "Live births", bwt_tbl2 = "Total") %>%
  bind_rows(mf1) %>%
  arrange(Race,SEX,bwt_tbl2)

#male and female, deaths, by weight category
mfd1 <- b17d1718 %>%
  filter(death) %>%
  group_by(Race, SEX, bwt_tbl2) %>%
  count() %>%
  mutate(bdi = "Infant deaths")

mfd2 <- b17d1718 %>%
  filter(death) %>%
  group_by(Race, SEX) %>%
  count() %>%
  mutate(bdi = "Infant deaths", bwt_tbl2 = "Total") %>%
  bind_rows(mfd1) %>%
  arrange(Race,SEX,bwt_tbl2)

#both sexes,births
bothb1 <- b17d1718 %>%
  group_by(Race,bwt_tbl2) %>%
  count() %>%
  mutate(bdi = "Live births", SEX = "Both sexes")

bothb2 <- b17d1718 %>%
  group_by(Race) %>%
  count() %>%
  mutate(bdi = "Live births", SEX = "Both sexes", bwt_tbl2 = "Total") %>%
  bind_rows(bothb1) %>%
  arrange(Race,SEX,bwt_tbl2)

#both sexes, deaths
bothd1 <- b17d1718 %>%
  filter(death) %>%
  group_by(Race,bwt_tbl2) %>%
  count() %>%
  mutate(bdi = "Infant deaths", SEX = "Both sexes")

bothd2 <- b17d1718 %>%
  filter(death) %>%
  group_by(Race) %>%
  count() %>%
  mutate(bdi = "Infant deaths", SEX = "Both sexes", bwt_tbl2 = "Total") %>%
  bind_rows(bothd1) %>%
  arrange(Race,SEX,bwt_tbl2)

#all races
allbothb <- b17d1718 %>%
  group_by(bwt_tbl2) %>%
  count() %>%
  mutate(bdi = "Live births",Race = "All races", SEX = "Both sexes")

allbothb2 <- b17d1718 %>%
  count() %>%
  mutate(bdi = "Live births",Race = "All races", SEX = "Both sexes", bwt_tbl2 = "Total") %>%
  bind_rows(allbothb)

allmfb <- b17d1718 %>%
  group_by(SEX,bwt_tbl2) %>%
  count() %>%
  mutate(bdi = "Live births",Race = "All races")

allmfb2 <- b17d1718 %>%
  group_by(SEX) %>%
  count() %>%
  mutate(bdi = "Live births",Race = "All races", bwt_tbl2 = "Total") %>%
  bind_rows(allmfb)

#all races, deaths
allbothd <- b17d1718 %>%
  filter(death) %>%
  group_by(bwt_tbl2) %>%
  count() %>%
  mutate(bdi = "Infant deaths",Race = "All races", SEX = "Both sexes")

allbothd2 <- b17d1718 %>%
  filter(death) %>%
  count() %>%
  mutate(bdi = "Infant deaths",Race = "All races", SEX = "Both sexes", bwt_tbl2 = "Total") %>%
  bind_rows(allbothd)

allmfd <- b17d1718 %>%
  filter(death) %>%
  group_by(SEX,bwt_tbl2) %>%
  count() %>%
  mutate(bdi = "Infant deaths",Race = "All races")

allmfd2 <- b17d1718 %>%
  filter(death) %>%
  group_by(SEX) %>%
  count() %>%
  mutate(bdi = "Infant deaths",Race = "All races", bwt_tbl2 = "Total") %>%
  bind_rows(allmfd)

racevals <- unique(tbl2$Race)
racesort <- c("All races",
              racevals[grep("White",racevals)],
              racevals[grep("Black",racevals)],
              racevals[grep("AIAN",racevals)],
              racevals[grep("Asian",racevals)],
              racevals[grep("NHOPI",racevals)],
              "Hispanic" )

sexsort <- c("Both sexes","M","F")

bdisort <- c("Live births","Infant deaths","imr")
  

#put it all together
tbl2 <- bothd2 %>%
  bind_rows(bothb2) %>%
  bind_rows(mfd2) %>%
  bind_rows(mf2) %>%
  bind_rows(allbothb2) %>%
  bind_rows(allmfb2) %>%
  bind_rows(allbothd2) %>%
  bind_rows(allmfd2) %>%
  filter(!is.na(Race)) %>%
  pivot_wider(names_from = bdi, values_from = n) %>%
  mutate(imr = case_when(
    `Infant deaths` >= 20 ~ round(1000*`Infant deaths`/`Live births`,2)
  )) %>%
  mutate(Race = factor(Race, levels = racesort),
         SEX = factor(SEX, levels = sexsort)) %>%
  mutate(SEX = fct_recode(SEX, "Male" = "M", "Female" = "F")) %>%
  pivot_longer(c(4:6), names_to = "bdi", values_to = "n") %>%
  mutate(bdi = factor(bdi, levels = bdisort)) %>%
  mutate(bdi = fct_recode(bdi,"Infant mortality rate" = "imr")) %>%
  pivot_wider(values_from = n, names_from = bwt_tbl2) %>%
  select(c(Race,SEX,bdi,Total,everything())) %>%
  #group_by(Race) %>%
  ungroup() %>%
  arrange(Race, SEX, bdi)

gt_tbl2 <- tbl2 %>%
  gt(groupname_col = c("Race","SEX")) %>%
  tab_header("Documentation Table 2 - Linked Cohort") %>%
  cols_label(bdi = "") %>%
  cols_align(columns = "bdi", align = "left") %>%
  cols_move(columns = c("500-749","750-999"),
            after = "<500") %>%
  fmt_number(columns = !bdi,
              rows = str_detect(bdi,"mortality", negate = T),
             decimals = 0,
              sep_mark = ",") %>%
  fmt_missing(columns = everything(), missing_text = "") %>%
  tab_source_note("Empty cells indicate either a) zero counts or b) infant mortality rate not calculated due to numerator count fewer than 20. Infant mortality rate calculated per 1,000 births.")

gt_tbl2

gtsave(gt_tbl2,"Table 2.html")
