library(tidyverse)

nchs_url <- 'https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/DVS/period-cohort-linked/2018PE2017CO.zip'
download.file(nchs_url,"2018PE2017CO.zip")
unzip("2018PE2017CO.zip")

vsfilenames <- list.files(pattern = 'LINK')