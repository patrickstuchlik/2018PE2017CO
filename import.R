library(tidyverse)

#download and unzip data files from CDC/NCHS
nchs_url <- 'https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/DVS/period-cohort-linked/2018PE2017CO.zip'
download.file(nchs_url,"2018PE2017CO.zip")
unzip("2018PE2017CO.zip")

#exact file names for import later, ordered 1=denom2017, 2=numer2017, 3=denom2018, 4=numer2018
vsfilenames <- list.files(pattern = 'LINK')

#explicit coding of data types (many errors if allow R to guess)
denomtypes <- read_csv('denom_vars_types.csv')
numertypes <- read_csv('numer_vars_types.csv')

#numerator file includes all columns also in denominator
numertypes <- numertypes %>%
  bind_rows(denomtypes) %>%
  arrange(Start)

#import "data dictionary" for ease of use later
datadict <- read_csv('recordlayout.csv')

#denominator file is too large, best to choose necessary variables first
denom_subset <- denomtypes %>%
  filter(Field %in% c("CO_SEQNUM", "DOB_YY", "CO_DODYY", "BWTR14", "SEX", "MRACEHISP", "RESTATUS","OEGest_R10") )

#set up fixed width parameters for importing denominator
denom_fwf <- fwf_positions(
  
  start = denom_subset$Start,
  end = denom_subset$End,
  col_names = denom_subset$Field
  
)

#data types for each column
denomcols <- str_flatten(denom_subset$col_type)

#same process for numerator
numer_subset <- numertypes %>%
  filter(Field %in% c("CO_SEQNUM", "DOB_YY", "CO_DODYY", "BWTR14", "SEX", "MRACEHISP", "RESTATUS","OEGest_R10"))

numer_fwf <- fwf_positions(
  
  start = numer_subset$Start,
  end = numer_subset$End,
  col_names = numer_subset$Field
  
)

numercols <- str_flatten(numer_subset$col_type)


#import data (also includes steps for making period-cohort linked dataset)
denom2017 <- read_fwf(vsfilenames[1], denom_fwf, col_types = denomcols) %>%
  filter(RESTATUS < 4)

numer2017 <- read_fwf(vsfilenames[2], numer_fwf, col_types = numercols) %>%
  filter(RESTATUS < 4 & DOB_YY == 2017)

numer2018 <- read_fwf(vsfilenames[4], numer_fwf, col_types = numercols) %>%
  filter(RESTATUS < 4 & DOB_YY == 2017)