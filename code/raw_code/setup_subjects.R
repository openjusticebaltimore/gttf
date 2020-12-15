### Setup subjects for investigation
### kbmorales@protonmail.com

# Uses a Google Sheet maintained by the GTTF team to read in and subset
# suspect officers in prep for data extraction from CaseHarvester

# Setup -------------------------------------------------------------------

library(tidyverse)
library(googlesheets4)

## Force no auth
gs4_deauth()

# Google sheet ----------------------------------------------------------

# Download data
subjects <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1wcq2rMb0OKX2_IzMHIAM7CqIR9F4i4-NLleXERA-MLI/edit?usp=sharing"
)

# Filter to initial set: indicted GTTF officers + Clewell, Gladstone
# Decided 2020-10-04

# variable phase on google sheet denotes phase of investigation -- restricting
# to phase 1 for the moment

subjects = subjects %>%
  filter(phase==1) # Need to revisit when planning on expanding

# Add in first initial for string matching
subjects$first_initial = str_extract(subjects$first_name, "^\\w")

subject_ids = unlist(str_split(subjects$officer_id,
                               ", ")
                     )

# Name Filter ----------------------------------------------------------------

### Decided to focus on

### Create last name filter
last_name_pat <- paste(paste0("^", subjects$last_name),
                       sep=" ",
                       collapse = "|")

# Using cleaned cop names -------------------------------------------------

## Return to this

# cops_df = read_csv(here::here("data/cleaned_cop_names.csv"))

# cops_df %>%
#   filter(officer_id %in% subject_ids) %>%
#   arrange(officer_id)

# Name filter work --------------------------------------------------------

# names_lst = str_split_fixed(cops_df$name_clean, ", | ", 3)

# cops_df %>%
#   mutate(last_name = names_lst[,1],
#          first_name = names_lst[,2],
#          other_name = names_lst[,3]) %>%
#   filter(last_name %in% subjects$last_name |
#            last_name %in% subjects$other_spellings) %>%
#   filter(first_name %in% subjects$first_name |
#            # str_detect(first_name, paste(cops$first_name, collapse = "|")) |
#            str_detect(first_name,
#                       # look for first initial
#                       paste0("^",
#                              substr(subjects$first_name, 1,1),
#                              "$"
#                       ))) %>%
#   View()
