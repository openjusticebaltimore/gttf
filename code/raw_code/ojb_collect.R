### Set up DBs from CaseHarvester
### kbmorales@protonmail.com

# Use the subjects data set up in setup_subjects.R to assemble dataframes
# from Caseharvester

# Setup -------------------------------------------------------------------

library(tidyverse)
library(dbplyr)

options(dplyr.print_max = 20)

# Connection
source(here::here("bin",
                  "connect.R"))

# Set up subjects
source(here::here("code",
                  "raw_code",
                  "setup_subjects.R"))

# District Court - Criminal --------------------------------------------------

# Dscr
# Start here, then can move to circuit court

# EDA
# Look at connection values
# con_desc <- dscr_rel %>%
#   count(connection) %>%
#   arrange(desc(n)) %>%
#   collect()

## Related Person subtable
# Start here to determine cases that are connected with suspects

# Catch wide net to try to get all possible cases for further filtering
# Using last name or officer id

dscr_rel_filt <- dscr_rel %>%
  # Filter by connection type
  filter(str_detect(connection,
                    "POLICE") |
           connection == "COMPLAINANT") %>%
  # Filter using string matching or officer id
  mutate(name = toupper(name)) %>% # For name matching with pattern
  filter(
    str_detect(name, !!(last_name_pat)) | # matches suspect last name string OR
      officer_id %in% subject_ids # officer_id is associated with suspects
  ) %>%
  mutate(uid=case_when(
    str_detect(name, "^ALLERS") | officer_id %in% c("F934",
                                                    "H648") ~ "ALLERS",
    str_detect(name, "^CLEWELL") | officer_id %in% c("I711") ~ "CLEWELL",
    str_detect(name, "^GONDO") | officer_id %in% c("H832") ~ "GONDO",
    str_detect(name, "^HENDRIX") | officer_id %in% c("I695") ~ "HENDRIX",
    str_detect(name, "^HERSL") | officer_id %in% c("A093",
                                                   "G491") ~ "HERSL",
    str_detect(name, "^JENKINS") | officer_id %in% c("H383") ~ "JENKINS",
    str_detect(name, "^RAYAM") | officer_id %in% c("H740") ~ "RAYAM",
    str_detect(name, "^TAYLOR") | officer_id %in% c("I725") ~ "TAYLOR",
    str_detect(name, "^WARD") | officer_id %in% c("H456") ~ "WARD",
    str_detect(name, "^GLADSTONE") | officer_id %in% c("E989",
                                                       "E987",
                                                       "F989") ~ "GLADSTONE",
    TRUE ~ "OTHER"
  ))

# EDA
# dscr_rel_filt %>% count(uid)

### TODO when package is setup convert this to a prepackaged database

# Review returned names
# Count name clusters and focus on parsing the worst offenders
names_review <- dscr_rel_filt %>%
  count(name) %>%
  arrange(desc(n)) %>%
  collect()

names_review <- names_review %>%
  # Separate out name elements
  mutate(last_name = str_extract(name,
                                 "^.+,") %>%
           str_remove(",$"),
         first_name = str_extract(name,
                                  ", .+$") %>%
           str_remove("^, "),
         extra_name = str_extract(first_name,
                                  " .+$") %>%
           str_remove("^ ")
  ) %>%
  mutate(first_name = str_remove(first_name,
                                 " .+$"))

cop_signifiers = c("CPL", "OFFR", "PO", "PFC", "OFC", "SGT", "DET", "P/O", "OFF",
                   "RETIRED", "RESIGNED", "TERMINATED") # weird ones

# Sort through returned names for likely matches with suspects list
# Done for all returns with counts >= 10

names_for_filt <- names_review %>%
  filter(
    # Exact matches with subjects name on file
    last_name %in% subjects$last_name &
      first_name %in% subjects$first_name |
      # match last name and first initial
      last_name %in% subjects$last_name &
      first_name %in% subjects$first_initial |
      # match last name and cop signifier
      last_name %in% subjects$last_name &
      first_name %in% cop_signifiers
  ) %>%
  # Names leftover after manual review of above filters
  filter(
    # Odd names out
    !(name == "HENDRIX,REGINAL"),
    # Jenkins
    !(last_name == "JENKINS" & first_name == "D"),
    !(last_name == "JENKINS" & first_name == "DANIEL"),
    !(last_name == "JENKINS" & first_name == "E"),
    !(last_name == "JENKINS" & first_name == "EARL"),
    !(last_name == "JENKINS" & first_name == "J"),
    !(last_name == "JENKINS" & first_name == "JOHN"),
    !(last_name == "JENKINS" & first_name == "K"),
    !(last_name == "JENKINS" & first_name == "KEITH"),
    !(last_name == "JENKINS" & first_name == "M"),
    !(last_name == "JENKINS" & first_name == "MARCUS"),
    !(last_name == "JENKINS" & first_name == "ROBERT"),
    !(last_name == "JENKINS" & first_name == "T"),
    !(last_name == "JENKINS" & first_name == "THOMAS"),
    # Jenkins "extra_name" filters
    !(last_name == "JENKINS" & extra_name %in% c("B",
                                                 "C",
                                                 "M",
                                                 "R",
                                                 "T")),
    # Rivera
    !(last_name == "RIVERA" & first_name == "ROBERT"),
    # Taylor
    !(name=="TAYLOR,SAMUEL"),
    !(last_name == "TAYLOR" & first_name == "CRAIG"),
    !(last_name == "TAYLOR" & first_name == "D"),
    !(last_name == "TAYLOR" & first_name == "DANIEL"),
    !(last_name == "TAYLOR" & first_name == "E"),
    !(last_name == "TAYLOR" & first_name == "ERIC"),
    !(last_name == "TAYLOR" & first_name == "J"),
    !(last_name == "TAYLOR" & first_name == "JOHN"),
    !(last_name == "TAYLOR" & first_name == "K"),
    !(last_name == "TAYLOR" & first_name == "KEITH"),
    !(last_name == "TAYLOR" & first_name == "LAMACAIRDIE"),
    # possibly confusing with Maurice Ward?
    # !(last_name == "TAYLOR" & first_name == "MAURICE"),
    !(last_name == "TAYLOR" & first_name == "ROBERT"),
    !(last_name == "TAYLOR" & first_name == "T"),
    !(last_name == "TAYLOR" & first_name == "THOMAS"),
    !(last_name == "TAYLOR" & first_name == "TROY"),
    !(last_name == "TAYLOR" & first_name == "VICTOR"),
    !(last_name == "TAYLOR" & first_name == "W"),
    # Taylor "extra_name" filters
    !(last_name == "TAYLOR" & extra_name %in% c("A",
                                                "CHANTEL",
                                                "D",
                                                "D II",
                                                "DAWNYELL",
                                                "EDWARD",
                                                "FORREST",
                                                "G",
                                                "J",
                                                "J J",
                                                "L",
                                                "LEON",
                                                "LEROY",
                                                "WILLIAM")),
    # Ward
    !(name=="WARD,ROBERT"),
    !(last_name == "WARD" & first_name == "D"),
    !(last_name == "WARD" & first_name == "DANIEL"),
    !(last_name == "WARD" & first_name == "E"),
    !(last_name == "WARD" & first_name == "ERIC"),
    !(last_name == "WARD" & first_name == "J"),
    !(last_name == "WARD" & first_name == "JOHN"),
    !(last_name == "WARD" & first_name == "K"),
    !(last_name == "WARD" & first_name == "ROBERT"),
    !(last_name == "WARD" & first_name == "THOMAS"),
    !(last_name == "WARD" & first_name == "VICTOR"),
    !(last_name == "WARD" & first_name == "W"),
    # Ward "extra_name" filters
    !(last_name == "WARD" & extra_name %in% c("REBECCA L"))
  )

### END PREPACKAGED DATA ###

# Use array of names we filtered to collect results
dscr_rel_filt_2 <- dscr_rel_filt %>%
  filter(
    # Filter to name entries identified
    name %in% !!(names_for_filt$name) |
      is.na(name) |
      # Odd names out we can't rule out
      name %in% c("TAYLOR",
                  "TAYLOR,")
  )

# Use case_number from dscr_rel_filt_2 to identify district criminal cases

# Circuit Court - Criminal ------------------------------------------------

# TODO review this--probably assemble dataset with circuit court cases and
# district court cases separately to start

### dsk8

## Look at connection values
# con_desc <- dsk8_rel %>%
#   count(connection) %>%
#   arrange(desc(n)) %>%
#   collect()

# dsk8_rel_filt <- dsk8_rel %>%
#   filter(str_detect(connection,
#                     "POLICE") |
#            is.na(connection) |
#            connection == "OTHER") %>%
#   mutate(name = toupper(name)) %>%
#   filter(str_detect(name,
#                     local(last_name_pat)))
# Remove middle initials
# mutate(name = (name,
#                          " [A-Z]$"))
## Need to double check name filters
# filter(name %in% local(cops$name))

## Attempt to filter out the worst offenders

# names_for_review <- dsk8_rel_filt %>%
#   count(name) %>%
#   arrange(desc(n)) %>%
#   collect()
#
# # names_for_review$name
#
# # names_for_review %>% View()
#
# names_test <- names_for_review %>%
#   mutate(last_name = str_extract(name,
#                                  "^.+,") %>%
#            str_remove(",$"),
#          first_name = str_extract(name,
#                                   ", .+$") %>%
#            str_remove("^, "),
#          extra_name = str_extract(first_name,
#                                   " .+$") %>%
#            str_remove("^ ")
#   ) %>%
#   mutate(first_name = str_remove(first_name,
#                                  " .+$"))
#
# names_test <- names_test %>%
#   filter(last_name %in% cops$last_name,
#          first_name %in% cops$first_name) %>%
#   arrange(last_name, first_name) %>%
#   filter(!(last_name == "JENKINS" & first_name == "DANIEL"),
#          !(last_name == "JENKINS" & first_name == "MARCUS"),
#          !(last_name == "JENKINS" & first_name == "ROBERT"),
#          !(last_name == "JENKINS" & first_name == "THOMAS"),
#          !(last_name == "RIVERA" & first_name == "ROBERT"),
#          !(last_name == "TAYLOR" & first_name == "CRAIG"),
#          !(last_name == "TAYLOR" & first_name == "DANIEL"),
#          !(last_name == "TAYLOR" & first_name == "ERIC"),
#          !(last_name == "TAYLOR" & first_name == "KEITH"),
#          !(last_name == "TAYLOR" & first_name == "MAURICE"),
#          !(last_name == "TAYLOR" & first_name == "ROBERT"),
#          !(last_name == "TAYLOR" & first_name == "THOMAS"),
#          !(last_name == "TAYLOR" & first_name == "VICTOR"),
#          !(last_name == "WARD" & first_name == "ERIC"),
#          !(last_name == "WARD" & first_name == "ROBERT"),
#          !(last_name == "WARD" & first_name == "THOMAS"),
#          !(last_name == "WARD" & first_name == "VICTOR"))
#
#
# dsk8_rel_filt_2 <- dsk8_rel %>%
#   filter(str_detect(connection,
#                     "POLICE") |
#            connection == "COMPLAINANT") %>%
#   mutate(name = toupper(name)) %>%
#   filter(name %in% local(names_test$name))
#
# dsk8_casenum <- dsk8_rel_filt_2 %>%
#   select(case_number) %>%
#   collect() %>%
#   pull(case_number) %>%
#   unique()

# length(dsk8_casenum)
# 23065

# dsk8_filt <- dsk8 %>%
#   filter(case_number %in% local(dsk8_casenum))

# Create cases / cop names data -------------------------------------------

# EDA:
# dscr_rel_filt_2 %>% count()

# 16.8k rows

# check # of total cases identified with only name and id filters
# dscr_rel_filt_2 %>%
#   select(case_number) %>%
#   distinct() %>%
#   count()

# 12,555 unique cases

# May be interesting to look at agency dub codes
dscr_subject_cases <- dscr_rel_filt_2 %>%
  select(case_number,
         uid,
         name,
         agency_code,
         agency_sub_code,
         officer_id
  )

# EDA: agency sub_code
dscr_case_names <- dscr_subject_cases %>%
  select(case_number,
         uid) %>%
  distinct() #remove dupes

# Determine who's up more than once
dscr_case_names = dscr_case_names %>%
  group_by(case_number) %>%
  mutate(num_cops = n()) %>%
  ungroup() %>%
  # arrange(desc(num_cops)) %>%
  mutate(uid_mult=ifelse(num_cops>1,
                         "MULTIPLE",
                         uid))

# dscr_cases: each row is a CASE,
# with flag for suspect ("MULTIPLE" if > 1 suspect)
dscr_cases = dscr_case_names %>%
  select(-uid) %>%
  distinct()

# Need as vector?
dscr_casenum <- dscr_cases %>%
  pull(case_number)

# length(dscr_casenum)
# 12555

# Use names to filter cases tables -----------------------------------------------

dscr_filt <- dscr %>%
  filter(case_number %in% !!dscr_casenum)

### Master cases table
# casenums <- c(dsk8_casenum, dscr_casenum)
#
# length(casenums)

cases_dscr_filt <- cases %>%
  filter(case_number %in% !!dscr_casenum) %>%
  select(case_number,
         court,
         case_type,
         filing_date) %>%
  distinct() %>%
  left_join(dscr_filt,
            by = "case_number") %>%
  select(-case_type.y) %>%
  mutate(year = year(filing_date))


# Collect -----------------------------------------------------------------

cases_dscr_filt = cases_dscr_filt %>% collect()
dscr_cases = dscr_cases %>% collect()

gttf_dcr = cases_dscr_filt %>%
  left_join(dscr_cases) %>%
  rename(case_type=case_type.x)

# cases_filt %>%
#   summarise(rows = n())

# To Export ------------------------------------------------------------------

# Use this file

# source(here::here("code",
#                   "raw_code",
#                   "write_dfs.R"))

