### Prepare Bates DF
### kbmorales
### kbmorales@protonmail.com

# TODO:
# Zach 2020-06-12
# We may need to amend the Bates dataset again. Now that he wants
# conviction/disposition info, that changes things. Basically, for any
# district court case whose case_disposition field says "Jury Trial Prayed"
# or "Forwarded to Circuit Court", those district records will not have the
# conviction/disposition results. Only the circuit court records pertaining
# to those prayed/forwarded cases will have the conviction/disposition
# results. What we may need to do is have two different datasets: one that
# displays district court cases that never went up to circuit, and another
# that displays circuit court cases. Otherwise, we'll be missing all circuit
# case conviction data. Unless there's another query solution you can
# think of.

# TODO:
# Change Filter 3 to use CJIS codes rather than string detection

# TODO:
# Cleaning pass through data

# Setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)

# Read im CJIS codes
cjis_codes = read_csv(file.path("data",
                                "cjis",
                                "cjis_codes_20200507.csv"))

# Filter 1: indicted cops (done elsewhere) --------------------------------

# Prep connections and initial dataset
source(file.path("code",
                 "raw_code",
                 "ojb_collect.R"))

# Using district court data for now

# Filter 2: year 2000 on --------------------------------------------------

bates_df = gttf_dcr %>%
  # May dupe cases
  # left_join(dscr_case_names) %>%
  mutate(year = year(issued_date)) %>%
  filter(year >= 2000)


# Filter 3: need to go into charges data ----------------------------------

bates_chr_df <- dscr_chr %>%
  filter(case_number %in% local(bates_df$case_number)) %>%
  collect()

# bates_chr_df %>%
#   pull(charge_description) %>%
#   unique()

# bates_chr_df %>%
#   count(statute_description) %>%
#   arrange(desc(n)) %>%
#   View()

## Use filter from analysis 1
# TODO: Set to CJIS code pulls
bates_chr_df <- bates_chr_df %>%
  mutate(charge_description_2 = case_when(
  statute == "CR.5.601.(a)(1)" &
    !str_detect(charge_description, "NOT") |
    str_detect(charge_description, "POSSESSION - MARI") |
    str_detect(charge_description, "CDS: POSS[-\\s]MARI[HJ]UANA") |
    str_detect(charge_description, "CON-CDS:POSS-MARIHUANA") ~
    "Marijuana Possession",
  statute == "CR.5.601.(a)(1)" &
    str_detect(charge_description, "NOT|POSSESS-NOT") |
    str_detect(charge_description, "POSSESS-NOT") |
    str_detect(charge_description, "CDS-UNLAWFUL POSSESSION ETC") |
    str_detect(charge_description, "CDS POSSESSION-CON") |
    str_detect(charge_description, "CDS:POSSESSION") |
    str_detect(charge_description, "CDS POSSESS - LG AMT") ~
    "Non-Marijuana Possession",
  str_detect(statute, "CR.5.602") |
    str_detect(charge_description, "DIST") |
    str_detect(charge_description, "NARC POSS W/") |
    str_detect(charge_description, "CDS[-\\s]POSS W/I") |
    str_detect(charge_description, "CDS-POSS WI MANUF/DIS/DISP-NARC-CON")	~
    "CDS Distribituion / Manufacture",
  str_detect(charge_description, "PARA") ~ "CDS Paraphernalia Possession",
  str_detect(charge_description, "FIREARM|RFL|PISTOL|RIFLE|GUN|AMMO") ~
    "Firearms-related",
  str_detect(charge_description, "ASSAULT") |
    str_detect(charge_description, "DANGEROUS WEAPON-INT/INJURE") ~
    "Assault",
  str_detect(charge_description, "ARREST") ~ "Resisting Arrest",
  str_detect(charge_description, "TRESPASS") ~ "Trespassing",
  str_detect(charge_description, "THEFT|ROBB|BURG|	CARJACKING") ~
    "Theft / Burglary / Robbery",
  str_detect(charge_description, "DISORDERLY CONDUCT") ~ "Disorderly Conduct",
  str_detect(charge_description, "VIOLATION OF PROB") ~ "Probation Violation",
  TRUE ~ "Other"
)
)

bates_chr_df %>%
  count(charge_description_2) %>%
  ggplot(aes(x = reorder(charge_description_2,n),
             y = n)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(x = "",
       y = "Count") +
  theme_minimal()


### Look for gun and drug charges
bates_chr_casenum <- bates_chr_df %>%
  filter(charge_description_2 %in% c("CDS Distribituion / Manufacture",
                                     "CDS Paraphernalia Possession",
                                     "Firearms-related",
                                     "Marijuana Possession",
                                     "Non-Marijuana Possession")) %>%
  pull(case_number) %>%
  unique()

bates_df <- bates_df %>%
  filter(case_number %in% bates_chr_casenum)


# Filter 4 ----------------------------------------------------------------

### will come back to


# Determine cases that went to circuit court ------------------------------

# unique(bates_df$case_disposition)

forward_prayed = bates_df %>%
  filter(case_disposition %in% c("FORWARDED TO CIRCUIT COURT",
                                 "JURY TRIAL PRAYED")) %>%
  pull(case_number)

dsk8_bates = dsk8 %>%
  filter(district_case_number %in% local(forward_prayed))

# Add in zip code ---------------------------------------------------------

# circuit court
bates_def <- dscr_def %>%
  filter(case_number %in% local(bates_df$case_number)) %>%
  select(case_number, zip_code) %>%
  collect()

bates_df <- bates_df %>% left_join(bates_def,
                       by = "case_number")

# district court
dsk8_def_filt = dsk8_def %>%
  semi_join(dsk8_bates, by = "case_number") %>%
  select(case_number, zip_code)

dsk8_bates = left_join(dsk8_bates, dsk8_def_filt)

# Join charges data ------------------------------------------------------

# circuit court
bates_df = left_join(bates_df, bates_chr_df, by = "case_number")

# district court
dsk8_chr_filt = semi_join(dsk8_chr, dsk8_bates, by = "case_number")

dsk8_bates = left_join(dsk8_bates, dsk8_chr, by = "case_number")

dsk8_bates_df = collect(dsk8_bates)

# Clean -------------------------------------------------------------------

# bates_df %>% glimpse()


# Prepare final dataset ---------------------------------------------------

bates_df %>% glimpse()

### Unsure what is needed -- write to CSV

write_csv(bates_df,
          paste0("data/cases_for_bates_",
          Sys.Date(),
          ".csv"))

write_csv(dsk8_bates_df,
          paste0("data/circ_cases_for_bates_",
                 Sys.Date(),
                 ".csv"))
