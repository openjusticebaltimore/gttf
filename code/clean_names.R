###############################################################################
#      ( .  (   (          (  (     (( (   ((    (( (   
#     ()) . )\  )\        ()) )\   (\())\  ))\  (\())\  
#    ((_)) ((_)((_)      ((_))(_) ))(_)_()((_))))(_)(_) 
#   (/ __|/ _ \| _ \    (/ __| |  | __|   \ \| | __| _ \
#   | (__| (_) |  _/    | (__| |__| _|| - | .  | _||   /
#    \___|\___/|_|       \___|____|___|_|_|_|\_|___|_|_\
#                                                 
# clean up Baltimore cop names from Case Harvester database
###############################################################################

library(tidyverse)
library(RPostgreSQL)
library(fuzzyjoin)
library(stringdist)

source(here::here("code/cleanup_utils.R"))

# bring in the cleaned up names from rosters, 2012–2017
rosters <- read_csv(here::here("data/cleaned_iis_cop_names.csv")) %>%
  mutate(name = toupper(name)) %>%
  rename(officer_id = id)

########################## QUEERY #############################################
# # after running this stuff & saving it to rds file, it's helpful to comment out
# # to avoid rerunning query every time
# user <- rstudioapi::askForSecret("ojb_user")
# pwd <- rstudioapi::askForSecret("ojb_pwd")
# con <- DBI::dbConnect("PostgreSQL", dbname = "mjcs",
#                       user = user,
#                       password = pwd,
#                       port = 5432,
#                       host = "db.openjusticebaltimore.org")
# 
# # basic cleanup
# # make sure space after comma to match names
# # since I'm only looking at differences in names within an ID, gonna remove initials
# fetch <- dbGetQuery(con, "
#             select case_number, name, officer_id
#             from dscr_related_persons
#             where case_number in (
#             	select case_number
#             	from cases
#             	where query_court = 'BALTIMORE CITY'
#             )
#             and connection ilike '%POLICE OFFICER%'
#             and name is not null
#             and officer_id not in ('0000', '9999', 'OOOO');
#            ")
# saveRDS(fetch, "~/dscr_cops_fetch.rds")
# DBI::dbDisconnect(con)
fetch <- readRDS("~/dscr_cops_fetch.rds")
###############################################################################


########################## SIMPLE CLEANUP ######################################
# drop titles (CPT, CPL, etc)
# handle commas, spaces, punctuation, digits
# move JR, SR, III, etc to end
# drop trailing initials--wanted to keep those but they just make it too hard to do cleanup
# make corrections from ^^ spreadsheets
# only keep if at least 3 alphabet characters remain
# fix a few places where IDs have O in place of 0
ids <- fetch %>%
  as_tibble() %>%
  mutate(name = name %>%
           str_remove_all("\\b(OF+I[CEIR]+|OFCR?|OF+[RN]?|CA?PT|DEP?T+|SERGEANT|MAJ|MA?JR|AGR|SGT+|TPR|TRP|CPL|TFC|PFC|SPECIAL|SPE?C|AGT|AGEN?T?|UNKNOWN|NOT NEEDED|NA|XX|RETIRED|DETECT\\w+|CORP|TE?CH|TEC)\\b") %>%
           str_remove_all("[\\*\\.\\d]") %>%
           str_replace_all(";", ",") %>%
           str_replace_all("(?<=\\b,)(\\b)", " ") %>%
           str_replace_all("(\\s+|\\-)", " ") %>%
           trimws() %>%
           str_remove_all("(?<=[A-Z])\\s[A-Z](?=($| JR| SR| IV| I{2,3}))") %>%
           str_replace_all("(?<=\\S)(JR|SR|III)", " \\1") %>% # not doing this for ii or iv because it might be at end of names
           str_remove("(?<=\\bMC)\\s") %>%
           str_replace_all(c(!!!corrections)) %>%
           str_replace_all("\\s(JR|SR|I{2,3}|IV)(,[\\w\\s]+)$", "\\2 \\1") %>%
           str_replace_all("(?<=,\\s)(JR|SR|I{2,3}|IV)\\s([\\w\\s]+$)", "\\2 \\1") %>%
           str_replace_all("\\s+", " ") %>%
           trimws()) %>%
  mutate(officer_id = str_replace_all(officer_id, "\\BO", "0")) %>%
  filter(str_count(name, "[A-Z]") >= 3) %>%
  distinct(name, officer_id) %>%
  arrange(officer_id)
###############################################################################


########################## TEST CASES #########################################
# ids <- ids %>% filter(officer_id %in% c("A089", "0031", "0196", "E466", "E428"))
###############################################################################
# could use .id to mark off source and weight rosters as higher than CV, but for now seems not necessary
ids2 <- bind_rows(rosters, ids) %>%
  distinct(name, officer_id, .keep_all = TRUE) %>%
  group_by(officer_id)

########################## CLUSTER & DEDUPE ###################################
# for IDs with more than one name, start by cleaning based on subsetting names.
# from that, "BUTT, EDA", "BUTT, EDA JR", "BUTT, E" all become "BUTT, EDA JR".
# then use those to put into clusters within each ID.
# within each ID & cluster, assign all names to be the one with most characters.
# default params make pretty conservative string similarity calculations, so I'm not 
# too concerned about merging names that shouldn't be seen as the same
clustered <- ids2 %>%
  filter(n() > 1) %>%
  mutate(subs = name_get_subset(name)) %>%
  nest() %>%
  mutate(cluster = data %>%
           map(pluck, "subs") %>%
           map(clust_strings)) %>%
  unnest(c(data, cluster)) %>%
  group_by(officer_id, cluster) %>%
  mutate(name_clean = longest(subs))

# ids %>% filter(n() == 1) are the IDs that are only associated with one name
# for now, assuming those are correct.
# bind that to the deduped ones
clean_names <- ids2 %>%
  filter(n() == 1) %>%
  mutate(name_clean = name) %>%
  bind_rows(clustered) %>%
  select(-cluster) %>%
  mutate(name_clean = str_remove(name_clean, ",$")) %>%
  arrange(name_clean, officer_id) %>%
  distinct(officer_id, name_clean)


# generate list of possible misspellings to check. maybe easiest is to do this in openrefine
# redo clustering, but with more liberal params, esp height to cut hclust tree
# so it grabs some almost-clusters, which is a good way to find candidates for manual cleanup
comb_thru <- clean_names %>%
  group_by(officer_id) %>%
  filter(n() > 1) %>%
  nest() %>%
  mutate(cluster = data %>%
           map(pluck, "name_clean") %>%
           map(clust_strings, dist_method = "jw", p = 0.1, h = 0.25)) %>%
  unnest(c(data, cluster)) %>%
  group_by(officer_id, cluster) %>%
  filter(n() > 1) %>%
  arrange(officer_id)
###############################################################################


########################## OUTPUT #############################################
# writing out a file of the names to comb thru manually
# any corrections to make from that should go in ethercalc spreadsheet &
# can feed back into subsequent runs of this script
write_csv(clean_names, here::here("data/cleaned_cop_names.csv"))
write_csv(comb_thru, here::here("data/cop_names_manual_fix.csv"))

# DBI::dbDisconnect(con)
###############################################################################


########################## NEXT STEPS ##########################################
# * thinking about doing name subsetting by first & last name separately
# * handle cases where an ID has the same first name with different last names--
#   so far seeing this with "female" names, so guessing they got married
# * want to start dealing with typos in IDs but need to be super careful before changing an ID
#   can do this based on frequency--if a number is switched in 1 instance out of 20,
#   assume that 1 is a typo
# ✓ split cases where II, JR, etc is attached directly to a name. need to make sure it's not 
#   legit part of a name, e.g. Javii

message("Rows pulled out of database: ", nrow(fetch))
message("Rows in initial set of names & IDs: ", nrow(ids))
message("Rows after clustering & merging: ", nrow(clean_names))
message("Rows to comb through externally: ", nrow(comb_thru))

pluck_n_clust <- function(x, id, ...) {
  i <- pluck(x, id)
  possibly(function(y) clust_strings(y, ...), NA)
}

# split_subs <- clean_names %>%
#   arrange(officer_id) %>%
#   separate(name_clean, into = c("last", "first"), sep = ", ", remove = FALSE) %>%
#   filter(n() > 1) %>%
#   nest() %>%
#   mutate(cluster_last = data %>%
#            map(pluck, "last") %>%
#            map(clust_strings, dist_method = "lcs"),
#          cluster_first = data %>%
#            # map(filter, !is.na(first)) %>%
#            # map(filter, n() > 1) %>%
#            map(pluck, "first") %>%
#            map(function(x) if (sum(!is.na(x)) > 1) clust_strings(x, "lcs") else rep_along(x, NA_integer_)))
# 
# 
# clean_names %>%
#   arrange(officer_id) %>%
#   filter(n() > 1, str_detect(officer_id, "[B-Z]")) %>%
#   ungroup() %>%
#   slice(1:1000) %>%
#   group_by(officer_id) %>%
#   separate(name_clean, into = c("last", "first"), sep = ", ", remove = FALSE) %>%
#   pivot_longer(last:first, names_to = "part", values_to = "name", values_drop_na = TRUE) %>%
#   group_by(officer_id, part) %>%
#   filter(n() > 1) %>%
#   mutate(clust = clust_strings(name, "jw")) %>%
#   mutate(n_clust = n_distinct(clust)) %>%
#   filter(n_clust == 1) %>%
#   print(n = 100)
