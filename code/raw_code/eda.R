### caseharvester EDA

library(tidyverse)
library(lubridate)

# EDA ---------------------------------------------------------------------

# dsk8_rel %>%
#   filter(str_detect(connection,
#                     "POLICE") |
#            is.na(connection) |
#            connection == "OTHER") %>%
#   mutate(name = toupper(name)) %>%
#   count(name) %>%
#   arrange(desc(n)) %>%
#   collect() %>%
#   View()

gttf_dcr %>%
  ggplot(aes(x = year)) +
  geom_histogram(binwidth = 1) +
  theme_minimal()

## Filtering to 2008-- GTTF founded in 2007
gttf_dcr %>%
  filter(year >= 2008) %>%
  # count()
  ggplot(aes(x = year)) +
  geom_histogram(binwidth = 1) +
  theme_minimal()

# plot_df <- cases_filt %>%
#   mutate(year = year(filing_date)) %>%
#   left_join(dscr_case_names,
#             copy = T) %>%
#   group_by(year,
#            last_name) %>%
#   count() %>%
#   collect()

# Viridis palette with black on the end
pal <- c(viridis::viridis(10, option = "D"),"#000000")

## Use recent data
plot_df <- gttf_dcr %>%
  group_by(year, uid_mult) %>%
  count() %>%
  mutate(uid_mult = toupper(uid_mult)) %>%
  mutate(uid_mult = factor(uid_mult,
                           levels=c(subjects$uid,
                                    "MULTIPLE"
                                    )))

# Line plot
# plot_df %>%
#   mutate(count = as.numeric(n)) %>%
#   ggplot(aes(x = year,
#              y = count,
#              color = uid_mult)) +
#   geom_line() +
#   theme_minimal() +
#   scale_fill_manual(values = pal) +
#   scale_x_continuous(breaks = seq(min(plot_df$year), max(plot_df$year), 1)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs(x = "", y = "Cases", fill = "")

# histogram over year
plot_df %>%
  mutate(count = as.numeric(n)) %>%
  ggplot(aes(x = year,
             y = count,
             fill = uid_mult)) +
  geom_col() +
  theme_minimal() +
  scale_fill_manual(values = pal) +
  scale_x_continuous(breaks = seq(min(plot_df$year), max(plot_df$year), 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "Cases", fill = "")

# histogram by suspect
plot_df %>%
  filter(year >= 2000) %>%
  mutate(count = as.numeric(n)) %>%
  ggplot(aes(x = year,
             y = count,
             fill = uid_mult)) +
  geom_col() +
  facet_wrap(~uid_mult,
             nrow = 4) +
  theme_minimal() +
  scale_fill_manual(values = pal) +
  scale_x_continuous(breaks = seq(2000, max(plot_df$year), 2),
                     expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(x = "", y = "Cases", fill = "")


# Charges -----------------------------------------------------------------


# Demographics ------------------------------------------------------------

### Race overall
race_overall <- dscr_def %>%
  left_join(dscr %>%
              mutate(year = year(issued_date)),
            by = "case_number") %>%
  filter(year >= 2000) %>%
  count(race) %>%
  rename(overall_n = n) %>%
  collect()

race_overall %>%
  filter(!is.na(race)) %>%
  mutate(n = as.integer(n),
         prop = n/sum(n)) %>%
  ggplot(aes(x = reorder(race, n),
             y = prop)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,1)) +
  labs(x = "",
       y = "# of Defendants",
       title = "Race of Defendants in CaseHarvester, overall")

## Bates DF race
bates_race <- dscr_def %>%
  filter(case_number %in% local(bates_df$case_number)) %>%
  count(race) %>%
  mutate(bates_n = as.integer(n)) %>%
  select(race, bates_n) %>%
  collect()

race_df <- bates_race %>%
  left_join(race_overall) %>%
  mutate(overall_n = as.integer(overall_n)) %>%
  filter(!is.na(race))

## Race breakdown
chisq.test(race_df$bates_n,
           race_df$overall_n,
           correct = F)

## Only use known race
test <- race_df %>% filter(race != "UNKNOWN, OTHER")

## Black vs nonblack
black_vs_nonblack <- tibble(race = c("black", "not black"),
                            bates_n = c(test$bates_n[test$race=="BLACK, AFRICAN AMERICAN"],
                                        sum(test$bates_n[test$race!="BLACK, AFRICAN AMERICAN"])
                                        ),
                            overall_n = c(test$overall_n[test$race=="BLACK, AFRICAN AMERICAN"],
                                          sum(test$overall_n[test$race!="BLACK, AFRICAN AMERICAN"])
                                          )
                            )

chisq.test(black_vs_nonblack$bates_n,
           black_vs_nonblack$overall_n,
           correct = F)
## Not significantly different

race_df %>%
  # filter(!is.na(race)) %>%
  gather(key = "group", value = "n", -race) %>%
  group_by(group) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(race, n),
             y = prop,
             fill = group)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,1),
                     breaks = seq(0,1,0.1)) +
  labs(x = "",
       y = "% of Defendants",
       fill = "Group",
       title = "Race of Defendants: Overall vs. Charged Officers") +
  theme(legend.position = c(0.8,0.2)) +
  scale_fill_viridis_d(labels = c("Charged", "Overall"))


