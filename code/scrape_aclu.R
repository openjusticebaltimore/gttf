library(tidyverse)
library(tabulizer)

path <- here::here("input/chasing_justice_report_2021_final.pdf")
get_n_pages(path)
pages <- list(
  complaints = 19,
  sustained = 20,
  q90 = 21
)

# locate_areas(path, pages = pages)
areas <- list(
  complaints = list(c(40, 35, 720, 215), c(40, 215, 720, 395), c(40, 395, 705, 575)),
  sustained = list(c(160, 47, 657, 563)),
  q90 = list(c(280, 40, 650, 305), c(280, 305, 650, 560))
)

dfs <- pmap(list(pages, areas), function(p, ars) {
  map(ars, ~extract_tables(path, pages = p, area = list(.x), output = "matrix", method = "lattice"))
}) %>%
  map_depth(2, 1)

out <- list()

out$complaints <- map_dfr(dfs$complaints, function(df) {
  as.data.frame(df[-1, ]) %>%
    set_names(df[1, ])
}) %>%
  janitor::clean_names() %>%
  select(1:2) %>%
  mutate(complaints = parse_number(complaints))

# need to make dummy (\\d) markers to be able to split e.g. Hersl but not Chanoine
out$sustained <- as.data.frame(dfs$sustained[[1]]) %>%
  set_names(c("officer", "total_sustained", "desc")) %>%
  as_tibble() %>%
  mutate(desc = str_squish(desc) %>%
           str_replace("(?<!\\))(\\b)$", " (666)") %>%
           str_match_all("([A-Z][\\w\\s,/\\-]+ \\(\\d+\\))") %>%
           map(~.[,1])) %>%
  unnest(desc) %>%
  extract(desc, into = c("desc", "sustained"), regex = "(^.+) \\((\\d+)\\)$") %>%
  mutate(sustained = na_if(parse_number(sustained), 666))

out$q90 <- map_dfr(dfs$q90, function(df) {
  as.data.frame(df[-1, ]) %>%
    set_names(df[1, ])
}) %>%
  janitor::clean_names() %>%
  mutate(across(complaints:use_of_force_incidents, parse_number))

iwalk(out, ~write_csv(.x, file.path("data", "aclu_extract", str_glue("{.y}.csv")), na = ""))
