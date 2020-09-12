library(tidyverse)
library(readxl)
library(raster)

paths <- list.files(here::here("input_data/rosters"), full.names = TRUE) %>%
  set_names(function(x) {
    bn <- basename(x)
    date <- str_extract(bn, "(\\d{4}\\-\\d{2}\\-\\d{2}|\\d{2}\\-\\d{2}\\-\\d{4}|\\w+\\s\\d{2}\\s\\d{4})")[1]
    # get standardized date format
    # ifelse(str_detect(date, "\\d{4}$"), lubridate::mdy(date), lubridate::ymd(date)) %>%
    #   format("%Y-%m-%d")
    if (str_detect(date, "\\d{4}$")) {
      date <- lubridate::mdy(date)
    } else {
      date <- lubridate::ymd(date)
    }
    type <- str_extract(bn, "\\b([A-Z\\s]+)\\b")
    paste(trimws(type), date, sep = "_")
  })



# rows are sometimes staggered, so marking off across whole table won't work. columns are more consistent, so reverse to do columns first, then rows
# wrap all this bullshit into a function
scrape_org <- function(path) {
    iis <- openxlsx::read.xlsx(path, colNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE, fillMergedCells = TRUE) %>%
    as_tibble() %>%
    mutate_all(as.character)
  
  iis_grid <- iis %>%
    mutate_all(function(x) +!is.na(x)) %>%
    as.matrix() %>%
    raster::raster() %>%
    raster::clump() %>%
    as.matrix() %>%
    as_tibble() %>%
    rowid_to_column("row") %>%
    pivot_longer(-row, names_to = "col", names_pattern = "(\\d+)", values_to = "grid") %>%
    mutate(col = as.integer(col)) %>%
    filter(!is.na(grid))
  
  # top ranking in this section listed at the top of sheet
  # pattern matching--IDs?
  # within each grid, first row is group name, second is leader, everything after is members
  # drop tallies by keeping grids based on number of columns
  iis_split <- iis %>%
    rowid_to_column("row") %>%
    pivot_longer(-row, names_to = "col", names_pattern = "(\\d+)") %>%
    mutate(col = as.integer(col)) %>%
    inner_join(iis_grid, by = c("row", "col")) %>%
    group_by(grid, has_text = str_detect(value, "[A-Za-z]")) %>%
    filter(sum(has_text) > 0) %>%
    ungroup() %>%
    dplyr::select(-has_text) %>%
    split(.$grid) %>%
    map(distinct, row, value, .keep_all = TRUE) %>%
    map(mutate, col = c("rank", "name", "id", "extra")[as.numeric(as.factor(col))]) %>%
    map(mutate, value = str_remove_all(value, "[^[:print:]]") %>% str_squish() %>% str_remove_all("\\.$")) %>%
    map(pivot_wider, names_from = col)
  
  # yellow boxes of leads all have locator ### at bottom
  tier2 <- iis_split %>%
    keep(~any(grepl("Locator", .$rank)))
  
  tier1 <- iis_split[[1]]
  
  # don't really know what to do with this
  top_rank <- lst(tier1, tier2 = bind_rows(tier2)) %>%
    bind_rows(.id = "command")
  
  iis_df <- iis_split %>%
    keep(function(df) "name" %in% names(df) && any(grepl("[A-Za-z]{2,}", df[["name"]]))) %>%
    map(function(df) mutate(df, group = ifelse(rowSums(is.na(df)) >= 2, coalesce(rank, name), NA_character_))) %>%
    map(mutate, group = na_if(group, "w")) %>%
    map(fill, group, .direction = "down") %>%
    bind_rows() %>%
    # dplyr::select(-extra) %>%
    mutate(id = na_if(id, "w")) %>%
    filter(!grepl("(vacant|vacancy)", name, ignore.case = TRUE),
           # !grid %in% unique(top_rank$grid),
           # !rank %in% tier1$rank,
           group != name) %>%
    group_by(grid, group) %>%
    mutate(top = row_number() == 1) %>%
    ungroup()

  df_out <- iis_df %>%
    filter(!is.na(id)) %>%
    mutate(name = name %>%
             str_remove("\\(.+\\)") %>% 
             str_remove_all("[.]") %>%
             str_remove_all("[-\\s/A-Z\\d]+$") %>%
             str_remove("\\-[\\w\\s]+$") %>%
             str_replace(",(?=\\S)", ", ") %>%
             str_squish())
  n_cops <- df_out %>%
    distinct(id) %>%
    nrow()
  message(crayon::green(str_glue("{path}: {n_cops} unique IDs")))
  df_out
}


dfs <- paths %>%
  map(possibly(scrape_org, otherwise = NULL))

staff <- bind_rows(dfs, .id = "file") %>%
  dplyr::select(-extra) %>%
  filter(!name %in% c("w", "Withheld")) %>%
  separate(file, into = c("file_type", "date"), sep = "_") %>%
  group_by(name, rank, id, group) %>%
  summarise(dates = paste(date, collapse = ",")) %>%
  ungroup()

dist_staff <- staff %>%
  distinct(name, id)

message(crayon::yellow(str_glue("Total unique cops & IDs: {nrow(dist_staff)}")))

write_csv(dist_staff, "data/cleaned_iis_cop_names.csv")

