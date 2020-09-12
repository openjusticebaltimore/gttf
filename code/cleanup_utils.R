########################## META SHIT ##########################################
# after running thru the script a couple times & adjusting parameters, these are running sets of 
# misspelled & abbreviated names to feed back through for cleaning
misspelled <- read_csv("https://calc.mayfirst.org/0axbk96wlw47.csv") %>%
  mutate_at(vars(misspelled, correct), toupper) %>%
  mutate(patt = case_when(
    first_or_last == "first" ~ "(?<=,\\s)\\b%s\\b",
    first_or_last == "last"  ~ "\\b%s\\b(?=,\\s)",
    TRUE                     ~ "\\b%s\\b"
  )) %>%
  mutate(misspelled = sprintf(patt, misspelled)) %>%
  select(misspelled, correct) %>%
  deframe()
abbr <- read_csv("https://calc.mayfirst.org/kv8dxti8lntv.csv") %>%
  mutate(short = sprintf("\\b%s\\b", short)) %>%
  deframe()
corrections <- c(misspelled, abbr)
rm(misspelled, abbr)
###############################################################################

########################## FUNCTIONS ##########################################
clust_strings <- function(x, dist_method = "jaccard", q = 2, h = 0.5, p = 0) {
  mtx <- 1 - stringsimmatrix(x, method = dist_method, q = q)
  as.dist(mtx) %>%
    hclust(method = "complete") %>%
    cutree(h = h)
}

same_length <- function(x) {
  if (length(x) == 1) {
    return(FALSE)
  } else {
    return(reduce(x, identical))
  }
}

longest <- function(x, i = NULL) {
  if (is.null(i)) {
    x[which.max(nchar(x))]
  } else {
    x[which.max(nchar(i))]
  }
}

strip_suffix <- function(x) {
  x %>%
    str_remove_all("(?<=[A-Z])\\s(JR|SR|I{2,3}|IV)$") %>%
    str_remove_all("[[:punct:]]")
}

name_get_subset <- function(x) {
  # very non-R way of doing things here but it's the best I can figure out
  s <- seq_along(x)
  map_chr(s, function(i) {
    # browser()
    j <- s[-i]
    this_name <- strip_suffix(x[i])
    other_names <- strip_suffix(x[j])
    
    # if any other names == this name, just take the longest
    d <- this_name == other_names
    if (any(d)) {
      dupes <- x[c(j[d], i)]
      out <- dupes[which.max(nchar(dupes))]
    } else {
      detected <- str_which(other_names, this_name)
      if (length(detected)) {
        replacement <- x[j][detected]
        out <- replacement[which.max(nchar(replacement))]
      } else {
        out <- x[i]
      }
    }
    out
  })
}
###############################################################################