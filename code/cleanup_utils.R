########################## META SHIT ##########################################

###############################################################################

########################## FUNCTIONS ##########################################
clust_strings <- function(x, dist_method = "jaccard", q = 2, h = 0.5, p = 0) {
  # replace na with multiples of 1000 so they're far apart from text, but also from each other

  mtx <- 1 - stringsimmatrix(x, method = dist_method, q = q)
  mtx[is.na(mtx)] <- seq_along(mtx[is.na(mtx)]) * 1000
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