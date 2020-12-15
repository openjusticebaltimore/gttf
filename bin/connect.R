# Connect to CaseHarvester
# kbmorales@protonmail.com

# Sets up the main connections to tables on CaseHarvester

# Setup -------------------------------------------------------------------

library(DBI)
library(dbplyr)
library(dplyr)
library(RPostgreSQL)

# DBI ---------------------------------------------------------------------

con <- dbConnect(odbc::odbc(),
                 dsn = "PostgreSQL30")

# Main cases table --------------------------------------------------------

cases <- tbl(con,
             in_schema("public", "cases"))

# dscivil - District Court Civil ------------------------------------------

# Civil cases currently out of scope of project

# cc - Circuit Court Civil ------------------------------------------------

# Civil cases currently out of scope of project

# dscr - District Court Criminal ------------------------------------------

dscr <- tbl(con,
            in_schema("public", "dscr"))

### Sub tables

# Relations
dscr_rel <- tbl(con,
                in_schema("public",
                          "dscr_related_persons"))

# Charges
dscr_chr <- tbl(con,
                in_schema("public",
                          "dscr_charges"))

##use cjis code

# $ cjis_code
## recommended sentencing - look at # of years threatened w/ initial charges

# Defendants
dscr_def <- tbl(con,
                in_schema("redacted",
                          "dscr_defendants"))


# dsk8 - Circuit Court Criminal -------------------------------------------

dsk8 = tbl(con,
           in_schema("public", "dsk8"))

### Sub tables

# related persons
dsk8_rel <- tbl(con, in_schema("public",
                               "dsk8_related_persons"))

# defendants
dsk8_def = tbl(con, in_schema("redacted",
                              "dsk8_defendants"))

# charges
dsk8_chr = tbl(con, in_schema("public",
                              "dsk8_charges"))
