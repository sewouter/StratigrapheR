# ----

setwd("C:/Users/Seb/Desktop/CycloCross/StratigrapheR/data-raw")

# ----

file.e <- "HB2000.svg"

example.HB2000.svg <- readLines(file.e)

usethis::use_data(example.HB2000.svg, overwrite = TRUE)
