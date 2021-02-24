# ----

setwd("C:/Users/Seb/Desktop/CycloCross/StratigrapheR/data-raw")

# ----

set.seed(42)

n <- 600
t <- seq_len(n)

p1 <- 30
p2 <- 240

xy <- (1 + 0.6 * sin(t*2*pi/p2)) * sin(t*2*pi/p1)  + 2 * sin(t*2*pi/p2) +
  rnorm(n, sd = 0.5) + t * 0.01

inter_dt <- round(runif(length(xy), min = 0.5, max = 1.5),1)

dt <- cumsum(inter_dt)

keep <- runif(length(dt)) < 0.5

xy <- xy[keep]
dt <- dt[keep]

irreg.example <- data.frame(dt, xy)

usethis::use_data(irreg.example, overwrite = TRUE)