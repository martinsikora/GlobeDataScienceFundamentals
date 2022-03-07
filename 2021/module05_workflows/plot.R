#!/usr/bin/Rscript --vanilla

########################################
## A simple R script to plot our data ##
########################################


## --------------------------------------------------------------------------------
## command line arguments

library(tidyverse)


## --------------------------------------------------------------------------------
## parse command line arguments

args <- commandArgs(trailingOnly = TRUE)
infile <- args[1]
outfile <- args[2]


## --------------------------------------------------------------------------------
## read and plot the data

d <- read_csv(infile)
pdf(outfile, height = 4)
ggplot(d, aes(x = date, y = new_cases)) +
    geom_point(size = 1, alpha = 0.5) +
    geom_line(size = 0.25) +
    theme_classic() +
    ggtitle(d$location[1])
dev.off()
