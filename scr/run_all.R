# Run all scripts
# 6.9.20 KLS

#libraries
library(tidyverse)
library(here)

# remove generated folders and add empty folders
unlink(here::here('figs'), recursive = TRUE)
unlink(here::here('output'), recursive = TRUE)
dir.create(here::here('figs'))
dir.create(here::here('output'))

# run all scripts in order
list.files(here::here(), full.names = TRUE) %>% walk(source)

