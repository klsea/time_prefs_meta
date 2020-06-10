library(tidyverse)

list.files('~/github/time_prefs_meta', full.names = TRUE) %>% walk(source)
