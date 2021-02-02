# Run basic meta-analysis and create forest plot
# 6.2.20 KLS

# load required packages
library(here)
library(tidyverse)
library(meta)
library(metafor)
#library(dmetar)

# load source functions

# set hard-coded variables
file <- 'effect_sizes.csv'

# load data
dt <- read.csv(here::here('output', file), stringsAsFactors = FALSE)
#dt <- dt[which(dt$Study.Identifier != 'Stoeckel 2013'),]

# Put in alpha order by design
dt <- dt[order(dt$fishers_z),]

# Random Effects model - Knapp-Hartung (-Sidik-Jonkman) adjustment ####
m.cor <- metacor(cor = fishers_z, 
                  n = n, 
                  data = dt, 
                  studlab= Study.Identifier,
                  method.tau = 'SJ', 
                  sm = 'ZCOR')
m.cor
saveRDS(m.cor, here::here('output', 'cor_model.RDS'))

# Forest plot - hksj model ####

png(file = 'figs/forestplot.png', width = 600, height = 750) 
meta::forest(m.cor, print.pval.Q = FALSE, print.I2.ci = TRUE, print.tau2 = FALSE)
dev.off() 

# for presentation
#ggsave(file = here::here('figs', 'forestplot.svg'), plot = meta::forest(m.cor), width = 7.5, height = 10)


# REM - DerSimonian-Laird ####
m.dl <- metacor(cor = fishers_z,
                n = n,
                data=dt,
                studlab= Study.Identifier,
                method.tau = 'SJ', 
                sm="ZCOR")
m.dl
saveRDS(m.dl, here::here('output', 'dl_model.RDS'))

# Forest plot - dl model####
meta::forest(m.dl)

rm(dt, m.cor, file, m.dl)

