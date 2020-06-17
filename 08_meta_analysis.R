# Run basic meta-analysis and create forest plot
# 6.2.20 KLS

# load required packages
library(here)
library(tidyverse)
library(meta)
library(metafor)
library(dmetar)

# load source functions

# set hard-coded variables
file <- 'effect_sizes.csv'

# load data
dt <- read.csv(here::here('output', file), stringsAsFactors = FALSE)
dt <- dt[which(dt$Study.Identifier != 'Stoeckel 2013'),]

# Put in alpha order by design
dt <- dt[order(dt$Design, dt$Study.Identifier),]

# Random Effects model - Knapp-Hartung (-Sidik-Jonkman) adjustment ####
m.cor <- metacor(cor = effect_size, 
                  n = n, 
                  data = dt, 
                  studlab= Study.Identifier,
                  method.tau = 'SJ', 
                  sm = 'ZCOR')
m.cor
saveRDS(m.cor, here::here('output', 'cor_model.RDS'))

# Forest plot - hksj model ####

png(file = 'figs/forestplot.png', width = 600, height = 800) 
meta::forest(m.cor)
dev.off() 

# REM HKSJ by design ####
s.m.cor <- subgroup.analysis.mixed.effects(x = m.cor, subgroups = dt$Design)
saveRDS(s.m.cor, here::here('output', 'hksj_model_subgroup.RDS'))

# Forest plot - hksj model with subgroups ####
meta::forest(s.m.cor)

png(file = 'figs/forestplotsubgroup.png', width = 600, height = 800) 
meta::forest(s.m.hksj)
dev.off() 

# REM - DerSimonian-Laird ####
m.dl <- metagen(TE = adj_effect_size,
                seTE = std_err,
                data=dt,
                studlab= Study.Identifier,
                comb.fixed = FALSE,
                comb.random = TRUE,
                hakn = FALSE,
                prediction=TRUE,
                sm="ZCOR")
m.dl
saveRDS(m.dl, here::here('output', 'dl_model.RDS'))

# Forest plot - dl model####
meta::forest(m.dl)

rm(dt, m.hksj, file, m.dl, s.m.hksj)
