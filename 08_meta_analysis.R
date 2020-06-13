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

# Put in alpha order by design
dt <- dt[order(dt$Design, dt$Study.Identifier),]

# Random Effects model - Knapp-Hartung (-Sidik-Jonkman) adjustment ####
m.hksj <- metagen(TE = adj_effect_size, 
                  seTE = std_err, 
                  data = dt, 
                  studlab= Study.Identifier,
                  comb.fixed = FALSE,
                  comb.random = TRUE, 
                  method.tau = 'SJ', 
                  hakn = TRUE, 
                  prediction = TRUE, 
                  sm = 'SMD')
m.hksj
saveRDS(m.hksj, here::here('output', 'hksj_model.RDS'))

# Forest plot - hksj model ####
meta::forest(m.hksj, leftlabs = c('Author', 'Effect Size', 'Standard Error'))

# REM HKSJ by design ####
s.m.hksj <- subgroup.analysis.mixed.effects(x = m.hksj, subgroups = dt$Design)
saveRDS(s.m.hksj, here::here('output', 'hksj_model_subgroup.RDS'))

# Forest plot - hksj model with subgroups ####
meta::forest(s.m.hksj)

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
                sm="SMD")
m.dl
saveRDS(m.dl, here::here('output', 'dl_model.RDS'))

# Forest plot - dl model####
meta::forest(m.dl)

rm(dt, m.hksj, file, m.dl, s.m.hksj)
