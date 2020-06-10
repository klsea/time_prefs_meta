# Run basic meta-analysis and create forest plot
# 6.2.20 KLS

# load required packages
library(here)
library(tidyverse)
library(meta)
library(metafor)

# load source functions

# set hard-coded variables
file <- 'effect_sizes.csv'

# load data
dt <- read.csv(here::here('data', file), stringsAsFactors = FALSE)

# Put in alpha order by design
dt <- dt[order(dt$Design, dt$Study.Identifier),]

# Random Effects model - Knapp-Hartung (-Sidik-Jonkman) adjustment
m.hksj <- metagen(TE = effect_size, 
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

# Forest plot
meta::forest(m.hksj, leftlabs = c('Author', 'Effect Size', 'Standard Error'))

# REM HKSJ by design
s.m.hksj <- subgroup.analysis.mixed.effects(x = m.hksj,
                                            subgroups = dt$Design)
meta::forest(s.m.hksj)


# REM - DerSimonian-Laird
m.dl <- metagen(TE = effect_size,
                seTE = std_err,
                data=dt,
                studlab= Study.Identifier,
                comb.fixed = FALSE,
                comb.random = TRUE,
                hakn = FALSE,
                prediction=TRUE,
                sm="SMD")
m.dl

# Forest plot
meta::forest(m.dl)

rm(dt, m.hksj, file)
