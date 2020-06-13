# Run meta-regression analysis and bubble plots
# 6.2.20 KLS

# load required packages
library(here)
library(meta)
library(metafor)

# load source functions

# set hard-coded variables
file <- 'effect_sizes.csv'

# load data
dt <- read.csv(here::here('output', file))

# Put in alpha order by design
dt <- dt[order(dt$Design, dt$Study.Identifier),]

# Fix factors ####
## Incentive
levels(dt$Incentive)
dt$Incentive[which(dt$Incentive == 'real ')] <- 'real'
dt$Incentive[which(dt$Incentive == '')] <- 'hypothetical'
dt$Incentive <- factor(dt$Incentive)

## Magnitude
levels(dt$Magnitude.of.Time.Delay)
dt$Magnitude.of.Time.Delay[which(dt$Magnitude.of.Time.Delay == 'months ')] <- 'months'
dt$Magnitude.of.Time.Delay <- ordered(dt$Magnitude.of.Time.Delay, levels = c('hours', 'days', 'weeks', 'months', 'years'))
dt$Delay <- dt$Magnitude.of.Time.Delay

## Measure
levels(dt$Measure)
dt$Measure[which(dt$Measure == 'parameter ')] <- 'parameter'
dt$Measure <- factor(dt$Measure)

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

# Design ####
mr1.design <- metareg(m.hksj, Design)
bubble(mr1.design, 
       xlab = 'Design', 
       col.line = 'blue', 
       studlab = TRUE)

# Incentive ####
mr2.incent <- metareg(m.hksj, Incentive)
bubble(mr2.incent, 
       xlab = 'Incentive', 
       col.line = 'blue', 
       studlab = TRUE)

# Delay ####
mr3.delay <- metareg(m.hksj, Delay)
bubble(mr3.delay, 
       xlab = 'Delay', 
       col.line = 'blue', 
       studlab = TRUE)

# Measure ####
mr4.measure <- metareg(m.hksj, Measure)
bubble(mr4.measure, 
       xlab = 'Measure', 
       col.line = 'blue', 
       studlab = TRUE)

rm(dt, m.hksj, mr1.design, mr2.incent, mr3.delay, mr4.measure, file)
