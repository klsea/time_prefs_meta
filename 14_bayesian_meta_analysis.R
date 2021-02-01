# Run bayesian meta-analysis using brms
# 1.24.21 KLS

# load required packages
library(here)
library(meta)
library(metafor)
library(brms)
library(ggplot2)

# load source functions

# set hard-coded variables
file <- 'effect_sizes.csv'
priors <- c(prior(normal(0,1), class = Intercept), 
             prior(cauchy(0,0.5), class = sd))

# load data
dt <- read.csv(here::here('output', file))

# run meta ####
# run this code to fit the model- takes a few minutes to complete
m.brm <- brm(fishers_z|se(var_fishers_z) ~ 1  + ( 1 | Study.Identifier),
              data = dt,
              prior = priors, sample_prior = TRUE,
              iter = 6000,
              control = list(max_treedepth = 18))

# save model
saveRDS(m.brm, here::here('output', 'bayesian_model.rds'))

# load model from saved ####
# load the model that has already been fit (see code above)
m.brm <- readRDS(here::here('output', 'bayesian_model.rds'))

# check convergence
pp_check(m.brm) # look at plot to see if replications are similar to observed data
pp_check(m.brm, nsamples = 1e3, type = "stat_2d") + theme_bw(base_size = 20) # another way to visualize
summary(m.brm) # look for rhat values to be less than 1.01

# posterior distribution
post.samples <- posterior_samples(m.brm, c("^b", "^sd"))
names(post.samples)
names(post.samples) <- c("smd", "tau")

# density plot of poterior distributions

#Plot for SMD
ggplot(aes(x = smd), data = post.samples) +
  geom_density(fill = "lightblue", color = "lightblue", alpha = 0.7) +
  geom_point(y = 0, x = mean(post.samples$smd)) +
  labs(x = expression(italic(SMD)),
       y = element_blank()) +
  theme_minimal()

# Plot for tau
ggplot(aes(x = tau), data = post.samples) +
  geom_density(fill = "lightgreen", color = "lightgreen", alpha = 0.7) +
  geom_point(y = 0, x = mean(post.samples$tau)) +
  labs(x = expression(tau),
       y = element_blank()) +
  theme_minimal()

# Empirical Cumulative Distriubtion Factor
smd.ecdf <- ecdf(post.samples$smd)
smd.ecdf(0) # can change to test different values

#testing hypotheses

h1 <- hypothesis(m.brm, "Intercept < 0")
print(h1, digits = 4)
EvidenceRatio1 <- round(hypothesis(m.brm, "Intercept < 0")$hypothesis$Evid.Ratio, 3)
Credibility1 <- round(hypothesis(m.brm, "Intercept < 0")$hypothesis$Post.Prob*100, 0)

plot(h1)

h2 <- hypothesis(m.brm, "Intercept < -0.2")
print(h2, digits = 4)
EvidenceRatio2 <- round(hypothesis(m.brm, "Intercept < -0.2")$hypothesis$Evid.Ratio, 3)
Credibility2 <- round(hypothesis(m.brm, "Intercept < -0.2")$hypothesis$Post.Prob*100, 0)


