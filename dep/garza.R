# Garza 
garza<- pivot_wider(garza, id_cols = colnames(garza[c(1:6, 13)]), names_from = 'Intervention', 
                    values_from = c('mean', 'sd', 'n', 'age_mean', 'age_range', 'age_sd'))

g1 <- mutate(garza, effect_size = esc_mean_sd(grp1m = mean_Older, grp1sd = sd_Older, grp1n = n_Older, 
                                              grp2m = mean_Middle_2,  grp2sd = sd_Middle_2,  grp2n = n_Middle_2, es.type = 'g')[1][[1]], 
             std_err = esc_mean_sd(grp1m = mean_Older, grp1sd = sd_Older, grp1n = n_Older, 
                                   grp2m = mean_Middle_2,  grp2sd = sd_Middle_2,  grp2n = n_Middle_2, es.type = 'g')[2][[1]], 
             var = esc_mean_sd(grp1m = mean_Older, grp1sd = sd_Older, grp1n = n_Older, 
                               grp2m = mean_Middle_2,  grp2sd = sd_Middle_2,  grp2n = n_Middle_2, es.type = 'g')[3][[1]])
g1$conditionID <- 'OldvM2'

g2 <- mutate(garza, effect_size = esc_mean_sd(grp1m = mean_Middle_2, grp1sd = sd_Middle_2, grp1n = n_Middle_2, 
                                              grp2m = mean_Middle_1,  grp2sd = sd_Middle_1,  grp2n = n_Middle_1, es.type = 'g')[1][[1]], 
             std_err = esc_mean_sd(grp1m = mean_Middle_2, grp1sd = sd_Middle_2, grp1n = n_Middle_2, 
                                   grp2m = mean_Middle_1,  grp2sd = sd_Middle_1,  grp2n = n_Middle_1, es.type = 'g')[2][[1]], 
             var = esc_mean_sd(grp1m = mean_Middle_2, grp1sd = sd_Middle_2, grp1n = n_Middle_2, 
                               grp2m = mean_Middle_1,  grp2sd = sd_Middle_1,  grp2n = n_Middle_1, es.type = 'g')[3][[1]])
g2$conditionID <- 'M2vM1'

g3 <- mutate(garza, effect_size = esc_mean_sd(grp1m = mean_Middle_1, grp1sd = sd_Middle_1, grp1n = n_Middle_1, 
                                              grp2m = mean_Younger,  grp2sd = sd_Younger,  grp2n = n_Younger, es.type = 'g')[1][[1]], 
             std_err = esc_mean_sd(grp1m = mean_Middle_1, grp1sd = sd_Middle_1, grp1n = n_Middle_1, 
                                   grp2m = mean_Younger,  grp2sd = sd_Younger,  grp2n = n_Younger, es.type = 'g')[2][[1]], 
             var = esc_mean_sd(grp1m = mean_Middle_1, grp1sd = sd_Middle_1, grp1n = n_Middle_1, 
                               grp2m = mean_Younger,  grp2sd = sd_Younger,  grp2n = n_Younger, es.type = 'g')[3][[1]])
g3$conditionID <- 'M1vYoung'
garza <- rbind(g1, g2, g3)