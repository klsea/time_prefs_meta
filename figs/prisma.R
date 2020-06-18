
library(PRISMAstatement)

# Create flow chart
prsm <- prisma(found = 7561,
       found_other = 0,
       no_dupes = 4738, 
       extra_dupes_box = TRUE, 
       screened = 4738, 
       screen_exclusions = 3959, 
       full_text = 779 ,
       full_text_exclusions = 749, 
       qualitative = 30,
       quantitative = 30,
       width = 800, height = 800)
prsm
