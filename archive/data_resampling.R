### PACKAGES
require(here)
require(tidyverse)

### LOAD DATA
redcard <- read.csv(here('Data', 'CrowdstormingDataJuly1st.csv'), stringsAsFactors = FALSE)

# Remove NA values
redcard <- na.omit(redcard)

# Take average of rater scores for player skin tone
redcard$avrate <- redcard$rater1 + ((redcard$rater2 - redcard$rater1) / 2)

# DV (redCards) needs to be restructured as dichotomous:
summary(as.factor(redcard$redCards))

# Binary DV: replacing the value of 2 with 1
redcard["redCards"][redcard["redCards"] == 2] <- 1
summary(as.factor(redcard$redCards))

# Select & resample observations without red cards
nocard <- redcard %>% filter (redCards == 0) 
nocard <- nocard[sample(nrow(nocard), size = 1455, replace = FALSE),]

# Select & resample observations with red cards 
# his step is redundant as sample = resample
# Included for procedural completeness
rc_only <- redcard %>% filter (redCards == 1) 
rc_only <- rc_only[sample(nrow(rc_only), size = 1455, replace = FALSE),]

# Arrange rows by players' ID and red cards
redcard_sample <- bind_rows(nocard,rc_only)
redcard_sample <- arrange(redcard_sample, playerShort, redCards)

# Save resampled data
write.csv(redcard_sample, here("Data", "redcard_sample.csv"))
