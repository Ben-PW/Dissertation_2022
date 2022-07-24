################################## Loading data

library(here)
redcard <- read.csv(here('Data', 'CrowdstormingDataJuly1st.csv'), stringsAsFactors = FALSE)

# Remove NA values
redcard <- na.omit(redcard)

# Take average of rater scores for player skin tone
redcard$avrate <- redcard$rater1 + ((redcard$rater2 - redcard$rater1) / 2)

############################################# Data transformatinos ####################################
  
# 1. DV (redCards) needs to be restructured as dichotomous:
summary(as.factor(redcard$redCards))
  
# replacing the value of 2 with 1
redcard["redCards"][redcard["redCards"] == 2] <- 1
summary(as.factor(redcard$redCards))

# refCountry needs to be recoded as a factor as well
redcard$refCountry <- as.factor(redcard$refCountry)

# age variable needs to be calculated
# numerical value for age will be calculated for ease of regression in same manner to team 11
redcard$birthday <- as.Date(redcard$birthday, '%d.%m.%Y')
season_date <- as.Date('2013-01-01')
redcard$age <- as.numeric((season_date-redcard$birthday)/365)
rm(season_date)

# collapse position variable in same manner to team 28 (hopefully reduce vector length)
library(forcats)

# Leaves with four categories, Goalkeeper, Back, Middle, Front
redcard$position <- 
  fct_recode(redcard$position, 
             "Back" = "Left Fullback",
             "Back" = "Right Fullback",
             "Back" = "Center Fullback",
             "Back" = "Center Back",
             "Middle" = "Left Midfielder",
             "Middle" = "Center Midfielder",
             "Middle" =  "Right Midfielder",
             "Middle" = "Attacking Midfielder",
             "Middle" = "Defensive Midfielder",
             "Front" = "Left Winger",
             "Front" = "Right Winger",
             "Front" = "Center Forward" )

# Specifying 'Back' as reference category
#### NB for some reason doing this makes the log regression angry
#redcard$position <- 
#  fct_relevel(redcard$position, 
#              "Back")

redcard$position <- as.factor(redcard$position)

################################## Resampling
library(tidyverse)
# Select & resample observations without red cards
nocard <- redcard %>% filter (redCards == 0) 
nocard <- nocard[sample(nrow(nocard), size = 1455, replace = FALSE),]

# Select & resample observations with red cards 
# This step is redundant as sample = resample
rc_only <- redcard %>% filter (redCards == 1) 
rc_only <- rc_only[sample(nrow(rc_only), size = 1455, replace = FALSE),]

# Arrange rows by players' ID and red cards
redcard <- bind_rows(nocard,rc_only)
redcard <- arrange(redcard_sample, playerShort, redCards)

# Clean up the environment
rm(nocard,rc_only)

######################################### Beginning Multiverse Analysis ################################

# Create list of potential covariates 
covariates_list <- list(position = c(NA, 'position'),
                        height = c(NA, 'height'),
                        weight = c(NA, 'weight'),
                        club = c(NA, 'club'),
                        goals = c(NA, 'goals'),
                        age = c(NA, 'age'),
                        victories = c(NA, 'victories'))
                        



############# Create list of all possible combinations

# Making a grid combining the NA and other values. This then outputs a list
# of every possible combination of the selected covariates
covariates_list <- expand.grid(covariates_list) 


# First row was two NA values so it was deleted (undone because it gives baseline avrate score)
#covariate_grid <- covariate_grid[-1,]

covariate_grid <- covariates_list %>%
  tidyr::unite(formula, position:victories, sep = '+', na.rm = TRUE)
# in the above code, covariate_grid was changed so that all the covariates were listed in a single
# columnn labelled 'formula', where each covariate was separated by a '+' to allow for inclusion in a 
# regression equation

######################################### Main multiverse loop ##########################################

# Define new variable 'output' as a list. This is to allow the loop to store multiple outputs 
# instead of just overwriting them
output <- list()


# Defining a new variables - NA for now as they will be filled once the loop is run
R2conditional <- NA
predictorR2 <- NA

library(lme4)
library(lmerTest)
library(tictoc)

for(i in 1:nrow(covariate_grid)) {
  # printing [i] just to track progress of analysis
  print(i)
  
  # see how long full loop is taking
  tic("Total")
  
  # see how long regression is taking
  tic("Regression")
  
  # each row of covariate_grid is now used as a formula for the regression
  output <- glmer(data = redcard,
                    formula = paste('redCards ~ avrate +',
                                    covariate_grid[i, 'formula'], 
                                    '+ (1 | playerShort) + (1 | refNum)'),
                    family = binomial(link="logit"),
                    control = glmerControl(optimizer = "bobyqa"),
                    nAGQ = 0)
  
  toc()
  
  # see how long data extraction is taking
  tic("Data extraction")
  
  # Getting overall model fit for each row of covariate_grid
  R2conditional[i] <- modelsummary::get_gof(output)$r2.conditional
  
  # Getting individual predictor R2 for each row of covariate_grid
  predictorR2[i] <- as.data.frame(summary(output)$coefficients[,1])
  
  toc()
  toc()
}

############ Turning list of R2 values into a data frame
# find length of each element of predictor_R2 list
len <- sapply(predictorR2, length)

# longest length dictates number of rows in data frame
n <- max(len)

# finds number of NAs required for each row to be of same length to longest
len <- n - len

# mapply(function(x,y) c( x , rep( NA , y )), predictorR2, len)
# above line does similar to below but long format

# magically creates a data frame don't ask me how
R2_df <- data.frame(t(mapply(function(x,y) c(x, rep(NA, y)), predictorR2, len)))

#################### Turning conditional R2 values into data frame

# Pads R2conditional with NA values to avoid errors in code below if whole MVA isn't performed
length(R2conditional) <- nrow(covariate_grid)

output_table <- data.frame(covariates = covariate_grid,
                           R2 = R2conditional)

# Remove output variable as it is large and no longer needed
rm(output)

######################################### Creating plot of results #####################################

plot <- cbind(covariate_grid, R2conditional) 

# Order results of plot by R2 value
plot2 <- plot[order(plot$R2conditional), ]
rm(plot)

# Creating a grouping variable (n) for each row of covariate_grid
plot2$n <- 1:nrow(plot2)

# Creating final plot
plotfinal <- ggplot(data = plot2, 
                    aes(x = n, y = R2conditional)) +
  geom_point() +
  labs(x = '')

# Creating dashboard to go underneath plot
dashboard <- plot2 %>% 
  gather(Bigdecision, Decision, -R2conditional, -n) %>%
  filter(Decision != 'NA')

rm(plot2)

# Creating levels in Bigdecision variable that correspond to data in covariate_grid rows

dashboard$Bigdecision <- factor(dashboard$Bigdecision, 
                                levels = names(covariate_grid))


dashboardfinal <- ggplot(data = dashboard,
                         aes(x = n, y = Decision, colour = Bigdecision)) +
  facet_grid(Bigdecision ~ ., scales = "free", space = "free", drop = ) +
  geom_point(aes(colour = Bigdecision), shape = 108, size = 7) +
  labs(x = 'specification number') +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        strip.background = element_blank())

library(patchwork)
plotfinal / dashboardfinal

# Save resampled data for reference - each run might differ slightly
write.csv(redcard, here('Data', 'redcard_resample.csv'))
