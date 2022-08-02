############################################# Data import ####################################
library(here)
redcard <- read.csv(here('Data', 'CrowdstormingDataJuly1st.csv'), stringsAsFactors = FALSE)

# Create identifying variable for data screening
redcard$rownumber <- 1:nrow(redcard)

# Remove NA values
redcard <- na.omit(redcard)

# Take average of rater scores for player skin tone
redcard$avrate <- redcard$rater1 + ((redcard$rater2 - redcard$rater1) / 2)

# Collapsing redCards = 2 into redCards = 1 to create a binary DV:
redcard["redCards"][redcard["redCards"] == 2] <- 1
summary(as.factor(redcard$redCards))


############################################# Data transformations ####################################

# 1. Collapsing IV levels >= 2 into binary values (0,1):
redcard["yellowCards"][redcard["yellowCards"] >= 2] <- 1
summary(as.factor(redcard$yellowCards))

redcard["yellowReds"][redcard["yellowReds"] >= 2] <- 1
summary(as.factor(redcard$yellowReds))

# 2. refCountry needs to be recoded as a factor as well
redcard$refCountry <- as.factor(redcard$refCountry)

# 3. age variable needs to be calculated
# numerical value for age will be calculated for ease of regression in same manner to team 11
redcard$birthday <- as.Date(redcard$birthday, '%d.%m.%Y')
season_date <- as.Date('2013-01-01')
redcard$age <- as.numeric((season_date-redcard$birthday)/365)
rm(season_date)

# 4. collapse position variable in same manner to team 28 (hopefully reduce vector length)
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
# Getting a warming: Unknown levels in `f`: Center Fullback
# Might be worh mentioning other positions (Golakeeper) just for the sake
# of clarity

# Specifying 'Back' as reference category
#### NB for some reason doing this makes the log regression angry
#redcard$position <- 
#  fct_relevel(redcard$position, 
#              "Back")

redcard$position <- as.factor(redcard$position)
redcard$refCountry <- as.factor(redcard$refCountry)

# 8. Removing illogical values from yellowCards and yellowReds

redcard$yellowCards <- ifelse(redcard$yellowCards > 1, 1, redcard$yellowCards)
redcard$yellowCards <- as.factor(redcard$yellowCards)

redcard$yellowReds <- ifelse(redcard$yellowReds > 1, 1, redcard$yellowReds)
redcard$yellowReds <- as.factor(redcard$yellowReds)

######################################### Beginning Multiverse Analysis ################################
library(tidyverse)
# Create list of potential covariates 
covariates_list <- list(position = c(NA, 'position'),
                        yellowCards = c(NA, 'yellowCards'),
                        height = c(NA, 'height'),
                        weight = c(NA, 'weight'),
                        club = c(NA, 'club'),
                        goals = c(NA, 'goals'),
                        age = c(NA, 'age'),
                        meanIAT = c(NA, 'meanIAT'),
                        meanEXP = c(NA, 'meanExp'),
                        games = c(NA, 'games'),
                        refCountry = c(NA, 'refCountry'),
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
# column labelled 'formula', where each covariate was separated by a '+' to allow for inclusion in a 
# regression equation

######################################### Main multiverse loop ##########################################

# Define new variable 'output' as a list. This is to allow the loop to store multiple outputs 
# instead of just overwriting them
output <- list()


# Defining a new variables - NA for now as they will be filled once the loop is run
R2conditional <- NA
R2marginal <- NA
predictorR2 <- NA

require(lme4)
require(lmerTest)
require(tictoc)

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
  
  # Getting marginal R2 for each row
  R2marginal[i] <- modelsummary::get_gof(output)$r2.marginal
  
  # Getting individual predictor R2 for each row of covariate_grid
  predictorR2[i] <- as.data.frame(summary(output)$coefficients[,1])
  
  toc()
  toc()
}

################################################# Visualising ##########################################

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
length(R2marginal) <- nrow(covariate_grid)

output_table <- data.frame(covariates = covariate_grid,
                           R2c = R2conditional,
                           R2m = R2marginal)

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
  geom_point(aes(colour = Bigdecision), shape = 108, size = 1) +
  labs(x = 'specification number') +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        strip.background = element_blank())

library(patchwork)
plotfinal
plotfinal / dashboardfinal

# Save resampled data for reference - each run might differ slightly
write.csv(redcard, here('Data', 'redcard_resampled.csv'))


#############################################                   ########################################
############################################# Second Multiverse ########################################
#############################################                   ########################################


############################################# Data re-import ####################################

library(here)
redcard <- read.csv(here('Data', 'CrowdstormingDataJuly1st.csv'), stringsAsFactors = FALSE)

# Create identifying variable for data screening
redcard$rownumber <- 1:nrow(redcard)

# Remove NA values
redcard <- na.omit(redcard)

# Take average of rater scores for player skin tone
redcard$avrate <- redcard$rater1 + ((redcard$rater2 - redcard$rater1) / 2)

########################################### Transformations ##########################################
library(tidyverse)

# refCountry recoded as a factor 
redcard$refCountry <- as.factor(redcard$refCountry)

# calculating age variable
redcard$birthday <- as.Date(redcard$birthday, '%d.%m.%Y')
season_date <- as.Date('2013-01-01')
redcard$age <- as.numeric((season_date-redcard$birthday)/365)
rm(season_date)

# Creating an alternative DV which covers likelihood of any kind of penalisation
# Weighting applied for yellowCards and yellowReds (1 = 0.5), not redCards (1 = 1)
redcard <- redcard %>% mutate(allcards = ((yellowCards + yellowReds) * 0.5) + redCards)
summary(as.factor(redcard$allcards))


######################################### Second multiverse loop ##########################################

# Define new variable 'output' as a list. This is to allow the loop to store multiple outputs 
# instead of just overwriting them
output_poiss <- list()


# Defining a new variables - NA for now as they will be filled once the loop is run
R2conditional_poiss <- NA
predictorR2_poiss <- NA

require(lme4)
require(lmerTest)
require(tictoc)

for(i in 1:nrow(covariate_grid)) {
  # printing [i] just to track progress of analysis
  print(i)
  
  # see how long full loop is taking
  tic("Total")
  
  # see how long regression is taking
  tic("Regression")
  
  # each row of covariate_grid is now used as a formula for the regression
  output_poiss <- glmer(data = redcard,
                  formula = paste('allcards ~ avrate +',
                                  covariate_grid[i, 'formula'], 
                                  '+ (1 | playerShort) + (1 | refNum)'),
                  family = poisson(link="log"),
                  control = glmerControl(optimizer = "bobyqa"),
                  nAGQ = 0)
  
  toc()
  
  # see how long data extraction is taking
  tic("Data extraction")
  
  # Getting overall model fit for each row of covariate_grid
  R2conditional_poiss[i] <- as.data.frame(performance::model_performance(output_poiss, metrics = 'R2'))
  
  # Getting individual predictor R2 for each row of covariate_grid
  predictorR2_poiss[i] <- as.data.frame(summary(output_poiss)$coefficients[,1])
  
  toc()
  toc()
}

################################################### Creating Data Frames ############################################

############ Turning list of R2 cond/marginal values into a data frame
# find length of each element of predictor_R2 list
len <- sapply(R2conditional_poiss, length)

# longest length dictates number of rows in data frame
n <- max(len)

# finds number of NAs required for each row to be of same length to longest
len <- n - len

cmR2_df_poiss <- data.frame(mapply(function(x,y) c( x , rep( NA , y )), R2conditional_poiss, len))
# above line does similar to below but long format

# magically creates a data frame don't ask me how
#cmR2_df_poiss <- data.frame(t(mapply(function(x,y) c(x, rep(NA, y)), R2conditional_poiss, len)))


############ Turning list of predictor R2 values into a data frame
# find length of each element of predictor_R2 list
len <- sapply(predictorR2_poiss, length)

# longest length dictates number of rows in data frame
n <- max(len)

# finds number of NAs required for each row to be of same length to longest
len <- n - len

# mapply(function(x,y) c( x , rep( NA , y )), predictorR2, len)
# above line does similar to below but long format

# magically creates a data frame don't ask me how
predR2_df_poiss <- data.frame(t(mapply(function(x,y) c(x, rep(NA, y)), predictorR2_poiss, len)))

#################### Turning conditional R2 values into data frame

# Pads R2conditional with NA values to avoid errors in code below if whole MVA isn't performed
length(R2conditional_poiss) <- nrow(covariate_grid)

output_table_poiss <- data.frame(covariates = covariate_grid,
                           R2 = R2conditional_poiss)

# Remove output variable as it is large and no longer needed
rm(output)

######################################### Creating plot of results #####################################

plot_poiss <- cbind(covariate_grid, R2conditional_poiss) 

# Order results of plot by R2 value
plot2_poiss <- plot[order(plot$R2conditional_poiss), ]
rm(plot)

# Creating a grouping variable (n) for each row of covariate_grid
plot2_poiss$n <- 1:nrow(plot2_poiss)

# Creating final plot
plotfinal_poiss <- ggplot(data = plot2_poiss, 
                    aes(x = n, y = R2conditional_poiss)) +
  geom_point() +
  labs(x = '')

# Creating dashboard to go underneath plot
dashboard_poiss <- plot2_poiss %>% 
  gather(Bigdecision, Decision, -R2conditional_poiss, -n) %>%
  filter(Decision != 'NA')

rm(plot2_poiss)

# Creating levels in Bigdecision variable that correspond to data in covariate_grid rows

dashboard_poiss$Bigdecision <- factor(dashboard$Bigdecision, 
                                levels = names(covariate_grid))


dashboardfinal_poiss <- ggplot(data = dashboard_poiss,
                         aes(x = n, y = Decision, colour = Bigdecision)) +
  facet_grid(Bigdecision ~ ., scales = "free", space = "free", drop = ) +
  geom_point(aes(colour = Bigdecision), shape = 108, size = 1) +
  labs(x = 'specification number') +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        strip.background = element_blank())

library(patchwork)
plotfinal_poiss
plotfinal_poiss / dashboardfinal_poiss
