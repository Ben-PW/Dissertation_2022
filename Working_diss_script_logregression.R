################################## Loading data

library(here)
redcard <- read.csv(here('Data', 'CrowdstormingDataJuly1st.csv'), stringsAsFactors = FALSE)

# Remove NA values
redcard <- na.omit(redcard)

# Take average of rater scores for player skin tone
redcard$avrate <- redcard$rater1 + ((redcard$rater2 - redcard$rater1) / 2)

################################## Data transformatinos for Logit MLM
  
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

########################## Beginning Multiverse Analysis

# Create list of potential covariates 
covariates_list <- list(position = c(NA, 'position'),
                        height = c(NA, 'height'),
                        weight = c(NA, 'weight'),
                        club = c(NA, 'club'),
                        goals = c(NA, 'goals'),
                        victories = c(NA, 'victories'))
                        



# Create list of all possible combinations
library(tidyverse)


###### Making a grid combining the NA and other values. This then outputs a list
###### of every possible combination of the selected covariates
covariate_grid <- expand.grid(covariates_list) 
rm(covariates_list)

# First row was two NA values so it was deleted
covariate_grid <- covariate_grid[-1,]

covariate_grid <- covariate_grid %>%
  tidyr::unite(formula, position:victories, sep = '+', na.rm = TRUE)
# in the above code, covariate_grid was changed so that all the covariates were listed in a single
# columnn labelled 'formula', where each covariate was separated by a '+' to allow for inclusion in a 
# regression equation


# Define new variable 'output' as a list. This is to allow the loop to store multiple outputs 
# instead of just overwriting them
output <- list()


# Defining a new variable R2 - NA for now as it will be filled once the loop is run
R2s <- NA

require(lme4)
require(lmerTest)

for(i in 1:nrow(covariate_grid)) {
  # printing [i] just to track progress of analysis
  print(i)
  
  # each row of covariate_grid is now used as a formula for the regression
  output[[i]] <- glmer(data = redcard,
                    formula = paste('redCards ~ avrate +',
                                    covariate_grid[i, 'formula'], 
                                    '+ (1 | playerShort) + (1 | refNum)'),
                    family = binomial(link="logit"),
                    control = glmerControl(optimizer = "bobyqa"),
                    nAGQ = 0)
  
  R2s[i] <- modelsummary::get_gof(output[[i]])$r2.conditional
}



output_table <- data.frame(covariates = covariate_grid,
                           R2 = R2s)

# As before, defining a new R2 variable to be used in loop. This loop is to extract specific values for 
# plotting

R2conditional <- NA

for (i in 1:nrow(covariate_grid)) {
  
  #selecting just the conditional R2 values from the results of the previous loop
  R2conditional[i] <- modelsummary::get_gof(output[[i]])$r2.conditional
}

# Remove output variable as it is large and no longer needed
rm(output)

######## Creating plot of results
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
