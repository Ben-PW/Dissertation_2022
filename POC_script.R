# Loading data

library(here)
redcard <- read.csv(here('Data', 'CrowdstormingDataJuly1st.csv'), stringsAsFactors = FALSE)

# Remove NA values
redcard <- na.omit(redcard)

?# Take average of rater scores for player skin tone
redcard$avrate <- redcard$rater1 + ((redcard$rater2 - redcard$rater1) / 2)

########################## Beginning Multiverse Analysis

# Create list of example potential covariates for proof of concept
covariates_list <- list(position = c(NA, 'position'),
                        height = c(NA, 'height'),
                        weight = c(NA, 'weight'),
                        refNum = c(NA, 'refNum'))



# Create list of all possible combinations
library(tidyverse)


###### Making a grid combining the NA and other values. This then outputs a list
###### of every possible combination of the selected covariates
covariate_grid <- expand.grid(covariates_list) 
rm(covariates_list)

# First row was two NA values so it was deleted
covariate_grid <- covariate_grid[-1,]

covariate_grid <- covariate_grid %>%
  tidyr::unite(formula, position:refNum, sep = '+', na.rm = TRUE)
# in the above code, covariate_grid was changed so that all the covariates were listed in a single
# columnn labelled 'formula', where each covariate was separated by a '+' to allow for inclusion in a 
# regression equation


# Define new variable 'output' as a list. This is to allow the loop to store multiple outputs 
# instead of just overwriting them
output <- list()


# Defining a new variable R2 - NA for now as it will be filled once the loop is run
R2s <- NA

for(i in 1:nrow(covariate_grid)) {
  # printing [i] just to track progress of analysis
  print(i)
  
  # each row of covariate_grid is now used as a formula for the regression
  output[[i]] <- lm(data = redcard,
                    formula = paste('redCards ~ avrate +',
                                    covariate_grid[i, 'formula']))
  R2s[i] <- summary(output[[i]])$r.squared
}


output_table <- data.frame(covariates = covariate_grid,
                           R2 = R2s)

# As before, defining a new R2 variable to be used in loop. This loop is to extract specific values for 
# plotting

R2 <- NA

for (i in 1:nrow(covariate_grid)) {
  
  #selecting just the R2 values from the results of the previous loop
  R2[i] <- summary(output[[i]])$r.squared
}

# Remove output variable as it is large and no longer needed
rm(output)

######## Creating plot of results
plot <- cbind(covariate_grid, R2) 

# Order results of plot by R2 value
plot2 <- plot[order(plot$R2), ]
rm(plot)

# Creating a grouping variable (n) for each row of covariate_grid
plot2$n <- 1:nrow(plot2)

# Creating final plot
plotfinal <- ggplot(data = plot2, 
                    aes(x = n, y = R2)) +
  geom_point() +
  labs(x = '')

# Creating dashboard to go underneath plot
dashboard <- plot2 %>% 
  gather(Bigdecision, Decision, -R2, -n) %>%
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
