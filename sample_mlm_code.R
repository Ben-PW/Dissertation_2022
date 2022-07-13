# Additional packages required for the analysis
require(lme4) #glmer for MLMs
require(lmerTest) # sig. values for glmer

# Define new variable 'output' as a list. This is to allow the loop to store multiple outputs 
# instead of just overwriting them
output <- list()

### IMPORTANT ###
# lme4 does not provide r2-like goodness of fit, but there are additional tests
#that can be used to calculate a measure of goodness of fit
#There is an alternative package - GLMMRR - that provides a r2-like measure
# Defining a new variable R2 - NA for now as it will be filled once the loop is run
R2s <- NA

for(i in 1:nrow(covariate_grid)) {
  # printing [i] just to track progress of analysis
  print(i)
  
  # each row of covariate_grid is now used as a formula for the regression
  output[[i]] <- glmer(paste('redCards ~ avrate +',
                                    covariate_grid[i, 'formula']
                             + (1 | playerShort) + (1 | refNum),
                             data = redcard,
                             family = binomial(link="logit"),
                             control = glmerControl(optimizer = "bobyqa"),
                             nAGQ = 0))
  R2s[i] <- summary(output[[i]])$r.squared
}

output_table <- data.frame(covariates = covariate_grid,
                           R2 = R2s)

# As before, defining a new R2 variable to be used in loop. This loop is to extract specific values for 
# plotting

R2 <- NA

for (i in 1:nrow(covariate_grid)) {
  
  #selecting just the R2 values from the results of the previous loop
}