### SETUP

require(here)
require(lme4) #glmer for MLMs
# read https://cran.r-project.org/web/packages/lme4/lme4.pdf for interpretation.
require(lmerTest) # sig. values for glmer
require(insight)
# extract R-squared for fixed, random, and mixed effects
require(sjPlot)
# creates nice overview tables.

#require(MuMIn) #R Squared
# read https://cran.r-project.org/web/packages/MuMIn/MuMIn.pdf for interpretation

### DATA MGMT
# read data
redcard <- read.csv(here('Data', 'CrowdstormingDataJuly1st.csv'), stringsAsFactors = FALSE)

# Remove NA values
redcard <- na.omit(redcard)

# Take average of rater scores for player skin tone
redcard$avrate <- redcard$rater1 + ((redcard$rater2 - redcard$rater1) / 2)

### Logit MLM

# 1. DV (redCards) needs to be restructured as dichotomous:
summary(as.factor(redcard$redCards))

# replacing the value of 2 with 1
redcard["redCards"][redcard["redCards"] == 2] <- 1
summary(as.factor(redcard$redCards))

#record output
#sink(here("output.txt"))

# null GLM logit model
mod0 <- glm(redCards ~ 1,
            family = binomial(link="logit"),
            data = redcard)
summary(mod0)

# random intercept for players
mod1 <- glmer(redCards ~ 1 + (1 | playerShort),
              data = redcard,
              family = binomial(link="logit"),
              control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 0)
summary(mod1)
get_variance(mod1, component = "all")

# random intercept for players & referees
mod1.1 <- glmer(redCards ~ 1 + (1 | playerShort) + (1 | refNum),
              data = redcard,
              family = binomial(link="logit"),
              control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 0)
summary(mod1.1)
get_variance(mod1.1, component = "all")

# compare models
anova(mod1, mod1.1)

mod2.1 <- glmer(redCards ~  avrate + (1 | playerShort) 
              + (1 | refNum),
                data = redcard,
                family = binomial(link="logit"),
                control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 0)
summary(mod2.1)
get_variance(mod2.1, component = "all")

# random intercept for players and referees
# fixed effect of avg skin rating and player position
# NOTE: Including fixed factors using as.factor() does not change the output.
mod2.2 <- glmer(redCards ~  avrate + position + (1 | playerShort) 
                + (1 | refNum),
                data = redcard,
                family = binomial(link="logit"),
                control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 0)
summary(mod2.2)
get_variance(mod2.2, component = "all")

# compare models
anova(mod1, mod1.1, mod2.1, mod2.2)