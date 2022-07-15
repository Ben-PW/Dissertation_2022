### SETUP

require(here)
require(lme4) #glmer for MLMs
# read https://cran.r-project.org/web/packages/lme4/lme4.pdf for interpretation.
require(lmerTest) # sig. values for glmer
require(MuMIn)
# read https://cran.r-project.org/web/packages/MuMIn/MuMIn.pdf for interpretation

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
r.squaredGLMM(mod1)
# random intercept for players & referees
mod1.1 <- glmer(redCards ~ 1 + (1 | playerShort) + (1 | refNum),
              data = redcard,
              family = binomial(link="logit"),
              control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 0)
summary(mod1.1)
r.squaredGLMM(mod1.1)
# compare models
anova(mod1, mod1.1)

# random intercept for players, referees, & league country
#mod1x <- glmer(redCards ~ 1 + (1 | playerShort) + (1 | refNum)
#                + (1 | leagueCountry),
#               data = redcard,
#                family = binomial(link="logit"),
#                control = glmerControl(optimizer = "bobyqa"),
#                nAGQ = 0)
#summary(mod1x)
# no additional variance explained by league country

# random intercept for players, referees, & league country
# fixed effects of player position and avg skin rating
mod2 <- glmer(redCards ~ position + avrate + (1 | playerShort) 
              + (1 | refNum),
                data = redcard,
                family = binomial(link="logit"),
                control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 0)
summary(mod2)
r.squaredGLMM(mod2)
# compare models
anova(mod1, mod1.1, mod2)

mod2x <- glmer(redCards ~ as.factor(position) + avrate 
               + (1 | playerShort) + (1 | refNum),
              data = redcard,
              family = binomial(link="logit"),
              control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 0)
summary(mod2x)
r.squaredGLMM(mod2x)
#as.factor approach gives different estimates - why?

anova(mod1, mod1.1, mod2, mod2x)

# random intercept for players & referees
# random slope for players over referees
# fixed effects of player position
mod3 <- glmer(redCards ~ position + avrate +  (1 | playerShort) 
              + (1 | refNum) + (0 + playerShort | refNum),
               data = redcard,
               family = binomial(link="logit"),
               control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 0)
# I do not have the computational power to do this:
# "cannot allocate vector of size 10.9 Gb"
#sink()
summary(mod3)
r.squaredGLMM(mod3)