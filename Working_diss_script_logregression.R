############################################# Data import ####################################
library(here)
library(tidyverse)

# Read the 'contaminated' df
redcardCont <- read.csv(here('Data', 'CrowdstormingDataJuly1st.csv'), stringsAsFactors = FALSE)

# Import refs identified as brought in from players' previous game history
contRefs <- read.csv(here('Data', 'decontRefs.csv')) %>% select(refNum)

# Exclude the selected refs
redcard <- redcardCont[!(redcardCont$refNum %in% contRefs$refNum), ]

# Remove now arbitrary dfs
rm(redcardCont, contRefs)


############################################# Data transformations ####################################

# Remove NA values
redcard <- na.omit(redcard)

# Create identifying variable for data screening
redcard$rownumber <- 1:nrow(redcard)

# Take average of rater scores for player skin tone
redcard$avrate <- redcard$rater1 + ((redcard$rater2 - redcard$rater1) / 2)

# Collapsing data from 'redCards' into a dichotomous variable:
redcard["redCards"][redcard["redCards"] == 2] <- 1
summary(as.factor(redcard$redCards))

# Collapsing IV levels >= 2 into binary values (0,1):
#redcard["yellowCards"][redcard["yellowCards"] >= 2] <- 1
#summary(as.factor(redcard$yellowCards))

#redcard["yellowReds"][redcard["yellowReds"] >= 2] <- 1
#summary(as.factor(redcard$yellowReds))

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
             "Front" = "Center Forward",
             "Goalkeeper" = "Goalkeeper")

# Getting a warming: Unknown levels in `f`: Center Fullback


# Specifying 'Back' as reference category
#### NB for some reason doing this makes the log regression angry
#redcard$position <- 
#  fct_relevel(redcard$position, 
#              "Back")

redcard$position <- as.factor(redcard$position)
redcard$refCountry <- as.factor(redcard$refCountry)

# 4. Removing illogical values from yellowCards and yellowReds (undone as we are no longer coding yellow)
# cards as a binary categorical predictor)
#redcard$yellowCards <- ifelse(redcard$yellowCards > 1, 1, redcard$yellowCards)
#redcard$yellowCards <- as.factor(redcard$yellowCards)

#redcard$yellowReds <- ifelse(redcard$yellowReds > 1, 1, redcard$yellowReds)
#redcard$yellowReds <- as.factor(redcard$yellowReds)



######################################### Creating Covariate Multiverse ################################


library(tidyverse)

# Create list of potential covariates

covariates_list <- list(avrate = c(NA, 'avrate'),
                        position = c(NA, 'position'),
                        yellowCards = c(NA, 'yellowCards'),
                        height = c(NA, 'height'),
                        weight = c(NA, 'weight'),
                        club = c(NA, 'club'),
                        #goals = c(NA, 'goals'), #collider - taken out
                        age = c(NA, 'age'),
                        meanIAT = c(NA, 'meanIAT'),
                        #meanEXP = c(NA, 'meanExp'),
                        #games = c(NA, 'games'), #collider - taken out
                        refCountry = c(NA, 'refCountry'),
                        victories = c(NA, 'victories'))

############# Create list of all possible combinations


# Making a grid combining the NA and other values. T
covariates_list <- expand.grid(covariates_list) 

covariates_list <- covariates_list[-1,]

# re-index covariates_list after removing first row
row.names(covariates_list) <- 1:nrow(covariates_list)

# create new grouping variable
covariates_list$rownumber <- row.names(covariates_list)

# all covariates moved to a single column, each separated by a '+' sign
covariate_grid <- covariates_list %>%
  tidyr::unite(formula, avrate:victories, sep = '+', na.rm = TRUE)



######################################### Main multiverse loop ##########################################



# Define new variable 'output' as a list to store multiple outputs
output <- list()


# Defining a new variables - NA for now as they will be filled once the loop is run
R2conditional <- NA
R2marginal <- NA
predictorR2 <- NA
predictorPval <- NA
or.avrate <- NA
or.lci <- NA
or.uci <- NA

require(lme4)
require(lmerTest)
require(tictoc)
require(R.oo)
require(broom.mixed)

# Begin loop
for(i in 1:nrow(covariate_grid)) {
  # printing [i] just to track progress of analysis
  print(i)
  
  skip_to_next <- FALSE
  
  # see how long full loop is taking
  tic("Total")
  
  # see how long regression is taking
  tic("Regression")
  
     # each row of covariate_grid is now used as a formula for the regression
     output <- tryCatch(glmer(data = redcard,
                    formula = paste('redCards ~ ',
                                    covariate_grid[i, 'formula'], 
                                    '+ (1 | playerShort) + (1 | refNum)'),
                    family = binomial(link="logit"),
                    control = glmerControl(optimizer = "bobyqa"),
                    nAGQ = 0),
                    
                    error = function(e) {
                      skip_to_next <<- TRUE
                    })
       
     if(skip_to_next) { next }
                    
     
  toc()
    
  # see how long data extraction is taking
  tic("Data extraction")
  
  
     # Getting overall model fit for each row of covariate_grid
     R2conditional[i] <- modelsummary::get_gof(output)$r2.conditional 
     # Getting marginal R2 for each row
     R2marginal[i] <- modelsummary::get_gof(output)$r2.marginal 
     # Getting individual predictor R2 for each row of covariate_grid
     predictorR2[i] <- as.data.frame(summary(output)$coefficients[,1])
     # Getting p values for individual predictors
     predictorPval[i] <- as.data.frame(summary(output)$coefficients[,4])
     or.avrate[i] <- as.numeric(tidy(output,conf.int=TRUE,exponentiate=TRUE,effects="fixed")[2,3])
     # Getting OR LCI
     or.lci[i] <- as.numeric(tidy(output,conf.int=TRUE,exponentiate=TRUE,effects="fixed")[2,7])
     # Getting OR UCI
     or.uci[i] <- as.numeric(tidy(output,conf.int=TRUE,exponentiate=TRUE,effects="fixed")[2,8])

  
  toc()
  toc()
}


############################################ Creating output dataframe ##########################################


############ Turning list of predictor R2 and P values into a data frame

# find length of each element of predictor_R2 list
len <- sapply(predictorR2, length)
len2 <- sapply(predictorPval, length)

# longest length dictates number of rows in data frame
n <- max(len)
n2 <- max(len2)

# finds number of NAs required for each row to be of same length to longest
len <- n - len
len2 <- n2 - len2

#predR2_df <- data.frame(mapply(function(x,y) c( x , rep( NA , y )), predictorR2, len))
# above line does similar to below but long format

# magically creates a data frame don't ask me how
predR2_df <- data.frame(t(mapply(function(x,y) c(x, rep(NA, y)), predictorR2, len)))
predPval_df <- data.frame(t(mapply(function(x,y) c(x, rep(NA, y)), predictorPval, len2)))

rm(len, len2, n, n2)

# select only relevant columns
predR2_df<- subset(predR2_df, select = X2)
predPval_df <- subset(predPval_df, select = X2)

# add variable to merge data frames by
predR2_df$rownumber <- row.names(predR2_df)
predPval_df$rownumber <- row.names(predPval_df)

# rename column names for interpretability
names(predR2_df)[1]<-paste("R2_first_covariate")
names(predPval_df)[1]<-paste("Pval_first_covariate")

############ Turning conditional R2 values into data frame

# Pads R2 and OR values with NA values to avoid errors in code below if whole MVA isn't performed
length(R2conditional) <- nrow(covariate_grid)
length(R2marginal) <- nrow(covariate_grid)
length(or.avrate) <- nrow(covariate_grid)
length(or.lci) <- nrow(covariate_grid)
length(or.uci) <- nrow(covariate_grid)

output_table <- data.frame(covariates = covariate_grid,
                             R2c = R2conditional,
                             R2m = R2marginal,
                           Avrate_OR = or.avrate,
                           Avrate_LCI = or.lci,
                           Avrate_UCI = or.uci)

output_table$rownumber <- row.names(output_table)

########## Combining into single data frame

output_table <- merge(output_table, predR2_df, by = "rownumber", all = TRUE)
output_table <- merge(output_table, predPval_df, by = "rownumber", all = TRUE)

# Set rownumber as numeric so dataframe can be sorted by this variable
output_table$rownumber <- as.numeric(output_table$rownumber)

# order and re-index data frame so rownames and rownumbers align
outtable1 <- output_table[order(output_table$rownumber),]
row.names(outtable1) <- 1:nrow(outtable1)

# Tidy up
rm(predictorPval, predPval_df, predictorR2, predR2_df, output_table, R2marginal, or.avrate, or.lci, or.uci)


############################################### Visualisation ##########################################


################################## Ben's plot 1: Average model fit per covariate

###### Subsetting outtable1 by matching rownames between covariates_list subsets and outtable1

#avrate
avrate_set<-subset(covariates_list, (!is.na(covariates_list[,1])))
output_avrate <- subset(outtable1, rownumber %in% avrate_set$rownumber)
#position
position_set<-subset(covariates_list, (!is.na(covariates_list[,2])))
output_position <- subset(outtable1, rownumber %in% position_set$rownumber)
#yellowCards
yellowCards_set<-subset(covariates_list, (!is.na(covariates_list[,3])))
output_yellowCards <- subset(outtable1, rownumber %in% yellowCards_set$rownumber)
#height
height_set<-subset(covariates_list, (!is.na(covariates_list[,4])))
output_height <- subset(outtable1, rownumber %in% height_set$rownumber)
#weight
weight_set<-subset(covariates_list, (!is.na(covariates_list[,5])))
output_weight <- subset(outtable1, rownumber %in% height_set$rownumber)
#club
club_set<-subset(covariates_list, (!is.na(covariates_list[,6])))
output_club <- subset(outtable1, rownumber %in% club_set$rownumber)
#goals
#goals_set<-subset(covariates_list, (!is.na(covariates_list[,7])))
#output_goals <- subset(outtable1, rownumber %in% goals_set$rownumber)
#age
age_set<-subset(covariates_list, (!is.na(covariates_list[,7])))
output_age <- subset(outtable1, rownumber %in% age_set$rownumber)
#meanIAT
meanIAT_set<-subset(covariates_list, (!is.na(covariates_list[,8])))
output_meanIAT <- subset(outtable1, rownumber %in% meanIAT_set$rownumber)
#meanEXP
#meanEXP_set<-subset(covariates_list, (!is.na(covariates_list[,8])))
#output_meanEXP <- subset(outtable1, rownumber %in% meanEXP_set$rownumber)
#games
#games_set<-subset(covariates_list, (!is.na(covariates_list[,11])))
#output_games <- subset(outtable1, rownumber %in% games_set$rownumber)
#refCountry
refCountry_set<-subset(covariates_list, (!is.na(covariates_list[,9])))
output_refCountry <- subset(outtable1, rownumber %in% refCountry_set$rownumber)
#victories
victories_set<-subset(covariates_list, (!is.na(covariates_list[,10])))
output_victories <- subset(outtable1, rownumber %in% victories_set$rownumber)

###################################### Ben's Plot 1: Categorical variables included

#create one dataframe of the filtered values to build the plot from
benplot1_df <- cbind(output_avrate$R2c, output_position$R2c, output_yellowCards$R2c, output_height$R2c,
                     output_weight$R2c, output_club$R2c, output_age$R2c,
                     output_meanIAT$R2c, output_refCountry$R2c,
                     output_victories$R2c)

benplot1_df <- as.data.frame(benplot1_df)

colnames(benplot1_df)[c(1,2,3,4,5,6,7,8,9,10)] <- c('Skin_Tone','Position','Yellow_Cards',
                                                             'Height','Weight','Club',
                                                             'Age','Mean_IAT',
                                                             'Ref_Country','Victories')
require(dplyr)
require(tidyr)

#data needs to be transformed to pass through ggplot
benplot1_df <- benplot1_df %>%
  pivot_longer(everything())

#create plot
require(ggplot2)
benplot1 <- ggplot(data=benplot1_df, mapping = aes(x = name, y = value)) + 
  geom_violin(scale = 'area', 
              width = 1.6,
              adjust = 0.1) +
  theme_bw()

benplot1 <- ggplot(data=benplot1_df, mapping = aes(x = name, y = value, colour = name)) +
  geom_dotplot(binaxis = "y", 
               binpositions = 'bygroup', 
               stackdir ="center",
               stackratio = 0.1,
               dotsize = 0.5,
               binwidth = 1/100,
               show.legend = FALSE) +
  labs(y = 'Overall model R2', x = 'Common model covariate') +
  theme_bw()
  
benplot1

########################################## End of Ben's plot 1: Categorical variables 

########################################## Ben's plot 2: Non cat variables

########################################## Ideas for OR plot

orplot <- as.data.frame(cbind(output_avrate$Avrate_OR, output_avrate$covariates.formula,
                              output_avrate$Avrate_LCI, output_avrate$Avrate_UCI,
                              output_avrate$Pval_first_covariate))

orplot <- orplot %>% mutate(signif = case_when(V5 > 0.05 ~ 'p > .05',
                                               V5 <= 0.05 ~ 'p <= .05'))


orplot <- orplot[order(orplot[,1]),]

orplot$V1 <- as.numeric(orplot$V1)
orplot$V3 <- as.numeric(orplot$V3)
orplot$V4 <- as.numeric(orplot$V4)
orplot$n <- 1:nrow(orplot)

groupcols <- c('red','blue')

plot_or <- ggplot(data = orplot, 
                    aes(x = n, y = V1, colour = signif)) +
  ylim(0.75, 1.75) +
  geom_errorbar(aes(ymin = V3, ymax = V4, alpha = 0.2)) +
  geom_point(show.legend = FALSE) +
  scale_fill_manual(values = groupcols) +
  labs(x = '')

plot_or

#################### Create OR plot without categorical variables
nocatset <- subset(covariates_list, (is.na(covariates_list[,c(2)])))
nocatset <- subset(nocatset, (is.na(nocatset[,c(6)])))
nocatset <- subset(nocatset, (is.na(nocatset[,c(8)])))
nocatset <- subset(nocatset, (is.na(nocatset[,c(9)])))
nocatset <- subset(nocatset,(!is.na(nocatset[,c(1)])))

ornocatdf <- subset(outtable1, rownumber %in% nocatset$rownumber)

ornocatdf <- ornocatdf[order(ornocatdf[,6]),]
ornocatdf$n <- 1:nrow(ornocatdf)

plot_ornocat <- ggplot(data = ornocatdf, 
                              aes(x = n, y = Avrate_OR)) +
  ylim(0.75, 1.75) +
  geom_errorbar(aes(ymin = Avrate_LCI, 
                    ymax = Avrate_UCI, alpha = 0.2, colour = 'red')) +
  geom_point(show.legend = FALSE) +
  labs(x = '')

plot_ornocat

######################################### End of OR plot ideas

############################################## Potential alternative final plot
nocatset<-subset(covariates_list, (is.na(covariates_list[,c(2)])))
nocatset<-subset(nocatset, (is.na(nocatset[,c(6)])))
nocatset<-subset(nocatset, (is.na(nocatset[,c(8)])))
nocatset<-subset(nocatset, (is.na(nocatset[,c(9)])))

nocatdf <- subset(outtable1, rownumber %in% nocatset$rownumber)
nocatdf <- nocatdf[order(nocatdf[,4]),]
nocatdf$n <- 1:nrow(nocatdf)

group <- c(1,1,1,1,1,1,1,1,
           2,2,2,2,2,2,2,2,
           3,3,3,3,3,3,3,3,
           4,4,4,4,4,4,4,4,
           5,5,5,5,5,5,5,5,
           6,6,6,6,6,6,6,6,
           7,7,7,7,7,7,7,8,
           8,8,8,8,8,8,8)

nocatdf$group <- group

nocatdf["group"][nocatdf["group"] == 1] <- "YellowCards"
nocatdf["group"][nocatdf["group"] == 2] <- "YellowCards + Age"
nocatdf["group"][nocatdf["group"] == 3] <- "YellowCards + Victories"
nocatdf["group"][nocatdf["group"] == 4] <- "YellowCards + Age + Victories"
nocatdf["group"][nocatdf["group"] == 5] <- "Victories"
nocatdf["group"][nocatdf["group"] == 6] <- "Age + Victories"
nocatdf["group"][nocatdf["group"] == 7] <- "Avrate/Height/Weight"
nocatdf["group"][nocatdf["group"] == 8] <- "Age"

nocatdf$group <- factor(nocatdf$group, levels = c("Age","Avrate/Height/Weight","Age + Victories", "Victories","YellowCards + Age + Victories",
                                                  "YellowCards + Victories","YellowCards + Age","YellowCards" 
                                                    
                                                  ))

bigplot <- ggplot(data = nocatdf,
                  aes(x = n, y = R2c, colour = group,
                      show.legend = TRUE)) +
  ylim(0, 0.5) +
  geom_point()
bigplot

################################################ End of potential alternative plot

plot <- cbind(covariate_grid$formula, outtable1$R2c)

# Order results of plot by R2 value
plot2 <- plot[order(plot[,2]), ]
rm(plot)

plot2[,2] <- as.numeric(plot2[,2])

plot2 <- as.data.frame(plot2)

# Creating a grouping variable (n) for each row of covariate_grid
plot2$n <- 1:nrow(plot2)

plot2$V2 <- as.numeric(plot2$V2)

# Creating final plot
plotfinal <- ggplot(data = plot2, 
                    aes(x = n, y = V2)) +
  geom_point() +
  labs(x = '')

# Creating dashboard to go underneath plot
dashboard <- plot2 %>% 
  gather(Bigdecision, Decision, -V2, -n) %>%
  filter(Decision != 'NA')

rm(plot2)

# Creating levels in Bigdecision variable that correspond to data in covariate_grid rows

dashboard$Bigdecision <- factor(dashboard$Bigdecision, 
                                levels = names(covariate_grid))


dashboardfinal <- ggplot(data = dashboard,
                         aes(x = n, y = Decision)) +
  facet_grid(Bigdecision ~ ., scales = "free", space = "free", drop = ) +
  geom_point(shape = 108, size = 1) +
  labs(x = 'specification number') +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        strip.background = element_blank())

library(patchwork)
plotfinal
dashboardfinal
plotfinal / dashboardfinal


#############################################                   ########################################
############################################# Second Multiverse ########################################
#############################################                   ########################################


############################################# Data re-import ####################################
library(here)
library(tidyverse)

# Read the 'contaminated' df
redcardCont <- read.csv(here('Data', 'CrowdstormingDataJuly1st.csv'), stringsAsFactors = FALSE)

# Import refs identified as brought in from players' previous game history
contRefs <- read.csv(here('Data', 'decontRefs.csv')) %>% select(refNum)

# Exclude the selected refs
redcard <- redcardCont[!(redcardCont$refNum %in% contRefs$refNum), ]

# Remove now arbitrary dfs
rm(redcardCont, contRefs)

# Remove NA values
redcard <- na.omit(redcard)

# Create identifying variable for data screening
redcard$rownumber <- 1:nrow(redcard)

########################################### Transformations ##########################################


library(tidyverse)

# Take average of rater scores for player skin tone
redcard$avrate <- redcard$rater1 + ((redcard$rater2 - redcard$rater1) / 2)

# refCountry recoded as a factor 
redcard$refCountry <- as.factor(redcard$refCountry)

# calculating age variable
redcard$birthday <- as.Date(redcard$birthday, '%d.%m.%Y')
season_date <- as.Date('2013-01-01')
redcard$age <- as.numeric((season_date-redcard$birthday)/365)
rm(season_date)

# Creating an alternative DV which covers all combinations of possible penalisations
redcard <- redcard %>% mutate(allcards = yellowCards + yellowReds + redCards)
redcard["allcards"][redcard["allcards"] > 1] <- 1
summary(as.factor(redcard$allcards))

# Collapse position variable in same manner to team 28 (hopefully reduce vector length)
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
             "Front" = "Center Forward",
             "Goalkeeper" = "Goalkeeper")

library(tidyverse)
# Create list of potential covariates 
covariates_list_2 <- list(position = c(NA, 'position'),
                        height = c(NA, 'height'),
                        weight = c(NA, 'weight'),
                        club = c(NA, 'club'),
                        #goals = c(NA, 'goals'),
                        age = c(NA, 'age'),
                        meanIAT = c(NA, 'meanIAT'),
                        #meanEXP = c(NA, 'meanExp'),
                        #games = c(NA, 'games'),
                        refCountry = c(NA, 'refCountry'),
                        victories = c(NA, 'victories'))

############# Create list of all possible combinations

# Functions same as loop 1, yellowcards not included as they are part of DV
covariates_list_2 <- expand.grid(covariates_list_2) 


covariate_grid_2 <- covariates_list_2 %>%
  tidyr::unite(formula, position:victories, sep = '+', na.rm = TRUE)



######################################### Second multiverse loop ##########################################


# Define new variable 'output' as a list to store multiple outputs
output_2 <- list()


# Defining a new variables - NA for now as they will be filled once the loop is run
R2conditional_2 <- NA
predictorR2_2 <- NA
R2marginal_2 <- NA
predictorPval_2 <- NA
or.avrate_2 <- NA
or.lci_2 <- NA
or.uci_2 <- NA

# Begin loop
for(i in 1:nrow(covariate_grid_2)) {
  # printing [i] just to track progress of analysis
  print(i)
  
  skip_to_next <- FALSE
  
  # see how long full loop is taking
  tic("Total")
  
  # see how long regression is taking
  tic("Regression")
  
  # each row of covariate_grid is now used as a formula for the regression
  output_2 <- tryCatch(glmer(data = redcard,
                  formula = paste('allcards ~ avrate +',
                                  covariate_grid_2[i, 'formula'], 
                                  '+ (1 | playerShort) + (1 | refNum)'),
                  family = binomial(link="logit"),
                  control = glmerControl(optimizer = "bobyqa"),
                  nAGQ = 0),
                  
                  error = function(e) {
                    skip_to_next <<- TRUE
                  })
  
  if(skip_to_next) { next }
  
  toc()
  
  # see how long data extraction is taking
  tic("Data extraction")
  
  # Getting overall model fit for each row of covariate_grid
  R2conditional_2[i] <- modelsummary::get_gof(output_2)$r2.conditional
  
  # Getting marginal R2 for each row
  R2marginal_2[i] <- modelsummary::get_gof(output_2)$r2.marginal
  
  # Getting individual predictor R2 for each row of covariate_grid
  predictorR2_2[i] <- as.data.frame(summary(output_2)$coefficients[,1])
  
  # Getting p values for individual predictors (check potential extraction issues)
  predictorPval_2[i] <- as.data.frame(summary(output_2)$coefficients[,4])
  
  # Getting ORs for avrate as a covariate in each model
  or.avrate_2[i] <- as.numeric(tidy(output_2,conf.int=TRUE,exponentiate=TRUE,effects="fixed")[2,3])
  
  # Getting OR LCI
  or.lci_2[i] <- as.numeric(tidy(output_2,conf.int=TRUE,exponentiate=TRUE,effects="fixed")[2,7])
  
  # Getting OR UCI
  or.uci_2[i] <- as.numeric(tidy(output_2,conf.int=TRUE,exponentiate=TRUE,effects="fixed")[2,8])
  
  toc()
  toc()
}

############################################# Creating Data Frames ############################################


############ Turning list of predictor R2 and P values into a data frame
# find length of each element of predictor_R2 list
len <- sapply(predictorR2_2, length)
len2 <- sapply(predictorPval_2, length)

# longest length dictates number of rows in data frame
n <- max(len)
n2 <- max(len2)

# finds number of NAs required for each row to be of same length to longest
len <- n - len
len2 <- n2 - len2

# mapply(function(x,y) c( x , rep( NA , y )), predictorR2, len)
# above line does similar to below but long format

# magically creates a data frame don't ask me how
predR2_df_2 <- data.frame(t(mapply(function(x,y) c(x, rep(NA, y)), predictorR2_2, len)))
predPval_df_2 <- data.frame(t(mapply(function(x,y) c(x, rep(NA, y)), predictorPval_2, len2)))

rm(len, len2, n, n2)
# select only relevant columns
predR2_df_2 <- subset(predR2_df_2, select = X2)
predPval_df_2 <- subset(predPval_df_2, select = X2)

# add variable to merge data frames by
predR2_df_2$rownumber <- row.names(predR2_df_2)
predPval_df_2$rownumber <- row.names(predPval_df_2)

# rename column names for interpretability
names(predR2_df_2)[1]<-paste("R2_avrate")
names(predPval_df_2)[1]<-paste("Pval_avrate")

#################### Turning conditional R2 values into data frame

# Pads R2conditional with NA values to avoid errors in code below if whole MVA isn't performed
length(R2conditional_2) <- nrow(covariate_grid_2)
length(R2marginal_2) <- nrow(covariate_grid_2)
length(or.avrate_2) <- nrow(covariate_grid_2)
length(or.lci_2) <- nrow(covariate_grid_2)
length(or.uci_2) <- nrow(covariate_grid_2)

output_table_2 <- data.frame(covariates = covariate_grid_2,
                           R2c = R2conditional_2,
                           R2m = R2marginal_2,
                           Avrate_OR = or.avrate_2,
                           Avrate_LCI = or.lci_2,
                           Avrate_UCI = or.uci_2)

output_table_2$rownumber <- row.names(output_table_2)

########## Combining into single data frame

output_table_2 <- merge(output_table_2, predR2_df_2, by = "rownumber", all = TRUE)
output_table_2 <- merge(output_table_2, predPval_df_2, by = "rownumber", all = TRUE)
output_table_2$rownumber <- as.numeric(output_table_2$rownumber)

# editing formula column for interpretability
output_table_2$formula <- paste("avrate", output_table_2$formula, sep="+")

# order and re-index data frame so rownames and rownumbers align
outtable2 <- output_table_2[order(output_table_2$rownumber),]
row.names(outtable2) <- 1:nrow(outtable2)

# add odds ratio ca

# Tidy up
rm(predictorPval_2, predPval_df_2, predictorR2_2, predR2_df_2, output_table_2, R2marginal_2, output_2)

######################################### Creating plot of results #####################################

########################################## Ideas for OR plot 2

# extract OR and confidence intervals from outtable2
orplot2 <- as.data.frame(cbind(outtable2$Avrate_OR, outtable2$Avrate_LCI, 
                              outtable2$Avrate_UCI, outtable2$Pval_avrate))

orplot2 <- orplot2 %>% mutate(signif = case_when(V4 > 0.05 ~ 'p > .05',
                                               V4 <= 0.05 ~ 'p <= .05'))
#order data by OR size
orplot2 <- orplot2[order(orplot2[,1]),]

#recode all as numeric and add grouping variable
orplot2$V1 <- as.numeric(orplot2$V1)
orplot2$V3 <- as.numeric(orplot2$V3)
orplot2$n <- 1:nrow(orplot2)

groupcols <- c('red','blue')

#plot OR on y axis and grouping variable (meaningless) on x
plot_or2 <- ggplot(data = orplot2, 
                  aes(x = n, y = V1, colour = signif)) +
  geom_errorbar(aes(ymin = V2, ymax = V3, alpha = 0.2)) +
  geom_point(show.legend = FALSE) +
  ylim(0.75, 1.75) +
  scale_fill_manual(values = groupcols, drop = FALSE) +
  labs(x = '')

plot_or2

#combine with plot from MVA 1
library(patchwork)
plot_or/#plot_ornocat/
  plot_or2

#flip 90 degrees for ease of comparison with main study
plot_orf <- plot_or + coord_flip()
plot_or2f <- plot_or2 + coord_flip()
plot_ornocatf <- plot_ornocat + coord_flip()

plot_orf/plot_ornocatf/plot_or2f

bigplot <- ggplot(data = outtable1,
                  aes(x = rownumber, y = R2c)) +
  geom_point()
bigplot
######################################### End of OR plot ideas
 
plot_2 <- cbind(covariate_grid_2, R2conditional_2) 

# Order results of plot by R2 value
plot2_2 <- plot[order(plot$R2conditional_2), ]
rm(plot)

# Creating a grouping variable (n) for each row of covariate_grid
plot2_2$n <- 1:nrow(plot2_2)

# Creating final plot
plotfinal_2 <- ggplot(data = plot2_2, 
                    aes(x = n, y = R2conditional_2)) +
  geom_point() +
  labs(x = '')

# Creating dashboard to go underneath plot
dashboard_2 <- plot2_2 %>% 
  gather(Bigdecision, Decision, -R2conditional_2, -n) %>%
  filter(Decision != 'NA')

rm(plot2_2)

# Creating levels in Bigdecision variable that correspond to data in covariate_grid rows

dashboard_2$Bigdecision <- factor(dashboard$Bigdecision, 
                                levels = names(covariate_grid_2))


dashboardfinal_2 <- ggplot(data = dashboard_2,
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
plotfinal_2
plotfinal_2 / dashboardfinal_2
