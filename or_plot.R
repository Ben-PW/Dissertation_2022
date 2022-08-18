################################## Ondrej's plot 1: avrate ORs per model where DV: redCards

### LIBRARIES ###
library(ggplot2)
library(gridExtra)
library(dplyr)

### OBTAINING AVRATE MODELS ###
#Subset MVA1 to obtain models containing avrate
avrate_set<-subset(covariates_list, (!is.na(covariates_list[,1])))
output_avrate <- subset(outtable1, rownumber %in% avrate_set$rownumber)

# Sort rows based on the indexed row number
output_avrate <- output_avrate[order(output_avrate$rownumber),]

# Re-index variable for the avrate subset
output_avrate$avrate.row <- 1:nrow(output_avrate)

### VISUALISATION ###
df.or.plot <- data.frame(spec.number = c(output_avrate$avrate.row, NA),
                          OR = c(output_avrate$Avrate_OR, NA),
                          Low = c(output_avrate$Avrate_LCI, NA),
                          Hi = c(output_avrate$Avrate_UCI, NA))

df.or.plot$spec.number <- as.integer(df.or.plot$spec.number)
# Arrange the df according to OR size
#df.or.plot <- arrange(df.or.plot, OR)

or.plot <- ggplot(df.or.plot, aes(OR, spec.number)) + 
  geom_errorbar(aes(xmax = Hi, xmin = Low)) + # error bars
  geom_vline(xintercept = 1, linetype = "longdash") + # intercept of 1
  geom_point(size = 1.5, col = ifelse(df.or.plot$OR < 1, 'red','blue')) +
  labs(x = "Odds Ratio", y = "Specification N",
       caption ='Note: 95% confidence intervals are displayed.') + # axis labels
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
        #plot.margin = unit(c(0,0,0,0), "lines"))


# Default visualisation
or.plot

# Rotated visualisation
or.plot + coord_flip(xlim = c(0.81,1.75), ylim = c(1, 512))


################################## Ondrej's plot 2: avrate ORs per model where DV: yellowReds

### LIBRARIES ###
library(ggplot2)
library(gridExtra)
library(dplyr)

### VISUALISATION ###
df.or.plot2 <- data.frame(spec.number = c(outtable2$rownumber, NA),
                          OR = c(outtable2$Avrate_OR, NA),
                          Low = c(outtable2$Avrate_LCI, NA),
                          Hi = c(outtable2$Avrate_UCI, NA))

df.or.plot2$spec.number <- as.integer(df.or.plot2$spec.number)
# Arrange the df according to OR size
df.or.plot2 <- arrange(df.or.plot2, OR)

or.plot2 <- ggplot(df.or.plot2, aes(OR, spec.number)) + 
  geom_errorbar(aes(xmax = Hi, xmin = Low)) + # error bars
  geom_vline(xintercept = 1, linetype = "longdash") + # intercept of 1
  geom_point(size = 1.5, col = ifelse(df.or.plot2$OR < 1, 'red','blue')) +
  labs(x = "Odds Ratio", y = "Specification N",
       caption ='Note: 95% confidence intervals are displayed.') + # axis labels
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(0,0,0,0), "lines"))

# Default visualisation
or.plot2

# Rotated visualisation
or.plot2 + coord_flip(xlim = c(0.82,1.31), ylim = c(1, 256))
