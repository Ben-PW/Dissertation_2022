################################## Ondrej's plot 1: avrate ORs per model where DV: redCards

### LIBRARIES ###
library(ggplot2)
library(gridExtra)
library(dplyr)

### VISUALISATION ###
df.or.plot <- data.frame( spec.number = c(outtable1$rownumber, NA),
                          OR = c(outtable1$Avrate_OR, NA),
                          Low = c(outtable1$Avrate_LCI, NA),
                          Hi = c(outtable1$Avrate_UCI, NA))

df.or.plot$spec.number <- as.integer(df.or.plot$spec.number)
# Arrange the df according to OR size
df.or.plot <- arrange(df.or.plot, OR)

or.plot <- ggplot(df.or.plot, aes(OR, spec.number)) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(xmax = Hi, xmin = Low), height = 0.2) + # error bars
  #geom_vline(xintercept = mean(df.or.plot$OR, na.rm = TRUE), linetype = "longdash") + # showing the OR of 1
  #scale_x_discrete(breaks = df.or.plot$spec.number, 
            #       labels = df.or.plot$spec.number) + 
  labs(x = "Odds Ratio", y = "Model Specification Number") + # axis labels
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        # axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), # connecting lines
        plot.margin = unit(c(0,0,0,0), "lines"))

# Default visualisation
or.plot

# Rotated visualisation
or.plot + coord_flip()


### TO DO ###
# Resolve the issue with scale_x_discrete not showing spec numbers
# Decide where to position the intersection - OR 1 or mean OR?

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
  geom_point(size = 2) + 
  geom_errorbar(aes(xmax = Hi, xmin = Low), height = 0.2) + # error bars
  #geom_vline(xintercept = mean(df.or.plot$OR, na.rm = TRUE), linetype = "longdash") + # showing the OR of 1
  #scale_x_discrete(breaks = df.or.plot$spec.number, 
  #       labels = df.or.plot$spec.number) + 
  labs(x = "Odds Ratio", y = "Model Specification Number") + # axis labels
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        # axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), # connecting lines
        plot.margin = unit(c(0,0,0,0), "lines"))

# Default visualisation
or.plot2

# Rotated visualisation
or.plot2 + coord_flip()