################################## Ondrej's plot 1: 'Tree plot' of ORs per covariate model
library(ggplot2)
library(gridExtra)
library(dplyr)

################################################################################

# Create tree plot

df.treeplot <- data.frame(spec.number = c(outtable1$rownumber, NA),
                         OR = c(outtable1$Avrate_OR, NA),
                         Low = c(outtable1$Avrate_LCI, NA),
                         Hi = c(outtable1$Avrate_UCI, NA))

# Arrange ORs according to their spec. number
df.treeplot <- arrange(df.treeplot, spec.number)

# Arrange ORs according to their size; I think we are more interested in spec. number?
#df.treeplot <- arrange(df.treeplot, OR)

or.plot <- ggplot(df.treeplot, aes(OR, spec.number)) + 
  geom_point(size = 2) + 
  geom_errorbarh(aes(xmax = Hi, xmin = Low), height = 0.5) + # error bars
  geom_vline(xintercept = 1, linetype = "longdash") + # showing the OR of 1
  scale_x_continuous(breaks = seq(-3,6, 1), labels = seq(-3,6,1)) +
# spacing the x axis (start, stop, step)
  labs(x = "Skintone Odds Ratio", y = "Specification Number") + # axis labels
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
#        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), # connecting lines
        plot.margin = unit(c(0,0,0,0), "lines"))

# Default visualisation
or.plot 

# Rotated visualisation
or.plot + coord_flip() 

# Create label table

lab <- data.frame(V0 = df.treeplot$spec.number,
                  V05 = rep(c(1.9, 2, 2.5), each = nrow(df.treeplot)),
                  V1 = c(as.character(df.treeplot$spec.number)[-30], "Model number",
                         as.character(df.treeplot$Covariates)[-30], "Covariate composition",
                         format(round(df.treeplot$OR[-30], 2), nsmall = 2), "Skin tone rating OR"))

treeplot <- ggplot(lab, aes(x = V05, y = V0, label = format(V1, nsmall = 1))) +
  geom_text(size = 4, hjust = 0, vjust = 0.5) + theme_bw() +
  geom_hline(aes(yintercept = c(29.5))) + geom_hline(aes(yintercept = c(30.5))) + 
  theme(panel.grid.major = element_blank(), 
        legend.position = "none",
        panel.border = element_blank(), 
        axis.text.x = element_text(colour="white"),
        axis.text.y = element_blank(), 
        axis.ticks = element_line(colour="white"),
        plot.margin = unit(c(0,0,0,0), "lines")) +
  labs(x = "",y = "") +
  coord_cartesian(xlim = c(1.9, 2.6))

treeplot

#rotate graph
#treeplot + coord_flip()