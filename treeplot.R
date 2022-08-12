################################## Ondrej's plot 1: 'Tree plot' of ORs per covariate model
library(ggplot2)
library(gridExtra)
library(dplyr)

################################################################################

# Create tree plot

df.treeplot <- data.frame(Covariates = factor(c(outtable1$covariates.formula, 0), 
                                              levels = c(outtable1$covariates.formula, 0)),
                          Model_number = c(outtable1$rownumber, NA),
                         OR = c(outtable1$Avrate_OR, NA),
                         Low = c(outtable1$Avrate_LCI, NA),
                         Hi = c(outtable1$Avrate_UCI, NA))

df.treeplot <- arrange(df.treeplot, OR)

df.treeplot$Covariates <- factor(df.treeplot$Covariates,
                          levels = c(rev(as.numeric(as.character(df.treeplot$Covariates))), 0)) #not sure what this does

# Temporarily remove NA values - until the analysis is ran completely.
# Will disrupt the subsequent steps
#df.treeplot <- na.omit(df.treeplot)

p <- ggplot(df.treeplot, aes(OR, Covariates)) + 
  geom_point(size = 3) +
  geom_errorbarh(aes(xmax = Hi, xmin = Low), height = 0.5) +
  geom_vline(xintercept = 1, linetype = "longdash") +
  scale_x_continuous(breaks = seq(-3, 6, 1), labels = seq(-3,6,1)) +
  geom_text(x = 5.1, y = 1, label = "*", size = 6) +
  geom_text(x = 5.1, y = 2, label = "*", size = 6) + 
  labs(x = "Odds Ratio", y = "") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0,0,0,0), "lines"))

p 
#rotate graph
# p + coord_flip() 

# Create label table

lab <- data.frame(V0 = df.treeplot$Model_number,
                  V05 = rep(c(1.9, 2, 2.5), each = nrow(df.treeplot)),
                  V1 = c(as.character(df.treeplot$Model_number)[-30], "Model number",
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