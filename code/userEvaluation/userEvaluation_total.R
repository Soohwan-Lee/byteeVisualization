library(ggplot2)
library(plyr)
library(dplyr)

# ---------------- #
# RED:   "#F8766D" #
# *BLUE: "#01BFC4" #
# GREEN: "#00ba38" #
# BLUE:  "#619cff" #
# ---------------- #


### Set File Path for Window Environment
setwd('C:/Users/LeeSooHwan/Desktop/github/byteeVisualization')
### Set File Path for Mac Environment
setwd("/Users/Soohwan/Desktop/github/byteeVisualization")


### Load Data
# Load
totalAverage <- read.csv(file = "./data/userEvaluation/revisedData/totalAverage.csv", header=T, fileEncoding="UTF-8-BOM")

# set Factors
totalAverage$participant <- as.factor(totalAverage$participant)
totalAverage$group <- as.factor(totalAverage$group)
totalAverage$measure <- as.factor(totalAverage$measure)
totalAverage$period <- as.factor(totalAverage$period)
totalAverage$score <- as.numeric(totalAverage$score)


### Box Plot Preparation
# Start Y axis from n
require(scales)
my_trans <- function(from=0) 
{
  trans <- function(x) x-from
  inv <- function(x) x+from
  trans_new("myscale", trans, inv, 
            domain = c(from, Inf))
}

# Elicit each measure from main dataframe
totalAverageA <- subset(totalAverage, measure == 'A')
totalAverageR <- subset(totalAverage, measure == 'R')
totalAverageC <- subset(totalAverage, measure == 'C')
totalAverageS <- subset(totalAverage, measure == 'S')

# Set Factors
totalAverageA$participant <- as.factor(totalAverageA$participant)
totalAverageA$group <- as.factor(totalAverageA$group)
totalAverageA$measure <- as.factor(totalAverageA$measure)
totalAverageA$period <- as.factor(totalAverageA$period)
totalAverageA$score <- as.numeric(totalAverageA$score)

totalAverageR$participant <- as.factor(totalAverageR$participant)
totalAverageR$group <- as.factor(totalAverageR$group)
totalAverageR$measure <- as.factor(totalAverageR$measure)
totalAverageR$period <- as.factor(totalAverageR$period)
totalAverageR$score <- as.numeric(totalAverageR$score)

totalAverageC$participant <- as.factor(totalAverageC$participant)
totalAverageC$group <- as.factor(totalAverageC$group)
totalAverageC$measure <- as.factor(totalAverageC$measure)
totalAverageC$period <- as.factor(totalAverageC$period)
totalAverageC$score <- as.numeric(totalAverageC$score)

totalAverageS$participant <- as.factor(totalAverageS$participant)
totalAverageS$group <- as.factor(totalAverageS$group)
totalAverageS$measure <- as.factor(totalAverageS$measure)
totalAverageS$period <- as.factor(totalAverageS$period)
totalAverageS$score <- as.numeric(totalAverageS$score)


### Normality Test (Shapiro-wilk)
# Test for A
shapiro.test(subset(totalAverageA, period == 'first' & group == 'control')$score)
shapiro.test(subset(totalAverageA, period == 'second' & group == 'control')$score)
shapiro.test(subset(totalAverageA, period == 'third' & group == 'control')$score)
shapiro.test(subset(totalAverageA, period == 'first' & group == 'experimental')$score)
shapiro.test(subset(totalAverageA, period == 'second' & group == 'experimental')$score)
shapiro.test(subset(totalAverageA, period == 'third' & group == 'experimental')$score)

# Test for R
shapiro.test(subset(totalAverageR, period == 'first' & group == 'control')$score)
shapiro.test(subset(totalAverageR, period == 'second' & group == 'control')$score)
shapiro.test(subset(totalAverageR, period == 'third' & group == 'control')$score)
shapiro.test(subset(totalAverageR, period == 'first' & group == 'experimental')$score)
shapiro.test(subset(totalAverageR, period == 'second' & group == 'experimental')$score)
shapiro.test(subset(totalAverageR, period == 'third' & group == 'experimental')$score)

# Test for C
shapiro.test(subset(totalAverageC, period == 'first' & group == 'control')$score)
shapiro.test(subset(totalAverageC, period == 'second' & group == 'control')$score)
shapiro.test(subset(totalAverageC, period == 'third' & group == 'control')$score)
shapiro.test(subset(totalAverageC, period == 'first' & group == 'experimental')$score)
shapiro.test(subset(totalAverageC, period == 'second' & group == 'experimental')$score)
shapiro.test(subset(totalAverageC, period == 'third' & group == 'experimental')$score)

# Test for S
shapiro.test(subset(totalAverageS, period == 'first' & group == 'control')$score)
shapiro.test(subset(totalAverageS, period == 'second' & group == 'control')$score)
shapiro.test(subset(totalAverageS, period == 'third' & group == 'control')$score)
shapiro.test(subset(totalAverageS, period == 'first' & group == 'experimental')$score)
shapiro.test(subset(totalAverageS, period == 'second' & group == 'experimental')$score)
shapiro.test(subset(totalAverageS, period == 'third' & group == 'experimental')$score)


### Sphericity Test (Mauchly)



#==========================

### Drawing Box Plot
# Draw Box Plot for A
totalAverageA$score <- as.numeric(totalAverageA$score)
totalAverageA$period <- factor(totalAverageA$period, level = c("first", "second", "third"))
totalAverageA$group <- factor(totalAverageA$group, level = c("control", "experimental"))

periodLabel <- c("First","Second", "Third") # Label should be revised!!!
groupLabel <- c("Control Group","Experimental Group") # Label should be revised!!!

totalAverageAPlot <- ggplot(totalAverageA, aes(x=period, y=score, fill=group)) + 
  geom_boxplot(alpha=0.4, outlier.color = 'black',outlier.shape = 2) + 
  scale_x_discrete(labels=periodLabel) +
  scale_fill_discrete(labels=groupLabel) +
  scale_y_continuous(trans = my_trans( from=1),breaks = c(1,2,3,4,5)) + 
  coord_cartesian(ylim = c(1, 5)) +
  geom_point(aes(colour = group), position=position_jitterdodge(), show.legend = F) +
  labs(title="Total - Attention", x="Period", y = "Score", fill = "Group") + 
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=15),legend.position = "bottom")
totalAverageAPlot

# Draw Box Plot for R
totalAverageR$score <- as.numeric(totalAverageR$score)
totalAverageR$period <- factor(totalAverageR$period, level = c("first", "second", "third"))
totalAverageR$group <- factor(totalAverageR$group, level = c("control", "experimental"))

periodLabel <- c("First","Second", "Third") # Label should be revised!!!
groupLabel <- c("Control Group","Experimental Group") # Label should be revised!!!

totalAverageRPlot <- ggplot(totalAverageR, aes(x=period, y=score, fill=group)) + 
  geom_boxplot(alpha=0.4, outlier.color = 'black',outlier.shape = 2) + 
  scale_x_discrete(labels=periodLabel) +
  scale_fill_discrete(labels=groupLabel) +
  scale_y_continuous(trans = my_trans( from=1),breaks = c(1,2,3,4,5)) + 
  coord_cartesian(ylim = c(1, 5)) +
  geom_point(aes(colour = group), position=position_jitterdodge(), show.legend = F) +
  labs(title="Total - Relevance", x="Period", y = "Score", fill = "Group") + 
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=15),legend.position = "bottom")
totalAverageRPlot

# Draw Box Plot for C
totalAverageC$score <- as.numeric(totalAverageC$score)
totalAverageC$period <- factor(totalAverageC$period, level = c("first", "second", "third"))
totalAverageC$group <- factor(totalAverageC$group, level = c("control", "experimental"))

periodLabel <- c("First","Second", "Third") # Label should be revised!!!
groupLabel <- c("Control Group","Experimental Group") # Label should be revised!!!

totalAverageCPlot <- ggplot(totalAverageC, aes(x=period, y=score, fill=group)) + 
  geom_boxplot(alpha=0.4, outlier.color = 'black',outlier.shape = 2) + 
  scale_x_discrete(labels=periodLabel) +
  scale_fill_discrete(labels=groupLabel) +
  scale_y_continuous(trans = my_trans( from=1),breaks = c(1,2,3,4,5)) + 
  coord_cartesian(ylim = c(1, 5)) +
  geom_point(aes(colour = group), position=position_jitterdodge(), show.legend = F) +
  labs(title="Total - Confidence", x="Period", y = "Score", fill = "Group") + 
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=15),legend.position = "bottom")
totalAverageCPlot

# Draw Box Plot for S
totalAverageS$score <- as.numeric(totalAverageS$score)
totalAverageS$period <- factor(totalAverageS$period, level = c("first", "second", "third"))
totalAverageS$group <- factor(totalAverageS$group, level = c("control", "experimental"))

periodLabel <- c("First","Second", "Third") # Label should be revised!!!
groupLabel <- c("Control Group","Experimental Group") # Label should be revised!!!

totalAverageSPlot <- ggplot(totalAverageS, aes(x=period, y=score, fill=group)) + 
  geom_boxplot(alpha=0.4, outlier.color = 'black',outlier.shape = 2) + 
  scale_x_discrete(labels=periodLabel) +
  scale_fill_discrete(labels=groupLabel) +
  scale_y_continuous(trans = my_trans( from=1),breaks = c(1,2,3,4,5)) + 
  coord_cartesian(ylim = c(1, 5)) +
  geom_point(aes(colour = group), position=position_jitterdodge(), show.legend = F) +
  labs(title="Total - Satisfaction", x="Period", y = "Score", fill = "Group") + 
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=15),legend.position = "bottom")
totalAverageSPlot


#############################
### Mixed Effect Model for A
# Repeated measure 2 way ANOVA
anova <- aov(score ~ group*period + Error(participant/period), data = totalAverageA)
summary(anova)

#Post-hoc Analysis
with(totalAverageA, pairwise.t.test(score,group,paired=T,p.adjust.method="bonferroni"))

# Mixed Effect Model
anova <- aov(score ~ (group*period) + Error(participant/(group*period)), data = totalAverageA)
summary(anova)

#############################
### Mixed Effect Model for R
# Mixed Effect Model
anova <- aov(score ~ (group*period) + Error(participant/(group*period)), data = totalAverageR)
summary(anova)

#############################
### Mixed Effect Model for C
# Mixed Effect Model
anova <- aov(score ~ (group*period) + Error(participant/(group*period)), data = totalAverageC)
summary(anova)

############################
### Mixed Effect Model for S
# Mixed Effect Model
anova <- aov(score ~ (group*period) + Error(participant/(group*period)), data = totalAverageS)
summary(anova)
anova
