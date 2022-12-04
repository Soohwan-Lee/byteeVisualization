library(ggplot2)
library(plyr)
library(dplyr)
library(psy)
library(psych)
library(MVTests)
library(s20x)

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


### Reliability Analysis & Factor Analysis ###
### A
# Cronbach Alpha
A <- read.csv(file = "./data/userEvaluation/revisedData/cronbach/ARCS/totalA.csv", header=T, fileEncoding="UTF-8-BOM")
pairs(A, panel=panel.smooth)
cronbach(A)
alpha(A)

# KMO (Factor Analysis)
KMO(A)


### R
# Cronbach Alpha
R <- read.csv(file = "./data/userEvaluation/revisedData/cronbach/ARCS/totalR.csv", header=T, fileEncoding="UTF-8-BOM")
pairs(R, panel=panel.smooth)
cronbach(R)
alpha(R)

# KMO (Factor Analysis)
KMO(R)

### C
# Cronbach Alpha
C <- read.csv(file = "./data/userEvaluation/revisedData/cronbach/ARCS/totalC.csv", header=T, fileEncoding="UTF-8-BOM")
pairs(C, panel=panel.smooth)
cronbach(C)
alpha(C)

# KMO (Factor Analysis)
KMO(C)


### S
# Cronbach Alpha
S <- read.csv(file = "./data/userEvaluation/revisedData/cronbach/ARCS/totalS.csv", header=T, fileEncoding="UTF-8-BOM")
pairs(S, panel=panel.smooth)
cronbach(S)
alpha(S)

# KMO (Factor Analysis)
KMO(S)


### Load Average Total Data
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


### Homogeneity of Variance Test (Levene)
# Test for A
fitA <- lm(score ~ period * group, data = totalAverageA)
levene.test(fitA)

# Test for R
fitR <- lm(score ~ period * group, data = totalAverageR)
levene.test(fitR)

# Test for C
fitC <- lm(score ~ period + group, data = totalAverageC)
levene.test(fitC)

# Test for S
fitC <- lm(score ~ period + group, data = totalAverageS)
levene.test(fitC)


### Sphericity Test (Mauchly)
mauchly.test(fitA)
mauchly.test(data=totalAverageA, score ~period)


### Homogeneity of Variacne Test (Box)
boxM(data=totalAverageA, group=period)

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

# Friedman Test for experimental
totalAverageA_experimental = subset(totalAverageA, group == 'experimental')
totalAverageA_experimental$score <- as.numeric(totalAverageA_experimental$score)
totalAverageA_experimental$period <- factor(totalAverageA_experimental$period)
totalAverageA_experimental$participant <- factor(totalAverageA_experimental$participant)
friedman.test(score ~ period | participant, data=totalAverageA_experimental)

# Post-hoc for Friedman Test
pairwise.wilcox.test(totalAverageA_experimental$score, totalAverageA_experimental$period, p.adjust='bonferroni')


#############################
### Mixed Effect Model for R
# Repeated measure 2 way ANOVA
anova <- aov(score ~ (group*period) + Error(participant/period), data = totalAverageR)
summary(anova)

# Friedman Test for experimental
totalAverageR_experimental = subset(totalAverageR, group == 'experimental')
totalAverageR_experimental$score <- as.numeric(totalAverageR_experimental$score)
totalAverageR_experimental$period <- factor(totalAverageR_experimental$period)
totalAverageR_experimental$participant <- factor(totalAverageR_experimental$participant)
friedman.test(score ~ period | participant, data=totalAverageR_experimental)

# Post-hoc for Friedman Test
pairwise.wilcox.test(totalAverageR_experimental$score, totalAverageR_experimental$period, p.adjust='bonferroni')


#############################
### Mixed Effect Model for C
# Repeated measure 2 way ANOVA
anova <- aov(score ~ (group*period) + Error(participant/period), data = totalAverageC)
summary(anova)

# Friedman Test for experimental
totalAverageC_experimental = subset(totalAverageC, group == 'experimental')
totalAverageC_experimental$score <- as.numeric(totalAverageC_experimental$score)
totalAverageC_experimental$period <- factor(totalAverageC_experimental$period)
totalAverageC_experimental$participant <- factor(totalAverageC_experimental$participant)
friedman.test(score ~ period | participant, data=totalAverageC_experimental)

# Post-hoc for Friedman Test
pairwise.wilcox.test(totalAverageC_experimental$score, totalAverageC_experimental$period, p.adjust='bonferroni')


############################
### Mixed Effect Model for S
# Repeated measure 2 way ANOVA
anova <- aov(score ~ (group*period) + Error(participant/period), data = totalAverageS)
summary(anova)
anova

# Friedman Test for experimental
totalAverageS_experimental = subset(totalAverageS, group == 'experimental')
totalAverageS_experimental$score <- as.numeric(totalAverageS_experimental$score)
totalAverageS_experimental$period <- factor(totalAverageS_experimental$period)
totalAverageS_experimental$participant <- factor(totalAverageS_experimental$participant)
friedman.test(score ~ period | participant, data=totalAverageS_experimental)

# Post-hoc for Friedman Test
pairwise.wilcox.test(totalAverageS_experimental$score, totalAverageS_experimental$period, p.adjust='bonferroni')

