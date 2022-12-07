library(ggplot2)
library(plyr)
library(dplyr)
library(psych)


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


### Reliability Analysis & Factor Analysis
# PF
PF <- read.csv(file = "./data/userEvaluation/revisedData/cronbach/bytee/PF.csv", header=T, fileEncoding="UTF-8-BOM")
pairs(PF, panel=panel.smooth)
cronbach(PF) # Cronbach Alpha
alpha(PF) # Cronbach Alpha
KMO(PF) # KMO

# SM
SM <- read.csv(file = "./data/userEvaluation/revisedData/cronbach/bytee/SM.csv", header=T, fileEncoding="UTF-8-BOM")
pairs(SM, panel=panel.smooth)
cronbach(SM) # Cronbach Alpha
alpha(SM) # Cronbach Alpha
KMO(SM) # KMO

# SS
SS <- read.csv(file = "./data/userEvaluation/revisedData/cronbach/bytee/SS.csv", header=T, fileEncoding="UTF-8-BOM")
pairs(SS, panel=panel.smooth)
cronbach(SS) # Cronbach Alpha
alpha(SS) # Cronbach Alpha
KMO(SS) # KMO


### Load Data
# Load
byteeAverage <- read.csv(file = "./data/userEvaluation/revisedData/byteeAverage.csv", header=T, fileEncoding="UTF-8-BOM")

# set Factors
byteeAverage$participant <- as.factor(byteeAverage$participant)
byteeAverage$measure <- as.factor(byteeAverage$measure)
byteeAverage$period <- as.factor(byteeAverage$period)
byteeAverage$score <- as.numeric(byteeAverage$score)

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
byteeAveragePF <- subset(byteeAverage, measure == 'PF')
byteeAverageSM <- subset(byteeAverage, measure == 'SM')
byteeAverageSS <- subset(byteeAverage, measure == 'SS')

### Normality Test
shapiro.test(subset(byteeAveragePF, period == 'first')$score)
shapiro.test(subset(byteeAveragePF, period == 'second')$score)
shapiro.test(subset(byteeAveragePF, period == 'third')$score)

shapiro.test(subset(byteeAverageSM, period == 'first')$score)
shapiro.test(subset(byteeAverageSM, period == 'second')$score)
shapiro.test(subset(byteeAverageSM, period == 'third')$score)

shapiro.test(subset(byteeAverageSS, period == 'first')$score)
shapiro.test(subset(byteeAverageSS, period == 'second')$score)
shapiro.test(subset(byteeAverageSS, period == 'third')$score)

### Homogeneity of Variance Test (Levene)
# Test for PF
fitPF <- lm(score ~ period, data = byteeAveragePF)
levene.test(fitPF)

# Test for SM
fitSM <- lm(score ~ period, data = byteeAverageSM)
levene.test(fitSM)

# Test for SS
fitSS <- lm(score ~ period, data = byteeAverageSS)
levene.test(fitSS)


### Drawing Box Plot
# Draw Box Plot for Bytee Average
byteeAverage$score <- as.numeric(byteeAverage$score)
byteeAverage$period <- factor(byteeAverage$period, level = c("first", "second", "third"))
byteeAverage$measure <- factor(byteeAverage$measure, level = c("PF", "SM", "SS"))

periodLabel <- c("1st Time","5th Time", "10th Time") # Label should be revised!!!
measureLabel <- c("Personalized\nVisual Feedback", "Visualization\nof Self-Monitoring", "Social Support\n with Visualization")
measureLabel <- c("Personalized Visual Feedback", "Visualization of Self-Monitoring", "Social Support with Visualization")
measureLabel <- c("Factor A","Factor B", "Factor C") # Label should be revised!!!

byteeAveragePlot <- ggplot(byteeAverage, aes(x=period, y=score, fill=measure)) + 
  geom_boxplot(alpha=0.4, outlier.color = 'black',outlier.shape = 2) + 
  scale_x_discrete(labels=periodLabel) +
  scale_fill_discrete(labels=measureLabel) +
  scale_y_continuous(trans = my_trans( from=1),breaks = c(1,2,3,4,5)) + 
  coord_cartesian(ylim = c(1, 5)) +
  geom_point(aes(colour = measure), position=position_jitterdodge(), show.legend = F) +
  labs(title="3 Factors of Bytee", x="Period", y = "Score", fill = "Measure") + 
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=15),legend.position = "bottom")
byteeAveragePlot

############################
### Mixed Effect Model
# Mixed Effect Model
anova <- aov(score ~ (measure*period) + Error(participant/(measure*period)), data = byteeAverage)
summary(anova)
anova$coefficients

anova

# Friedman Test & Post-hoc Test for PF
byteeAveragePF$participant <- as.factor(byteeAveragePF$participant)
byteeAveragePF$measure <- as.factor(byteeAveragePF$measure)
byteeAveragePF$period <- as.factor(byteeAveragePF$period)
byteeAveragePF$score <- as.numeric(byteeAveragePF$score)
anova <- aov(score ~ period + Error(participant/period) , data = byteeAveragePF)
summary(anova)
friedman.test(score ~ period | participant, data=byteeAveragePF)
pairwise.wilcox.test(byteeAveragePF$score, byteeAveragePF$period, p.adjust='bonferroni')


# Friedman Test & Post-hoc Test for SM
byteeAverageSM$participant <- as.factor(byteeAverageSM$participant)
byteeAverageSM$measure <- as.factor(byteeAverageSM$measure)
byteeAverageSM$period <- as.factor(byteeAverageSM$period)
byteeAverageSM$score <- as.numeric(byteeAverageSM$score)
anova <- aov(score ~ period + Error(participant/period) , data = byteeAverageSM)
summary(anova) # RM one-way ANOVA
friedman.test(score ~ period | participant, data=byteeAverageSM) # Friedman Test
pairwise.wilcox.test(byteeAverageSM$score, byteeAverageSM$period, p.adjust='bonferroni') # post-hoc


# Friedman Test & Post-hoc Test for SS
byteeAverageSS$participant <- as.factor(byteeAverageSS$participant)
byteeAverageSS$measure <- as.factor(byteeAverageSS$measure)
byteeAverageSS$period <- as.factor(byteeAverageSS$period)
byteeAverageSS$score <- as.numeric(byteeAverageSS$score)
anova <- aov(score ~ period + Error(participant/period) , data = byteeAverageSS)
summary(anova)
friedman.test(score ~ period | participant, data=byteeAverageSS)
pairwise.wilcox.test(byteeAverageSS$score, byteeAverageSS$period, p.adjust='bonferroni')
