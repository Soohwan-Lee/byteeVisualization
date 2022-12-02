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
systemEvaluationAverage <- read.csv(file = "./data/userEvaluation/revisedData/systemEvaluationAverage.csv", header=T, fileEncoding="UTF-8-BOM")

# set Factors
systemEvaluationAverage$measure <- as.factor(systemEvaluationAverage$measure)
systemEvaluationAverage$score <- as.numeric(systemEvaluationAverage$score)


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
systemEvaluationAverageYE <- subset(systemEvaluationAverage, measure == 'YE')
systemEvaluationAverageVI <- subset(systemEvaluationAverage, measure == 'VI')
systemEvaluationAverageSE <- subset(systemEvaluationAverage, measure == 'SE')

# Normality Test
shapiro.test(systemEvaluationAverageYE$score)
shapiro.test(systemEvaluationAverageVI$score)
shapiro.test(systemEvaluationAverageSE$score)


### Drawing Box Plot
# Draw Box Plot for System Evaluation
systemEvaluationAverage$score <- as.numeric(systemEvaluationAverage$score)
systemEvaluationAverage$measure <- factor(systemEvaluationAverage$measure, level = c("YE", "VI", "SE"))

measureLabel <- c("YE","VI", "SE") # Label should be revised!!!

systemEvaluationAveragePlot <- ggplot(systemEvaluationAverage, aes(x=measure, y=score, fill = measure)) + 
  geom_boxplot(alpha=0.4, outlier.color = 'black',outlier.shape = 2) + 
  scale_x_discrete(labels=measureLabel) +
  scale_y_continuous(trans = my_trans( from=1),breaks = c(1,2,3,4,5,6,7)) + 
  coord_cartesian(ylim = c(1, 7)) +
  geom_point(aes(colour = measure), position=position_jitterdodge(), show.legend = F) +
  labs(title="Bytee - System Evaluation", x="Measure", y = "Score") + 
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=15),legend.position = "")
systemEvaluationAveragePlot
