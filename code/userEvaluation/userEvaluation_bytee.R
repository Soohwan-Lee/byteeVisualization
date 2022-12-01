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


### Drawing Box Plot
# Draw Box Plot for Bytee Average
byteeAverage$score <- as.numeric(byteeAverage$score)
byteeAverage$period <- factor(byteeAverage$period, level = c("first", "second", "third"))
byteeAverage$measure <- factor(byteeAverage$measure, level = c("PF", "SM", "SS"))

periodLabel <- c("First","Second", "Third") # Label should be revised!!!
measureLabel <- c("PF","SM", "SS") # Label should be revised!!!

byteeAveragePlot <- ggplot(byteeAverage, aes(x=period, y=score, fill=measure)) + 
  geom_boxplot(alpha=0.4, outlier.color = 'black',outlier.shape = 2) + 
  scale_x_discrete(labels=periodLabel) +
  scale_fill_discrete(labels=measureLabel) +
  scale_y_continuous(trans = my_trans( from=1),breaks = c(1,2,3,4,5)) + 
  coord_cartesian(ylim = c(1, 5)) +
  geom_point(aes(colour = measure), position=position_jitterdodge(), show.legend = F) +
  labs(title="Bytee - ARCS", x="Period", y = "Score", fill = "Measure") + 
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=15),legend.position = "bottom")
byteeAveragePlot
