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


### Start y axis
# Start Y axis from n
require(scales)
my_trans <- function(from=0) 
{
  trans <- function(x) x-from
  inv <- function(x) x+from
  trans_new("myscale", trans, inv, 
            domain = c(from, Inf))
}


### failFactor Visualization
# Load Whole Data
failFactor <- read.csv(file = "./data/backgroundResearch/revisedData/failFactor_44.csv", header=T, fileEncoding="UTF-8-BOM")

# Set Variables
failFactor$factor <- factor(failFactor$factor, level = c("badCondition", "temptation", "bothering", "lackOfTime", 
                                                                   "methodUncertainty", "boring", "slightEffect", "ETC"))
failFactor$type <- factor(failFactor$type, level = c("total", "youtube", "app", "alone"))
failFactor$score <- as.numeric(failFactor$score)

# Everything Visualization
factorLabel <- c("Bad Condition", "Temptation", "Bothering", "Lack of Time", 
                 "An Uncertain Way", "Boring", "Slight Effect", "ETC") # Label should be revised!!!

p <- ggplot(failFactor, aes(x=factor, y=score, fill=type, label = score)) +
  geom_bar(stat="identity", position=position_dodge(), alpha=0.4) +
  scale_x_discrete(labels = factorLabel) +
  scale_fill_manual(name = "", labels = c("Total", "YouTube", "App", "No service"), values = c("total" = "#01BFC4", "youtube" = "#f8766d","app" = "#619cff", "alone" = "#00ba38")) +
  geom_text(size = 4, position=position_dodge(0.9), vjust=-0.25) +
  #scale_y_continuous(trans = my_trans( from=1),breaks = c(0,2,3)) + 
  #coord_cartesian(ylim = c(1, 17)) +
  ylim(0,20) +
  labs(title="Hindrance Factors of Doing Yoga", x="", y = "count") + theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), text=element_text(size=15))
p

# Total Only
failFactorTotal <- subset(failFactor, type == "total")
failFactorTotal$factor <- factor(failFactorTotal$factor, level = c("badCondition", "temptation", "bothering", "lackOfTime", 
                                                         "methodUncertainty", "boring", "slightEffect", "ETC"))
factorLabel <- c("Bad condition", "Temptation", "Bothering", "Lack of Time", 
                 "An uncertain way", "Boring", "Slight Effect", "ETC") # Label should be revised!!!

p<- ggplot(data = failFactorTotal, aes(x=factor, y=score, label=score))+
  geom_bar(stat="identity", fill = "#01BFC4", alpha=0.4) +
  scale_x_discrete(labels = factorLabel) +
  geom_text(size = 5, position = "identity") +
  labs(title="", x="", y = "count") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), text=element_text(size=15))
print(p)

# 3 Factors Only (YouTube, App, Alone)
failFactorThree <- subset(failFactor, type %in% c("youtube", "app", "alone"))
failFactorThree$factor <- factor(failFactorThree$factor, level = c("badCondition", "temptation", "bothering", "lackOfTime", 
                                                                   "methodUncertainty", "boring", "slightEffect", "ETC"))
failFactorThree$type <- factor(failFactorThree$type, level = c("youtube", "app", "alone"))
failFactorThree$score <- as.numeric(failFactorThree$score)
factorLabel <- c("Bad condition", "Temptation", "Bothering", "Lack of Time", 
                 "An uncertain way", "Boring", "Slight Effect", "ETC") # Label should be revised!!!

p <- ggplot(failFactorThree, aes(x=factor, y=score, fill=type, label = score)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_x_discrete(labels = factorLabel) +
  scale_fill_manual(name = "", labels = c("YouTube", "App", "Alone"), values = c("youtube" = "#f8766d","app" = "#619cff", "alone" = "#00ba38")) +
  geom_text(size = 4, position=position_dodge(0.9), vjust=-0.25) +
  labs(title="", x="", y = "count") + theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), text=element_text(size=15))
p


### successFactor Visualization
# Load Whole Data
successFactor <- read.csv(file = "./data/backgroundResearch/revisedData/successFactor_44.csv", header=T, fileEncoding="UTF-8-BOM")

# Everything Visualization
successFactor$factor <- factor(successFactor$factor, level = c("pleasure", "desireProfessional", "dailyComfortable", "satisfaction", 
                                                         "necessityRecognition", "beMyself", "specificGoal","positiveResult", "ETC"))
successFactor$type <- factor(successFactor$type, level = c("total", "youtube", "app", "alone"))
successFactor$score <- as.numeric(successFactor$score)
factorLabel <- c("Pleasure", "Desire Professionality", "Daily Comfort", "Achievement", 
                 "Awareness of Necessity", "The Body Shape\nI desire", "Specific Goal","Positive Result", "ETC") # Label should be revised!!!

p <- ggplot(successFactor, aes(x=factor, y=score, fill=type, label = score)) +
  geom_bar(stat="identity", position=position_dodge(), alpha=0.4) +
  scale_x_discrete(labels = factorLabel) +
  #scale_y_continuous(trans = my_trans( from=1),breaks = c(1,2,3,4,5)) + 
  #coord_cartesian(ylim = c(1, 15)) +
  scale_fill_manual(name = "", labels = c("Total", "YouTube", "App", "No Service"), values = c("total" = "#01BFC4", "youtube" = "#f8766d","app" = "#619cff", "alone" = "#00ba38")) +
  geom_text(size = 4, position=position_dodge(0.9), vjust=-0.25) +
  ylim(0,15) +
  labs(title="Motivational Factors of Doing Yoga", x="", y = "count") + theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), text=element_text(size=15))
p

# Total Only
successFactorTotal <- subset(successFactor, type == "total")
successFactorTotal$factor <- factor(successFactorTotal$factor, level = c("pleasure", "desireProfessional", "dailyComfortable", "satisfaction", 
                                                               "necessityRecognition", "beMyself", "specificGoal","positiveResult", "ETC"))
factorLabel <- c("Pleasure", "Desire professionality", "Daily Comfort", "Achievement", 
                 "Awareness of necessity", "Look I desire", "Specific Goal","Positive result", "ETC") # Label should be revised!!!

p<- ggplot(data = successFactorTotal, aes(x=factor, y=score, label=score))+
  geom_bar(stat="identity", fill = "#01BFC4") +
  scale_x_discrete(labels = factorLabel) +
  geom_text(size = 5, position = "identity") +
  labs(title="", x="", y = "count") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), text=element_text(size=15))
print(p)

# 3 Factors Only (Youtube, App, Alone)
successFactorThree <- subset(successFactor, type %in% c("youtube", "app", "alone"))
successFactorThree$factor <- factor(successFactorThree$factor, level = c("pleasure", "desireProfessional", "dailyComfortable", "satisfaction", 
                                                                         "necessityRecognition", "beMyself", "specificGoal","positiveResult", "ETC"))
successFactorThree$type <- factor(successFactorThree$type, level = c("youtube", "app", "alone"))
successFactorThree$score <- as.numeric(successFactorThree$score)
factorLabel <- c("Pleasure", "Desire professionality", "Daily Comfort", "Achievement", 
                 "Awareness of necessity", "Look I desire", "Specific Goal","Positive result", "ETC") # Label should be revised!!!

p <- ggplot(successFactorThree, aes(x=factor, y=score, fill=type, label = score)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_x_discrete(labels = factorLabel) +
  scale_fill_manual(name = "", labels = c("YouTube", "App", "Alone"), values = c("youtube" = "#f8766d","app" = "#619cff", "alone" = "#00ba38")) +
  geom_text(size = 4, position=position_dodge(0.9), vjust=-0.25) +
  labs(title="", x="", y = "count") + theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), text=element_text(size=15))
p

### 5-points Likert Scale Visualization
# Necessary Function - Start Y axis from n
require(scales)
my_trans <- function(from=0) 
{
  trans <- function(x) x-from
  inv <- function(x) x+from
  trans_new("myscale", trans, inv, 
            domain = c(from, Inf))
}


# Load Whole Data
likertScale <- read.csv(file = "./data/backgroundResearch/revisedData/likertScale_44.csv", header=T, fileEncoding="UTF-8-BOM")
likertScale <-likertScale[,c(1:4)]
likertScale$type <- as.factor(likertScale$type)
likertScale$total <- as.factor(likertScale$total)

likertScaleYoutube <- subset(likertScale, type == "youtube")
likertScaleApp <- subset(likertScale, type == "app")
likertScaleAlone <- subset(likertScale, type == "alone")

# Normality Test
shapiro.test(likertScale$score)    # Whole
shapiro.test(likertScaleYoutube$score)    # Youtube
shapiro.test(likertScaleApp$score)    # App
shapiro.test(likertScaleAlone$score)    # Alone

# Check number of each reponses
length(which(likertScale$type=="youtube"))
length(which(likertScale$type=="app"))
length(which(likertScale$type=="alone"))

# Composition Plot
methodComposition <- read.csv(file = "./data/backgroundResearch/revisedData/methodComposition_44.csv", header=T, fileEncoding="UTF-8-BOM")
methodComposition$method <- factor(methodComposition$method, level = c("youtube", "app", "alone"))
factorLabel <- c("YouTube", "App", "No Service") # Label should be revised!!!

p <- ggplot(methodComposition, aes("", fill=method)) + geom_bar(position="fill", alpha=0.4) +
  labs(title="Methods of Yoga Practices", x="Total", y = "%") +
  #scale_fill_discrete(name = "Method", labels = factorLabel) +
  scale_fill_manual(name = "Method", labels = factorLabel, values = c("youtube" = "#f8766d","app" = "#619cff", "alone" = "#00ba38")) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), text=element_text(size=15))
  #coord_polar(theta = "y")
p

# Boxplot - Whole
likertScaleWithTotal <- read.csv(file = "./data/backgroundResearch/revisedData/likertScaleWithTotal_44.csv", header=T, fileEncoding="UTF-8-BOM")
likertScaleWithTotal <-likertScaleWithTotal[,c(1:3)]
likertScaleWithTotal$type <- factor(likertScaleWithTotal$type, level = c("total", "youtube", "app", "alone"))
factorLabel <- c("Total", "YouTube", "App", "No Service") # Label should be revised!!!

p <- ggplot(likertScaleWithTotal, aes(x=type, y=score, fill = type)) + geom_boxplot(alpha=0.4, outlier.color = 'black',outlier.shape = 2) +
  scale_x_discrete(labels = factorLabel) +
  scale_fill_manual(name = "", labels = c("Total", "YouTube", "App", "Alone"), values = c("total"="#01BFC4", "youtube" = "#f8766d","app" = "#619cff", "alone" = "#00ba38")) +
  geom_point(aes(color = type), position=position_jitterdodge(), show.legend = F) +
  scale_color_manual(values = c("total" = "#01BFC4", "youtube" = "#f8766d","app" = "#619cff", "alone" = "#00ba38"))+
  #scale_color_manual(values = c("#01BFC4", "#01BFC4", "#01BFC4", "#01BFC4", "#01BFC4", "#01BFC4", "#01BFC4")) +
  labs(title="The Difficulty of Continuing To Do Yoga", x="", y = "score") +
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=15),legend.position = "")
p

# Boxplot - Total Only
p <- ggplot(likertScale, aes(x=total, y=score)) + geom_boxplot(fill="#01BFC4", alpha=0.4, outlier.color = 'black',outlier.shape = 2) +
  scale_x_discrete(labels = "Total") +
  geom_point(aes(color = total), position=position_jitterdodge(), show.legend = F) +
  scale_color_manual(values = "#01BFC4")+
  labs(title="", x="", y = "score") +
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=15),legend.position = "bottom")
p

# Boxplot - 3 factors Only (youtube, app, alone)
likertScale$type <- factor(likertScale$type, level = c("youtube", "app", "alone"))
factorLabel <- c("YouTube", "App", "Alone") # Label should be revised!!!

p <- ggplot(likertScale, aes(x=type, y=score, fill = type)) + geom_boxplot(alpha=0.4, outlier.color = 'black',outlier.shape = 2) +
  scale_x_discrete(labels = factorLabel) +
  scale_fill_manual(name = "", labels = c("YouTube", "App", "Alone"), values = c("youtube" = "#f8766d","app" = "#619cff", "alone" = "#00ba38")) +
  geom_point(aes(color = type), position=position_jitterdodge(), show.legend = F) +
  scale_color_manual(values = c("youtube" = "#f8766d","app" = "#619cff", "alone" = "#00ba38"))+
  #scale_color_manual(values = c("#01BFC4", "#01BFC4", "#01BFC4", "#01BFC4", "#01BFC4", "#01BFC4", "#01BFC4")) +
  labs(title="", x="", y = "score") +
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=15),legend.position = "")
p

#################
### Total
# 95% Confidence Interval - Total
likertScaleTotalOnly <- read.csv(file = "./data/backgroundResearch/revisedData/likertScaleTotalOnly_44.csv", header=T, fileEncoding="UTF-8-BOM")
os_summary <- data.frame(matrix(ncol = 5))
colnames(os_summary) <- c("type", "meanVal", "sdVal", "seVal", "ciVal")
meanVal <- mean(likertScaleTotalOnly$score)
sdVal <- sd(likertScaleTotalOnly$score)
seVal <- sdVal/sqrt(38)
#ciVal <- 1.96*seVal
ciVal <- qnorm(0.975)*seVal
os_summary <- rbind(os_summary, c("total", round(meanVal,2), round(sdVal,2), round(seVal,2), round(ciVal,2)))
os_summary <- os_summary[-1 , ]
row.names(os_summary) = NULL

os_summary$meanVal = as.numeric(os_summary$meanVal)
os_summary$sdVal = as.numeric(os_summary$sdVal)
os_summary$seVal = as.numeric(os_summary$seVal)
os_summary$ciVal = as.numeric(os_summary$ciVal)

# Default bar plot with 95% confidence level error bar  - Total
os_summary$type <- factor(os_summary$type)
satisfactionLabel <- "Total"
p <- ggplot(os_summary, aes(x=type, y=meanVal)) + 
  geom_bar(stat="identity", position=position_dodge(), fill = "#01BFC4") +
  geom_errorbar(aes(ymin=meanVal-ciVal, ymax=meanVal+ciVal), width=.2,position=position_dodge(.9)) +
  scale_y_continuous(trans = my_trans( from=1),breaks = c(1,2,3,4,5)) + 
  scale_x_discrete(labels = satisfactionLabel) +
  coord_cartesian(ylim = c(1, 5)) +
  #scale_fill_manual(values = "#01BFC4")+
  labs(title="", x="", y = "score") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), text=element_text(size=15))
p

### YouTube
# 95% Confidence Interval - Youtube
likertScaleTotalOnly <- read.csv(file = "./data/backgroundResearch/revisedData/likertScaleYoutubeOnly_44.csv", header=T, fileEncoding="UTF-8-BOM")
youtube_summary <- data.frame(matrix(ncol = 5))
colnames(youtube_summary) <- c("type", "meanVal", "sdVal", "seVal", "ciVal")
meanVal <- mean(likertScaleTotalOnly$score)
sdVal <- sd(likertScaleTotalOnly$score)
seVal <- sdVal/sqrt(38)
#ciVal <- 1.96*seVal
ciVal <- qnorm(0.975)*seVal
youtube_summary <- rbind(youtube_summary, c("total", round(meanVal,2), round(sdVal,2), round(seVal,2), round(ciVal,2)))
youtube_summary <- youtube_summary[-1 , ]
row.names(youtube_summary) = NULL

youtube_summary$meanVal = as.numeric(youtube_summary$meanVal)
youtube_summary$sdVal = as.numeric(youtube_summary$sdVal)
youtube_summary$seVal = as.numeric(youtube_summary$seVal)
youtube_summary$ciVal = as.numeric(youtube_summary$ciVal)

# Default bar plot with 95% confidence level error bar  - Total
youtube_summary$type <- factor(os_summary$type)
satisfactionLabel <- "YouTube"
p <- ggplot(youtube_summary, aes(x=type, y=meanVal)) + 
  geom_bar(stat="identity", position=position_dodge(), fill = "#F8766D") +
  geom_errorbar(aes(ymin=meanVal-ciVal, ymax=meanVal+ciVal), width=.2,position=position_dodge(.9)) +
  scale_y_continuous(trans = my_trans( from=1),breaks = c(1,2,3,4,5)) + 
  scale_x_discrete(labels = satisfactionLabel) +
  coord_cartesian(ylim = c(1, 5)) +
  #scale_fill_manual(values = "#01BFC4")+
  labs(title="", x="", y = "score") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), text=element_text(size=15))
p

### Age Plot
ageComposition <- read.csv(file = "./data/backgroundResearch/revisedData/age_44.csv", header=T, fileEncoding="UTF-8-BOM")
ageComposition$age <- factor(ageComposition$age, level = c("10s", "20s", "30s", "40s"))
factorLabel <- c("10s", "20s", "30s", "40s") # Label should be revised!!!

p <- ggplot(ageComposition, aes("", fill=age)) + geom_bar(position="fill", alpha=0.4) +
  labs(title="Participant Age", x="Participants", y = "%") +
  scale_fill_manual(name = "Age", labels = factorLabel, values = c("10s" = "#619cff","20s" = "#f8766d", "30s" = "#00ba38", "40s" = "#01BFC4")) +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), text=element_text(size=15))
#scale_fill_discrete(name = "Age", labels = factorLabel) #+
#coord_polar(theta = "y")
p


v = c(23,15,5)
k = c('1st','2nd','3rd')
df = data.frame(rank=k,value=v)

pie = ggplot(df,aes(x="",y=value,fill=rank))+geom_bar(width=10,stat='identity') + coord_polar('y',start=0)+geom_text(aes(label=value),position = position_stack(vjust=0.5))
pie
