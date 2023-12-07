setwd("C:/Users/ekara/Desktop/Emreproject/Final_ImageJ/Results/ReadyForAnalysis")
remotes::install_github("R-CoderDotCom/ridgeline@main")
install.packages("remotes")

library(ggridges)
library(remotes)
library(ridgeline)
setwd("C:/Users/Emre Kara/Documents")
flight_performance <- read.csv("C:/FlightScore.csv")

install.packages("xlsx")
library("xlsx")
library(ggplot2)

# Make a scatter plot by using Median and PercentageDie:

flight_incompetence <-  read.xlsx ("C:/Medians_And_CVs.xlsx", sheetIndex = 1, header = TRUE)
ggplot(flight_incompetence, aes(x = Flight_Impairment_Index, y = PercentageDie)) + geom_point() + geom_smooth(method=lm, se=FALSE)
eruption.lm = lm(Flight_Impairment_Index ~ PercentageDie, data=flight_incompetence)
summary(eruption.lm)$r.squared
##


ggplot(flight_performance, aes(x = Line , y= Landing_height_cm , fill = Line)) +
  geom_boxplot(alpha = 0.1) + theme (axis.text.x = element_text (size = '11')) + theme (axis.text.x = element_text (angle = 45))

ggplot(flight_performance, aes(x= Landing_height_cm, y= Line, fill =Line)) +
  geom_density_ridges(alpha = 0.1) + theme (axis.text.y = element_text (size = '11'))


Lines <- unique(flight_performance$Line)

for (i in Lines){
a <- nrow(subset(flight_performance, Line == i))
print(i)
print(a)
}

# Make a ridgeline plot to compare several conditions
# where $Name is the column with the name for each condition type (e.g., "No selection" vs "Flight selection" or "Female" vs "Male")
ridgeline(flight_performance$Landing_height_cm, flight_performance$Tube_Type, 
          palette = c("cornsilk4", "cornsilk3"))

install.packages("ggsignif")
library(ggsignif)


## Scatter Plot for %die and %flight performance
scatter_data <- read.csv("C:/Users/ekara/Desktop/ScatterPlotDataset.csv")
ggplot(flight_incompetence, aes(x = Median, y = PercentageDie)) + geom_point() + geom_smooth(method=lm, se=FALSE)

# Checking the residual values
model1 <- lm(PercentageDie~Median, data=scatter_data)
res <- resid(model1)
res
plot(fitted(model1),res)
abline(0,0)
qqnorm(res)
qqline(res)
plot(density(res))

# Obtaining the Pearson coefficient
result = cor(X, Y, method = "pearson")
print(result)

Y <- scatter_data[,"PercentageEscape"]
X <- scatter_data[,"Median"]
model <- lm(Y~X)


res <- resid(scatter_data)
res
plot(density(res))

# Obtaining the R squared value to see the correlation
eruption.lm = lm(Median ~ PercentageDie, data=scatter_data)
summary(eruption.lm)$r.squared
eruption.res = resid(eruption.lm)
summary(eruption.res)
cor.test(scatter_data$Median, log2(scatter_data$PercentageDie), method = c("pearson", "kendall", "spearman"))

## Linear regression for the %escape and median

ggplot(scatter_data, aes(x = Median, y = PercentageEscape)) + geom_point() + geom_smooth(method=lm, se=FALSE)
eruption.lm = lm(Median ~ PercentageEscape, data=scatter_data)
summary(eruption.lm)$r.squared


######## STATISTICAL TESTS

flight_data <- read.csv("C:/Users/ekara/Desktop/Emreproject/Final_ImageJ/Results/ReadyForAnalysis/FlightAnalysis.csv")
# Assuming your dataset is already loaded and named flight_data

B04 <- subset(flight_data, Line == 'B04')
N01 <- subset(flight_data, Line == 'N01')
T01 <- subset(flight_data, Line == 'T01')
I23 <- subset(flight_data, Line == 'I23')
ZW184 <- subset(flight_data, Line == 'ZW184')
# Extract the Landing_height_cm columns from each subset
B04_height <- B04$Landing_height_cm
N01_height <- N01$Landing_height_cm
T01_height <- T01$Landing_height_cm
I23_height <- I23$Landing_height_cm
ZW184_height <- ZW184$Landing_height_cm

result_B04_1 <- t.test(B04_height, I23_height)
result_B04_2 <- t.test(B04_height, N01_height)
result_B04_3 <- t.test(B04_height, T01_height, var.equal = FALSE)
result_B04_4 <- t.test(B04_height, ZW184_height)

result_I23_1 <- t.test(I23_height, N01_height)
result_I23_2 <- t.test(I23_height, T01_height)
result_I23_3 <- t.test(I23_height, ZW184_height)

result_N01_1 <- t.test(N01_height, T01_height)
result_N01_2 <- t.test(N01_height, ZW184_height)
result_N01_3 <- t.test(N01_height, T01_height, var.equal = FALSE)

result_T01_1 <- t.test(T01_height, ZW184_height)
result_T01_2 <- t.test(T01_height, ZW184_height, var.equal = FALSE)
# View the results of the t-test

print(result_T01_1)
print(result_T01_2)


### CALCULATING THE COEFFICIENT OF VARIANCES

library(ggridges)
library(remotes)
library(ridgeline)
library(ggplot2)
flight_performance <- read.csv("C:/Users/ekara/Desktop/Acceleration2.csv")
# Make a density plot:
ggplot(flight_performance, aes(x= Tube_Type, y= Landing_height_cm, fill = Tube_Type)) +
  geom_boxplot(alpha = 0.1)
ShortT <- subset(flight_performance, Tube_Type == 'Short')
LongT <- subset(flight_performance, Tube_Type == 'Long')

Short_height <- ShortT$Landing_height_cm
Long_height <- LongT$Landing_height_cm

result1 <- var.test(Short_height, Long_height)
print(result1)
mean1 <- mean(Short_height)
mean2 <- mean(Long_height)
print(mean1)
print(mean2)



s1<-sqrt(sum((Short_height-mean1)^2/(length(Short_height)-1)))

print(s1)

s2<-sqrt(sum((Long_height-mean1)^2/(length(Long_height)-1)))
print(s2)


COV1 <- (s1 / mean1)*100
print(COV1)
COV2 <- (s2 / mean2)*100
print(COV2)



## Calling the IQR values of dataset
library(readxl)
iqr_value <- read.xlsx("C:/2MWide(Normal)TubeResults.xlsx",sheetIndex = 1, header = TRUE)
CoeffValue <- iqr_value$CoEV
FlightPerfMedian <- iqr_value$Median
ggplot(iqr_value, aes(x = IQR_Value, y = FlightPerfMedian)) + geom_point() + geom_smooth(method=lm, se=FALSE)
eruption.lm = lm(IQR_Value ~ Median, data=iqr_value)
summary(eruption.lm)$r.squared
##
MyOutbredData <- read.xlsx("C:/AllOutbredResults.xlsx",sheetIndex = 1, header = TRUE)
ggplot(MyOutbredData, aes(x = Line, y= Landing_height_cm , fill = Gender)) +
  geom_boxplot(alpha = 0.1) + theme (axis.text.x = element_text (size = '11')) + theme (axis.text.x = element_text (angle = 45))

ANOVA_Result <- aov(Landing_height_cm ~ Gender * Line, data = MyOutbredData)
summary(ANOVA_Result)

Beijing <- subset(MyOutbredData, Line == 'Beijing')
Beijing_Males <- subset(Beijing, Gender=='Male')
Beijing_Females <- subset(Beijing, Gender=='Female')
var.test(Beijing_Males$Landing_height_cm, Beijing_Females$Landing_height_cm)
wilcox.test(Beijing_Males$Landing_height_cm, Beijing_Females$Landing_height_cm)
##Another Way to Perform F-test between the sexes within the population
Beijing_Males_Flight <- Beijing_Males$Landing_height_cm
Beijing_Females_Flight <- Beijing_Females$Landing_height_cm

var.test(Beijing_Males_Flight, Beijing_Females_Flight)
wilcox.test(Beijing_Males_Flight)
##


Ithaca <- subset(MyOutbredData, Line == 'Ithaca')
Ithaca_Males <- subset(Ithaca, Gender=='Male')
Ithaca_Females <- subset(Ithaca, Gender=='Female')
var.test(Ithaca_Males$Landing_height_cm, Ithaca_Females$Landing_height_cm)
wilcox.test(Ithaca_Males$Landing_height_cm, Ithaca_Females$Landing_height_cm)


Netherlands <- subset(MyOutbredData, Line == 'Netherlands')
Netherlands_Males <- subset(Netherlands, Gender=='Male')
Netherlands_Females <- subset(Netherlands, Gender=='Female')
var.test(Netherlands_Males$Landing_height_cm, Netherlands_Females$Landing_height_cm)
wilcox.test(Netherlands_Males$Landing_height_cm, Netherlands_Females$Landing_height_cm)

Tasmania <- subset(MyOutbredData, Line == 'Tasmania')
Tasmania_Males <- subset(Tasmania, Gender=='Male')
Tasmania_Females <- subset(Tasmania, Gender=='Female')
var.test(Tasmania_Males$Landing_height_cm, Tasmania_Females$Landing_height_cm)
wilcox.test(Tasmania_Males$Landing_height_cm, Tasmania_Females$Landing_height_cm)

Zimbabwe <- subset(MyOutbredData, Line == 'Zimbabwe')
Zimbabwe_Males <- subset(Zimbabwe, Gender=='Male')
Zimbabwe_Females <- subset(Zimbabwe, Gender=='Female')
var.test(Zimbabwe_Males$Landing_height_cm, Zimbabwe_Females$Landing_height_cm)
wilcox.test(Zimbabwe_Males$Landing_height_cm, Zimbabwe_Females$Landing_height_cm)
## Calculation of CV for the outbreds
show(CV_BeiFem <- sd (Beijing_Females$Landing_height_cm) / mean (Beijing_Females$Landing_height_cm) * 100)
show(CV_BeiMale <- sd (Beijing_Males$Landing_height_cm) / mean (Beijing_Males$Landing_height_cm) * 100)
show(CV_IthFem <- sd (Ithaca_Females$Landing_height_cm) / mean (Ithaca_Females$Landing_height_cm) * 100)
show(CV_IthMale <- sd (Ithaca_Males$Landing_height_cm) / mean (Ithaca_Males$Landing_height_cm) * 100)
show(CV_NetFem <- sd (Netherlands_Females$Landing_height_cm) / mean (Netherlands_Females$Landing_height_cm) * 100)
show(CV_NetMale <- sd (Netherlands_Males$Landing_height_cm) / mean (Netherlands_Males$Landing_height_cm) * 100)
show(CV_TasmFem <- sd (Tasmania_Females$Landing_height_cm) / mean (Tasmania_Females$Landing_height_cm) * 100)
show(CV_TasmMale <- sd (Tasmania_Males$Landing_height_cm) / mean (Tasmania_Males$Landing_height_cm) * 100)
show(CV_ZimbFem <- sd (Zimbabwe_Females$Landing_height_cm) / mean (Zimbabwe_Females$Landing_height_cm) * 100)
show(CV_ZimbMale <- sd (Zimbabwe_Males$Landing_height_cm) / mean (Zimbabwe_Males$Landing_height_cm) * 100)


## Calculating the IQR values for outbred populations
show(IQR(Beijing_Females$Landing_height_cm))
show(IQR(Beijing_Males$Landing_height_cm))
show(IQR(Ithaca_Females$Landing_height_cm))
show(IQR(Ithaca_Males$Landing_height_cm))
show(IQR(Netherlands_Females$Landing_height_cm))
show(IQR(Netherlands_Males$Landing_height_cm))
show(IQR(Tasmania_Females$Landing_height_cm))
show(IQR(Tasmania_Males$Landing_height_cm))
show(IQR(Zimbabwe_Females$Landing_height_cm))
show(IQR(Zimbabwe_Males$Landing_height_cm))

## Applying the Levene Test to again check the variances between the sexes among the populations

library(car)
library(stats)
install.packages("lawstat")
library(lawstat)
Ithaca <- subset(MyOutbredData, Line == 'Ithaca')
Ithaca_Males <- subset(Ithaca, Gender=='Male')
Ithaca_Males_Landings <- Ithaca_Males$Landing_height_cm
Ithaca_Females_Landings <- Ithaca_Females$Landing_height_cm

Ithaca_Variance <- kruskal.test(Ithaca_Males_Landings, Ithaca_Females_Landings)
print(Ithaca_Variance)


# Conduct Tukey's post-hoc test for the interaction effect
interaction_posthoc <- TukeyHSD(ANOVA_Result, "Gender:Line")

# Print the post-hoc test results
print(interaction_posthoc)

# Calculate variance for males and females within each population
variances <- tapply(MyOutbredData$Landing_height_cm, list(MyOutbredData$Line, MyOutbredData$Gender), var)

# Print the variances
print(variances)




##
install.packages("stats")
library(car)
# Assuming 'data' is your data frame containing 'Line', 'Gender', and 'Landing_height_cm'
# Calculate variance for each population based on landing height and gender
variances <- tapply(MyOutbredData$Landing_height_cm, list(MyOutbredData$Line, MyOutbredData$Gender), var)

# Perform Levene's test for each population
levene_results <- leveneTest(variances, 1, function(x) {
  stats::levene(MyOutbredData$Landing_height_cm[MyOutbredData$Line == names(variances)[which(variances == x)] & MyOutbredData$Gender == "Male"],
                MyOutbredData$Landing_height_cm[MyOutbredData$Line == names(variances)[which(variances == x)] & MyOutbredData$Gender == "Female"])
})
levene_results <- leveneTest(Line ~ Gender*Landing_height_cm, data = MyOutbredData, center = "median")
# Print the results
levene_results
29536811140
