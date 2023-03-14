# Which factor causes Diabetes

library(dplyr)
library(ggplot2)
library(tidyr)
library(ggcorrplot)

# Load the diabetes dataset
diabetes_data <- read.csv('diabetes.csv')
diabetes_data
#checking the structure of the data

str(diabetes_data)
glimpse(diabetes_data)
head(diabetes_data, 10)
# Check for missing values
sum(is.na(diabetes_data))
# Check for duplicates
sum(duplicated(diabetes_data))
# Check for outliers
boxplot(diabetes_data$Age)
boxplot(diabetes_data$BMI)


# Check for data consistency
summary(diabetes_data$Age)
summary(diabetes_data$Blood_Pressure)
# Calculate the overall prevalence of diabetes in the dataset
overall_prevalence <- mean(diabetes_data$Outcome)
cat("Overall prevalence of diabetes in the dataset: ", round(overall_prevalence * 100, 2), "%\n")
prevalence <- diabetes_data %>% summarize(Prevalence = mean(Outcome) * 100)
prevalence

# Create a bar chart of the Outcome variable
ggplot(diabetes_data, aes(x = factor(Outcome))) + 
  geom_bar(fill = "steelblue") +
  labs(x = "Outcome", y = "Frequency", 
       title = "Distribution of Diabetes Outcome in the Dataset")
# create histogram of ages

ggplot(data = diabetes_data, aes(x = Age)) +
  geom_histogram(bins = 20, color = "white", fill = "dodgerblue") +
  labs(title = "Distribution of Ages in the Diabetes Dataset",
       x = "Age",
       y = "Frequency") +
  theme_bw()
# create bar chart of pregnancies distribution
ggplot(data = diabetes_data, aes(x = Pregnancies)) +
  geom_bar(fill = "dodgerblue") +
  labs(title = "Pregnancies Distribution in the Diabetes Dataset",
       x = "pregnancies",
       y = "Frequency") +
  theme_bw()
# create bar chart of Glucose distribution
ggplot(data = diabetes_data, aes(x = Glucose)) +
  geom_histogram(binwidth = 10, color = "black", fill = "orange") +
  ggtitle("Distribution of Glucose Levels in Diabetes Patients") +
  xlab("Glucose Level") +
  ylab("Frequency")
ggplot(data = diabetes_data, aes(x = Insulin)) + 
  geom_histogram(binwidth = 50, color = "black", fill = "lightblue") + 
  labs(title = "Distribution of Insulin Levels", x = "Insulin (mu U/ml)", y = "Frequency")
# Create histogram of blood pressure values
ggplot(data = diabetes_data, aes(x = BloodPressure)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "black") +
  labs(x = "Blood Pressure (mmHg)", y = "Frequency", 
       title = "Histogram of Blood Pressure Values") +
  theme_bw()
# Calculate correlation between BMI and glucose
correlation <- cor(diabetes_data$BMI, diabetes_data$Glucose)
correlation
# Create scatterplot
ggplot(diabetes_data, aes(x = BMI, y = Glucose)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Scatterplot of BMI and Glucose Levels") +
  xlab("BMI") +
  ylab("Glucose") +
  annotate("text", x = 20, y = 150, label = paste("Correlation:", round(correlation, 2)))
correlation <- cor(diabetes_data$Age, diabetes_data$Insulin)
correlation

ggplot(data = diabetes_data, aes(x = Age, y = Insulin)) +
  geom_point() +
  labs(x = "Age", y = "Insulin Levels", title = "Scatter Plot of Age and Insulin Levels")
corr<- cor(diabetes_data$Age,diabetes_data$Pregnancies )
corr

ggplot(data = diabetes_data,aes(x=Age,fill=factor(Outcome)))+geom_density(alpha=0.4)+scale_fill_manual(values=c("red", "blue"))+labs(title="Distribution of Age")


ggplot(data = diabetes_data, aes(x = Age, y = Pregnancies)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlation between Age and Pregnancies",
       x = "Age",
       y = "Number of Pregnancies")


corr<-round(cor(diabetes_data),1)
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("red", "white", "blue"), 
           title="Correlogram of Diabetes data", 
           ggtheme=theme_bw)
'''
Conclusion
Diabetes is a serious health condition that affects a significant portion of the population. In this dataset, around 35% of the individuals had diabetes.

Age, BMI, pregnancies, and blood pressure are among the most significant risk factors for diabetes. Individuals who are older, have a higher BMI, have had more pregnancies, and have higher blood pressure are at a higher risk of developing diabetes.

There is a strong correlation between age and the number of pregnancies. Younger women tend to have more pregnancies, and as they age, the number of pregnancies decreases.

There is a negative correlation between age and insulin levels. As individuals age, their insulin levels tend to decrease.

There is a positive correlation between BMI and blood pressure. Individuals with higher BMI tend to have higher blood pressure.
'''
