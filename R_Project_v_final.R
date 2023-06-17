#install required packages
install.packages("tidyverse")
library(tidyverse)
library(corrplot)
install.packages("plotrix")
library(plotrix)
library(ggplot2)
library(dplyr)
install.packages("factoextra")
library(factoextra)

#Exploratory Data Analysis
df <- read.csv("C:/Users/MONSTER/Desktop/FE418_R/Mall_Customers.csv")
df
summary(df)
structure(df)
dim(df)
names(df)
#renaming
df <- rename(df, Income = "Annual.Income..k..", Score = "Spending.Score..1.100.")
names(df)
#To remove missing packages
df <- na.omit(df)


#Remove first collum and create df1. CustomerID not necessary in matrix operations.
df1<-df[-c(1)]
df1

#Gender vs. Spending Score
boxplot(df1$Score ~ df1$Gender,
        xlab = "Gender",
        ylab = "Spending Score",
        main = "Gender vs. Spending Score")

#Visulation of Genders in piechart
a <- table(df$Gender)
pct <- round(a / sum(a) * 100)
lbs <- paste(c("Female", "Male"), " ", pct, "%", sep=" ")
colors <- c("skyblue", "pink")
pie3D(a, labels = lbs, col = colors, main = "Pie Chart Depicting Ratio of Female and Male")


# Create the bar plot for Gender
colors <-c("pink", "skyblue")
barplot(a, col = colors, main = "Bar Plot Depicting Ratio of Female and Male", 
        xlab = "Gender", ylab = "Count")



#Remove first and second collum. Remaind numeric 'integer values for analysis.'
df<-df[-c(1,2)]
df

#this boxplot shows that there is no outliers among numeric datas.
boxplot(df, 
        xlab="Variables",
        ylab="Values",
        main= "Numeric Variables from Dataset") 

#To analyze create histograms with density plot. Density plots are small so, rescale them to show together.

#Age Histogram and density plot

hist(df$Age, main = "Age Histogram", xlab = "Ages")
density_values <- density(df$Age)
scaling_factor <- 1000 
rescaled_density <- density_values$y * scaling_factor
lines(density_values$x, rescaled_density, col = "red",lwd=2)

#Annual Income and Density Plot
hist(df$Income, main = "Annual Income Histogram", xlab = "Income")
density_values <- density(df$Income)
scaling_factor <- 2000  
rescaled_density <- density_values$y * scaling_factor
lines(density_values$x, rescaled_density, col = "blue",lwd=2)

#Spending Score and Density Plot

hist(df$Score, main = "Spending Score Histogram", xlab = "Spending Score")
density_values <- density(df$Score)
scaling_factor <- 2000 
rescaled_density <- density_values$y * scaling_factor
lines(density_values$x, rescaled_density, col = "violet",lwd = 2)


#Scatter plots to obtain the relationship between numeric values

plot(df$Age,df$Score,col="red",xlab="Spending Score",ylab = "Age", main="Age VS Spending Score")
plot(df$Income,df$Score,col="red",xlab="Spending Score",ylab = "Annual Income", main="Annual Income VS Spending Score")
plot(df$Age,df$Income,col="red",xlab="Age",ylab = "Annual Income", main="Age VS Anual Income")


#Statistical Analysis

#QQ plot
qqnorm(df$Score,main="QQ plot of Spending Score",pch=19)
qqline(df$Score)
qqnorm(df$Income,main="QQ plot of Annual Income",pch=19)
qqline(df$Income)

#Shapiro-Wilk test
shapiro.test(df$Score) #p_value<0.05, non-parametric
shapiro.test(df$Income) #p_value<0.05, non-parametric
shapiro.test(df$Age) # p_value<0.05 non-parametric

#Ansari Test
ansari.test(df$Score, df$Income) #p_value=0.9159
ansari.test(df$Age, df$Income)#p_value= 1.761e-06
ansari.test(df$Age, df$Score) #p_value=1.307e-07

#t-test
group1 <- subset(df1, Gender == "Male")$Score
group2 <- subset(df1, Gender == "Female")$Score
t.test(group1, group2)

t.test(df$Age, df$Income, var.equal = TRUE) #p_value<2.2e-16
t.test(df$Age, df$Score, var.equal = TRUE) #p_value=8.089e-08

# Linear Regression
#Age Vs Spending Score Linear Regression Analysis
plot(df$Age,df$Score,col="lightblue", pch=16,xlab="Spending Score",ylab = "Age", main="Age VS Spending Score")
cor(df$Age,df$Score, use = "pairwise.complete.obs") #output=-0.327
lm1 <- lm(Score ~ Age, data = df) #Create the linear regression
summary(lm1)
plot(df$Score,df$Age,col="lightblue", pch=16,xlab="Spending Score",ylab = "Age", main="Age VS Spending Score")
abline(lm1)  #Add a regression line

# Linear Regression Age Vs Annual Income
plot(df$Age,df$Income,col="lightblue", pch=16,xlab="Spending Score",ylab = "Age", main="Age VS Spending Score")
cor(df$Age,df$Income, use = "pairwise.complete.obs") #output=-0.327
lm2 <- lm(Income ~ Age, data = df) #Create the linear regression
summary(lm2)
plot(df$Age,df$Income,col="lightblue", pch=16,xlab="Age",ylab = "Annual Income", main="Age vs Annual Income")
abline(lm2)  #Add a regression line

#Linear Regression Annual Income Vs Spending Score
plot(df$Income, df$Score, col="lightblue",pch=16, xlab="Annual Income (k)", ylab = "Spending Score", main="Annual Income vs Spending Score")
cor(df$Income, df$Score, use = "pairwise.complete.obs") 
lm3 <- lm(Score ~ Income, data = df)
summary(lm3)
plot(df$Income, df$Score, col="lightblue",pch=16, xlab="Annual Income (k)", ylab = "Spending Score (1-100)", main="Annual Income vs Spending Score")
abline(lm3)





#To Visualise the Age Vs Spending Score Linear Regression among genders

# Subset the data for females and males
df_female <- subset(df1, Gender == "Female")
df_male <- subset(df1, Gender == "Male")

# Fit linear regression models for females and males
lm_female <- lm(Score ~ Age, data = df_female)
lm_male <- lm(Score ~ Age, data = df_male)
summary(lm_female)
summary(lm_male)

# Create a scatter plot of Age vs. Spending Score
plot(df$Age, df$Score, col = "black", pch = 16, xlab = "Age", ylab = "Spending Score", main = "Age VS Spending Score")

# Add regression lines for females and males
abline(lm_female, col = "pink", lwd = 2)
abline(lm_male, col = "skyblue", lwd = 2)

# Add a legend
legend("topright", legend = c("Female", "Male"), col = c("pink", "skyblue"), lwd = 2, bg = "white")

#To Visualise the Age Vs Annual Income Linear Regression among genders
lm2_female <- lm(Income ~ Age, data = df_female)
lm2_male <- lm(Income ~ Age, data = df_male)
summary(lm2_female)
summary(lm2_male)

# Create a scatter plot of Age vs. Annual Income
plot(df$Age, df$Income, col = "black", pch = 16, xlab = "Age", ylab = "Annual Income", main = "Age VS Annual Income")

# Add regression lines for females and males
abline(lm2_female, col = "pink", lwd = 2)
abline(lm2_male, col = "skyblue", lwd = 2)

legend("topright", legend = c("Female", "Male"), col = c("pink", "skyblue"), lwd = 2, bg = "white")

# To Visualise the Annual Income Vs Spending Score among Genders
lm3_female <- lm(Score ~ Income, data = df_female)
lm3_male <- lm(Score ~ Income, data = df_male)
summary(lm3_female)
summary(lm3_male)

# Create a scatter plot of Annual Income vs. Spending Score
plot(df$Income, df$Score, col = "black", pch = 16, xlab = "Annual Income", ylab = "Spending Score", main = "Annual Income VS Spending Score")

# Add regression lines for females and males
abline(lm3_female, col = "pink", lwd = 2)
abline(lm3_male, col = "skyblue", lwd = 2)
legend("topright", legend = c("Female", "Male"), col = c("pink", "skyblue"), lwd = 2, bg = "white")



#Fuction to divide categories for age
# First Approach using cut function categories
classify_ages <- function(df) {
  df$Age_Class <- cut(df$Age,
                      breaks = c(-Inf, 20, 25, 30, 40, 50, 60, 70, Inf),
                      labels = c('Teenager',
                                 'Young A',
                                 'Young B',
                                 'Middle Age A',
                                 'Middle Age B',
                                 'Elder A',
                                 'Elder B',
                                 'Above 70'),
                      include.lowest = TRUE,
                      right = FALSE)
  return(df)
}

df_new <-classify_ages(df)[, c("Income", "Score", "Age_Class")]
df_new

boxplot(df_new$Score ~ df_new$Age_Class,
        xlab = "Age Class",
        ylab = "Spending Score",
        main = "Age Class vs. Spending Score")

boxplot(df_new$Income ~ df_new$Age_Class,
        xlab = "Age Class",
        ylab = "Annual Income",
        main = "Age Class vs. Annual Income")

# Visualise Age_Class Disturbution
# Assuming df_new is your dataframe with Age_Class column
age_class_counts <- df_new %>%
  group_by(Age_Class) %>%
  summarise(Count = n())

ggplot(age_class_counts, aes(x = "", y = Count, fill = Age_Class)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + 
  theme_void() +
  labs(title = "Age Classes Distribution", fill = "Age Class")


# Calculate mean spending score for each age class
df_means <- df_new %>%
  group_by(Age_Class) %>%
  summarise(
    mean_score = mean(Score, na.rm = TRUE),
    .groups = 'drop'  # drops the grouping
  )

# Print the mean of Spending Score for each age class
print(df_means)


# Create the Mean Spending Score Vs Age Class
ggplot(df_means, aes(x = Age_Class, y = mean_score)) +
  geom_col(aes(fill = mean_score)) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  xlab("Age Class") +
  ylab("Mean Spending Score") +
  ggtitle("Mean Spending Score by Age Class") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#Second approach for dividing distinct age categories
#Create a function with using for loop and also if/else conditions.

age_classification <- function(df) {
  df$Age_Class <- NA
  
  for (i in 1:nrow(df)) {
    if (df$Age[i] < 20) {
      df$Age_Class[i] <- 'Teenager'
    } else if (df$Age[i] >= 20 & df$Age[i] < 25) {
      df$Age_Class[i] <- 'Young A'
    } else if (df$Age[i] >= 25 & df$Age[i] <30) {
      df$Age_Class[i] <- 'Young B'
    } else if (df$Age[i] >= 30 & df$Age[i] < 40) {
      df$Age_Class[i] <- 'Middle Age A'
    } else if (df$Age[i] >= 40 & df$Age[i] <50) {
      df$Age_Class[i] <- 'Middle Age B'
    } else if (df$Age[i] >= 50 & df$Age[i] < 60) {
      df$Age_Class[i] <- 'Elder A'
    } else if (df$Age[i] >= 60 & df$Age[i] < 70) {
      df$Age_Class[i] <- 'Elder B'
    } else {
      df$Age_Class[i] <- 'Above 70'
    }
  }
  
  return(df)
}

df_newest <- age_classification(df)[, c("Income", "Score", "Age_Class")]
df_newest
summary(df_newest)


#df_new and df_newest give the same output.



#K-means Clusterring to obtain Annual Income Vs Spending Score Relationship


#Denotes the appropriate number of clusters required.
#the location of a bend or knee is indication of optimum number of clusters.
fviz_nbclust(df, kmeans, method = "wss")
fviz_nbclust(df, kmeans, method = "silhouette")

#Performing clustering for k=6
k6 <- kmeans(df, centers = 6, nstart = 25)
str(k6)

#Income Vs Spending Score Clustering
ggplot(df, aes(x =Income, y =Score)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")

#Age Vs Spending Score Clustering
ggplot(df, aes(x =Age, y =Score)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")



#Correlation Matrix
cor_matrix <- cor(df[c("Age", "Income", "Score")])
# Create the correlation plot using ggcorrplot
title <- "Correlation Heatmap: Age, Spending Score, Annual Income"
col <- colorRampPalette(c("#BB4444", "#FFFFFF", "#77AADD"))(100)  # Specify a fixed number of colors
# Create the correlation plot using corrplot
corrplot(cor_matrix, method = "color", col = col, type = "lower",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.7,
         main = title)




