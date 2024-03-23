getwd()
library(tidyverse)

income <-read_csv('income.csv')
view(income)

##########
#Simple Linear Regression
#Predicting income from years of experience
incomeslr2 <- lm(income ~ experience, data = income)
summary(incomeslr2)

#plot of single linear regression
incomeslrplot2 <- ggplot(income, aes(x = experience, y = income)) +
  geom_point() +
  labs(title = "Income by Years of Experience",
       x = "Years of Experience",
       y = "Income in Dollars") +
  theme(plot.title=element_text(hjust=0.5))
incomeslrplot2

#values for line
slrintercept2 <- 28248.45
slrslope2 <- 2014.04
income$fitted2 <- slrintercept2 + slrslope2 * income$experience

#plot of single linear regression with fitted line
incomeslrline2 <- ggplot(income, aes(x = experience, y = income)) +
  geom_point() +
  labs(title = "Income by Years of Experience",
       x = "Years of Experience",
       y = "Income in Dollars") +
  geom_abline(slope = slrslope2, intercept = slrintercept2, color = "blue") +
  theme(plot.title=element_text(hjust=0.5))
incomeslrline2

#single linear regression plot with residuals
incomeslrresid2 <- ggplot(income, aes(x = experience, y = income)) +
  labs(title = "Income by Years of Experience",
       x = "Years of Experience",
       y = "Income in Dollars") +
  geom_abline(slope = slrslope2, intercept = slrintercept2, color = "blue") +
  geom_segment(aes(xend = experience, yend = fitted2, color = "resid")) +
  geom_point() +
  scale_color_manual(values = c(resid = "gray"), labels = c(resid = "residuals")) +
  theme(plot.title=element_text(hjust=0.5))
incomeslrresid2

#same slr plot with residuals but with no gridlines
incomeslrresidnogrid2 <- ggplot(income, aes(x = experience, y = income)) +
  labs(title = "Income by Years of Experience",
       x = "Years of Experience",
       y = "Income in Dollars") +
  geom_abline(slope = slrslope2, intercept = slrintercept2, color = "blue") +
  geom_segment(aes(xend = experience, yend = fitted2, color = "resid")) +
  geom_point() +
  scale_color_manual(values = c(resid = "gray"), labels = c(resid = "residuals")) +
  theme(panel.background = element_blank(),
        plot.title=element_text(hjust=0.5))
incomeslrresidnogrid2

#Single Linear Regression example 2
#Predicting income from age

incomeslr <- lm(income ~ age, data = income)
summary(incomeslr)

#plot of single linear regression
incomeslrplot <- ggplot(income, aes(x = age, y = income)) +
  geom_point() +
  labs(title = "Income by Age",
       x = "Age",
       y = "Income in Dollars") +
  theme(plot.title=element_text(hjust=0.5))
incomeslrplot

#values for line
slrintercept <- 22975.2
slrslope <- 447.9
income$fitted <- slrintercept + slrslope * income$age

#plot of single linear regression with fitted line
incomeslrline <- ggplot(income, aes(x = age, y = income)) +
  geom_point() +
  labs(title = "Income by Age",
       x = "Age",
       y = "Income in Dollars") +
  geom_abline(slope = slrslope, intercept = slrintercept, color = "blue") +
  theme(plot.title=element_text(hjust=0.5))
incomeslrline

#single linear regression plot with residuals
incomeslrresid <- ggplot(income, aes(x = age, y = income)) +
  labs(title = "Income by Age",
       x = "Age",
       y = "Income in Dollars") +
  geom_abline(slope = slrslope, intercept = slrintercept, color = "blue") +
  geom_segment(aes(xend = age, yend = fitted, color = "resid")) +
  geom_point() +
  scale_color_manual(values = c(resid = "gray"), labels = c(resid = "residuals")) +
  theme(plot.title=element_text(hjust=0.5))
incomeslrresid

#same slr plot with residuals but with no gridlines
incomeslrresidnogrid <- ggplot(income, aes(x = age, y = income)) +
  labs(title = "Income by Age",
       x = "Age",
       y = "Income in Dollars") +
  geom_abline(slope = slrslope, intercept = slrintercept, color = "blue") +
  geom_segment(aes(xend = age, yend = fitted, color = "resid")) +
  geom_point() +
  scale_color_manual(values = c(resid = "gray"), labels = c(resid = "residuals")) +
  theme(panel.background = element_blank(),
        plot.title=element_text(hjust=0.5))
incomeslrresidnogrid


###############
#Multiple Linear Regression

#Installing a house prices dataset provided through an R package
install.packages("KingCountyHouses")
library(KingCountyHouses)

view(home_prices)
#saving home_prices tibble as a dataframe
houses <- home_prices

#converting sales price to dollars
houses$adjprice <- 10^houses$price
view(houses$adjprice)

#multiple linear regression equation
housesmlr <- lm(adjprice ~ sqft_living + bathrooms + bedrooms, data = houses, na.action = na.omit)
summary(housesmlr)

#Example for Regression tree
#data
drug <-read_csv('drug effectiveness.csv')

#Visualization of non-linear data
drugreg1 <- ggplot(drug, aes(x = Drug_Dosage_mg, y = Drug_Effectiveness_per)) +
  labs(title = "Effectiveness of Drug by Dosage",
       x = "Drug Dosage (mg)",
       y = "Drug Effectiveness (%)") +
  geom_point() +
  theme(plot.title=element_text(hjust=0.5))
drugreg1

#calculating a line for the non-linear data
drugregstat <- lm(Drug_Effectiveness_per ~ Drug_Dosage_mg, data = drug)
summary(drugregstat)
drugint <- 27.142
drugsl <- 0.487
drug$fitted <- drugint + drugsl * drug$Drug_Dosage_mg

#visualizing a line on the non-linear data
drugreg2 <- ggplot(drug, aes(x = Drug_Dosage_mg, y = Drug_Effectiveness_per)) +
  labs(title = "Effectiveness of Drug by Dosage",
       x = "Drug Dosage (mg)",
       y = "Drug Effectiveness (%)") +
  geom_abline(slope = drugsl, intercept = drugint, color = "blue") +
  geom_point() +
  theme(plot.title=element_text(hjust=0.5))
drugreg2

#visualizing the residuals 
drugreg3 <- ggplot(drug, aes(x = Drug_Dosage_mg, y = Drug_Effectiveness_per)) +
  labs(title = "Effectiveness of Drug by Dosage",
       x = "Drug Dosage (mg)",
       y = "Drug Effectiveness (%)") +
  geom_abline(slope = drugsl, intercept = drugint, color = "blue") +
  geom_segment(aes(xend = Drug_Dosage_mg, yend = fitted, color = "resid")) +
  geom_point() +
  scale_color_manual(values = c(resid = "gray"), labels = c(resid = "residuals")) +
  theme(plot.title=element_text(hjust=0.5))
drugreg3


#Logistic Regression
#loading in the data on admittance to college
admit <- read_csv('https://stats.idre.ucla.edu/stat/data/binary.csv')

#formula for logistic regression
logitadmit <- glm(admit ~ gre + gpa, data = admit, family="binomial")
summary(logitadmit)

# =============================================================

#testing model fit across different datasets
# Given data
data <- data.frame(
  x = c("blue", "yellow", "red", "green"),
  y = c(1.0, 4.0, 1, 3.0)
)
data$x <- factor(data$x, levels = c("blue", "yellow", "red", "green"))

# Create a scatter plot with ggplot2
ggplot(data, aes(x = x, y = y)) +
  geom_point(size = 3) +
  labs(x = "Favorite Color", y = "GPA") +
  theme_bw() + 
  theme(axis.text = element_text(size = 12))
ggsave('/Users/hge/Desktop/Intro_Stats_Model_Workshop/data_vis.pdf')

# Display the plot
ggplot(data, aes(x = factor(x), y = y)) +
  geom_point(size = 3) +
  geom_line(aes(group = 1), linewidth = 1, linetype = "solid", color = "black") +
  labs(x = "Favorite Color", y = "GPA") +
  theme_bw() + 
  theme(axis.text = element_text(size = 12))
ggsave('/Users/hge/Desktop/Intro_Stats_Model_Workshop/perf_mod.pdf')



# Convert x to factor
# Create a scatter plot with ggplot2
p <- ggplot(data, aes(x = x, y = y)) +
  geom_point(size = 3) +
  labs(x = "Favorite Color", y = "GPA")

# Calculate and add the linear regression line
lm_eqn <- lm(y ~ as.numeric(x), data = data)
sum((lm_eqn$fitted.values - c(1,4,1,3))^2)/4
p + geom_abline(intercept = coef(lm_eqn)[1], slope = coef(lm_eqn)[2], linetype = "solid", color = "black") +
  theme_bw() +
  theme(axis.text = element_text(size = 12))  # Adjust the font size as needed
ggsave('/Users/hge/Desktop/Intro_Stats_Model_Workshop/linear_mod.pdf')

# ==============================================================================

# Given data
dataB <- data.frame(
  x = c("blue", "yellow", "red", "green"),
  y = c(4.0, 1.0, 3, 1.0)
)
dataB$x <- factor(dataB$x, levels = c("blue", "yellow", "red", "green"))

# Create a scatter plot with ggplot2
ggplot(dataB, aes(x = x, y = y)) +
  geom_point(size = 3, shape = 1) +
  labs(x = "Favorite Color", y = "GPA") +
  theme_bw() + 
  theme(axis.text = element_text(size = 12))
ggsave('/Users/hge/Desktop/Intro_Stats_Model_Workshop/data_vis_B.pdf')



# Display the plot
ggplot(dataB, aes(x = x, y = y)) +
  geom_point(size = 3, shape = 1) +
  geom_line(data = data, aes(x = x, y=y, group=1), linewidth = 1, linetype = "solid", color = "black") +
  labs(x = "Favorite Color", y = "GPA") +
  theme_bw() + 
  theme(axis.text = element_text(size = 12))
ggsave('/Users/hge/Desktop/Intro_Stats_Model_Workshop/perf_mod_B.pdf')


# Create a scatter plot with ggplot2
pB <- ggplot(dataB, aes(x = x, y = y)) +
  geom_point(size = 3, , shape = 1) +
  labs(x = "Favorite Color", y = "GPA")

# Calculate and add the linear regression line
pB + geom_abline(intercept = coef(lm_eqn)[1], slope = coef(lm_eqn)[2], linetype = "solid", color = "black") +
  theme_bw() +
  theme(axis.text = element_text(size = 12))  # Adjust the font size as needed
ggsave('/Users/hge/Desktop/Intro_Stats_Model_Workshop/linear_mod_B.pdf')


