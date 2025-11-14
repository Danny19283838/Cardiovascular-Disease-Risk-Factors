#Installation of Packages
install.packages("ggplot2")
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
install.packages("ggpubr")
library(ggpubr)
install.packages("ggsignif")
library(ggsignif)
install.packages("arsenal")
library(arsenal)




#Separating Total Cholesterol into Categories
cvd_data <- cvd_data %>%  
  mutate(category_cholestral = case_when(      
    
    Total.Cholesterol..mg.dL. >= 240 ~ "Dangerous",
    Total.Cholesterol..mg.dL. >= 200 & Total.Cholesterol..mg.dL. <= 239 ~ "At-Risk",    
    Total.Cholesterol..mg.dL. < 200 ~ "Heart Healthy",
    TRUE ~ "Unknown"
    # Catch-all for unexpected cases
  ))


#Seperating Systolic BP into Categories
cvd_data <- cvd_data %>%  
  mutate(category_systolic = case_when(     
    
    Systolic.BP < 120  ~ "Normal",
    Systolic.BP >= 120 & Systolic.BP <= 129  ~ "Elevated",
    Systolic.BP >= 130 & Systolic.BP <= 139  ~ "Stage 1 HTN",
    Systolic.BP >= 140 & Systolic.BP <= 180 ~ "Stage 2 HTN",
    Systolic.BP > 180  ~ "Hypertensive Crisis",
    
    # Catch-all for unexpected cases
  ))


# Pie Chart of Systolic Blood Pressure Categories 
cvd_data$category_systolic <- factor(cvd_data$category_systolic, levels = c("Normal", "Elevated", "Stage 1 HTN", "Stage 2 HTN", "Hypertensive Crisis"))
category_systolic <- cvd_data %>%
  count(category_systolic)%>%  
  mutate(Percentage = n / sum(n) * 100,  # Calculate percentage         
         Label = paste0(category_systolic, " ", round(Percentage, 1), "%"))  # Format label
ggplot(category_systolic, aes(x = "", y = n, fill = category_systolic)) +  
  geom_bar(stat = "identity", width = 1) +  # Create bars (converted to pie slices)
  coord_polar("y", start = 0) +  # Convert to pie chart  
  labs(title = "Pie Chart of Systolic Blood Pressure Category Distribution", x = NULL, y = NULL) +  
  theme_void() +  # Remove axis and gridlines  
  scale_fill_manual(values = c("Normal" = "green", "Elevated" = "yellow" , "Stage 1 HTN" = "orange", "Stage 2 HTN" = "red", "Hypertensive Crisis" = "darkred" )) +  
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 4, color = "black")  # Add percentage labels


# Pie Chart of Total Cholesteral Categories 
cvd_data$category_cholestral <- factor(cvd_data$category_cholestral, levels = c("Heart Healthy", "At-Risk", "Dangerous"))
category_cholestral <- cvd_data %>%
  count(category_cholestral)%>%  
  mutate(Percentage = n / sum(n) * 100,  # Calculate percentage         
         Label = paste0(category_cholestral, " ", round(Percentage, 1), "%"))  # Format label
ggplot(category_cholestral, aes(x = "", y = n, fill = category_cholestral)) +  
  geom_bar(stat = "identity", width = 1) +  # Create bars (converted to pie slices)
  coord_polar("y", start = 0) +  # Convert to pie chart  
  labs(title = "Pie Chart of Total Cholesteral Category Distribution", x = NULL, y = NULL) +  
  theme_void() +  # Remove axis and gridlines  
  scale_fill_manual(values = c("Heart Healthy" = "green", "At-Risk" = "yellow" , "Dangerous" = "red")) +  
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 4, color = "black")  # Add percentage labels



# Histogram of CVD Risk Score (Smoker vs. Non-Smoker)
cvd_data$Smoking.Status <- factor(cvd_data$Smoking.Status, levels = c("Y", "N")) 
ggplot(cvd_data, aes(x = CVD.Risk.Score)) +  
geom_histogram(bins = 10, fill = "blue", color = "black", alpha = 0.2) + 
facet_wrap(~factor(Smoking.Status, levels = c("Y", "N"))) +
labs(title = "Histograms of CVD Risk Score Amongst Smokers (Y) vs. Non-Smokers (N)", x = "CVD Risk Score", y = "Frequency") +  
theme_bw(base_size = 14)
                

#Density Plot of CVD Risk Score (Diabetics vs. Non-Diabetics)
cvd_data$Diabetes.Status <- factor(lung_cancer$Diabetes.Status, levels = c("Y", "N")
ggplot(cvd_data, aes(x = CVD.Risk.Score)) +  
geom_density(fill = "purple", alpha = 0.3) +
facet_wrap(~factor(Diabetes.Status, levels = c("Y", "N"))) +  
labs(title = "Density Plots of CVD Risk Score Amongst Diabetics (Y) vs. Non-Diabetics (N)", 
x = "CVD Risk Score", y = "Density") +  
theme_bw(base_size = 14)


#Scatterplots showing relationship between Level of Physical Activity, Systolic BP, and CVD Risk Score
cvd_data$Physical.Activity.Level <- factor(lung_cancer$Physical.Activity.Level, levels = c("Low", "Moderate", "High")
ggplot(cvd_data, aes(x = Systolic.BP , y = CVD.Risk.Score)) +  geom_point() +
geom_point(alpha = 0.4, size = 2) +
geom_smooth(method = "lm", se = TRUE, linetype = "solid", size = 1.2, alpha = 0.3) +  # Regression line with confidence interval  
facet_wrap(~factor(Physical.Activity.Level, levels = c("Low", "Moderate", "High"))) +  #ncol by default; ncol = 2 arranges facets into 2 columns.
stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top", size = 5) +
labs(title = "Scatterplots Representing Relationship Between Level of Physical Activity (Low, Moderate, High), Systolic BP, and CVD Risk Score", x = "Systolic BP (mmHg)", y = "CVD Risk Score") + 
theme_bw()


#Scatterplots showing relationship between Level of Physical Activity, Total Cholesterol, and CVD Risk Score
cvd_data$Physical.Activity.Level <- factor(lung_cancer$Physical.Activity.Level, levels = c("Low", "Moderate", "High")
ggplot(cvd_data, aes(x = Total.Cholesterol..mg.dL., y = CVD.Risk.Score)) +
geom_point(alpha = 0.4, size = 2) +
geom_smooth(method = "lm", se = TRUE, linetype = "solid", size = 1.2, alpha = 0.3) +  # Regression line with confidence interval  
facet_wrap(~factor(Physical.Activity.Level, levels = c("Low", "Moderate", "High"))) +  #ncol by default; ncol = 2 arranges facets into 2 columns.
stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top", size = 5) +
labs(title = "Scatterplots Representing Relationship Between Level of Physical Activity (Low, Moderate, High), Total Cholesterol, and CVD Risk Score", x = "Total Cholesterol (mg/dl)", y = "CVD Risk Score") + 
theme_bw()













#Creation of a Summary Report
controls_report <- tableby.control(test = FALSE, total = FALSE,
                                   numeric.test= "t", cat.test= "chisq",
                                   numeric.stats = c("Nmiss", "median", "q1q3",
                                                     "range"),
                                   cat.stats = c("Nmiss","countpct"),
                                   stats.labels = list(median="Median", q1q3="Q
                            1,Q3", range = "Range"))

statistical_report <- tableby( ~ Age + Sex + BMI  + Smoking.Status +  Diabetes.Status +  Systolic.BP +  Blood.Pressure.Category + CVD.Risk.Score + Total.Cholesterol..mg.dL. 
                               ,
                               data=cvd_data ,
                               digits=1 ,
                               test=TRUE,
                               digits.p=2,
                               total = T,
                               control=controls_report)
summary(statistical_report,
        test = TRUE,
        labelTranslations = report_labels,
        control = controls_report,
        title = "Summary: Overall", text = TRUE)
write2word(statistical_report,labelTranslations=report_labels,("Report_summary.doc"))                                           
getwd()











