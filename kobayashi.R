#' Kobayashi Maru
#' Ahmed Mokhtar

#' Loading necessary packages
library(funModeling)
library(tidyverse)
library(MASS)
library(mice)
library(dplyr)
library(gtsummary)
library(ggplot2)
library(lme4)
library(tidyr)
library(missForest)
library(car)
library(MuMIn)
library(lmerTest)
library(car)
library(gridExtra)
library(grid)

#' Start by uploading data
rawdata <- read.csv("C:/Users/ahmed/OneDrive - University of Toronto/MBiotech/Fall 2024/Data Science II/Kobayashi/study2_n1.csv")

#' Since we're only focusing on Cohort A, I will delete all other cohorts right off 
#' the bat. 
data1 <- rawdata[rawdata$COHORT == "A", ]


# Adjust relevant column names to match the exact casings and spellings in rawdata
# Adjust relevant column names to match the exact casings and spellings in rawdata
relevant_columns <- c(
  "ptid", "AGE", 
  "T1RP", "t2rp", "t3rp", 
  "T1RT", "t2rt", "t3rt", 
  "t1ADT", "t2ADT", "t3ADT",
  "T1EQTOT", "T2EQTOT", "T3EQTOT",
  "T1DIABET", "T2DIAB", "T3DIAB",  # Correcting T1DIABET to T1DIAB
  "T1KIDNEY", "T2KIDNEY", "T3KIDNEY",
  "T1SPBONE", "T2SPBONE", "T3SPBONE", 
  "T1VIAGRA", "T2VIAGRA", "T3VIAGRA",  
  "t1heart", "T2HEART", "T3HEART",  # Correcting heart variables
  "t1sporg", "T2SPORG", "T3SPORG",  # Correcting org variables
  "t1arthritis", "t2arthritis", "t3arthritis",
  "T1CHEMO"  # Keep T1CHEMO as requested
)






# Select relevant columns
data1 <- data1 %>% select(any_of(relevant_columns))

#No missingness
status(data1)
any(is.na(data1))


# Columns to exclude based on being continuous or ID
exclude_columns <- c("ptid", "AGE", "T1EQTOT", "T2EQTOT", "T3EQTOT")

# Loop through columns and display unique values for categorical columns
for (col in names(data1)) {
  if (!(col %in% exclude_columns)) {
    cat("Unique values in", col, ":\n")
    print(unique(data1[[col]]))
    cat("\n")
  }
}

#' Seeing how many unsure ppl in each column: 
# T1SPBONE (2 = unsure)
length(data1$T1SPBONE[data1$T1SPBONE == "2"])  # Output: 8

# t1sporg (2 = unsure)
length(data1$t1sporg[data1$t1sporg == "2"])  # Output: 13

# t1arthritis (3 = unsure)
length(data1$t1arthritis[data1$t1arthritis == "3"])  # Output: 1

# T2SPBONE (2 = unsure)
length(data1$T2SPBONE[data1$T2SPBONE == "2"]) # 1

# T3SPBONE (2 = unsure)
length(data1$T3SPBONE[data1$T3SPBONE == "2"]) #0

# T2SPORG (2 = unsure)
length(data1$T2SPORG[data1$T2SPORG == "2"]) #2

# T3SPORG (2 = unsure)
length(data1$T3SPORG[data1$T3SPORG == "2"]) #1

# t2arthritis (2 = unsure)
length(data1$t2arthritis[data1$t2arthritis == "2"]) #0

# t3arthritis (2 = unsure)
length(data1$t3arthritis[data1$t3arthritis == "2"]) #0

# T1KIDNEY (2 = unsure)
length(data1$T1KIDNEY[data1$T1KIDNEY == "2"]) #1

# T2KIDNEY (2 = unsure)
length(data1$T2KIDNEY[data1$T2KIDNEY == "2"]) #0

# T3KIDNEY (2 = unsure)
length(data1$T3KIDNEY[data1$T3KIDNEY == "2"]) #0

# T1HEART (2 = unsure)
length(data1$t1heart[data1$t1heart == "2"]) #15

# T2HEART (2 = unsure)
length(data1$T2HEART[data1$T2HEART == "2"]) #0

# T3HEART (2 = unsure)
length(data1$T3HEART[data1$T3HEART == "2"]) #1

# T1VIAGRA (2 = unsure)
length(data1$T1VIAGRA[data1$T1VIAGRA == "2"]) #0

# T2VIAGRA (2 = unsure)
length(data1$T2VIAGRA[data1$T2VIAGRA == "2"]) #0

# T3VIAGRA (2 = unsure)
length(data1$T3VIAGRA[data1$T3VIAGRA == "2"]) #0

# T1DIABET (2 = unsure)
length(data1$T1DIABET[data1$T1DIABET == "2"]) #7

# T2DIAB (2 = unsure)
length(data1$T2DIAB[data1$T2DIAB == "2"]) #0

# T3DIAB (2 = unsure)
length(data1$T3DIAB[data1$T3DIAB == "2"]) #0


#' Changing those values to NAs
# Replacing "unsure" values with NA:

# T1SPBONE (2 = unsure)
data1$T1SPBONE[data1$T1SPBONE == 2] <- NA

# t1sporg (2 = unsure)
data1$t1sporg[data1$t1sporg == 2] <- NA

# t1arthritis (3 = unsure)
data1$t1arthritis[data1$t1arthritis == 3] <- NA

# T2SPBONE (2 = unsure)
data1$T2SPBONE[data1$T2SPBONE == 2] <- NA

# T3SPBONE (2 = unsure)
data1$T3SPBONE[data1$T3SPBONE == 2] <- NA

# T2SPORG (2 = unsure)
data1$T2SPORG[data1$T2SPORG == 2] <- NA

# T3SPORG (2 = unsure)
data1$T3SPORG[data1$T3SPORG == 2] <- NA

# t2arthritis (2 = unsure)
data1$t2arthritis[data1$t2arthritis == 2] <- NA

# t3arthritis (2 = unsure)
data1$t3arthritis[data1$t3arthritis == 2] <- NA

# T1KIDNEY (2 = unsure)
data1$T1KIDNEY[data1$T1KIDNEY == 2] <- NA

# T2KIDNEY (2 = unsure)
data1$T2KIDNEY[data1$T2KIDNEY == 2] <- NA

# T3KIDNEY (2 = unsure)
data1$T3KIDNEY[data1$T3KIDNEY == 2] <- NA

# T1HEART (2 = unsure)
data1$t1heart[data1$t1heart == 2] <- NA

# T2HEART (2 = unsure)
data1$T2HEART[data1$T2HEART == 2] <- NA

# T3HEART (2 = unsure)
data1$T3HEART[data1$T3HEART == 2] <- NA

# T1VIAGRA (2 = unsure)
data1$T1VIAGRA[data1$T1VIAGRA == 2] <- NA

# T2VIAGRA (2 = unsure)
data1$T2VIAGRA[data1$T2VIAGRA == 2] <- NA

# T3VIAGRA (2 = unsure)
data1$T3VIAGRA[data1$T3VIAGRA == 2] <- NA

# T1DIABET (2 = unsure)
data1$T1DIABET[data1$T1DIABET == 2] <- NA

# T2DIAB (2 = unsure)
data1$T2DIAB[data1$T2DIAB == 2] <- NA

# T3DIAB (2 = unsure)
data1$T3DIAB[data1$T3DIAB == 2] <- NA


#IMPUTATION


# Load the missForest package
# install.packages("missForest")
#library(missForest)

# Convert low-unique-value columns (likely categorical) to factors
#categorical_columns <- c("T1SPBONE", "t1sporg", "t1arthritis")  # Add other categorical columns as needed
#data1[categorical_columns] <- lapply(data1[categorical_columns], as.factor)

# Run missForest with the transformed data1 and save results directly to `data1`
#set.seed(123)
#data1 <- missForest(data1, maxiter = 5, variablewise = TRUE)$ximp

#' No other NAs. Imputation complete.
#any(is.na(data1))


# GETTING THE TOTAL DURATION OF EACH TREATMENT

#' 2 assumptions
#' 1. patients who had the treatmnet at baseline received it 
#' 6 months prior to the beginning of the study. 
#' 2. they received the treatment at the midpoint of t1-t2, or t2-t3.

status(data1)

# Create new columns for treatment durations in months based on counts across T1, T2, T3
# Create new columns for treatment durations in months based on counts across T1, T2, T3
data1 <- data1 %>%
  mutate(
    RP_duration_months = case_when(
      T1RP + t2rp + t3rp == 3 ~ 18,
      T1RP + t2rp + t3rp == 2 ~ 10.5,
      T1RP + t2rp + t3rp == 1 ~ 4.5,
      TRUE ~ 0
    ),
    RT_duration_months = case_when(
      T1RT + t2rt + t3rt == 3 ~ 18,
      T1RT + t2rt + t3rt == 2 ~ 10.5,
      T1RT + t2rt + t3rt == 1 ~ 4.5,
      TRUE ~ 0
    ),
    ADT_duration_months = case_when(
      t1ADT + t2ADT + t3ADT == 3 ~ 18,
      t1ADT + t2ADT + t3ADT == 2 ~ 10.5,
      t1ADT + t2ADT + t3ADT == 1 ~ 4.5,
      TRUE ~ 0
    )
  )

#' Create Baseline QOL column
data1 <- data1 %>%
  mutate(
    baseline_QOL = case_when(
      # No treatment until T2: average T1 and T2 QOL (if T2EQTOT exists)
      T1RP == 0 & T1RT == 0 & t1ADT == 0 & "T2EQTOT" %in% colnames(data1) ~ (T1EQTOT + T2EQTOT) / 2,
      
      # Only received treatment at T3: average T1, T2, and T3 QOL (if T2EQTOT and T3EQTOT exist)
      T1RP == 0 & T1RT == 0 & t1ADT == 0 & "T2EQTOT" %in% colnames(data1) & "T3EQTOT" %in% colnames(data1) ~ (T1EQTOT + T2EQTOT + T3EQTOT) / 3,
      
      # Default: baseline QOL as T1 QOL score
      TRUE ~ T1EQTOT
    )
  )

# Define the Final QOL score as the T3EQTOT value
data1 <- data1 %>%
  mutate(final_QOL = T3EQTOT)

# Load the necessary package for mixed-effects modeling


# Create a new variable for the change in QOL from baseline to final
data1 <- data1 %>%
  mutate(QOL_change = final_QOL - baseline_QOL)


### LONG FORM CONVERSIONS



# T1 Data Frame
df_t1 <- data1 %>%
  transmute(
    ptid = ptid,
    timepoint = "T1",
    age = AGE,
    rp = T1RP,
    rt = T1RT,
    adt = t1ADT,
    eqtot = T1EQTOT,
    diabet = T1DIABET,  # T1 diabetes variable
    kidney = T1KIDNEY,
    spbone = T1SPBONE,
    viagra = T1VIAGRA,
    heart = t1heart,
    sporg = t1sporg,
    arthritis = t1arthritis,
    chemo = T1CHEMO,  # Only available at T1
    rp_duration = NA,  # No duration at T1
    rt_duration = NA,  # No duration at T1
    adt_duration = NA,  # No duration at T1
    baseline_qol = baseline_QOL,  # Baseline QOL only for T1
    final_qol = NA,  # Not applicable for T1
    qol_change = NA  # Not applicable for T1
  )

# T2 Data Frame
df_t2 <- data1 %>%
  transmute(
    ptid = ptid,
    timepoint = "T2",
    age = AGE,  # Age remains constant
    rp = t2rp,  # T2 values for prostatectomy
    rt = t2rt,  # T2 values for radiation therapy
    adt = t2ADT,  # T2 values for ADT
    eqtot = T2EQTOT,  # T2 EQ-5D utility score
    diabet = T2DIAB,  # T2 diabetes variable
    kidney = T2KIDNEY,  # T2 kidney variable
    spbone = T2SPBONE,  # T2 bone spread
    viagra = T2VIAGRA,  # T2 Viagra use
    heart = T2HEART,  # T2 heart problems
    sporg = T2SPORG,  # T2 organ spread
    arthritis = t2arthritis,  # T2 arthritis variable
    chemo = NA,  # Chemo only available at T1
    rp_duration = NA,  # Duration will only be filled for T3
    rt_duration = NA,  # Duration will only be filled for T3
    adt_duration = NA,  # Duration will only be filled for T3
    baseline_qol = NA,  # Baseline QOL is only available for T1
    final_qol = NA,  # Final QOL is not applicable for T2
    qol_change = NA  # QOL change is only applicable for T3
  )

# T3 Data Frame
df_t3 <- data1 %>%
  transmute(
    ptid = ptid,
    timepoint = "T3",
    age = AGE,  # Age remains constant
    rp = t3rp,  # T3 values for prostatectomy
    rt = t3rt,  # T3 values for radiation therapy
    adt = t3ADT,  # T3 values for ADT
    eqtot = T3EQTOT,  # T3 EQ-5D utility score
    diabet = T3DIAB,  # T3 diabetes variable
    kidney = T3KIDNEY,  # T3 kidney variable
    spbone = T3SPBONE,  # T3 bone spread
    viagra = T3VIAGRA,  # T3 Viagra use
    heart = T3HEART,  # T3 heart problems
    sporg = T3SPORG,  # T3 organ spread
    arthritis = t3arthritis,  # T3 arthritis variable
    chemo = NA,  # Chemo only available at T1
    rp_duration = RP_duration_months,  # Duration available at T3
    rt_duration = RT_duration_months,  # Duration available at T3
    adt_duration = ADT_duration_months,  # Duration available at T3
    baseline_qol = NA,  # Baseline QOL is only available for T1
    final_qol = final_QOL,  # Final QOL available for T3
    qol_change = QOL_change  # QOL change only available for T3
  )


# Combine the three dataframes in the specified order (T1, T2, T3)
combined_df <- bind_rows(df_t1, df_t2, df_t3)

status(combined_df)











# remove chemo, 
combined_df <- combined_df %>% select(-starts_with("spbone"))
# Remove the specified columns from the dataframe
combined_df_clean <- combined_df %>%
  select(-c("chemo", "rp_duration", "rt_duration", "adt_duration", "qol_change"))
# Convert the 'timepoint' column to a factor
combined_df_clean$timepoint <- as.factor(combined_df_clean$timepoint)


# Prepare the data by converting categorical columns to factors
combined_df_clean$diabet <- as.factor(combined_df_clean$diabet)
combined_df_clean$kidney <- as.factor(combined_df_clean$kidney)
combined_df_clean$viagra <- as.factor(combined_df_clean$viagra)
combined_df_clean$heart <- as.factor(combined_df_clean$heart)
combined_df_clean$sporg <- as.factor(combined_df_clean$sporg)
combined_df_clean$arthritis <- as.factor(combined_df_clean$arthritis)

# Exclude columns that we do not want to impute (e.g., baseline_qol, final_qol)
columns_to_impute <- combined_df_clean %>%
  select(-baseline_qol, -final_qol)

# Run missForest for classification-based imputation
imputed_data <- missForest(columns_to_impute)

# Extract the imputed dataset
combined_df_clean_imputed <- imputed_data$ximp

# View the imputed dataset
head(combined_df_clean_imputed)




# Assuming `data1` is the wide-format dataframe
data1 <- data1 %>%
  mutate(
    qol_change_t2 = T2EQTOT - T1EQTOT,  # Change from T1 to T2
    qol_change_t3 = T3EQTOT - T2EQTOT   # Change from T2 to T3
  )



# Merge calculated QOL changes into the long-form dataframe
combined_df_clean_imputed <- combined_df_clean_imputed %>%
  left_join(
    data1 %>% select(ptid, qol_change_t2, qol_change_t3),  # Select ptid and calculated QOL changes
    by = "ptid"
  )

# Creating a new column `qol_change` and setting all values to 0
combined_df_clean_imputed <- combined_df_clean_imputed %>%
  mutate(qol_change = 0)

# Ensure that qol_change_t2 exists in the dataset and correctly apply to t2 rows
combined_df_clean_imputed <- combined_df_clean_imputed %>%
  mutate(qol_change = ifelse(timepoint == "T2", qol_change_t2, qol_change))

# Update qol_change for rows where timepoint is T3
combined_df_clean_imputed <- combined_df_clean_imputed %>%
  mutate(qol_change = ifelse(timepoint == "T3", qol_change_t3, qol_change))



# Fitting the model

mixed_model <- lmer(
  eqtot ~ age + timepoint + rp * rt * adt + diabet + kidney + heart + arthritis + (1 | ptid),
  data = combined_df_clean_imputed
)


# View the model summary
summary(mixed_model)

# Load necessary packages
library(broom.mixed)  # For tidying mixed-model results
library(ggplot2)

# Tidy the mixed-model results to get a data frame of coefficients
mixed_model_tidy <- tidy(mixed_model, effects = "fixed", conf.int = TRUE)

# Create the coefficient plot
ggplot(mixed_model_tidy, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(aes(color = ifelse(p.value < 0.05, "Significant", "Not Significant")), size = 1) +  # Color significant predictors differently
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Zero line for reference
  coord_flip() +  # Flip the coordinates to make the plot horizontal
  labs(
    title = "Mixed-Model Coefficients and 95% Confidence Intervals",
    x = "Predictors",
    y = "Effect Size (Estimate)",
    color = "Significance Threshold (p < 0.05)"  # Update the legend title
  ) +
  
  # Customizing the theme for background and grid lines
  theme_minimal(base_size = 14) +  # Base font size for better readability
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray80", size = 0.5),  # Lighten the major grid lines
    panel.grid.minor = element_line(color = "gray90", size = 0.3),  # Lighten the minor grid lines
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  # Rotating x-axis labels for clarity
    panel.grid.major.x = element_line(color = "gray80", size = 0.3, linetype = "dashed"),  # Lighten the x-axis major grid lines
    panel.grid.minor.x = element_line(color = "gray90", size = 0.2),  # Add lighter minor grid lines for the x-axis
    
    # Positioning and resizing the legend
    legend.position = c(0.95, 0.95),  # Move the legend to the top-right corner
    legend.justification = c(1, 1),   # Align the legend at the top-right
    legend.text = element_text(size = 8),  # Make the legend text smaller
    legend.title = element_text(size = 9),  # Slightly smaller title for the legend
    legend.key.size = unit(0.5, "cm"),  # Make the legend box smaller
    
    # Put the legend inside a box with a white background
    legend.box = "rect",  # Add a box around the legend
    legend.background = element_rect(fill = "white", color = "black")  # White background with black border
  ) +
  
  # Adding more x-axis labels for detailed intervals
  scale_y_continuous(breaks = seq(-1, 2, by = 0.1)) +  # Adjust breaks for more detailed x-axis labels
  
  # Customize colors for significant and non-significant values
  scale_color_manual(values = c("Significant" = "#2ca02c", "Not Significant" = "#d62728"))  # Set colors for significance



# Add significance annotations (optional, based on model results)


# Assumption checks

# 1. Linearity
plot(fitted(mixed_model), residuals(mixed_model),
     xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# 2. Normality of residuals
qqnorm(residuals(mixed_model), main = "Q-Q Plot of Residuals")
qqline(residuals(mixed_model), col = "red")

# 3. Homoscedasticity (constant variance)
plot(fitted(mixed_model), abs(residuals(mixed_model)),
     xlab = "Fitted values", ylab = "Absolute Residuals", main = "Absolute Residuals Plot")
abline(h = 0, col = "red")

# 4. Independence (ACF plot of residuals)
acf(residuals(mixed_model), main = "ACF Plot of Residuals")

# 5. Random effects normality (for ptid random effect)
ranef_ptid <- ranef(mixed_model)$ptid[, "(Intercept)"]
qqnorm(ranef_ptid, main = "Q-Q Plot of Random Effects (ptid)")
qqline(ranef_ptid, col = "red")

# 6 # Fit a linear model (without random effects) for VIF calculation
lm_model <- lm(eqtot ~ age + rp + rt + adt + diabet + kidney + heart + arthritis, data = combined_df_clean_imputed)

# Calculate VIF
vif_values <- vif(lm_model)

# Print the VIF values
print(vif_values)














# Second model


# Fit a linear model with eqtot as the response
lm_eqtot <- lm(eqtot ~ age + rp * rt * adt + diabet + kidney + heart + arthritis, data = combined_df_clean_imputed)

# Calculate Cook's distance
cooksd_eqtot <- cooks.distance(lm_eqtot)

# Plot Cook's distance to visualize influential points
plot(cooksd_eqtot, main = "Cook's Distance Plot for eqtot", ylab = "Cook's Distance")
abline(h = 4 / length(cooksd_eqtot), col = "red")  # Threshold for influential points

# Identify influential points (Cook's distance > 4 / number of observations)
influential_points_eqtot <- which(cooksd_eqtot > (4 / length(cooksd_eqtot)))

# View the influential points
print(influential_points_eqtot)

# Remove influential points from the dataset
combined_df_clean_imputed_no_influential <- combined_df_clean_imputed[-influential_points_eqtot, ]

# Fit the mixed-effects model without the influential points
mixed_model_no_influential <- lmer(
  eqtot ~ age + timepoint + rp * rt * adt + diabet + kidney + heart + arthritis + (1 | ptid),
  data = combined_df_clean_imputed_no_influential
)

# View the model summary
summary(mixed_model_no_influential)


# 1. Linearity: Residuals vs Fitted
plot(fitted(mixed_model_no_influential), residuals(mixed_model_no_influential),
     xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# 2. Normality of residuals: Q-Q plot
qqnorm(residuals(mixed_model_no_influential), main = "Q-Q Plot of Residuals")
qqline(residuals(mixed_model_no_influential), col = "red")

# 3. Homoscedasticity (constant variance): Scale-Location Plot
plot(fitted(mixed_model_no_influential), abs(residuals(mixed_model_no_influential)),
     xlab = "Fitted values", ylab = "Absolute Residuals", main = "Scale-Location Plot")
abline(h = 0, col = "red")

# 4. Independence: ACF Plot of Residuals
acf(residuals(mixed_model_no_influential), main = "ACF Plot of Residuals")

# 5. Random effects normality: Q-Q Plot for ptid random effects
ranef_ptid_no_influential <- ranef(mixed_model_no_influential)$ptid[, "(Intercept)"]
qqnorm(ranef_ptid_no_influential, main = "Q-Q Plot of Random Effects (ptid)")
qqline(ranef_ptid_no_influential, col = "red")





# Comparing both models

# AIC and BIC for both models
AIC(mixed_model, mixed_model_no_influential)
BIC(mixed_model, mixed_model_no_influential)

# Compute R-squared for both models
r.squaredGLMM(mixed_model)
r.squaredGLMM(mixed_model_no_influential)






# First, calculate the means and medians of EQTOT for each combination of timepoint and treatment
mean_median_eqtot <- combined_df_clean_imputed %>%
  group_by(timepoint, rp, rt, adt) %>%
  summarise(mean_eqtot = mean(eqtot, na.rm = TRUE),
            median_eqtot = median(eqtot, na.rm = TRUE)) %>%  # Calculate the median
  ungroup()

# Reshape the data for plotting (we'll melt it for easier ggplot manipulation)
mean_median_eqtot_long <- mean_median_eqtot %>%
  pivot_longer(cols = c(rp, rt, adt), names_to = "treatment", values_to = "value")

# Filter only the treatment variables where treatment was applied (value == 1)
mean_median_eqtot_long <- mean_median_eqtot_long %>% filter(value == 1)

# Set the new color mapping for the treatments (more subtle colors)
color_mapping <- c("rp" = "#6baed6", "rt" = "#74c476", "adt" = "#fd8d3c")  # Light blue, green, and orange

# Create the bar plot with means and add a horizontal line for the median on top
ggplot(mean_median_eqtot_long, aes(x = timepoint, y = mean_eqtot, fill = treatment)) +
  
  # Plot the bars for mean EQTOT
  geom_bar(stat = "identity", position = "dodge") +
  
  # Add horizontal lines for the median using geom_segment for each treatment and timepoint
  geom_segment(aes(x = as.numeric(timepoint) - 0.2, 
                   xend = as.numeric(timepoint) + 0.2, 
                   y = median_eqtot, yend = median_eqtot),
               color = "black", size = 1.2) +  # One bold black line for the median
  
  # Set color mapping for treatments
  scale_fill_manual(values = color_mapping) +
  
  # Add labels and theme customizations
  labs(title = "Mean EQTOT Scores by Treatment and Timepoint (Median Marked)",
       x = "Timepoint",
       y = "Mean EQTOT",
       fill = "Treatment") +
  
  theme_minimal(base_size = 14) +  # Updated theme with a minimal look and larger base font size
  ylim(0, 1.2) +  # Set the y-axis limits to 0-1.2
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center the title and make it bold
    axis.title.x = element_text(face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(face = "bold"),  # Bold y-axis title
    legend.position = "top",  # Move legend to the top
    legend.title = element_text(face = "bold")  # Bold legend title
  )






### MACHINE LEARNING MODELS

# Load necessary package
library(geepack)

# Fit a GEE model
gee_model <- geeglm(eqtot ~ age + timepoint + rp * rt * adt + diabet + kidney + heart + arthritis,
                    data = combined_df_clean_imputed,
                    id = ptid,             # Patient ID as the grouping factor
                    corstr = "exchangeable") # Assume correlation structure between repeated measures

# Summary of the model
summary(gee_model)

plot(fitted(gee_model), residuals(gee_model), 
     xlab = "Fitted values", ylab = "Residuals", 
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")


# Step 2: Extract the coefficients and confidence intervals using tidy()
gee_model_tidy <- tidy(gee_model, conf.int = TRUE)

# Step 3: Create a coefficient plot
ggplot(gee_model_tidy, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(aes(color = ifelse(p.value < 0.05, "Significant", "Not Significant")), size = 1) +  # Color significant predictors
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Zero line for reference
  coord_flip() +  # Flip coordinates for better readability
  labs(
    title = "GEE Model Coefficients and 95% Confidence Intervals",
    x = "Predictors",
    y = "Effect Size (Estimate)",
    color = "Significance Threshold (p < 0.05)"  # Legend title
  ) +
  # Customizing the theme for the plot
  theme_minimal(base_size = 14) +  # Base font size
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center the title
    axis.title.x = element_text(face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(face = "bold"),  # Bold y-axis title
    panel.grid.major = element_line(color = "gray80", size = 0.5),  # Lighten major grid lines
    panel.grid.minor = element_line(color = "gray90", size = 0.3),  # Lighten minor grid lines
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  # Rotate x-axis labels for clarity
    panel.grid.major.x = element_line(color = "gray80", size = 0.3, linetype = "dashed"),  # Dashed x-axis major grid lines
    panel.grid.minor.x = element_line(color = "gray90", size = 0.2),  # Light minor grid lines on x-axis
    
    # Positioning and resizing the legend
    legend.position = c(0.95, 0.95),  # Move the legend to top-right corner
    legend.justification = c(1, 1),   # Align legend to top-right corner
    legend.text = element_text(size = 8),  # Smaller legend text
    legend.title = element_text(size = 9),  # Slightly smaller legend title
    legend.key.size = unit(0.5, "cm"),  # Smaller legend boxes
    
    # Put the legend inside a box with a white background
    legend.box = "rect",  # Add a box around the legend
    legend.background = element_rect(fill = "white", color = "black")  # White background with black border for legend
  ) +
  
  # Adding more y-axis labels for better detail
  scale_y_continuous(breaks = seq(-1, 2, by = 0.1)) +  # Adjust breaks for y-axis labels
  
  # Customize colors for significant and non-significant values
  scale_color_manual(values = c("Significant" = "#2ca02c", "Not Significant" = "#d62728"))  # Set colors for significance



##### Kruskal Wallas


# Group data by timepoint and perform Kruskal-Wallis test
kruskal_result <- kruskal.test(qol_change ~ timepoint, data = combined_df_clean_imputed)

# Display the result
print(kruskal_result)

library(dunn.test)
# Perform Dunn's test for pairwise comparisons
dunn_result <- dunn.test(combined_df_clean_imputed$qol_change, combined_df_clean_imputed$timepoint, method = "bonferroni")

# Create a data frame with the Dunn's test results
dunn_results_df <- data.frame(
  Comparison = c("T1 vs T2", "T1 vs T3", "T2 vs T3"),
  Z_score = c(1.039752, -1.388428, -2.428180),
  p_value = c(0.4477, 0.2475, 0.0228),
  Significant = c("No", "No", "Yes")  # Based on p < 0.05 (Bonferroni adjusted)
)

# Create the table using tableGrob and remove row labels
table <- tableGrob(dunn_results_df, rows = NULL)

# Add title to the table
title <- textGrob("Dunn's Test Results (Bonferroni Adjusted)", gp = gpar(fontsize = 14, fontface = "bold"))

# Combine the title and table, and plot them together
grid.arrange(title, table, nrow = 2)




# Generate the histogram
ggplot(combined_df_clean_imputed, aes(x = qol_change)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  labs(
    title = "Histogram of QOL Change",
    x = "QOL Change",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )
library(ggplot2)
library(dplyr)





# Generate a faceted histogram for QOL change by timepoint
ggplot(combined_df_clean_imputed, aes(x = qol_change, fill = timepoint)) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.6) +  # Adjust alpha for transparency
  facet_wrap(~ timepoint, ncol = 1) +  # Separate plots by timepoint in a single column
  labs(
    title = "Histogram of QOL Change by Timepoint",
    x = "QOL Change",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    strip.text = element_text(face = "bold")  # Bold labels for each timepoint
  )

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Filter out T1 from the data
qol_t2_t3 <- combined_df_clean_imputed %>%
  filter(timepoint %in% c("T2", "T3"))

# Custom labels for T2 and T3 facets
facet_labels <- c("T2" = "T1-T2 EQ-5D Change", "T3" = "T2-T3 EQ-5D Change")

# Generate the faceted histogram for QOL change by timepoint (only T2 and T3)
ggplot(qol_t2_t3, aes(x = qol_change, fill = timepoint)) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.6) +  # Adjust alpha for transparency
  facet_wrap(~ timepoint, ncol = 1, labeller = as_labeller(facet_labels)) +  # Custom facet labels
  labs(
    title = "Histogram of QOL Change for T2 and T3",
    x = "QOL Change",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    strip.text = element_text(face = "bold")  # Bold labels for each timepoint
  )

















# Install car package if you haven't already
install.packages("car")

# Load the car package
library(car)

# Levene's test for equal variances
leveneTest(qol_change ~ timepoint, data = combined_df_clean_imputed)


