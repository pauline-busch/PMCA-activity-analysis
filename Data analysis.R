library(dplyr)
library(readxl)
library(openxlsx)
library(ggplot2)
library(ggthemes)
library(ggpubr)

# Read in the file that contains the baseline data
baseline_data <- read_xlsx("C:/Users/miri/Documents/GitHub/R-graphs/Flow cytometry analysis/events-Jerone Van1.xlsx")
PMCA_data <- read_xlsx("C:/Users/miri/Documents/GitHub/R-graphs/Flow cytometry analysis/events-Jerone Van1.1.xlsx")

# Baseline data -----------------------------------------------------------
# Bin data (1 second intervals - median)
baseline_data <- baseline_data %>%
  mutate(TimeBin = floor(Time)) %>%
  group_by(TimeBin) %>%
  summarise(across(where(is.numeric), ~median(.x, na.rm = TRUE)))

# Save the binned data as excel file
write.xlsx(baseline_data, "C:/Users/miri/Documents/GitHub/R-graphs/Flow cytometry analysis/binned-Jerone Van1.xlsx")

# Define the time interval for the baseline
start_time <- 200
end_time <- 300

baseline_region <- baseline_data %>%
  filter(TimeBin >= start_time & TimeBin <= end_time)

# Calculate the baseline equation
lin_reg <- lm(FL1 ~ TimeBin, 
              data = baseline_region)

lin_eq <- paste(
  round(coef(lin_reg)[2], 2), 
  "* x", 
  "+", 
  round(coef(lin_reg)[1], 2)
  )

R_squared <- summary(lin_reg)$r.squared

# Plot your data
calcium_baseline <- ggplot(baseline_data, aes(TimeBin, FL1)) +
  
  geom_line() +
  
  geom_smooth(data = baseline_region, 
              method = "lm", 
              se = FALSE, 
              color = "red", 
              formula = y ~ x) +
  
  annotate("text", 
           x = (start_time + end_time) / 2, 
           y = max(baseline_region$FL1), 
           label = lin_eq, 
           color = "red") +
  
  labs(title = "Calcium kinetics with regression line",
       x = "Time [seconds]",
       y = "FL1 [RFU]")

# Save your plot as an image
ggsave("FC_data.tiff", 
       path = "C:/Users/miri/Documents/GitHub/R-graphs/Flow cytometry analysis/graphs", 
       units = "in", 
       dpi=300, 
       compression = 'lzw',
       width = 7, 
       height = 5)


# PMCA activity data ------------------------------------------------------
# Bin data (1 second intervals - median)
PMCA_data <- PMCA_data %>%
  mutate(TimeBin = floor(Time)) %>%
  group_by(TimeBin) %>%
  summarise(across(where(is.numeric), ~median(.x, na.rm = TRUE)))

# Save the binned data as excel file
write.xlsx(PMCA_data, "C:/Users/miri/Documents/GitHub/R-graphs/Flow cytometry analysis/binned-Jerone Van1.1.xlsx")

# Define the time interval for the baseline
start_time_2 <- 6
end_time_2 <- 36

PMCA_region <- PMCA_data %>%
  filter(TimeBin >= start_time_2 & TimeBin <= end_time_2)

# Calculate the baseline equation
lin_reg_2 <- lm(FL1 ~ TimeBin, 
              data = PMCA_region)

lin_eq_2 <- paste(
  round(coef(lin_reg_2)[2], 2), 
  "* x", 
  "+", 
  round(coef(lin_reg_2)[1], 2)
)

R_squared_2 <- summary(lin_reg_2)$r.squared

# Plot your data
calcium_PMCA <- ggplot(PMCA_data, aes(TimeBin, FL1)) +
  
  geom_line() +
  
  geom_smooth(data = PMCA_region, 
              method = "lm", 
              se = FALSE, 
              color = "red", 
              formula = y ~ x) +
  
  annotate("text", 
           x = end_time_2 * 2, 
           y = max(PMCA_region$FL1), 
           label = lin_eq_2, 
           color = "red") +
  
  labs(title = "Calcium kinetics with regression line",
       x = "Time [seconds]",
       y = "FL1 [RFU]")

# Save your plot as an image
ggsave("FC_data_2.tiff", 
       path = "C:/Users/miri/Documents/GitHub/R-graphs/Flow cytometry analysis/graphs", 
       units = "in", 
       dpi=300, 
       compression = 'lzw',
       width = 7, 
       height = 5)

