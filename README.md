## Load the required R libraries
We need some R packages to read in excel files and make a nice graph. These are included in the following libraries:


``` r
library(dplyr)
library(readxl)
library(openxlsx)
library(ggplot2)
library(ggthemes)
library(ggpubr)
```

## Read in your data
The first step in your analysis is to upload your FCS-files to Floreada.io and to export events (by right clicking the files). You will now have .csv files. Save those files together in a folder as .xlsx documents. Rename the columns of interest to something easy. For example rename "FL1::FL1-A" to "FL1". Now you are ready to read in your data to R. Copy the path of the folder that contains all the files and put it into the code block. To read in the different excel files you need to add the file name to the path (e.g., add "/events-Jerone Van1.xlsx") **IMPORTANT** - Your file path needs to have "/" instead of backslashes (you can use the find and replace function for this by pressing strg + f). 


``` r
baseline_data <- read_xlsx("./mock objects/events-Jerone Van1.xlsx")
PMCA_data <- read_xlsx("./mock objects/events-Jerone Van1.1.xlsx")
```
Very nice! After running this you should find the "baseline_data" and "PMCA_data" in your environment.

## Baseline analysis
### Bin data (1 second intervals - median)
The following code block is used to create the time-bins. You don't need to understand this, but I want to highlight one thing that could be interesting. In the last line you can see that I put **median**. That means that from all the values of one bin the median is taken. You could put **mean** instead to take the mean. I think the median is more robust and accurate here:


``` r
baseline_data <- baseline_data %>%
  mutate(TimeBin = floor(Time)) %>%
  group_by(TimeBin) %>%
  summarise(across(where(is.numeric), ~median(.x, na.rm = TRUE)))
```

### Save an excel file that contains the binned data
In the following code block, enter the file path to the folder in which you want to save the data in the same way as before. Here you also need to add the desired file name (e.g., add "/binned-Jerone Van1.xlsx").


``` r
write.xlsx(baseline_data, "./mock objects/binned-Jerone Van1.xlsx")
```

### Define the time interval for the baseline
Define the start and end time of the interval by adjusting the numbers in the following code block:


``` r
start_time <- 200
end_time <- 300

baseline_region <- baseline_data %>%
  filter(TimeBin >= start_time & TimeBin <= end_time)
```

### Linear regression and equation for baseline data
Here we make the linear regression. Please notice that we are using "FL1 ~ TimeBin" for the regression. Depending on what data you want to look at this can be adjusted.


``` r
lin_reg <- lm(FL1 ~ TimeBin, 
              data = baseline_region)

lin_eq <- paste(
  round(coef(lin_reg)[2], 2), 
  "* x", 
  "+", 
  round(coef(lin_reg)[1], 2)
  )

print(paste("This is the equation of your linear regression line: ", lin_eq))
```

```
## [1] "This is the equation of your linear regression line:  2.73 * x + 10237.55"
```

#### Optional: R-squared


``` r
R_squared <- summary(lin_reg)$r.squared

print(paste("This is the R-squared of your linear regression: ", R_squared))
```

```
## [1] "This is the R-squared of your linear regression:  0.389834339796532"
```

### Plot your data 
There are many options when making a graph in R so we may adjust this to your needs. The important part in the coding block below is that you can choose a **title**, **x-** and **y-axis labels**. You can find it in the last section (labs) and you may  replace the text I put there with anything. 


``` r
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
```

### Save your plot as an image
The **ggsave()** function is used to save your plot. It first expects you to name the image. Instead of .tiff like in the example you could also save the image in other formats like .jpg. The path again is the folder where you want to save the image. You can also adjust the other parameter (e.g., width and height) to your liking here. Below you can see the graph you just made.


``` r
ggsave("FC_data.tiff", 
       path = "./graphs", 
       units = "in", 
       dpi=300, 
       compression = 'lzw',
       width = 7, 
       height = 5)

print(calcium_baseline)
```

![](Data-analysis---markdown_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

## PMCA activity analysis
### Adjusted repetition of the steps above
Bin your data and save as a new excel:


``` r
PMCA_data <- PMCA_data %>%
  mutate(TimeBin = floor(Time)) %>%
  group_by(TimeBin) %>%
  summarise(across(where(is.numeric), ~median(.x, na.rm = TRUE)))

write.xlsx(PMCA_data, "./mock objects/binned-Jerone Van1.1.xlsx")
```

Define the time interval for the linear regression and calculate equation (and maybe R-squared):


``` r
start_time_2 <- 6
end_time_2 <- 36

PMCA_region <- PMCA_data %>%
  filter(TimeBin >= start_time_2 & TimeBin <= end_time_2)

lin_reg_2 <- lm(FL1 ~ TimeBin, 
              data = PMCA_region)

lin_eq_2 <- paste(
  round(coef(lin_reg_2)[2], 2), 
  "* x", 
  "+", 
  round(coef(lin_reg_2)[1], 2)
)

R_squared_2 <- summary(lin_reg_2)$r.squared

print(paste("This is the equation of your linear regression line: ", lin_eq_2))
```

```
## [1] "This is the equation of your linear regression line:  -1075.76 * x + 170813.34"
```

``` r
print(paste("This is the R-squared of your linear regression: ", R_squared_2))
```

```
## [1] "This is the R-squared of your linear regression:  0.883164832917344"
```

Plot and save the graph as an image:


``` r
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
       path = "./graphs", 
       units = "in", 
       dpi=300, 
       compression = 'lzw',
       width = 7, 
       height = 5)

print(calcium_PMCA)
```

![](Data-analysis---markdown_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

**Great job! You are done with the analysis!** 
