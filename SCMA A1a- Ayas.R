data <- read.csv('C:/Users/Cyber World/Downloads/NSSO68.csv')
install.packages('dplyr')   
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA","glue")
lapply(libraries, install_and_load)





# Filtering for MH
df <- data %>%
  filter(state_1 == "MH")


# Display dataset info
cat("Dataset Information:\n")
print(names(df))
print(head(df))
print(dim(df))

# Finding missing values
missing_info <- colSums(is.na(df))
cat("Missing Values Information:\n")
print(missing_info)

# Subsetting the data
MHnew <- df %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}
cat("Missing Values in Subset:\n")
 print(colSums(is.na(MHnew)))
apnew$Meals_At_Home <- impute_with_mean(apnew$Meals_At_Home)
unique(apnew$Meals_At_Home)
> any(is.na(apnew))
cat("missing values in Subset:\n")
print(colSums(is.na(apnew)))

# Finding outliers and removing them
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}

outlier_columns <- c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  MHnew <- remove_outliers(MHnew, col)

  MH_clean <- data.frame(
    ricetotal_q = c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55)
  )
  q1 <- quantile(MH_clean$ricetotal_q, 0.25)
  q3 <- quantile(MH_clean$ricetotal_q, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  
  MH_clean_filtered <- MH_clean[which(MH_clean$ricetotal_q >= lower_bound & MH_clean$ricetotal_q <= upper_bound), ]
  print(MH_clean_filtered)
  
  # Summarize consumption
MHnew$total_consumption <- rowSums(MHnew[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)

# Summarize and display top consuming districts and regions
summarize_consumption <- function(group_col) {
  summary <- MHnew %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")

cat("Top Consuming Districts:\n")
print(head(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)

# Rename districts and sectors
district_mapping <- c("21" = "Thane", "07" = "Amravati", "14" = "Yavatmal", "25" = "Pune")
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")
district_mapping <- c("21" = "Thane", "07" = "Amravati", "14" = "Yavatmal", "25" = "Pune")
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")
cat("District Mapping:\n")
for (code in names(district_mapping)) {
  cat("Code:", code, "- District:", district_mapping[code], "\n")
}
cat("\nSector Mapping:\n")
for (code in names(sector_mapping)) {
  cat("Code:", code, "- Sector:", sector_mapping[code], "\n")
}

MHnew$District <- as.character(MHnew$District)
MHnew$Sector <- as.character(MHnew$Sector)
MHnew$District <- ifelse(MHnew$District %in% names(district_mapping), district_mapping[MHnew$District], MHnew$District)
MHnew$Sector <- ifelse(MHnew$Sector %in% names(sector_mapping), sector_mapping[MHnew$Sector], MHnew$Sector)


# Test for differences in mean consumption between urban and rural
rural <- MHnew %>%
  filter(Sector == "RURAL") %>%
  select(total_consumption)

urban <- apnew %>%
  filter(Sector == "URBAN") %>%
  select(total_consumption)
cat("Top 3Consuming Districts:\n")
print(head(district_summary,3))
cat("Bottom 3Consuming Districts:\n")
print(tail(district_summary,3))
cat("Region Consumtion Summary:\n")
print(region_summary)

z_test_result <- z.test(rural, urban, alternative = "two.sided", mu = 0, sigma.x = 2.56, sigma.y = 2.34, conf.level = 0.95)

if (z_test_result$p.value < 0.05) {
  cat("P value is <", 0.05, ", Therefore we reject the null hypothesis.\n")
  cat("There is a difference between mean consumptions of urban and rural.\n")
} else {
  cat("P value is >=", 0.05, ", Therefore we fail to reject the null hypothesis.\n")
  cat("There is no significant difference between mean consumptions of urban and rural.\n")
  
  
  # Test for differences in mean consumption between urban and rural
  rural <- MHnew %>%
    filter(Sector == "RURAL") %>%
    select(total_consumption)
  
  urban <- MHnew %>%
    filter(Sector == "URBAN") %>%
    select(total_consumption)
  
  mean_rural <- mean(rural$total_consumption)
  mean_urban <- mean(urban$total_consumption)
  
  # Perform z-test
  z_test_result <- z.test(rural, urban, alternative = "two.sided", mu = 0, sigma.x = 2.56, sigma.y = 2.34, conf.level = 0.95)
  
  # Generate output based on p-value
  if (z_test_result$p.value < 0.05) {
    cat(glue::glue("P value is < 0.05 i.e. {round(z_test_result$p.value, 5)}, Therefore we reject the null hypothesis.\n"))
    cat(glue::glue("There is a difference between mean consumptions of urban and rural.\n"))
    cat(glue::glue("The mean consumption in Rural areas is {mean_rural} and in Urban areas its {mean_urban}\n"))
  } else {
    cat(glue::glue("P value is >= 0.05 i.e. {round(z_test_result$p.value, 5)}, Therefore we fail to reject the null hypothesis.\n"))
    cat(glue::glue("There is no significant difference between mean consumptions of urban and rural.\n"))
    cat(glue::glue("The mean consumption in Rural area is {mean_rural} and in Urban area its {mean_urban}\n"))
  }
  