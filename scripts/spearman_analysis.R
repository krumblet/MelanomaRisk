#Author: Karl Schrader
#PURPOSE: Sun safety policies and determing Spearman ranks based on Strength
#Date: 13 Aug 2024

library(tidyverse)
library(purrr)
library(dplyr)
library(knitr)
setwd("C:/Users/kamas/Desktop/sun_policy")

#reading in data. Since the data was originally in excel, i separated all the different sheets into .csv files to differentiate and combine
high_snapshot <- read.csv("high_snapshot.csv")
high_presence <- read.csv("high_presence.csv")
high_strength <- read.csv("high_strength.csv")
high_intent <- read.csv("high_intent.csv")

low_snapshot <- read.csv("low_snapshot.csv")
low_presence <- read.csv("low_presence.csv")
low_strength <- read.csv("low_strength.csv")
low_intent <- read.csv("low_intent.csv")

table_data <- read.csv("table_data.csv")

names(high_intent) == names(low_intent) # not the same
low_intent[, 'Modeling'] = NA #columns are not the same in intent, so I add a blank Modeling to low_intent

colnames(high_intent) <- paste0("intent_", colnames(high_intent)) # so i can differientiate
colnames(low_intent) <- paste0("intent_", colnames(low_intent))

colnames(high_presence) <- paste0("presence_", colnames(high_presence))
colnames(low_presence) <- paste0("presence_", colnames(low_presence))

colnames(high_strength) <- paste0("strength_", colnames(high_strength))
colnames(low_strength) <- paste0("strength_", colnames(low_strength))



#I look at all the averages of all the school districts for presence

print(sapply(high_presence,mean))
print(sapply(low_presence,mean))

print(sapply(high_strength,mean))
print(sapply(low_strength,mean))

print(sapply(high_intent,mean))
print(sapply(low_intent,mean))



########## spearman's rank
##########

#high melonoma

head(high_snapshot)

high_intent <- high_intent %>% 
  rename(ISD = intent_ISD)
low_intent <- low_intent %>% 
  rename(ISD = intent_ISD)

high_strength <- high_strength %>% 
  rename(ISD = strength_ISD)
low_strength <- low_strength %>% 
  rename(ISD = strength_ISD)

high_presence <- high_presence %>% 
  rename(ISD = presence_ISD)
low_presence <- low_presence %>% 
  rename(ISD = presence_ISD)


#Remove " ISD" (ith a space before ISD) from district name

high_snapshot$X <- gsub("\\s*ISD\\s*$", "", high_snapshot$X)
high_snapshot$X <- trimws(high_snapshot$X) # trim leading/trailing white space from names

high_snapshot <- high_snapshot %>% 
  rename(ISD = X)


combined_1 <- merge(high_strength,high_intent,by = "ISD")
combined_2 <- merge(combined_1, high_presence, by = "ISD")
combined_high <- merge(combined_2, high_snapshot, by = "ISD")

#list all metrics
metrics_cols <- high_snapshot %>% 
  select(Total.number.of.enrolled.sutdents, Community.Type, Property.Wealth,X..of.Students.Identify.as.Caucasian, X..Students.in.Gifted...Talented.Education, X..of.Female.Student,
         X..of.Students.Enrolled.in.ESL.Classes, Attendance.Rate, Percent.of.Student.on.Free.and.Reduced.Lunch, Total.Staff..FTE....n,
         Total.Number.of.Teachers..FTE., X.Staff....Central.Administrative, X.Staff....School.Administritative, X..of.teachers.who.Identify.as.Caucasian,
         Total.Operating.and.Other.Revenue..2020.2021....., Total.Operating.and.Other.Revenue.per.Pupil..2020.2021....., X..of.School.Nurses..across.all.campuses.within.ISD., Median.Household.Income..if.required..can.family.afford.hat.sunscreen....Community,
         Teacher.Turnover.Rate) %>% 
  mutate(across(where(is.character), str_trim)) #trim white space



#standardization and creating levels
metrics_cols <- metrics_cols %>% 
  mutate(Community.Type = case_when(
    str_detect(Community.Type, "Non-metropolitan") ~ "non-metro", #standardize non-metro
    TRUE ~ Community.Type #keep other values
  ))

metrics_cols$Community.Type <- as.numeric(factor(metrics_cols$Community.Type,
                                                 
                                                 levels = c("Rural", "non-metro", "Other Central City Suburban",
                                                            "Independent Town")))

#extract numeric values from the Property.Wealth column
#extract numeric values from the Property.Wealth column
metrics_cols$min_value <- as.numeric(gsub("[^0-9]", "", sub(".*\\$(\\d+,\\d+).*", "\\1", metrics_cols$Property.Wealth)))
metrics_cols$max_value <- as.numeric(gsub("[^0-9]", "", sub(".*\\$(\\d+,\\d+)(?: to| and over).*", "\\1", metrics_cols$Property.Wealth)))

#assigning an arbitrary high value
metrics_cols$max_value[is.na(metrics_cols$max_value)] <- Inf

#define the bins and labels
bins <- c(-Inf, 250000, 500000, 750000, 1000000, Inf)
labels <- c(">250,000", "250,000-500,000", "500,000-750,000", "750,000-1,000,000", ">1,000,000")

#creaete numeric vector so spearman can run
metrics_cols$Property.Wealth <- cut(metrics_cols$max_value, breaks = bins, labels = FALSE, right = FALSE)



#cols with "unavailable"
cols_transform <- c("Total.Staff..FTE....n", "Total.Number.of.Teachers..FTE.", "X..of.School.Nurses..across.all.campuses.within.ISD.")


metrics_cols <- metrics_cols %>% 
  mutate(across(all_of(cols_transform), ~ case_when(
    . == "Unavailable" ~ NA_character_,  # Replace "Unavailable" with NA
    TRUE ~ .  # Keep all other values unchanged
  ) %>% as.numeric()))  # Convert the column to numeric

metrics_cols <- metrics_cols %>%
  mutate(across(where(is.character), ~ as.numeric(str_remove_all(., "\\$|,"))))

metrics_cols <- metrics_cols %>% slice(1:(n() - 3))

sort_ISD <- high_strength %>% 
  arrange(ISD)

ISD <- sort(sort_ISD$ISD)

metrics_cols <- metrics_cols %>% 
  mutate(ISD = ISD)

metrics_cols <- metrics_cols %>%
  select(-min_value, -max_value)

#calculate sum of policy strength for all ISDs (regardless of intent)
all_strength <- high_strength %>% 
  group_by(ISD) %>% 
  summarise(sum_all_strength = sum(strength_Sunscreen.Use) + sum(strength_UV.Protective.Clothing)
            + sum(strength_Hats) + sum(strength_Modeling) + sum(strength_Student.Education) +sum(strength_Teacher.Education)
            + sum(strength_Outdoor.Shade) + sum(strength_Scheduling)+ sum(strength_Resource.Allocatoin) +sum(strength_Accountability)
            + sum(strength_Parent.Outreach))

combined_spear <- all_strength %>% 
  inner_join(metrics_cols, by = "ISD")

metric_cols_list <- setdiff(names(combined_spear), c("ISD", "sum_all_strength"))


#calculate Spearman rank correlation for each metric
correlations_high <- map_dfr(metric_cols_list, function(metric) {
  #perform the correlation test
  cor_test <- cor.test(combined_spear$sum_all_strength, combined_spear[[metric]], method = "spearman", exact = FALSE)
  
  #return results as a data frame
  data.frame(
    Metric = metric,
    Spearman_Rank = cor_test$estimate,
    P_value = cor_test$p.value
  )
})


print(correlations_high)


#format the table
formatted_table_high <- correlations_high %>%
  rename(`Metric` = Metric, `Spearman Rank Correlation` = Spearman_Rank, `P-Value` = P_value) %>%
  kable(format = "markdown", digits = 3, col.names = c("Metric", "Spearman Rank Correlation", "P-Value"))


print(formatted_table_high)

#### low melonoma                              ############
#Remove " ISD" (with a space before ISD) from district name


combined_1 <- merge(low_strength,low_intent,by = "ISD")
combined_2 <- merge(combined_1, low_presence, by = "ISD")
combined_low <- merge(combined_2, low_snapshot, by = "ISD")

#list all metrics
low_metrics_cols <- low_snapshot %>% 
  select(Total.number.of.enrolled.sutdents, Community.Type, Property.Wealth,X..of.Students.Identify.as.Caucasian,
         X..of.Students.Enrolled.in.ESL.Classes, Total.Staff..FTE.,
         Total.Number.of.Teachers..FTE., X.Staff....Central.Administrative, X.Staff....School.Administritative, X..of.teachers.who.Identify.as.Caucasian,
         Toral.Operating.and.Other.Revenue..2020.2021....., Toral.Operating.and.Other.Revenue.per.Pupil..2020.2021....., X..of.School.Nurses, Median.Household.Income..if.required..can.family.afford.hat.sunscreen....Community,
         Teacher.Turnover.Rate) %>% 
  mutate(across(where(is.character), str_trim)) #trim white space

#checking to see if all metrics are numeric
numeric_cols <- low_metrics_cols %>%
  select_if(is.numeric)
character_cols <- low_metrics_cols %>% 
  select_if(is.character)

#standardization and creating levels
low_metrics_cols <- low_metrics_cols %>% 
  mutate(Community.Type = case_when(
    str_detect(Community.Type, "Non-metropolitan") ~ "non-metro", #standardize non-metro
    TRUE ~ Community.Type #keep other values
  ))



low_metrics_cols$Community.Type <- as.numeric(factor(low_metrics_cols$Community.Type,
                                                 
                                                 levels = c("Major Urban", "Rural", "Other Central City",
                                                            "Major Suburban", "non-metro")))
### creating property value levels

low_metrics_cols$min_value <- as.numeric(gsub("[^0-9]", "", sub(".*\\$(\\d+,\\d+).*", "\\1", low_metrics_cols$Property.Wealth)))
low_metrics_cols$max_value <- as.numeric(gsub("[^0-9]", "", sub(".*\\$(\\d+,\\d+)(?: to| and over).*", "\\1", low_metrics_cols$Property.Wealth)))


low_metrics_cols$max_value[is.na(low_metrics_cols$max_value)] <- Inf


bins <- c(-Inf, 250000, 500000, 750000, 1000000, Inf)
labels <- c(">250,000", "250,000-500,000", "500,000-750,000", "750,000-1,000,000", ">1,000,000")


low_metrics_cols$Property.Wealth <- cut(low_metrics_cols$max_value, breaks = bins, labels = FALSE, right = FALSE)




sort_ISD <- low_snapshot %>% 
  arrange(ISD)

ISD <- sort(sort_ISD$ISD)

low_metrics_cols <- low_metrics_cols %>% 
  mutate(ISD = ISD)

low_metrics_cols <- low_metrics_cols %>%
  select(-min_value, -max_value)

#calculate sum of policy strength for all ISDs (regardless of intent)
low_all_strength <- low_strength %>% 
  group_by(ISD) %>% 
  summarise(
    sum_all_strength = sum(strength_Sunscreen.Use, na.rm = TRUE) +
      sum(strength_UV.Protective.Clothing, na.rm = TRUE) +
      sum(strength_Hats, na.rm = TRUE) +
      sum(strength_Modeling, na.rm = TRUE) +
      sum(strength_Student.Education, na.rm = TRUE) +
      sum(strength_Teacher.Education, na.rm = TRUE) +
      sum(strength_Outdoor.Shade, na.rm = TRUE) +
      sum(strength_Scheduling, na.rm = TRUE) +
      sum(strength_Resource.Allocatoin, na.rm = TRUE) +
      sum(strength_Accountability, na.rm = TRUE) +
      sum(strength_Parent.Outreach, na.rm = TRUE)
  )

low_combined_spear <- low_all_strength %>% 
  inner_join(low_metrics_cols, by = "ISD")

low_metric_cols_list <- setdiff(names(low_combined_spear), c("ISD", "sum_all_strength"))

#Filter out rows with NA values 
low_combined_spear_filtered <- low_combined_spear %>%
  filter(
    !is.na(sum_all_strength) & 
      rowSums(is.na(select(., all_of(low_metric_cols_list)))) == 0
  )

correlations_low <- map_dfr(low_metric_cols_list, function(metric) {
  
  if (sum(!is.na(low_combined_spear_filtered[[metric]])) > 1) {
    
    cor_test <- cor.test(low_combined_spear_filtered$sum_all_strength, low_combined_spear_filtered[[metric]], method = "spearman", exact = FALSE)
    
    
    data.frame(
      Metric = metric,
      Spearman_Rank = cor_test$estimate,
      P_value = cor_test$p.value
    )
  }} )

print(correlations_low)



#format the table
formatted_table_low <- correlations_low %>%
  rename(`Metric` = Metric, `Spearman Rank Correlation` = Spearman_Rank, `P-Value` = P_value) %>%
  kable(format = "markdown", digits = 3, col.names = c("Metric", "Spearman Rank Correlation", "P-Value"))


print(formatted_table_low)

### double check

#filter out rows with NA values in sum_all_strength and all metric columns
low_combined_spear_filtered <- low_combined_spear %>%
  filter(
    !is.na(sum_all_strength) & 
    rowSums(is.na(select(., all_of(low_metric_cols_list)))) == 0
  )

# Calculate Spearman rank correlation for each metric
correlations_low <- map_dfr(low_metric_cols_list, function(metric) {
  # Check if there are enough data points
  if (sum(!is.na(low_combined_spear_filtered[[metric]])) > 1) {
    
    cor_test <- cor.test(low_combined_spear_filtered$sum_all_strength, low_combined_spear_filtered[[metric]], method = "spearman", exact = FALSE)
    
  
    data.frame(
      Metric = metric,
      Spearman_Rank = cor_test$estimate,
      P_value = cor_test$p.value
    )
  } else {
    #return NA for cases with insufficient data
    data.frame(
      Metric = metric,
      Spearman_Rank = NA,
      P_value = NA
    )
  }
})


######### COMBINED again


metrics_cols <- read.csv("metrics_cols.csv")
low_metrics_cols <- read.csv("low_metrics_cols.csv")

final_strength <- bind_rows(low_all_strength, all_strength)
final_combination <- bind_rows(metrics_cols, low_metrics_cols)

final_combination <-final_combination %>%
  inner_join(final_strength, by = "ISD")

metrics <- setdiff(names(final_combination), c("ISD", "sum_all_strength"))

# 3. Calculate Spearman rank correlation for each metric
final_combined_correlations <- map_dfr(metrics, function(metric) {
  # Perform the correlation test
  cor_test <- cor.test(final_combination$sum_all_strength, final_combination[[metric]], method = "spearman", exact = FALSE)
  
  # Return results as a data frame
  data.frame(
    Metric = metric,
    Spearman_Rank = cor_test$estimate,
    P_value = cor_test$p.value
  )
})

#format the table
formatted_table_combined <- final_combined_correlations %>%
  rename(`Metric` = Metric, `Spearman Rank Correlation` = Spearman_Rank, `P-Value` = P_value) %>%
  kable(format = "markdown", digits = 3, col.names = c("Metric", "Spearman Rank Correlation", "P-Value"))
print(formatted_table_combined)

print(formatted_table_high)
print(formatted_table_low)
print(formatted_table_combined)

write.csv(final_combined_correlations, "formatted_table_combined.csv", row.names = FALSE)
write.csv(correlations_low, "low_table_combined.csv", row.names = FALSE)
write.csv(correlations_high, "high_table_combined.csv", row.names = FALSE)


