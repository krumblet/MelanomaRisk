#Author: Karl Schrader
#PURPOSE: Sun safety policies and determining Spearman ranks based on Strength
#Date: 13 Aug 2024

library(tidyverse)
library(purrr)
library(dplyr)
library(knitr)
setwd("C:/Users/ks162/Desktop/sun_policy")

#reading in data. Since the data was originally in excel, i separated all the different sheets into .csv files to differentiate and combine
high_snapshot <- read.csv("high_snapshot.csv")
high_presence <- read.csv("high_presence.csv")
high_strength <- read.csv("high_strength.csv")
high_intent <- read.csv("high_intent.csv")

low_snapshot <- read.csv("low_snapshot.csv")
low_presence <- read.csv("low_presence.csv")
low_strength <- read.csv("low_strength.csv")
low_intent <- read.csv("low_intent.csv")


names(high_intent) == names(low_intent) # not the same
low_intent[, 'Modeling'] = NA #columns are not the same in intent, so I add a blank Modeling to low_intent

colnames(high_intent) <- paste0("intent_", colnames(high_intent)) # so i can differientiate
colnames(low_intent) <- paste0("intent_", colnames(low_intent))

colnames(high_presence) <- paste0("presence_", colnames(high_presence))
colnames(low_presence) <- paste0("presence_", colnames(low_presence))

colnames(high_strength) <- paste0("strength_", colnames(high_strength))
colnames(low_strength) <- paste0("strength_", colnames(low_strength))


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




########## spearman's rank
##########

#high melonoma

tibble(high_snapshot)


#remove ISDs

high_snapshot$X <- gsub("\\s*ISD\\s*$", "", high_snapshot$X)
high_snapshot$X <- trimws(high_snapshot$X) # trim leading/trailing white space from names

high_snapshot <- high_snapshot %>% 
  rename(ISD = X)



#list all metrics
metrics_cols <- high_snapshot %>% 
  select(Total.number.of.enrolled.sutdents, Community.Type, TAX.Rate, Property.Wealth,X..of.Students.Identify.as.Caucasian, X..Students.in.Gifted...Talented.Education, X..of.Female.Student,
         X..of.Students.Enrolled.in.ESL.Classes, Attendance.Rate, Percent.of.Student.on.Free.and.Reduced.Lunch, Total.Staff..FTE....n,
         Total.Number.of.Teachers..FTE., X.Staff....Central.Administrative, X.Staff....School.Administritative, X..of.teachers.who.Identify.as.Caucasian,
         Total.Operating.and.Other.Revenue..2020.2021....., Total.Operating.and.Other.Revenue.per.Pupil..2020.2021....., X..of.School.Nurses..across.all.campuses.within.ISD., Median.Household.Income..if.required..can.family.afford.hat.sunscreen....Community,
         Teacher.Turnover.Rate, Median.Household.Income..if.required..can.family.afford.hat.sunscreen....Parents., X..of.students.graduating) %>% 
  mutate(across(where(is.character), str_trim)) #trim white space



#standardization and creating levels
metrics_cols <- metrics_cols %>% 
  mutate(Community.Type = case_when(
    str_detect(Community.Type, "Non-metropolitan") ~ "non-metro", #standardize non-metro
    TRUE ~ Community.Type 
  ))

metrics_cols$Community.Type <- as.numeric(factor(metrics_cols$Community.Type,
                                                 
                                                 levels = c("Rural", "non-metro", "Other Central City Suburban",
                                                            "Independent Town")))

#extract numeric values 
metrics_cols$min_value <- as.numeric(gsub("[^0-9]", "", sub(".*\\$(\\d+,\\d+).*", "\\1", metrics_cols$Property.Wealth)))
metrics_cols$max_value <- as.numeric(gsub("[^0-9]", "", sub(".*\\$(\\d+,\\d+)(?: to| and over).*", "\\1", metrics_cols$Property.Wealth)))

#assigning an arbitrary high value
metrics_cols$max_value[is.na(metrics_cols$max_value)] <- Inf

#define the bins and labels
bins <- c(-Inf, 250000, 500000, 750000, 1000000, Inf)
labels <- c(">250,000", "250,000-500,000", "500,000-750,000", "750,000-1,000,000", ">1,000,000")

#create numeric vector so spearman can run
metrics_cols$Property.Wealth <- cut(metrics_cols$max_value, breaks = bins, labels = FALSE, right = FALSE)

#fix tax rate to numeric values
levels_order <- c("Under $1.0809", 
                  "$1.0809 to under $1.2148", 
                  "$1.2148 to under $1.3239", 
                  "$1.3239 and over")

metrics_cols <- metrics_cols %>%
  mutate(TAX.Rate = as.numeric(factor(TAX.Rate, levels = levels_order)))


#cols with "unavailable"
cols_transform <- c("Total.Staff..FTE....n", "Total.Number.of.Teachers..FTE.", "X..of.students.graduating")


metrics_cols <- metrics_cols %>% 
  mutate(across(all_of(cols_transform), ~ case_when(
    . == "Unavailable" ~ NA_character_,  
    TRUE ~ .  #keep all other values unchanged
  ) %>% as.numeric()))  

metrics_cols <- metrics_cols %>%
  mutate(across(where(is.character), ~ as.numeric(str_remove_all(., "\\$|,"))))

metrics_cols <- metrics_cols %>% slice(1:(n() - 3))



metrics_cols <- metrics_cols %>%
  left_join(high_snapshot %>% select(Total.number.of.enrolled.sutdents,
                                    X..of.Students.Identify.as.Caucasian,
                                    ISD), 
            by = c("Total.number.of.enrolled.sutdents",
                   "X..of.Students.Identify.as.Caucasian"))


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
  
  cor_test <- cor.test(combined_spear$sum_all_strength, combined_spear[[metric]], method = "spearman", exact = FALSE)
  
  
  data.frame(
    Metric = metric,
    Spearman_Rank = cor_test$estimate,
    P_value = format(cor_test$p.value, scientific = FALSE)
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


low_metrics_cols <- low_snapshot %>% 
  select(Total.number.of.enrolled.sutdents, Community.Type, TAX.Rate, Property.Wealth,X..of.Students.Identify.as.Caucasian, X..Students.in.Gifted...Talented.Education, X..of.Female.Student,
         X..of.Students.Enrolled.in.ESL.Classes, Attendance.Rate, Percent.of.Student.on.Free.and.Reduced.Lunch, Total.Staff..FTE.,
         Total.Number.of.Teachers..FTE., X.Staff....Central.Administrative, X.Staff....School.Administritative, X..of.teachers.who.Identify.as.Caucasian,
         Toral.Operating.and.Other.Revenue..2020.2021....., Toral.Operating.and.Other.Revenue.per.Pupil..2020.2021....., X..of.School.Nurses, Median.Household.Income..if.required..can.family.afford.hat.sunscreen....Community,
         Teacher.Turnover.Rate, Median.Household.Income..if.required..can.family.afford.hat.sunscreen....Parents., X..of.students.graduating) %>% 
  mutate(across(where(is.character), str_trim)) #trim white space

#checking to see if all metrics are numeric
#numeric_cols <- low_metrics_cols %>%
#  select_if(is.numeric)
#character_cols <- low_metrics_cols %>% 
#  select_if(is.character)

#standardization and creating levels
low_metrics_cols <- low_metrics_cols %>% 
  mutate(Community.Type = case_when(
    str_detect(Community.Type, "Non-metropolitan") ~ "non-metro", #standardize non-metro
    TRUE ~ Community.Type #keep other values
  ))

low_metrics_cols <- low_metrics_cols %>% 
  mutate(Community.Type = case_when(
    str_detect(Community.Type, "Other Central City") ~ "other", #standardize non-metro
    TRUE ~ Community.Type #keep other values
  ))

#unique(low_metrics_cols$Community.Type)

low_metrics_cols$Community.Type <- as.numeric(factor(low_metrics_cols$Community.Type,
                                                 
                                                 levels = c("Major Urban", "Rural", "other",
                                                            "Major Suburban", "non-metro")))
### creating property value levels

low_metrics_cols$min_value <- as.numeric(gsub("[^0-9]", "", sub(".*\\$(\\d+,\\d+).*", "\\1", low_metrics_cols$Property.Wealth)))
low_metrics_cols$max_value <- as.numeric(gsub("[^0-9]", "", sub(".*\\$(\\d+,\\d+)(?: to| and over).*", "\\1", low_metrics_cols$Property.Wealth)))


low_metrics_cols$max_value[is.na(low_metrics_cols$max_value)] <- Inf


bins <- c(-Inf, 250000, 500000, 750000, 1000000, Inf)
labels <- c(">250,000", "250,000-500,000", "500,000-750,000", "750,000-1,000,000", ">1,000,000")


low_metrics_cols$Property.Wealth <- cut(low_metrics_cols$max_value, breaks = bins, labels = FALSE, right = FALSE)



#fix tax rate to numeric values
levels_order <- c("Under $1.0809", 
                  "$1.0809 to under $1.2148", 
                  "$1.2148 to under $1.3239", 
                  "$1.3239 and over")

low_metrics_cols <- low_metrics_cols %>%
  mutate(across(where(is.character), str_trim)) %>% 
  mutate(TAX.Rate = as.numeric(factor(TAX.Rate, levels = levels_order)))

#merge ISDs

low_metrics_cols <- low_metrics_cols %>%
  left_join(low_snapshot %>% select(Total.number.of.enrolled.sutdents,
                                    X..of.Students.Identify.as.Caucasian,
                                    ISD), 
            by = c("Total.number.of.enrolled.sutdents",
                   "X..of.Students.Identify.as.Caucasian"))

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


#calculate Spearman rank correlation for each metric
correlations_low <- map_dfr(low_metric_cols_list, function(metric) {
  #perform the correlation test
  cor_test <- cor.test(low_combined_spear$sum_all_strength, low_combined_spear[[metric]], method = "spearman", exact = FALSE)
  
  #return results as a data frame
  data.frame(
    Metric = metric,
    Spearman_Rank = cor_test$estimate,
    P_value = format(cor_test$p.value, scientific = FALSE)
  )
})


print(correlations_low)



#format the table
formatted_table_low <- correlations_low %>%
  rename(`Metric` = Metric, `Spearman Rank Correlation` = Spearman_Rank, `P-Value` = P_value) %>%
  kable(format = "markdown", digits = 3, col.names = c("Metric", "Spearman Rank Correlation", "P-Value"))


print(formatted_table_low)



######### COMBINED again

low_snap_cols <- low_snapshot %>% 
  select(Total.number.of.enrolled.sutdents, Community.Type, TAX.Rate, Property.Wealth,X..of.Students.Identify.as.Caucasian, X..Students.in.Gifted...Talented.Education, X..of.Female.Student,
         X..of.Students.Enrolled.in.ESL.Classes, Attendance.Rate, Percent.of.Student.on.Free.and.Reduced.Lunch, Total.Staff..FTE.,
         Total.Number.of.Teachers..FTE., X.Staff....Central.Administrative, X.Staff....School.Administritative, X..of.teachers.who.Identify.as.Caucasian,
         Toral.Operating.and.Other.Revenue..2020.2021....., Toral.Operating.and.Other.Revenue.per.Pupil..2020.2021....., X..of.School.Nurses, Median.Household.Income..if.required..can.family.afford.hat.sunscreen....Community,
         Teacher.Turnover.Rate, Median.Household.Income..if.required..can.family.afford.hat.sunscreen....Parents., X..of.students.graduating) %>% 
  mutate(across(where(is.character), str_trim))

high_snap_cols <- high_snapshot %>% 
  select(Total.number.of.enrolled.sutdents, Community.Type, TAX.Rate, Property.Wealth,X..of.Students.Identify.as.Caucasian, X..Students.in.Gifted...Talented.Education, X..of.Female.Student,
         X..of.Students.Enrolled.in.ESL.Classes, Attendance.Rate, Percent.of.Student.on.Free.and.Reduced.Lunch, Total.Staff..FTE....n,
         Total.Number.of.Teachers..FTE., X.Staff....Central.Administrative, X.Staff....School.Administritative, X..of.teachers.who.Identify.as.Caucasian,
         Total.Operating.and.Other.Revenue..2020.2021....., Total.Operating.and.Other.Revenue.per.Pupil..2020.2021....., X..of.School.Nurses..across.all.campuses.within.ISD., Median.Household.Income..if.required..can.family.afford.hat.sunscreen....Community,
         Teacher.Turnover.Rate, Median.Household.Income..if.required..can.family.afford.hat.sunscreen....Parents., X..of.students.graduating) %>% 
  mutate(across(where(is.character), str_trim)) #trim white space

#mapping vector from low_snap to high_snap

# Desired column names for low_snap_cols

# Desired column names for low_snap_cols
desired_names_low <- c(
  "Total.number.of.enrolled.sutdents" = "Total.number.of.enrolled.sutdents",
  "Community.Type" = "Community.Type",
  "TAX.Rate" = "TAX.Rate",
  "Property.Wealth" = "Property.Wealth",
  "X..of.Students.Identify.as.Caucasian" = "X..of.Students.Identify.as.Caucasian",
  "Percentage.Students.in.Gifted.Talented.Education" = "X..Students.in.Gifted...Talented.Education",
  "Percentage.Female.Student" = "X..of.Female.Student",
  "Percentage.Students.Enrolled.in.ESL.Classes" = "X..of.Students.Enrolled.in.ESL.Classes",
  "Attendance.Rate" = "Attendance.Rate",
  "Percentage.Students.on.Free.and.Reduced.Lunch" = "Percent.of.Student.on.Free.and.Reduced.Lunch",
  "Total.Staff.FTE" = "Total.Staff..FTE.",
  "Total.Number.of.Teachers.FTE" = "Total.Number.of.Teachers..FTE.",
  "Percentage.Staff.Central.Administrative" = "X.Staff....Central.Administrative",
  "Percentage.Staff.School.Administritative" = "X.Staff....School.Administritative",
  "Percentage.Teachers.Identify.as.Caucasian" = "X..of.teachers.who.Identify.as.Caucasian",
  "Total.Operating.and.Other.Revenue.2020.2021" = "Toral.Operating.and.Other.Revenue..2020.2021.....",
  "Total.Operating.and.Other.Revenue.per.Pupil.2020.2021" = "Toral.Operating.and.Other.Revenue.per.Pupil..2020.2021.....",
  "Percentage.School.Nurses" = "X..of.School.Nurses",
  "Median.Household.Income.Community" = "Median.Household.Income..if.required..can.family.afford.hat.sunscreen....Community",
  "Teacher.Turnover.Rate" = "Teacher.Turnover.Rate",
  "Median.Household.Income.Parents" = "Median.Household.Income..if.required..can.family.afford.hat.sunscreen....Parents.",
  "Percentage.Students.Graduating" = "X..of.students.graduating"
)

# Desired column names for high_snap_cols
desired_names_high <- c(
  "Total.number.of.enrolled.sutdents" = "Total.number.of.enrolled.sutdents",
  "Community.Type" = "Community.Type",
  "TAX.Rate" = "TAX.Rate",
  "Property.Wealth" = "Property.Wealth",
  "X..of.Students.Identify.as.Caucasian" = "X..of.Students.Identify.as.Caucasian",
  "Percentage.Students.in.Gifted.Talented.Education" = "X..Students.in.Gifted...Talented.Education",
  "Percentage.Female.Student" = "X..of.Female.Student",
  "Percentage.Students.Enrolled.in.ESL.Classes" = "X..of.Students.Enrolled.in.ESL.Classes",
  "Attendance.Rate" = "Attendance.Rate",
  "Percentage.Students.on.Free.and.Reduced.Lunch" = "Percent.of.Student.on.Free.and.Reduced.Lunch",
  "Total.Staff.FTE" = "Total.Staff..FTE....n",
  "Total.Number.of.Teachers.FTE" = "Total.Number.of.Teachers..FTE.",
  "Percentage.Staff.Central.Administrative" = "X.Staff....Central.Administrative",
  "Percentage.Staff.School.Administritative" = "X.Staff....School.Administritative",
  "Percentage.Teachers.Identify.as.Caucasian" = "X..of.teachers.who.Identify.as.Caucasian",
  "Total.Operating.and.Other.Revenue.2020.2021" = "Total.Operating.and.Other.Revenue..2020.2021.....",
  "Total.Operating.and.Other.Revenue.per.Pupil.2020.2021" = "Total.Operating.and.Other.Revenue.per.Pupil..2020.2021.....",
  "Percentage.School.Nurses" = "X..of.School.Nurses..across.all.campuses.within.ISD.",
  "Median.Household.Income.Community" = "Median.Household.Income..if.required..can.family.afford.hat.sunscreen....Community",
  "Teacher.Turnover.Rate" = "Teacher.Turnover.Rate",
  "Median.Household.Income.Parents" = "Median.Household.Income..if.required..can.family.afford.hat.sunscreen....Parents.",
  "Percentage.Students.Graduating" = "X..of.students.graduating"
)

#rename cols
low_snap_cols_renamed <- low_snap_cols %>%
  rename(!!!desired_names_low)


high_snap_cols_renamed <- high_snap_cols %>%
  rename(!!!desired_names_high)

#write.csv(high_snap_cols_renamed, "high_snap_fix.csv", row.names = FALSE)

high_snap_cols_renamed <- read.csv("high_snap_fix.csv")





final_metrics <- bind_rows(low_snap_cols_renamed, high_snap_cols_renamed)




low_snapshot <- low_snapshot %>%
  rename(Median.Household.Income.Parents = Median.Household.Income..if.required..can.family.afford.hat.sunscreen....Parents.)

metrics_cols <- metrics_cols %>%
  rename(Median.Household.Income.Parents = Median.Household.Income..if.required..can.family.afford.hat.sunscreen....Parents.)



final_metrics <- final_metrics %>%
  left_join(low_snapshot %>% select(Total.number.of.enrolled.sutdents, 
                                    X..of.Students.Identify.as.Caucasian,
                                    ISD), 
            by = c("Total.number.of.enrolled.sutdents", 
                   "X..of.Students.Identify.as.Caucasian"))

final_metrics <- final_metrics %>%
  left_join(high_snapshot %>% select(Total.number.of.enrolled.sutdents, 
                                     X..of.Students.Identify.as.Caucasian,
                                     ISD), 
            by = c("Total.number.of.enrolled.sutdents", 
                   "X..of.Students.Identify.as.Caucasian"), 
            suffix = c(".low", ".high"))

final_metrics <- final_metrics %>%
  mutate(ISD = coalesce(ISD.high, ISD.low)) %>%
  select(-ISD.low, -ISD.high)


final_strength <- bind_rows(low_all_strength, all_strength)


final_combination <-final_metrics %>%
  inner_join(final_strength, by = "ISD")

#final_combination2 <- final_metrics %>%
#  full_join(final_strength, by = "ISD")

#missing_in_final_strength <- final_metrics %>%
#  anti_join(final_strength, by = "ISD")

#missing_in_final_metrics <- final_strength %>%
#  anti_join(final_metrics, by = "ISD")




###fix metrics to numeric

#checking to see if all metrics are numeric
#numeric_cols <- final_combination %>%
  select_if(is.numeric)
#character_cols <- final_combination %>% 
 select_if(is.character)

#unique(final_combination$Community.Type)

#standardization and creating levels
final_combination <- final_combination %>% 
  mutate(Community.Type = case_when(
    str_detect(Community.Type, "Non-metropolitan") ~ "non-metro", #standardize non-metro
    TRUE ~ Community.Type #keep other values
  ))

final_combination <- final_combination %>% 
  mutate(Community.Type = case_when(
    str_detect(Community.Type, "Other Central City") ~ "other", #standardize non-metro
    TRUE ~ Community.Type #keep other values
  ))


final_combination$Community.Type <- as.numeric(factor(final_combination$Community.Type,
                                                     
                                                     levels = c("Major Urban", "Rural", "other",
                                                                "Major Suburban", "non-metro", "Independent Town")))

#fix tax rate to numeric values
levels_order <- c("Under $1.0809", 
                  "$1.0809 to under $1.2148", 
                  "$1.2148 to under $1.3239", 
                  "$1.3239 and over")

final_combination <- final_combination %>%
  mutate(across(where(is.character), str_trim)) %>% 
  mutate(TAX.Rate = as.numeric(factor(TAX.Rate, levels = levels_order)))

### creating property value levels

final_combination$min_value <- as.numeric(gsub("[^0-9]", "", sub(".*\\$(\\d+,\\d+).*", "\\1", final_combination$Property.Wealth)))
final_combination$max_value <- as.numeric(gsub("[^0-9]", "", sub(".*\\$(\\d+,\\d+)(?: to| and over).*", "\\1", final_combination$Property.Wealth)))


final_combination$max_value[is.na(final_combination$max_value)] <- Inf


bins <- c(-Inf, 250000, 500000, 750000, 1000000, Inf)
labels <- c(">250,000", "250,000-500,000", "500,000-750,000", "750,000-1,000,000", ">1,000,000")


final_combination$Property.Wealth <- cut(final_combination$max_value, breaks = bins, labels = FALSE, right = FALSE)




metrics <- setdiff(names(final_combination), c("ISD", "sum_all_strength"))

final_combination <- final_combination %>%
  select(-min_value, -max_value)


#spearman
correlations_combined <- map_dfr(metrics, function(metric) {
  cor_test <- cor.test(final_combination$sum_all_strength, final_combination[[metric]], method = "spearman", exact = FALSE)
  
  data.frame(
    Metric = metric,
    Spearman_Rank = cor_test$estimate,
    P_value = format(cor_test$p.value, scientific = FALSE)
  )
})

correlations_combined

#format the table
formatted_table_combined <- correlations_combined %>%
  rename(`Metric` = Metric, `Spearman Rank Correlation` = Spearman_Rank, `P-Value` = P_value) %>%
  kable(format = "markdown", digits = 3, col.names = c("Metric", "Spearman Rank Correlation", "P-Value"))
print(formatted_table_combined)

print(formatted_table_high)
print(formatted_table_low)
print(formatted_table_combined)

write.csv(final_combined_correlations, "1formatted_table_combined.csv", row.names = FALSE)
write.csv(correlations_low, "2low_table_combined.csv", row.names = FALSE)
write.csv(correlations_high, "3high_table_combined.csv", row.names = FALSE)
write.csv(final_combination, "4clean_table_for_analysis.csv", row.names = FALSE)


#step wise regression 

stepwise_df <- data.frame(
  policy_strength = final_combination$sum_all_strength,
  tax_rate = factor(final_combination$TAX.Rate),
  property_wealth = factor(final_combination$Property.Wealth),
  community_type = factor(final_combination$Community.Type),
  enrolled_students = final_combination$Total.number.of.enrolled.sutdents,
  percentage_nurses = final_combination$Percentage.School.Nurses, # we have data
  percentage_female_student = final_combination$Percentage.Female.Student, #we have complete data
  percentage_esl_enrolled = final_combination$Percentage.Students.Enrolled.in.ESL.Classes,
  percentage_staff_central_admin = final_combination$Percentage.Staff.Central.Administrative,
  percentage_students_caucasian = final_combination$X..of.Students.Identify.as.Caucasian,
  percentage_staff_school_admin = final_combination$Percentage.Staff.School.Administritative,
  percentage_students_gifted_talented = final_combination$Percentage.Students.in.Gifted.Talented.Education,
  total_operating_2020_2021 = final_combination$Total.Operating.and.Other.Revenue.2020.2021
  
)

#sum(is.na(final_combination$Total.Number.of.Teachers.FTE))


full_model <- lm(policy_strength ~ tax_rate + property_wealth + community_type + enrolled_students + percentage_nurses + percentage_female_student +
                 + percentage_esl_enrolled + percentage_staff_central_admin +percentage_students_caucasian +percentage_staff_school_admin +percentage_students_gifted_talented  
                 + total_operating_2020_2021 , data = stepwise_df)

stepwise_model <- step(full_model, direction = "both", trace = TRUE)

summary(stepwise_model)
coeff <- coefficients(stepwise_model)
print(coeff)

sum(is.na(stepwise_df))


#chi squared 

final_combination2 <- final_combination %>% 
  select(-ISD)

chi_data = table(final_combination2$Property.Wealth, final_combination2$Community.Type) 

print(chi_data)

chisq.test(chi_data)



