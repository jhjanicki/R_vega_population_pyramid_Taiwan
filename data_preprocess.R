library(tidyverse)

# Taiwan population pyramid data
# read the initial CSV file (from UN world data prospects 2022)
data <- read_csv("taiwan_raw.csv")

# convert the data from wide to long
# first add a "X" in front of all year columns (essentially all columns except for the first two)
# then take all the ones that start with "X" and make them long
# then remove the "X" from the values in the age column
data_long <- data %>% rename_at(vars(3:length(data)),~paste0("X",.x))%>% pivot_longer(cols = starts_with("X"), names_to = "age", values_to = "value") %>% mutate(age=str_remove(age,"^X"))

#create an order column to keep track of the age group order by year & sex for the population pyramid, descending within each year and sex group (descending order necessary for vega viz)
data_long <- data_long %>%  arrange(year, sex) %>%  group_by(year, sex) %>%  mutate(order = 102-row_number()) %>% ungroup()

# rename columns
data_long <- data_long %>% rename(ageGroup = age, population = value)

#export csv in long data format with order column and renamed columns, this can be used in vega to create population pyramids by year
write_csv(data_long,"taiwan_data_long.csv")

# if you want to create a csv file for each year...
# create a list of data frames, each containing one value of "year"
# To split a CSV file with multiple rows into multiple CSV files based on a unique value in the "year" column using tidyverse in R, use pmap() from purrr package
# pmap: Map over multiple input simultaneously
list_of_dfs <- pmap(
       list(unique(data_long$year)), 
       function(year_val) {
             data_long %>% filter(year == year_val)
         }
   )

# write each data frame to a separate CSV file, add 1949 so the first file would start at 1950 as opposed to 1
iwalk(list_of_dfs, function(df_val, idx) {
  write.csv(df_val, file = paste0("./data/year", idx+1949, ".csv"), row.names = FALSE)
})
