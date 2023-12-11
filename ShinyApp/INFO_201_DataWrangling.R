library(dplyr)
library(stringr)
library(tidyr)
death_df <- read.csv("Death_rates_for_suicide__by_sex__race__Hispanic_origin__and_age__United_States.csv")
age <- read.csv("internet_age_data.csv")
gender <- read.csv("internet_gender.csv")
income <- read.csv("internet_income.csv")

# rename age
age <- age %>% 
  rename("Year" = "X",
         "18-29" = "X18.29",
         "30-49" = "X30.49",
         "50-64" = "X50.64",
         "65+" = "X65.")

# rename gender
gender <- gender %>%
  rename("Year" = "X.nbsp.",
         "Male" = "Men",
         "Female" = "Women")

# rename income
income <- income %>%
  rename("Year" = "X.nbsp.",
         "Less than $30,000" = "Less.than..30.000",
         "$30,000-$49,999" = "X.30.000..49.999",
         "$50,000-$74,999" = "X.50.000..74.999",
         "$75,000+" =  "X.75.000.")

# rename death_df
death_df <- death_df %>%
  rename("Year" = "YEAR",
         "Gender" = "STUB_LABEL")

# filter death_df -> age, Year, gender, Estimate (Deaths per 100,000), age-adjusted data only 
death_df <- death_df %>%
  filter(AGE == "All ages") %>%
  select(Gender, Year, ESTIMATE, UNIT) %>%
  filter(str_detect(UNIT, "age-adjusted"))

death_df <- death_df[death_df$Gender == "Male" | death_df$Gender == "Female", ]

# Make a new numerical column: take out %, convert values to numeric, divide by 100,
# find average of internet usage for all ages
for (count in 1:nrow(age)) {
  num1 <- as.numeric(str_replace(age$`18-29`[count], "%", ""))
  num2 <- as.numeric(str_replace(age$`30-49`[count], "%", ""))
  num3 <- as.numeric(str_replace(age$`50-64`[count], "%", ""))
  num4 <- as.numeric(str_replace(age$`65+`[count], "%", ""))
  age$ALL_AGE[count] <- ((num1 + num2 + num3 + num4) / 4) / 100
}

# replacing the values in gender df's to numeric values
# using transform because if directly replace the value, it will still be chr
for (count in 1:nrow(gender)) {
  gender$Male[count] <- as.numeric(str_replace(gender$Male[count], "%", "")) / 100
  gender$Female[count] <- as.numeric(str_replace(gender$Female[count], "%", "")) / 100
}

gender <- gender %>%
  transform(Male = as.numeric(Male),
            Female = as.numeric(Female))

# replacing all the values in income to numeric values
for (count in 1:nrow(income)) {
  income$`Less than $30,000`[count] <- as.numeric(str_replace(income$`Less than $30,000`[count], "%", "")) / 100
  income$`$30,000-$49,999`[count] <- as.numeric(str_replace(income$`$30,000-$49,999`[count], "%", "")) / 100
  income$`$50,000-$74,999`[count] <- as.numeric(str_replace(income$`$50,000-$74,999`[count], "%", "")) / 100
  income$`$75,000+`[count] <- as.numeric(str_replace(income$`$75,000+`[count], "%", "")) / 100
}

income <- income %>%
  transform(`Less than $30,000` = as.numeric(`Less than $30,000`),
            `$30,000-$49,999` = as.numeric(`$30,000-$49,999`),
            `$50,000-$74,999` = as.numeric(`$50,000-$74,999`),
            `$75,000+` = as.numeric(`$75,000+`))


# joining the age and gender of internet usage data
new_df <- merge(x = age, y = gender,
                by = "Year", 
                all = TRUE)

# joining age, gender, and income of internet usage data
join_df <- merge(x = new_df, y = income,
                 by = "Year",
                 all = TRUE)

# joining the internet dataset with Death rate by suicide data
df <- merge(x = join_df, y = death_df,
            by = "Year",
            all.x = TRUE)

# clear all rows containing NA values
df <- df %>%
  drop_na()

# New categorical column: Categorizing death rates by levels: Low, Moderate-Low, 
# Moderate, Moderate-High, High
death_categories <- c()

for (i in 1:nrow(df)) {
  if (df$ESTIMATE[i] <= 5) {
    death_categories[i] <- "Low"
  } else if (df$ESTIMATE[i] <= 10) {
    death_categories[i] <- "Moderate-Low"
  } else if (df$ESTIMATE[i] <= 15) {
    death_categories[i] <- "Moderate"
  } else if (df$ESTIMATE[i] <= 20) {
    death_categories[i] <- "Moderate-High"
  } else {
    death_categories[i] <- "High"
  }
}
df$Death_Category <- death_categories

# A group by summarise calculating the number of deaths with a hypothetical population of a 1,000,000 people (can change)
# and comparing it with internet usage by multiplying percent internet usage with ALL_AGE
combo_df <- df %>%
  group_by(Year, ALL_AGE, Gender, ESTIMATE)
combo_df <- combo_df %>%
  summarise(death_vs_internet = round((ESTIMATE/100000 * 1000000) * ALL_AGE))

write.csv(df, "cleaned.csv")