library(haven)

df <- read_dta("welfare_ps5.dta")
df$fed <- ifelse(df$fed == 2, 1, df$fed)
# ------------------------------------------------------------------------------
colnames(df)
# ------------------------------------------------------------------------------
unique(df$country)
# ------------------------------------------------------------------------------
start_year <- min(df$year)
end_year <- max(df$year)

# print out the time period
cat("The data covers the period from", start_year, "to", end_year)
# ------------------------------------------------------------------------------
# subset the data for year 2017
df_2017 <- subset(df, year == 2017)

# find the row index of the country with the highest TOT_GEN score in 2017
row_index <- which.max(df_2017$TOT_GEN)

# get the country name and TOT_GEN score for the row with the highest score
country <- df_2017$country[row_index]
score <- df_2017$TOT_GEN[row_index]

# print out the result
cat("The country with the highest TOT_GEN score in 2017 is", country, "with a score of", score)
# ------------------------------------------------------------------------------
# subset the data for year 2017
df_2017 <- subset(df, year == 2017)

# find the row index of the country with the lowest TOT_GEN score in 2017
row_index <- which.min(df_2017$TOT_GEN)

# get the country name and TOT_GEN score for the row with the lowest score
country <- df_2017$country[row_index]
score <- df_2017$TOT_GEN[row_index]

# print out the result
cat("The country with the lowest TOT_GEN score in 2017 is", country, "with a score of", score)
# ------------------------------------------------------------------------------
# subset the data for countries with presidential systems
presidential_countries <- unique(subset(df, pres == 1)$country)

# print out the result
cat("The countries with Presidential systems are:", presidential_countries)
# ------------------------------------------------------------------------------
# subset the data for countries with federal systems
federal_countries <- unique(subset(df, fed == 1)$country)

# print out the result
cat("The countries with Federal systems are:", federal_countries)
# ------------------------------------------------------------------------------
library(dplyr)

# check for changes in federal system
df %>% 
  group_by(country) %>% 
  summarize(min_year = min(year), max_year = max(year), unique_fed = length(unique(fed))) %>% 
  filter(unique_fed > 1)

# check for changes in presidential system
df %>% 
  group_by(country) %>% 
  summarize(min_year = min(year), max_year = max(year), unique_pres = length(unique(pres))) %>% 
  filter(unique_pres > 1)
# ------------------------------------------------------------------------------
library(dplyr)

# calculate the average GDP growth rate for each country
gdp_growth_by_country <- df %>%
  group_by(country) %>%
  summarize(avg_gdp_growth = mean(gdp_growth_oecd, na.rm = TRUE))

# sort the countries based on their average GDP growth rates
ranked_gdp_growth <- gdp_growth_by_country %>%
  arrange(avg_gdp_growth, .by_group = TRUE) %>%
  pull(country)

# find the rank of the United States
us_rank <- which(ranked_gdp_growth == "United States")

# print out the result
cat("The rank of the United States in terms of average GDP growth rate during this period is", us_rank)
# ------------------------------------------------------------------------------
library(dplyr)

# calculate the average GDP growth rate for each country
gdp_growth_by_country <- df %>%
  group_by(country) %>%
  summarize(avg_gdp_growth = mean(gdp_growth_oecd, na.rm = TRUE))

# sort the countries based on their average GDP growth rates
ranked_gdp_growth <- gdp_growth_by_country %>%
  arrange(avg_gdp_growth, .by_group = TRUE)

# extract the country with the highest and lowest average GDP growth rates
fastest_growing <- tail(ranked_gdp_growth, n = 1)
slowest_growing <- head(ranked_gdp_growth, n = 1)

# print out the result
cat("The country that grew the fastest on average in this period is", fastest_growing$country, "with an average GDP growth rate of", round(fastest_growing$avg_gdp_growth, 2))
cat("\nThe country that grew the slowest on average in this period is", slowest_growing$country, "with an average GDP growth rate of", round(slowest_growing$avg_gdp_growth, 2))

# calculate the cumulative GDP growth rate for each country
gdp_growth_by_country <- df %>%
  group_by(country) %>%
  summarize(cumulative_gdp_growth = prod(1 + gdp_growth_oecd/100) - 1)

# sort the countries based on their cumulative GDP growth rates
ranked_cumulative_gdp_growth <- gdp_growth_by_country %>%
  arrange(cumulative_gdp_growth, .by_group = TRUE)

# extract the country with the highest and lowest cumulative GDP growth rates
fastest_growing_cumulative <- tail(ranked_cumulative_gdp_growth, n = 1)
slowest_growing_cumulative <- head(ranked_cumulative_gdp_growth, n = 1)

# print out the result
cat("The country that grew the fastest based on the cumulative rate of growth in the entire period is", fastest_growing_cumulative$country, "with a cumulative GDP growth rate of", round(fastest_growing_cumulative$cumulative_gdp_growth, 2))
cat("\nThe country that grew the slowest based on the cumulative rate of growth in the entire period is", slowest_growing_cumulative$country, "with a cumulative GDP growth rate of", round(slowest_growing_cumulative$cumulative_gdp_growth, 2))
# ------------------------------------------------------------------------------
# estimate the regression model
model <- lm(TOT_GEN ~ unemr + fed + pres + gdp_growth_oecd + lag_left_gov, data = df)

# print the model summary
summary(model)
# ------------------------------------------------------------------------------
library(plm)

df_panel <- pdata.frame(df, index = c("country", "year"))

# estimate the two-way fixed effects model
model2 <- plm(TOT_GEN ~ unemr + fed + pres + gdp_growth_oecd + lag_left_gov, 
             data = df_panel, 
             index = c("country", "year"), 
             model = "within", 
             effect = "twoways")

# print the model summary
summary(model2)
# ------------------------------------------------------------------------------
# estimate the regression model
model3 <- lm(gdp_growth_oecd ~ lag_generosity + lag_left_gov + fed + pres, data = df)

# print the model summary
summary(model3)
# ------------------------------------------------------------------------------
# convert the data frame to a panel data format
df_panel <- pdata.frame(df, index = c("country", "year"))

# estimate the model with fixed effects for country and year
model4 <- plm(gdp_growth_oecd ~ lag_generosity + lag_left_gov + fed + pres,
             data = df_panel, model = "within", effect = "twoways", index = c("country", "year"))

# print the model summary
summary(model4)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------





