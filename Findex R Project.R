library(tidyverse)
dataframe <- read.csv("findex_microdata2021.csv")
dataframe_small <- dataframe %>% select(economy, economycode, regionwb, pop_adult, wgt, account, female, educ, country_access_clean_fuels, country_access_electricity, country_GDP_ppp, country_life_expectancy, country_net_migration, country_carbon_emissions)
dataframe_small_renamed <- dataframe_small %>% rename(Country = economy, Country_code = economycode, Region = regionwb, Adult_population = pop_adult, Weight = wgt, Has_account = account, Gender = female, Education = educ, Clean_Fuel = country_access_clean_fuels, Electricity_Access = country_access_electricity, GDP_PPP = country_GDP_ppp, Life_expectancy = country_life_expectancy, Migration = country_net_migration, Carbon_emissions = country_carbon_emissions)
data_transformed <- dataframe_small_renamed %>%
  mutate(
    Has_account = case_when(
      Has_account == 1 ~ "Yes",
      Has_account == 0 ~ "No",
      TRUE ~ NA_character_
    ),
    Gender = case_when(
      Gender == 1 ~ "Female",
      Gender == 2 ~ "Male",
      TRUE ~ NA_character_
    ),
    Education = case_when(
      Education == 0 ~ "No formal education",
      Education == 1 ~ "Primary",
      Education == 2 ~ "Secondary",
      Education == 3 ~ "Tertiary",
      TRUE ~ NA_character_
    )
  )
Distribution <- data_transformed %>%
  filter(!is.na(Has_account), !is.na(Gender), !is.na(Education)) %>%
  group_by(Gender, Education, Has_account) %>%
  summarise(weighted_n = sum(Weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(Gender, Education) %>%
  mutate(percent = weighted_n / sum(weighted_n) * 100)
library(ggplot2)

ggplot(Distribution, aes(x = Education, y = percent, fill = Has_account)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(percent, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.25, size = 3) +
  facet_wrap(~ Gender) +
  labs(
    title = "Financial Inclusion by Gender and Education Level",
    x = "Education Level",
    y = "Percentage (%)",
    fill = "Has Account"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
data_transformed_cleaned <- data_transformed %>%
  mutate(GDP_per_capita = GDP_PPP / Adult_population)
Country_level <- data_transformed_cleaned %>%
  filter(!is.na(Has_account), !is.na(GDP_per_capita)) %>%
  group_by(Country_code, Country, Region, GDP_per_capita) %>%
  summarise(
    total_weight = sum(Weight, na.rm = TRUE),
    account_weight = sum(Weight[Has_account == "Yes"], na.rm = TRUE),
    percent_with_account = account_weight / total_weight * 100,
    .groups = "drop"
  )
Scatter_plot <- ggplot(Country_level, aes(x = GDP_per_capita, y = percent_with_account, color = Region)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +  # optional trend line
  labs(
    title = "Financial Inclusion vs. GDP per Capita",
    x = "GDP per Capita (PPP, 2011 USD)",
    y = "Percentage with an Account",
    color = "World Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )
print(Scatter_plot)
Country_variables <- dataframe_small_renamed %>% select(Country, Country_code, Region, Adult_population, Weight, Has_account, Gender, Education, Clean_Fuel, Electricity_Access, GDP_PPP, Life_expectancy, Migration, Carbon_emissions) %>% distinct()
Country_level_final <- Country_level %>% left_join(Country_variables, by = "Country_code")
Country_level_clean <- Country_level_final %>% filter(!is.na(percent_with_account),!is.na(GDP_PPP),!is.na(Life_expectancy))
model <- lm(percent_with_account ~ GDP_PPP + Life_expectancy, data = Country_level_clean)
summary(model)

# interpret
# Both GDP_PPP and Life_expectancy are highly significant predictors of percent_with_account, with p-values < 2e-16.
# The model explains about 43.1% of the variation in account ownership across countries (R-squared = 0.4314).
# The coefficients suggest that higher GDP_PPP and longer life expectancy are associated with increased account ownership.
