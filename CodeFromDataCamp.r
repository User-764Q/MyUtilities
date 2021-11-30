# this file is to store some code I might want to use again that I learned from datacamp 

# mean of a binary 

# Start with by_year_country dataset
by_year_country <- votes_processed %>%
  group_by(year, country) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))


# Analysing a bunch of different models with adjusted p values nest and un-nest. 

# Add one more step that unnests the tidied column
country_coefficients <- by_year_country %>%
  nest(-country) %>%
  mutate(model = map(data, ~ lm(percent_yes ~ year, data = .)),
         tidied = map(model, tidy)) %>%
         unnest(tidied)
# Filter for only the slope terms
slope_terms <- country_coefficients %>%
  filter(term == "year")

# Add p.adjusted column, then filter
slope_terms %>%
mutate(p.adjusted = p.adjust(p.value)) %>%
  filter(p.adjusted < .05)

# Print the resulting country_coefficients variable
country_coefficients

# interquartile_range 

# Lower limit = Q1 – 1.5*IQR = 5 – 1.5*15.75 = -18.625

# And the upper limited is calculated as:

# Upper limit = Q3 + 1.5*IQR = 20.75 + 1.5*15.75 = 44.375

# From previous step
prediction_data <- explanatory_data %>% 
  mutate(   
    has_churned = predict(mdl_churn_vs_relationship, explanatory_data, type = "response"),
    most_likely_outcome = round(has_churned)
  )

# Fit a logistic regression of churn vs. 
# length of relationship using the churn dataset
mdl_churn_vs_relationship <- glm(has_churned 
~ time_since_first_purchase, data = churn, family = binomial)

# Update the plot
plt_churn_vs_relationship +
  # Add most likely outcome points from prediction_data, 
  # colored yellow, size 2
  geom_point(data = prediction_data, aes(x = time_since_first_purchase, y = most_likely_outcome), size = 2, color = 'yellow')


