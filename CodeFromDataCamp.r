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


