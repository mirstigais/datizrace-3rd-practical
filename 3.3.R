library(dplyr)

# Load dataset
vakances <- read.csv("vakances-2026-01-18.csv", stringsAsFactors = FALSE)

View(vakances)

# Remove 'Attels' and 'Vakances_paplasinats_apraksts' columns
vakances <- vakances %>%
  select(-Attels, -Vakances_paplasinats_apraksts)


# Fill Darba_stundas_nedela = 40
# if full-time and normal working hours
vakances <- vakances %>%
  mutate(
    Darba_stundas_nedela = ifelse(
      is.na(Darba_stundas_nedela) &
        Slodzes_tips == "Viena vesela slodze" &
        Darba_laika_veids == "Normālais darba laiks",
      40,
      Darba_stundas_nedela
    )
  )

# If Alga_lidz < 20, set Darba_stundas_nedela = 1
vakances <- vakances %>%
  mutate(
    Darba_stundas_nedela = ifelse(
      Alga_lidz < 20,
      1,
      Darba_stundas_nedela
    )
  )

# Remove rows with NA
vakances <- na.omit(vakances)

# Remove rows with empty strings
vakances <- vakances[!apply(vakances == "", 1, any), ]

# Filter out unrealistic working hours (0–60)
vakances <- vakances %>% filter(Darba_stundas_nedela > 0 & Darba_stundas_nedela <= 60)

# Create hourly wage variable
vakances <- vakances %>%
  mutate(
    Alga_stunda = ifelse(
      Alga_lidz < 20 & Darba_stundas_nedela == 1,
      Alga_lidz,
      Alga_lidz / (Darba_stundas_nedela * 4.33)
    )
  )

# Remove extreme hourly wages (top 1%)
upper_limit <- quantile(vakances$Alga_stunda, 0.99)
vakances <- vakances %>% filter(Alga_stunda < upper_limit)

vakances$Pilseta <- sapply(vakances$Vieta, function(x) {
  pilseta <- trimws(tail(unlist(strsplit(x, ",")), 1))
  pilseta <- gsub("\\d+.*", "", pilseta)  # make sure home numbers (like 1A) are not in the way
  pilseta <- trimws(pilseta)
  if(pilseta == "") return(NA)  # replace empty ones with NA to remove later
  return(pilseta)
})

# Remove NA
vakances <- vakances %>% filter(!is.na(Pilseta))

# Convert as a factor
vakances$Pilseta <- as.factor(vakances$Pilseta)

# Build the model
salary_hour_model_final <- lm(Alga_stunda ~
                                Vakances_kategorija +
                                Slodzes_tips +
                                Darba_laika_veids +
                                Pilseta +
                                Darba_stundas_nedela,
                              data = vakances)

summary(salary_hour_model_final)

# Predict vacancy hour rate by using newly created module
new_vacancy <- data.frame(
  Vakances_kategorija = "Informācijas tehnoloģijas / Telekomunikācijas",
  Slodzes_tips = "Viena vesela slodze",
  Darba_laika_veids = "Normālais darba laiks",
  Pilseta = "Rīga",
  Darba_stundas_nedela = 40
)

predicted_hourly_wage <- predict(
  salary_hour_model_final,
  newdata = new_vacancy
)

predicted_hourly_wage



library(ggplot2)

# Average working hour rate by city
city_salary <- vakances %>%
  group_by(Pilseta) %>%
  summarise(mean_salary = mean(Alga_stunda), .groups = "drop") %>%
  arrange(desc(mean_salary))

ggplot(city_salary, aes(x = reorder(Pilseta, mean_salary), y = mean_salary)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + 
  labs(title = "Darba stundas atalgojums pēc novadiem un pilsētam",
       x = "Pilsēta",
       y = "Darba stundas atalgojums (€ / st)") +
  theme_minimal(base_size = 12)

# Average working hour rate per category
category_salary <- vakances %>%
  group_by(Vakances_kategorija) %>%
  summarise(mean_salary = mean(Alga_stunda), .groups = "drop") %>%
  arrange(desc(mean_salary))

ggplot(category_salary, aes(x = reorder(Vakances_kategorija, mean_salary), y = mean_salary)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  labs(title = "Darba stundas atalgojums pēc kategorijam",
       x = "Vakanču kategorija",
       y = "Darba stundas atalgojums (€ / st)") +
  theme_minimal(base_size = 12)