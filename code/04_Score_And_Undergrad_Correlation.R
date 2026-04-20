#Does PISA score predict total undergraduate students sent to the US,
#regardless of field?

data("gisco_countrycode")

country_meta <- gisco_countrycode |>
  select(ISO3_CODE, country = iso.name.en)

pisa_avg <- bind_rows(
  pisamath_reg |> select(country, Value),
  pisareading_reg |> select(country, Value),
  pisascience_reg |> select(country, Value)
) |>
  summarise(avg_pisa_score = mean(Value, na.rm = TRUE), .by = country) |>
  mutate(iso3c = countrycode(country, "country.name", "iso3c"))

#International undergrad students in the US
students <- read_excel("data/OD25_Intl-Student-Census_Tables.xlsx", sheet = "8", skip = 5) |>
  select(code = 1, country = 2, N_Students = 6) |>
  mutate(across(c(code, N_Students), as.numeric)) |>
  filter(!is.na(code), !is.na(country), code %% 100 != 0,
         !code %in% c(3999, 4999), N_Students > 0) |>
  mutate(iso3c = countrycode(country, "country.name", "iso3c")) |>
  select(iso3c, N_Students)

#Population
population <- WDI(country = "all", indicator = "SP.POP.TOTL", start = 2020, end = 2020) |>
  select(iso3c, population = SP.POP.TOTL)

#Merge & plot
plot_4 <- pisa_avg |>
  inner_join(students, by = "iso3c") |>
  inner_join(pop, by = "iso3c") |>
  filter(iso3c != "USA") |>
  mutate(students_per_million = N_Students / pop_millions) |>
  ggplot(aes(avg_pisa_score, students_per_million)) +
  geom_point(size = 2.5, colour = cb10[9]) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey30",
              linetype = "dashed", linewidth = 0.5) +
  geom_text_repel(aes(label = iso3c), size = 2.5, max.overlaps = 20, segment.color = NA) +
  geom_text(
    aes(label = paste0("r = ", round(cor(avg_pisa_score, log10(students_per_million)), 2))),
    x = Inf, y = -Inf,
    hjust = 1.1, vjust = -1,
    size = 3.2, colour = "#4A4E6A", fontface = "bold",
    stat = "unique"
  ) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "IT'S A TRAP! PISA Score vs Undergraduate Students per Million (US)",
    x = "Average PISA Score",
    y = "Undergraduate Students per Million (log scale)",
    caption = "Sources: PISA (OECD), Open Doors 2025; population: World Bank 2020"
  ) +
  theme_custom() +
  theme(
    axis.title = element_text(size = rel(0.88), colour = "#4A4E6A")
  )

print(plot_4)
ggsave("plots/4.1 Score_VS_Undergraduate Students_per_Million_USA.png", plot_4, width = 12, height = 5, dpi = 150)

###INSIGHTS

#PISA explains field choice better than volume

#The majority of variance is explained by other factors: English language proficiency,
#wealth, visa access, cultural ties with the US
#Many European countries send students to the UK or France, not USA.