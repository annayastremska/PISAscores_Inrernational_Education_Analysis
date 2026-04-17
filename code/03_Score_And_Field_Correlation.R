#Do countries where students score higher on PISA tests
#also send more students abroad to study the related field?

library(readxl)
library(countrycode)
library(ggrepel)
library(WDI)

#Pisa dataset
pisa <- pisa_wide |>
  filter(!is.na(country)) |>
  pivot_longer(cols = c(math_score, reading_score, science_score),
               names_to = "subject", values_to = "score") |>
  mutate(
    subject = str_remove(subject, "_score"),
    iso3c   = countrycode(country, "country.name", "iso3c")
  ) |>
  summarise(avg_score = mean(score, na.rm = TRUE), .by = c(iso3c, subject))

#International students dataset
fields <- read_excel("data/OD25_Intl-Student-Census_Tables.xlsx",
                     sheet = "5", skip = 3) |>
  set_names(c("country", "total", "business", "education", "engineering",
              "fine_arts", "health", "humanities", "intensive_english",
              "math_cs", "physical_life_sciences", "social_sciences",
              "other", "undeclared")) |>
  filter(!is.na(total), !str_detect(country, "^\\*|^Note")) |>
  mutate(
    across(total:undeclared, as.numeric),
    iso3c               = countrycode(str_trim(country), "country.name", "iso3c"),
    n_math_cs           = total * math_cs             / 100,
    n_physical_sciences = total * physical_life_sciences / 100,
    n_humanities        = total * humanities           / 100
  ) |>
  select(iso3c, total, n_math_cs, n_physical_sciences, n_humanities)

#Population dataset 
pop <- WDI(indicator = "SP.POP.TOTL", start = 2019, end = 2019, extra = TRUE) |>
  filter(!is.na(iso3c)) |>
  transmute(iso3c, pop_millions = SP.POP.TOTL / 1e6)

#Merge
dat <- pisa |>
  inner_join(fields, by = "iso3c") |>
  inner_join(pop,    by = "iso3c") |>
  mutate(
    n_stem_field = case_when(
      subject == "math"    ~ n_math_cs,
      subject == "science" ~ n_physical_sciences,
      subject == "reading" ~ n_humanities
    ),
    students_per_million = n_stem_field / pop_millions,
    subject_label = case_when(
      subject == "math"    ~ "Math → Math/CS",
      subject == "science" ~ "Science → Physical & Life Sci",
      subject == "reading" ~ "Reading → Humanities"
    )
  )

#Correlation: Pisa score & Adjacent field of study
cor_labels <- dat |>
  group_by(subject_label) |>
  summarise(
    r = cor(avg_score, log10(students_per_million), use = "complete.obs"),
    .groups = "drop"
  ) |>
  mutate(label = paste0("r = ", round(r, 2)))

#Plotting
plot_3 <- ggplot(dat, aes(avg_score, students_per_million)) +
  geom_point(aes(colour = subject), size = 2.5, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey30",
              fill = "grey80", linetype = "dashed", linewidth = 0.5,
              fullrange = FALSE) +
  geom_text_repel(aes(label = iso3c), size = 2.5, max.overlaps = 20, segment.color = NA) +
  geom_text(
    data = cor_labels,
    aes(label = label),
    x = Inf, y = -Inf,
    hjust = 1.1, vjust = -1,
    size = 3.2, colour = "#4A4E6A", fontface = "bold",
    inherit.aes = FALSE
  ) +
  facet_wrap(~subject_label, scales = "free") +
  scale_y_log10(labels = scales::comma) +
  scale_colour_manual(values = cb10) +
  labs(
    title   = "Score and Adjacent Field of Study Correlation",
    x       = "Average PISA Score",
    y       = "Students in matched field per million population (log)",
    caption = "Sources: PISA (OECD), Open Doors 2025 International Student Census; population: World Bank 2019"
  ) +
  theme_custom() +
  theme(
    axis.title = element_text(size = rel(0.88), colour = "#4A4E6A")
  )

print(plot_3)
ggsave("plots/3.1 Score_And_Field_Correlation.png", plot_3, width = 12, height = 5, dpi = 150)


###INSIGHTS

#READING: Countries where 15-year-olds read well tend to send
#many students to humanities programs abroad.

#The analysis only captures students studying abroad in the US,
#not global enrollment

#Correlation ≠ causation: wealthier countries tend to score higher
#and send more students abroad, so GDP could be a dominant variable