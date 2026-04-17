library(patchwork)

#Math gap calculation
math_gap_2018 <- pisa_wide |>
  filter(!is.na(math_score), SUBJECT %in% c("BOY", "GIRL"), TIME == 2018) |>
  select(country, continent, SUBJECT, math_score) |>
  pivot_wider(names_from = SUBJECT, values_from = math_score) |>
  mutate(
    gap     = BOY - GIRL,
    iso3c   = countrycode(country, "country.name", "iso3c"),
    subject = "Math"
  ) |>
  filter(!is.na(gap)) |>
  arrange(gap) |>
  mutate(iso3c = fct_inorder(iso3c))

plot_5 <- ggplot(math_gap_2018, aes(gap, iso3c, fill = continent)) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.4) +
  geom_text(aes(label = iso3c,
                hjust = if_else(gap >= 0, -0.2, 1.2)),
            size = 2, colour = "grey20") +
  scale_fill_manual(
    values = c(
      "Americas"     = "#E8956D",   
      "Asia"         = "#6AABD2",  
      "Europe"       = "#A8C770",  
      "Oceania"      = "#C49FD4",   
      "OECD Average" = "#F0C675"   
    ),
    name = NULL
  ) +
  facet_wrap(~ subject) +
  labs(x = "Score difference: Boys − Girls", y = NULL) +
  theme_custom(base_size = 20) +
  theme(
    axis.text.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    axis.title.x       = element_text(size = rel(0.88), colour = "#4A4E6A"),
    legend.position    = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

print(plot_5)

#Reading gap calculation
reading_gap_2018 <- pisa_wide |>
  filter(!is.na(reading_score), SUBJECT %in% c("BOY", "GIRL"), TIME == 2018) |>
  select(country, continent, SUBJECT, reading_score) |>
  pivot_wider(names_from = SUBJECT, values_from = reading_score) |>
  mutate(
    gap     = BOY - GIRL,
    iso3c   = countrycode(country, "country.name", "iso3c"),
    subject = "Reading"
  ) |>
  filter(!is.na(gap)) |>
  arrange(gap) |>
  mutate(iso3c = fct_inorder(iso3c))

plot_5b <- ggplot(reading_gap_2018, aes(gap, iso3c, fill = continent)) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.4) +
  geom_text(aes(label = iso3c,
                hjust = if_else(gap >= 0, -0.2, 1.2)),
            size = 2, colour = "grey20") +
  scale_fill_manual(
    values = c(
      "Americas"     = "#E8956D",
      "Asia"         = "#6AABD2",
      "Europe"       = "#A8C770",
      "Oceania"      = "#C49FD4",
      "OECD Average" = "#F0C675"
    ),
    name = NULL
  ) +
  facet_wrap(~ subject) +
  labs(x = "Score difference: Boys − Girls", y = NULL) +
  theme_custom(base_size = 20) +
  theme(
    axis.text.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    axis.title.x       = element_text(size = rel(0.88), colour = "#4A4E6A"),
    legend.position    = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

#Science gap calculation
science_gap_2018 <- pisa_wide |>
  filter(!is.na(science_score), SUBJECT %in% c("BOY", "GIRL"), TIME == 2018) |>
  select(country, continent, SUBJECT, science_score) |>
  pivot_wider(names_from = SUBJECT, values_from = science_score) |>
  mutate(
    gap     = BOY - GIRL,
    iso3c   = countrycode(country, "country.name", "iso3c"),
    subject = "Science"
  ) |>
  filter(!is.na(gap)) |>
  arrange(gap) |>
  mutate(iso3c = fct_inorder(iso3c))

plot_5c <- ggplot(science_gap_2018, aes(gap, iso3c, fill = continent)) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.4) +
  geom_text(aes(label = iso3c,
                hjust = if_else(gap >= 0, -0.2, 1.2)),
            size = 2, colour = "grey20") +
  scale_fill_manual(
    values = c(
      "Americas"     = "#E8956D",
      "Asia"         = "#6AABD2",
      "Europe"       = "#A8C770",
      "Oceania"      = "#C49FD4",
      "OECD Average" = "#F0C675"
    ),
    name = NULL
  ) +
  facet_wrap(~ subject) +
  labs(x = "Score difference: Boys − Girls", y = NULL) +
  theme_custom(base_size = 20) +
  theme(
    axis.text.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    axis.title.x       = element_text(size = rel(0.88), colour = "#4A4E6A"),
    legend.position    = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

#Combined chart
plot_5_combined <- (
  (
    plot_5 + plot_5b + plot_5c +
      plot_layout(ncol = 3, guides = "collect")
  ) &
    theme(legend.position = "top")
) +
  plot_annotation(
    title    = "Gender Gap in PISA (2018)",
    subtitle = "Negative = girls outperform boys",
    caption  = "Source: PISA (OECD)",
    theme    = theme_custom(base_size = 20) + theme(
      plot.title    = element_text(size = rel(1.35), face = "bold",
                                   colour = "#1A1E3A", hjust = 0.5,
                                   margin = margin(b = 2)),
      plot.subtitle = element_text(size = rel(0.92), colour = "#4A4E6A",
                                   hjust = 0.5, margin = margin(b = 12, t = 10)),
      plot.caption  = element_text(size = rel(0.85), colour = "#8A8FA8",
                                   hjust = 1, margin = margin(t = 8))
    )
  )

print(plot_5_combined)
ggsave("plots/5.1 Gender_Gap_All_Subjects.png", plot_5_combined, width = 12, height = 7, dpi = 150)

###INSIGHTS

#Boys lead in math almost everywhere; the gap is largest in Latin America and Southern Europe.
#Girls lead in math only in Northern/Eastern Europe and a few Asian countries —
#where gender equality norms are strongest (Nordics) or the reversal is observed without a clear explanation (East Asia).

#Girls lead in reading in every single country, often by more than boys lead in math.
#Leading explanation: girls develop verbal skills earlier and read more for pleasure;
#this holds regardless of cultural context, suggesting a developmental component.

#Science is mixed — slight average boy advantage, but many near-zero gaps.
#No clean STEM pattern: science gaps are country-specific, not universal.