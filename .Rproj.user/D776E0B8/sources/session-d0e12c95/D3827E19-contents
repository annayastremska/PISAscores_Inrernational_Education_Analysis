#Do developing countries converge with time?

pisa_country <- pisa_wide |>
  pivot_longer(cols = c(math_score, reading_score, science_score),
               names_to = "subject", values_to = "score") |>
  group_by(country, continent, TIME) |>
  summarise(avg_score = mean(score, na.rm = TRUE), .groups = "drop")

pisa_convergence <- pisa_country |>
  filter(!is.na(country)) |>
  group_by(country) |>
  filter(n_distinct(TIME) >= 3) |>
  ungroup() |>
  group_by(country, continent) |>
  summarise(
    first_score = avg_score[TIME == min(TIME)],
    last_score  = avg_score[TIME == max(TIME)],
    .groups = "drop"
  ) |>
  mutate(change = last_score - first_score)

# label only top 5 and bottom 5 by change
label_countries <- c(
  pisa_convergence |> arrange(desc(change)) |> slice_head(n = 5) |> pull(country),
  pisa_convergence |> arrange(change)       |> slice_head(n = 5) |> pull(country)
)

# plot
plot_2 <- pisa_convergence |>
  ggplot(aes(first_score, change, colour = continent)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", linetype = "dashed", linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  ggrepel::geom_text_repel(
    aes(label = ifelse(country %in% label_countries, country, "")), size = 4,
    max.overlaps = 15,
    box.padding = 0.4,
    segment.color = NA
  ) +
  labs(
    title = "Convergence in PISA scores",
    x = "Initial average score",
    y = "Change in score (last - first)",
    caption  = "Source: PISA / OECD",
    colour = ""
  ) +
  scale_colour_manual(values = cb10) +
  theme_custom() +
  theme(
    legend.direction  = "horizontal",
    legend.text       = element_text(size = rel(0.95), colour = "#2E3250"),
    legend.key.width  = unit(20, "pt"),
    legend.key.height = unit(2, "pt"),
    legend.spacing.x  = unit(6, "pt"),
    axis.title        = element_text(size = rel(0.98), colour = "#4A4E6A")
  )

print(plot_2)
ggsave("plots/2.1 Convergence in PISA Scores.png", plot_2, width = 12, height = 5, dpi = 150)

###INSIGHTS

#Countries that started low improved; countries that started
#high declined or stagnated

#Top 5 by Change:
#Luxemburg, Poland, Chile, Turkey, Latvia
pisa_convergence |> 
  arrange(desc(change)) |> 
  head(5)
#Developing countries that implement efficient educational policies
#have rising results

#   - (outlier) Luxembourg's early scores were depressed by a multilingual student
#     population; improvement may partly reflect demographic or curricular
#     adjustments rather than a general quality gain.
#   - Poland and Latvia implemented structural education reforms during
#     this period and are frequently cited as positive examples in the
#     OECD literature — but attributing the score gains to those specific
#     reforms goes beyond what this chart can show.
#   - Chile and Turkey were among the fastest-growing economies in their
#     regions over the same period; rising incomes and school enrolment
#     rates are plausible contributors alongside any policy changes.


#Bottom 5 by Change:
#Austria, Iceland, Finland, New Zealand, The Netherlands 
pisa_convergence |> 
  arrange(change) |> 
  head(5)

#Wealthy developed countries experience a downfall in performance

#TOP AND BOTTOM PERFORMERS CLOSER LOOK
top3    <- pisa_convergence |> arrange(desc(first_score)) |> slice_head(n = 3) |> pull(country)
bottom3 <- pisa_convergence |> arrange(first_score)      |> slice_head(n = 3) |> pull(country)
# top3:    Finland, Canada, New Zealand
# bottom3: Indonesia, Brazil, Chile

slope_colours <- c(
  "Finland"     = "#7F77DD",
  "Canada"      = "#D85A30",
  "New Zealand" = "#D4537E",
  "Indonesia"   = "#378ADD",
  "Brazil"      = "#1D9E75",
  "Chile"       = "#BA7517"
)

slope_data <- pisa_convergence |>
  filter(country %in% c(top3, bottom3)) |>
  mutate(
    group = if_else(country %in% top3, "Top 3 (2000)", "Bottom 3 (2000)"),
    group = factor(group, levels = c("Bottom 3 (2000)", "Top 3 (2000)"))
  ) |>
  select(country, group, first_score, last_score) |>
  pivot_longer(c(first_score, last_score), names_to = "period", values_to = "score") |>
  mutate(period = factor(if_else(period == "first_score", "First\n2000", "Last\n2018"),
                         levels = c("First\n2000", "Last\n2018")))

#plotting
plot_2_2 <- slope_data |>
  ggplot(aes(period, score, colour = country, group = country)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3.5) +
  ggrepel::geom_text_repel(
    data          = \(d) filter(d, period == "First\n2000"),
    aes(label     = paste0(country, "  ", round(score))),
    direction     = "y",
    hjust         = 1,
    nudge_x       = -0.15,
    segment.color = NA,
    size          = 3.4
  ) +
  ggrepel::geom_text_repel(
    data          = \(d) filter(d, period == "Last\n2018"),
    aes(label     = round(score)),
    direction     = "y",
    hjust         = 0,
    nudge_x       = 0.15,
    segment.color = NA,
    size          = 3.4
  ) +
  facet_wrap(~ group, scales = "free_y") +
  scale_colour_manual(values = slope_colours, name = NULL) +
  scale_x_discrete(expand = expansion(add = c(2.2, 1.0))) +
  labs(
    title   = "PISA Score Change: Top VS Bottom Initial Scorers",
    x       = NULL,
    y       = "Average PISA score",
    caption = "Source: PISA / OECD"
  ) +
  theme_custom() +
  theme(
    strip.background = element_rect(fill = "#2E3250", colour = NA),
    strip.text       = element_text(colour = "white", face = "bold"),
    strip.placement  = "outside"
  )

ggsave("plots/2.2 Top VS Bottom Inital Scorers Over Time.png",
       plot_2_2, width = 11, height = 5, dpi = 150)
