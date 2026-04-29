#What trends and patterns are there in our data?
#What can it tell us?

library(tidyverse)
library(plotly)
library(htmlwidgets)
library(giscoR)

#math scores time series
pisamath <- read_csv('data/pisamath.csv')
data("gisco_countrycode")
summary(pisamath) #starts at 2003 !!!

pisamath_reg <- pisamath |>
  select(LOCATION, SUBJECT, TIME, Value) |> 
  left_join(gisco_countrycode |>
              select(ISO3_CODE, continent, country = iso.name.en),
            by = c("LOCATION" = "ISO3_CODE")) |> 
  mutate(continent = case_when(
    LOCATION == "OAVG" ~ "OECD Average",
    TRUE ~ continent)) |> 
  select(-LOCATION)

pisamath_reg |>
  group_by(continent, TIME) |>
  summarise(avg_score = mean(Value, na.rm=T)) |> 
  arrange(desc(avg_score)) |> 
  ggplot(aes(TIME, avg_score)) + geom_line(aes(colour = continent))

#reading scores time series
pisareading <- read_csv('data/pisareading.csv')
summary(pisareading) #starts at 2000 !!!

pisareading_reg <- pisareading |>
  select(LOCATION, SUBJECT, TIME, Value) |> 
  left_join(gisco_countrycode |>
              select(ISO3_CODE, continent, country = iso.name.en),
            by = c("LOCATION" = "ISO3_CODE")) |> 
  mutate(continent = case_when(
    LOCATION == "OAVG" ~ "OECD Average",
    TRUE ~ continent)) |> 
  select(-LOCATION)

pisareading_reg |>
  group_by(continent, TIME) |>
  summarise(avg_score = mean(Value, na.rm=T)) |> 
  arrange(desc(avg_score)) |> 
  ggplot(aes(TIME, avg_score)) + geom_line(aes(colour = continent))

#science scores time series
pisascience <- read_csv('data/pisascience.csv')
summary(pisascience) #starts at 2006 !!!

pisascience_reg <- pisascience |>
  select(LOCATION, SUBJECT, TIME, Value) |> 
  left_join(gisco_countrycode |>
              select(ISO3_CODE, continent, country = iso.name.en),
            by = c("LOCATION" = "ISO3_CODE")) |> 
  mutate(continent = case_when(
    LOCATION == "OAVG" ~ "OECD Average",
    TRUE ~ continent)) |> 
  select(-LOCATION)

pisascience_reg |>
  group_by(continent, TIME) |>
  summarise(avg_score = mean(Value, na.rm=T)) |> 
  arrange(desc(avg_score)) |> 
  ggplot(aes(TIME, avg_score)) + geom_line(aes(colour = continent))

#united tibble
pisa_wide <- pisamath_reg |>
  select(country, continent, SUBJECT, TIME, math_score = Value) |>
  full_join(
    pisareading_reg |>
      select(country, continent, SUBJECT, TIME, reading_score = Value),
    by = c("country", "continent", "TIME", "SUBJECT")
  ) |>
  full_join(
    pisascience_reg |>
      select(country, continent, SUBJECT, TIME, science_score = Value),
    by = c("country", "continent", "TIME", "SUBJECT")
  )

#combined chart
pisa_long <- pisa_wide |>
  pivot_longer(cols = c(math_score, reading_score, science_score),
               names_to = "subject",
               values_to = "score") |>
  group_by(continent, TIME, subject) |>
  summarise(avg_score = mean(score, na.rm = T), .groups = "drop")

my_colours <- cb10[1:5]
names(my_colours) <- c("Americas", "Asia", "Europe", "OECD Average", "Oceania")

asia_spike_label <- tibble(
  subject = c("math_score", "reading_score", "science_score"),
  TIME = 2015,
  avg_score = c(516, 494, 508),
  label = c("SGP, HKG,\nTWN, MAC\njoined", "", "")
)

plot_1 <- ggplot(pisa_long, aes(TIME, avg_score, colour = continent, alpha = continent)) +
  geom_line(linewidth = 0.9) +
  geom_point(
    data = pisa_long,
    aes(alpha = continent),
    size = 1.4, show.legend = F
  ) +
  geom_text(
    data = asia_spike_label,
    aes(x = TIME, y = avg_score, label = label),
    colour = "#FC7D0B",
    size = 2.8,
    lineheight  = 0.85,
    hjust = -0.1,
    yjust = 2,
    inherit.aes = F
  ) +
  facet_wrap(~ subject, nrow = 1,
             labeller = as_labeller(c(
               math_score = "Math",
               reading_score = "Reading",
               science_score = "Science"
             ))) +
  scale_colour_manual(values = my_colours) + 
  scale_alpha_manual(
    values = c(
      Americas = 1, Asia = 1, Europe = 1,
      Oceania = 1, "OECD Average" = 0.15
    ),
    guide = "none"
  ) +
  scale_x_continuous(breaks = seq(2000, 2018, 6)) +
  labs(
    title = "PISA scores by continent, 2000–2018",
    subtitle = "Oceania is leading?",
    caption = "Source: PISA / OECD",
    colour = NULL
  ) +
  theme_custom() +
  theme(
    plot.subtitle = ggtext::element_markdown(size = rel(0.85), colour = "#4A4E6A",
                                                   hjust = 0.5, margin = margin(b = 12, t =10)),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.text = element_text(size = rel(0.85), colour = "#2E3250"),
    legend.key.width = unit(20, "pt"),
    legend.key.height = unit(2, "pt"),
    legend.spacing.x = unit(6, "pt"),
  )

print(plot_1)
ggsave("plots/1.1 Pisa Scores by Continent Over Time.png", plot_1, width = 12, height = 4, dpi = 150)

###INSIGHTS

#1. Oceania performs best in every category
unique(pisa_wide$country[pisa_wide$continent == "Oceania"])
#Oceania consists of only Australia and New Zealand
#-> less developed countries are excluded
#India does not participate in Pisa  !!!

#2. Asia has a spike around 2015
pisa_wide |>
  filter(continent == "Asia") |>
  pivot_longer(
    cols = c(math_score, reading_score, science_score),
    names_to = "subject",
    values_to = "score"
  ) |>
  group_by(subject, TIME) |>
  summarise(avg_score = mean(score, na.rm = TRUE), .groups = "drop") |>
  group_by(subject) |>
  slice_max(avg_score, n = 1, with_ties = FALSE) |>
  select(subject, TIME, avg_score) |>
  mutate(continent = "Asia")

unique(pisa_wide$country[pisa_wide$continent == "Asia" &
                           pisa_wide$TIME == 2009])
#"JPN" "KOR" "TUR" "IDN" "ISR" participated in 2009

unique(pisa_wide$country[pisa_wide$continent == "Asia" &
                           pisa_wide$TIME == 2015])
#"JPN" "KOR" "TUR" "IDN" "ISR" "SGP" "HKG" "TWN" "MAC"
#participated in 2015 => the spike

unique(pisa_wide$country[pisa_wide$continent == "Asia" &
                           pisa_wide$TIME == 2018])
#list of participants fell back to the initial one => fall

#average math score per Asian country
avg_scores <- pisa_wide |>
  filter(continent == "Asia") |>
  group_by(country) |>
  summarise(avg_score = round(mean(math_score, na.rm = TRUE), 1), .groups = "drop")

#chart: country & number of participations
pisa_asian_countries <- pisa_wide |>
  filter(continent == "Asia") |>
  distinct(country, TIME) |>
  count(country, name = "N_participations") |>
  left_join(avg_scores, by = "country")

plot_1_2 <- ggplot(pisa_asian_countries, aes(x = reorder(country, -(avg_scores$avg_score)),
             y = N_participations,
             fill = avg_scores$avg_score)) +
  geom_col() +
  geom_text(
    aes(label = paste0("avg: ", avg_scores$avg_score)),
    vjust = -0.5,
    size = 4
  ) +
  scale_fill_gradientn(
    colours = c("#a8c8e8", "#5b9dc9", "#2166ac", "#0a3d6b"),
    name = "Avg Score",
    guide = guide_colorbar(
      title.position = "top",
      barwidth = 10,
      barheight = 1
    )
  ) +
  labs(
    title = "Asia: Number of PISA Participations VS Score",
    x = "Country",
    y = "Number of Participations"
  ) +
  theme_custom()  + theme(
    axis.title.y = element_text(),
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5)
  )

ggsave("plots/1.2 Asia--Number of PISA Participations VS Score.png", plot_1_2, width = 11, height = 5, dpi = 150)

### INSIGHTS
# Asian countries with the highest scores almost never participate.