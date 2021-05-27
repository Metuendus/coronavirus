# Load Libaries
if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(coronavirus,
              countrycode,
              plotly,
              tidyverse)

# Keep the installed version with the most recent data available
update_dataset()
rs.restartR()

# Analysis ----
data("coronavirus")
head(coronavirus)

# Summary of the total confrimed cases by country (top 20)
summary_df <- coronavirus %>%
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases)

summary_df %>% head(20)

# Summary of new cases during the past 24 hours by country and type
coronavirus %>%
  filter(date == max(date)) %>%
  select(country, type, cases) %>%
  group_by(country, type) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type,
              values_from = total_cases) %>%
  arrange(-confirmed)

# Plotting the total cases by type worldwide
coronavirus %>%
  group_by(type, date) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total_cases) %>%
  arrange(date) %>%
  mutate(active = confirmed - death - recovered) %>%
  mutate(active_total = cumsum(active),
        recovered_total = cumsum(recovered),
        death_total = cumsum(death)) %>%
  plot_ly(x = ~ date,
          y = ~ active_total,
          name = "Active",
          fillcolor = "#1f77b4",
          type = "scatter",
          mode = "none",
          stackgroup = "one") %>%
  add_trace(y = ~ death_total,
            name = "Death",
            fillcolor = "#E41317") %>%
  add_trace(y = ~recovered_total,
            name = "Recovered",
            fillcolor = "forestgreen") %>%
  layout(title = "Distribution of Covid19 Cases Worldwide",
        legend = list(x = 0.1, y = 0.9),
        yaxis = list(title = "Number of Cases"),
        xaxis = list(title = "Source: Johns Hopkins University Center for Systems Science and Engineering"))

# Plot the confirmed cases distribution by counrty with treemap plot
conf_df <- coronavirus %>%
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases) %>%
  mutate(parents = "Confirmed") %>%
  ungroup()

plot_ly(data = conf_df,
        type = "treemap",
        values = ~total_cases,
        labels = ~ country,
        parents = ~ parents,
        domain = list(column = 0),
        name = "Confirmed",
        textinfo = "label+value+percent parent")

# Plot confirmed cases by country with map plot
conf_df$code <- countrycode(conf_df$country,
                            origin = "country.name",
                            destination = "iso3c")
map <- plot_ly(conf_df,
              type = "choropleth",
              locations = conf_df$code,
              z = conf_df$total_cases,
              text = conf_df$country,
              colorscale = "Viridis")
map <- map %>% colorbar(title = "Coronavirus cases",
                        len = 1)
map <- map %>% layout(title = "Coronavirus cases by country")
map


# Plot cases per millon by country

coronavirus$cases