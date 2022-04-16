if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidytuesdayR, janitor, transformr, # transformr if needed
               gganimate, rgdal, sf, rnaturalearth, rnaturalearthdata)

# Set your GH PAT if you haven't!
# credentials::set_github_pat()

tuesdata <- tt_load('2022-04-12',
                    auth = github_pat())

death_source <- tuesdata$death_source

world <- ne_countries(scale = "medium", 
                      continent = c("Europe",
                                    "North America",
                                    "South America",
                                    "Asia",
                                    "Australia",
                                    "Africa",
                                    "Australia",
                                    "Oceania"),
                      returnclass = "sf")

# Data setup

death_source <- death_source %>%
  clean_names() %>%
  filter(!is.na(code)) %>%
  rename(death_iap = deaths_cause_all_causes_risk_household_air_pollution_from_solid_fuels_sex_both_age_age_standardized_rate)

# Map setup

world_year <- NULL

# Note: dplyr::expand() does not like geometry types

for (y in unique(death_source$year)) {
  
  world$year <- y
  
  world_year <- bind_rows(world_year,
                          world)
  
}

timeseries_world_map <- world_year %>%
  select(iso_a3, year) %>%
  left_join(death_source %>%
              select(code, year, death_iap),
             by = c("iso_a3" = "code", "year"))

# Change projection

target_crs <- "+proj=eqearth +wktext"

timeseries_world_map_proj <- st_transform(timeseries_world_map, target_crs)

# Create map and animate

ocean_color = "#cfeffa"

ts_plot <- ggplot(data = timeseries_world_map_proj) +
  geom_sf(aes(fill = death_iap),
          lwd = 0.25) +
  scale_fill_viridis_c(option = "viridis",
                       direction = -1) +
  theme_void() +
  theme(plot.background = element_rect(fill = ocean_color,
                                       color = ocean_color),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Mortality Due to Indoor Air Pollution: {closest_state}",
       fill = "Deaths per 100,000",
       caption = "Data: Our World in Data courtesy of Tidy Tuesday") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5)) +
  transition_states(year, state_length = 0, wrap = FALSE) +
  ease_aes("linear")

animate(ts_plot, 
        nframes = 300, 
        fps = 50, 
        height = 600,
        width = 1200,
        end_pause = 20)

# Write out

out_dir <- "output/2022/4_12"

dir.create(file.path(out_dir))

anim_save(paste0(out_dir, "/air_pollution_death.gif"))
