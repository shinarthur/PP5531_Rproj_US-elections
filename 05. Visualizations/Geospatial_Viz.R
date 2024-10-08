# Geospatial Viz

library(ggplot2)
library(dplyr)
library(maps)

df <- read_rds("04. Outputs/2024-10-07_electiondataset.rds")

# Filter data for 2012 and ensure state_abv is character type
df_2012 <- df %>%
  filter(year == 2012) %>%
  mutate(state_abv = as.character(state_abv)) %>%
  select(state_abv, judiciary_VAR, court_action_none_count_VAR, 
         court_map_drawn_count_VAR, court_return_leg_count_VAR, 
         court_challenge_rejected_count_VAR)

# Get the US state map data
us_states <- map_data("state")

# Map state abbreviations to lowercase full state names for merging
state_abbreviations <- data.frame(
  state_abv = c(state.abb, "DC"),
  region = tolower(c(state.name, "District of Columbia"))
)

# Merge state-level variables with map data
map_data <- us_states %>%
  left_join(state_abbreviations, by = "region") %>%
  left_join(df_2012, by = "state_abv")

# Function to plot map for a given variable
plot_court_action_map <- function(var_name, title, fill_colors) {
  ggplot(map_data) +
    geom_polygon(aes_string(x = "long", y = "lat", group = "group", fill = var_name), color = "black") +
    scale_fill_gradient(low = "white", high = fill_colors) +
    labs(title = title, fill = var_name) +
    theme_void() +
    coord_map()
}

# Plot for judiciary_VAR
plot_judiciary <- plot_court_action_map("judiciary_VAR", 
                                        "Judiciary Action (2012)", "orange")

# Plot for court_action_none_count_VAR
plot_none_count <- plot_court_action_map("court_action_none_count_VAR", 
                                         "Court Action None Count (2012)", "orange")

# Plot for court_map_drawn_count_VAR
plot_map_drawn <- plot_court_action_map("court_map_drawn_count_VAR", 
                                        "Court Map Drawn Count (2012)", "orange")

# Plot for court_return_leg_count_VAR
plot_return_leg <- plot_court_action_map("court_return_leg_count_VAR", 
                                         "Court Return Leg Count (2012)", "orange")

# Plot for court_challenge_rejected_count_VAR
plot_challenge_rejected <- plot_court_action_map("court_challenge_rejected_count_VAR", 
                                                 "Court Challenge Rejected Count (2012)", "orange")

# Show the individual maps
plot_judiciary
plot_none_count
plot_map_drawn
plot_return_leg
plot_challenge_rejected

# Create a new column that categorizes states based on court action
map_data <- map_data %>%
  mutate(court_combined_VAR = case_when(
    court_challenge_rejected_count_VAR > 0 ~ "Challenge Rejected",
    court_return_leg_count_VAR > 0 ~ "Returned to Legislature",
    court_map_drawn_count_VAR > 0 ~ "Map Drawn",
    court_action_none_count_VAR > 0 ~ "No Action",
    TRUE ~ "None"
  ))

# Define a color palette
color_palette <- c("Challenge Rejected" = "red",
                   "Returned to Legislature" = "blue",
                   "Map Drawn" = "orange",
                   "No Action" = "grey",
                   "None" = "white")

# Prepare for overlapping colors using transparency
map_data <- map_data %>%
  mutate(
    fill_color = case_when(
      court_combined_VAR == "Challenge Rejected" ~ "Challenge Rejected",
      court_combined_VAR == "Returned to Legislature" ~ "Returned to Legislature",
      court_combined_VAR == "Map Drawn" ~ "Map Drawn",
      court_combined_VAR == "No Action" ~ "No Action",
      TRUE ~ "None"
    ),
    fill_alpha = case_when(
      fill_color == "Challenge Rejected" ~ 0.8,
      fill_color == "Returned to Legislature" ~ 0.6,
      fill_color == "Map Drawn" ~ 0.4,
      fill_color == "No Action" ~ 0.2,
      TRUE ~ 0
    )
  )

# Plot the combined map with transparency for overlapping actions
ggplot(map_data) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = fill_color, alpha = fill_alpha), color = "black") +
  scale_fill_manual(values = color_palette) +
  scale_alpha(range = c(0.1, 1), guide = FALSE) +  # Set alpha range and hide alpha legend
  labs(title = "Combined Court Actions (2012)", fill = "Court Action") +
  theme_void() +
  coord_map()

