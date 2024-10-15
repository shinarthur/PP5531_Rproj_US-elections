# Geospatial Viz

library(ggplot2)
library(dplyr)
library(maps)
library(RColorBrewer)
library(usmap)

df <- read_rds("/Users/moritzludwig/Documents/GitHub/PP531_Rproj_US-elections/04. Outputs/2024-10-09_electiondataset.rds")

# Filter data for 2012 and ensure state_abv is character type
df_2012 <- df %>%
  filter(year == 2012) %>%
  mutate(state_abv = as.character(state_abv)) %>%
  select(state_abv, court_action_none_count_VAR, 
         court_map_drawn_count_VAR, court_return_leg_count_VAR, 
         court_challenge_rejected_count_VAR)

# Create a new column that categorizes states based on court action
df_2012 <- df_2012 %>%
  mutate(court_combined_VAR = case_when(
    court_challenge_rejected_count_VAR > 0 ~ "Challenge Rejected",
    court_return_leg_count_VAR > 0 ~ "Returned to Legislature",
    court_map_drawn_count_VAR > 0 ~ "Map Drawn by Court",
    court_action_none_count_VAR > 0 ~ "No Court Action",
    TRUE ~ "No Redistricting"
  ))

# Map state abbreviations to full state names for usmap
state_abbreviations <- data.frame(
  state_abv = c(state.abb, "DC"),
  state = c(state.name, "District of Columbia")
)

# Join the full state names to the data
df_2012 <- df_2012 %>%
  left_join(state_abbreviations, by = "state_abv")

# Define a color palette using RColorBrewer (YlOrRd)
color_palette <- brewer.pal(n = 5, name = "YlOrRd")
category_colors <- c("Challenge Rejected" = color_palette[5],  # Deep Red
                     "Returned to Legislature" = color_palette[4],  # Orange-Red
                     "Map Drawn by Court" = color_palette[3],  # Orange
                     "No Court Action" = color_palette[2],  # Light Orange-Yellow
                     "No Redistricting" = "#FFFFFF")  # White for None

# Plot using usmap (now with full state names)
plot_usmap(data = df_2012, values = "court_combined_VAR", regions = "states") +
  scale_fill_manual(values = category_colors, name = "Court Action") +
  labs(title = "Court Actions in 2012 by State") +
  theme(legend.position = "bottom",  # Move the legend to the bottom
        plot.title = element_text(hjust = 0.5, size = 16),  # Center the title
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.key.size = unit(1, "cm")) +
  guides(fill = guide_legend(
    title.position = "top",  # Place the legend title above the categories
    title.hjust = 0.5,  # Center the legend title
    nrow = 1  # Place categories in a single row
  ))
