library(ggplot2)
library(dplyr)
library(RColorBrewer)

# Load the dataset
df <- read.csv("/Users/moritzludwig/Documents/GitHub/PP531_Rproj_US-elections/04. Outputs/FactivaGerrymandering.csv", header = FALSE, stringsAsFactors = FALSE)

# Clean the data by identifying the rows that contain year ranges and document counts
cleaned_data <- df %>%
  filter(grepl("Start Date", V1)) %>%
  mutate(Year = as.numeric(gsub(".*(\\d{4}).*", "\\1", V1)),  # Extract the start year
         Document_Count = as.numeric(V2))  # Convert document count to numeric

# Remove rows with missing data (if any)
cleaned_data <- cleaned_data %>%
  filter(!is.na(Year), !is.na(Document_Count))

# Create the bar chart with the YlOrRd color palette
ggplot(cleaned_data, aes(x = Year, y = Document_Count)) +
  geom_bar(stat = "identity", fill = brewer.pal(9, "YlOrRd")[5]) +  # Use a specific shade from YlOrRd
  labs(title = "Number of Documents Mentioning Gerrymandering (2000-2024)", 
       x = "Year", 
       y = "Document Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )
