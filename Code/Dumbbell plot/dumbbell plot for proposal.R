###ggplot effort to create dumbell plot for proposal (contact: Jana; Kim;)
###2/14/25
###Yuqi Liao

###Reference: https://r-graph-gallery.com/web-dumbbell-chart-with-a-gap-column.html

# Load necessary libraries
library(ggplot2)
library(ggalt)
library(dplyr)

# Create the dataset
data <- data.frame(
  TRS_IA_dimension = c(
    "Whole School Safety Planning",
    "Whole School Prevention Planning",
    "Whole School Trauma Programming",
    "Classroom Strategies",
    "Prevention/Early Intervention Trauma Programming",
    "Targeted Trauma-Informed Programming",
    "Staff Self-Care",
    "Family and Community Engagement"
  ),
  Time1 = c(2.6, 2.7, 1.9, 2.3, 1.8, 2.3, 1.8, 2.1),
  Time2 = c(2.9, 2.9, 2.3, 2.6, 2.1, 2.6, 2.2, 2.4)
)

# Reorder factor levels for proper ordering in the plot
data$TRS_IA_dimension <- factor(data$TRS_IA_dimension, levels = rev(data$TRS_IA_dimension))

# Create the dumbbell plot
plot <- ggplot(data, aes(y = TRS_IA_dimension, x = Time1, xend = Time2)) +
  
  # Add dumbbell lines and points
  geom_dumbbell(size = 3.5, color = "gray80", 
                colour_x = "#215F9A", colour_xend = "#3B7D23",
                size_x = 5, size_xend = 5) + 
  
  # Label each point with its value
  geom_text(aes(x = Time1, label = Time1), hjust = 1.5, size = 4, color = "#215F9A") +
  geom_text(aes(x = Time2, label = Time2), hjust = -0.5, size = 4, color = "#3B7D23") +
  
  # Adjust title and subtitle
  labs(
    title = "Changes in Average TRS-IA Score Over Time",
    subtitle = "",
    x = "Score",
    y = NULL
  ) +
  
  # Add custom legend
  annotate("text", x = 2, y = 8.8, label = "Time 1", color = "#215F9A", size = 5, fontface = "bold", hjust = 1) +
  annotate("text", x = 2.6, y = 8.8, label = "Time 2", color = "#3B7D23", size = 5, fontface = "bold", hjust = 0) +
  geom_point(aes(x = 2.1, y = 8.8), color = "#215F9A", size = 6) +
  geom_point(aes(x = 2.5, y = 8.8), color = "#3B7D23", size = 6) +
  coord_cartesian(clip = 'off') +
  
  # Make x-axis ticks and labels subtle (gray)
  scale_x_continuous(
    limits = c(1, 3.5),
    breaks = seq(1, 3.5, 0.5),
    labels = scales::comma_format()
  ) +
  
  # Remove vertical axis line
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove grid lines
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, color = "gray50"),  # Lighten x-axis labels
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.line.y = element_blank(),  # Ensure vertical axis line is removed
    axis.ticks.x = element_line(color = "gray60"),  # Light x-axis ticks
    axis.ticks.length = unit(0.15, "cm")
  ) 


plot

#export to png and eps using the Export button; scale set up 1000 * 500
# ggsave(filename = "trs-ia-score-change.png", plot = last_plot(), width = 1000, height = 500, units = "px")
