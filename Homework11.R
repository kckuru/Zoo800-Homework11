##################
### Homework 10 ###
##################

# Group members: Keeley Kuru, Lonnie Parry, Joseph Munoz
# Date: 11/13/25

# ===== Exercise 1a: ANCOVA Simulation ===== #
# Exercise: Simulation and estimation of linear models with one categorical and one continuous predictor
# Adapted for Trout Bog Lake: Epilimnion vs. Hypolimnion

# Ecological scenario:
# These simulated data represent mean water color (absorbance normalized to a 1 cm path length)
# measured in the epilimnion (surface layer) and hypolimnion (deep layer) of Trout Bog Lake, WI
# from 1990–2020. The hypolimnion is more isolated and often contains more dissolved organic matter,
# leading to darker color. We ask: does the rate of browning (increase in color) differ between layers?

library(tidyverse)

set.seed(123)

# Continuous predictor: year (1990–2020)
year <- rep(seq(1990, 2020, length.out = 50), times = 2) # 50 observations per depth zone, total 100

# Categorical predictor: depth zone
depth_zone <- rep(c("Epilimnion", "Hypolimnion"), each = 50) # layers of lake, 50 observations each

# Define true model parameters
# Hypothesis: both zones are browning, but the hypolimnion is darker overall and increasing faster
intercept_epi <- -3.7      # Baseline intercept for epilimnion (chosen so predictions ≈ 0.25–0.35 absorbance)
slope_epi <- 0.0020        # Rate of increase in color per year for the epilimnion
intercept_hypo <- intercept_epi - 0.8   # Hypolimnion starts darker (lower intercept)
slope_hypo <- slope_epi + 0.0010        # Hypolimnion color increases faster (steeper slope)

# Simulate data with noise, different intercepts and slopes, including some random variation
mean_absorbance <- ifelse(
  depth_zone == "Epilimnion",
  intercept_epi + slope_epi * year + rnorm(50, 0, 0.02), # Epilimnion, lighter and slower increase
  intercept_hypo + slope_hypo * year + rnorm(50, 0, 0.02) # Hypolimnion, darker and faster increase
)

# Combine into dataframe
ancova_data <- data.frame(year, depth_zone, mean_absorbance)

# Fit ANCOVA model (with interaction term)
ancova_model <- lm(mean_absorbance ~ year * depth_zone, data = ancova_data)
summary(ancova_model)

# Plot simulated ANCOVA results
library(ggplot2)

ggplot(ancova_data, aes(x = year, y = mean_absorbance, color = depth_zone)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_color_manual(values = c("#2C7BB6", "#D7191C")) +
  labs(
    title = "Simulated Long-Term Browning of Trout Bog Lake (1990–2020)",
    subtitle = "Comparison of Epilimnion and Hypolimnion Mean Absorbance",
    x = "Year",
    y = "Mean Absorbance (1 cm)",
    color = "Depth Zone"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Save simulated data for sharing
write_csv(ancova_data, "Keeley_ANCOVA_simulated_troutbog.csv")

head(read_csv("Keeley_ANCOVA_simulated_troutbog.csv"))


# ===== Exercise 1b: Ecological scenario ===== #
# This simulated data represents mean water color (absorbance, normalized to a 1 cm pathlength) 
# measured in the epilimnion and hypolimnion of Trout Bog Lake, Wisconsin, between 1990 and 2020. 
# Because the hypolimnion is permanently anoxic and rich in dissolved organic matter,
# it tends to have darker color than the epilimnion.

# Ecological Question: 
# -> Has water color (browning) increased over time in Trout Bog Lake,
# -> and does the rate of increase differ between the epilimnion and hypolimnion?


# == Working with partner's data: Lonnie's Brook Trout Activity Data == #

# from Lonnie: #In this simulated dataset, brook trout activity (movements/hr) declines as stream temperature increases. 
#               Streams with cold-water refugia have higher baseline activity and a weaker decline in activity with increasing temperature compared to streams without refugia. 
#               The ANCOVA model tests whether the slopes of activity–temperature relationships differ between the two stream types, 
#               answering this ecological question: Do brook trout with cold-water refugia maintain higher activity levels as temperatures rise compared to those without refugia?

# Load tidyverse
library(tidyverse)

# Load the data your partner shared with you
lonnie_data <- read.csv("brook_trout_activity.csv")

# Inspect the data
head(lonnie_data)
str(lonnie_data)

# Full model with interaction
full_model <- lm(activity ~ temp * group, data = lonnie_data)
summary(full_model)

# Reduced model without interaction
no_interaction <- lm(activity ~ temp + group, data = lonnie_data)
summary(no_interaction)

# Plot the data and model fits
ggplot(lonnie_data, aes(x = temp, y = activity, color = group)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_color_manual(values = c("#1b9e77", "#d95f02")) +
  labs(
    title = "Brook Trout Activity vs. Stream Temperature",
    subtitle = "Comparison of Streams With and Without Cold-Water Refugia",
    x = "Stream Temperature (°C)",
    y = "Brook Trout Activity (movements/hr)",
    color = "Stream Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Compare models using ANOVA
anova(no_interaction, full_model)
# -> If the interaction is significant (p < 0.05), it means the relationship between temperature and activity differs by group. 
# -> If not significant, we would drop the interaction and use the simpler model.

# Ecological Interpretation (2–3 sentences):
# Brook trout activity declines as water temperature increases.
# Streams with cold-water refugia have higher baseline activity and a shallower decline in activity with rising temperatures 
#  compared to streams without refugia. This suggests that cold-water refugia help brook trout maintain activity levels in warmer conditions.

