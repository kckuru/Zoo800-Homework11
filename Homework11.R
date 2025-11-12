##################
### Homework 10 ###
##################

# Group members: Keeley Kuru, Joseph Munoz
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


# ===== Exercise 2: Partner portion ===== #

# --- Libraries ---
library(tidyverse)

# --- 2a. Import dataset ---
data <- read_csv("Keeley_ANCOVA_simulated_troutbog.csv")

# Quick check of data
head(data)
summary(data)

# Fit full model with interaction
# (to test if the relationship between year and mean absorbance differs by depth zone)
full_model <- lm(mean_absorbance ~ year * depth_zone, data = data)
summary(full_model)

# Fit reduced model without interaction 
# (to test if interaction is significant)
no_interaction <- lm(mean_absorbance ~ year + depth_zone, data = data)

# Compare using ANOVA
anova(no_interaction, full_model)
# -> Since the interaction between year and depth_zone is significant, that means:
# -> The relationship between year and mean absorbance (rate of browning) differs between the epilimnion and hypolimnion

# --- 2b. Ecological Interpretation (2–3 sentences) ---
# Mean absorbance (a proxy for water color) increased significantly from 1990 to 2020, indicating strong browning of Trout Bog.
# The interaction between year and depth zone shows that browning trends differ by depth. 
# The hypolimnion both started darker and browned faster than the epilimnion.
