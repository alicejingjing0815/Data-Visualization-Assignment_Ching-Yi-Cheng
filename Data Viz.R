##Assignment 3
#Educational attainment of young people in English towns
library(tidyverse)
library(psych)
library(readxl)
library(gt)
library(janitor)
library(ggplot2)
library(readr)
library(dplyr)
library(scales)
raw_data <- read_csv("english_education.csv")
glimpse(raw_data)

#Check missing data
colSums(is.na(raw_data))

#Choose variables and rename
data <- raw_data |>
  select(
    size_flag,
    rgn11nm,
    income_flag,
    coastal_detailed,
    job_density_flag,
    university_flag,
    key_stage_2_attainment_school_year_2007_to_2008,
    key_stage_4_attainment_school_year_2012_to_2013,
    education_score
  ) |>
  rename(
    ks2_attainment =
      key_stage_2_attainment_school_year_2007_to_2008,
    ks4_attainment =
      key_stage_4_attainment_school_year_2012_to_2013
  )

#Version A: Scientific Article
#Keep the original variable names
income_order <- c(
  "Higher deprivation towns",
  "Mid deprivation towns",
  "Lower deprivation towns"
)

scientific_data <- raw_data |>
  select(
    size_flag,
    rgn11nm,
    income_flag,
    education_score
  ) |>
  filter(
    size_flag %in% c(
      "Small Towns",
      "Medium Towns",
      "Large Towns"
    ),
    !is.na(rgn11nm),
    income_flag %in% income_order,
    !is.na(education_score)
  ) |>
  mutate(
    income_flag = factor(
      income_flag,
      levels = income_order
    )
  )

# 1.Box plot: Distribution of town education scores
#| label: fig-education-boxplot
#| fig-width: 8
#| fig-height: 11
scientific_data |>
  ggplot(
    aes(
      x = income_flag,
      y = education_score,
      fill = income_flag
    )
  ) +
  geom_boxplot(
    width = 0.55,
    colour = "black",
    linewidth = 0.5,
    outlier.size = 1.5,
    outlier.alpha = 0.6,
    show.legend = FALSE
  ) +
  facet_wrap(
    vars(rgn11nm),
    ncol = 2
  ) +
  scale_x_discrete(
    labels = c(
      "Higher deprivation towns" = "Higher",
      "Mid deprivation towns" = "Mid",
      "Lower deprivation towns" = "Lower"
    ),
    drop = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "Higher deprivation towns" = "#7B7B7B",
      "Mid deprivation towns" = "#ADADAD",
      "Lower deprivation towns" = "#E0E0E0"
    )
  ) +
  labs(
    title = "Town educational attainment by income deprivation and region",
    x = "Income deprivation category",
    y = "Town education score",
  ) +
  theme_bw(
    base_size = 12
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(
      fill = "grey95",
      colour = "grey60"
    ),
    strip.text = element_text(
      face = "bold"
    ),
    plot.title.position = "plot",
    plot.caption = element_text(
      hjust = 0
    )
  )
# Calculate the number of towns and mean score in each group
regional_education_summary <- scientific_data |>
  summarise(
    n_towns = n(),
    mean_education_score = mean(education_score),
    .by = c(
      rgn11nm,
      income_flag
    )
  )

# Calculate the difference between Lower and Higher categories
regional_education_gap <- regional_education_summary |>
  filter(
    income_flag %in% c(
      "Higher deprivation towns",
      "Lower deprivation towns"
    )
  ) |>
  select(
    rgn11nm,
    income_flag,
    mean_education_score
  ) |>
  pivot_wider(
    names_from = income_flag,
    values_from = mean_education_score
  ) |>
  mutate(
    education_gap =
      `Lower deprivation towns` -
      `Higher deprivation towns`
  ) |>
  filter(
    !is.na(education_gap)
  )


# Dumbbell plot
# 1. Calculate mean education scores within each region and category
regional_education_summary <- scientific_data |>
  summarise(
    n_towns = n(),
    mean_education_score = mean(education_score),
    .by = c(
      rgn11nm,
      income_flag
    )
  )

# 2. Calculate the difference between Lower and Higher categories
regional_education_gap <- regional_education_summary |>
  filter(
    income_flag %in% c(
      "Higher deprivation towns",
      "Lower deprivation towns"
    )
  ) |>
  select(
    rgn11nm,
    income_flag,
    mean_education_score
  ) |>
  pivot_wider(
    names_from = income_flag,
    values_from = mean_education_score
  ) |>
  mutate(
    education_gap =
      `Lower deprivation towns` -
      `Higher deprivation towns`
  ) |>
  filter(
    !is.na(education_gap)
  )

# 3. Order regions by the difference
# The largest difference appears at the top
gap_region_order <- regional_education_gap |>
  arrange(education_gap) |>
  pull(rgn11nm)

# 4. Prepare the two group means for plotting
regional_education_means <- regional_education_summary |>
  filter(
    income_flag %in% c(
      "Higher deprivation towns",
      "Lower deprivation towns"
    ),
    rgn11nm %in% gap_region_order
  ) |>
  mutate(
    rgn11nm = factor(
      rgn11nm,
      levels = gap_region_order
    ),
    income_flag = factor(
      income_flag,
      levels = c(
        "Higher deprivation towns",
        "Lower deprivation towns"
      )
    )
  )

# 5. Draw the dumbbell plot
regional_education_means |>
  ggplot() +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "#BEBEBE",
    linewidth = 0.5
  ) +
  geom_segment(
    data = regional_education_gap,
    aes(
      x = `Higher deprivation towns`,
      xend = `Lower deprivation towns`,
      y = factor(
        rgn11nm,
        levels = gap_region_order
      ),
      yend = factor(
        rgn11nm,
        levels = gap_region_order
      )
    ),
    colour = "#BEBEBE",
    linewidth = 1
  ) +
  geom_point(
    aes(
      x = mean_education_score,
      y = rgn11nm,
      shape = income_flag,
      colour = income_flag,
      fill = income_flag
    ),
    size = 3.5,
    stroke = 1
  ) +
  scale_shape_manual(
    name = "Town income deprivation",
    values = c(
      "Higher deprivation towns" = 21,
      "Lower deprivation towns" = 19
    ),
    labels = c(
      "Higher deprivation towns" = "Higher deprivation",
      "Lower deprivation towns" = "Lower deprivation"
    )
  ) +
  scale_colour_manual(
    name = "Town income deprivation",
    values = c(
      "Higher deprivation towns" = "#3C3C3C",
      "Lower deprivation towns" = "#9D9D9D"
    ),
    labels = c(
      "Higher deprivation towns" = "Higher deprivation",
      "Lower deprivation towns" = "Lower deprivation"
    )
  ) +
  scale_fill_manual(
    name = "Town income deprivation",
    values = c(
      "Higher deprivation towns" = "#3C3C3C",
      "Lower deprivation towns" = "#9D9D9D"
    ),
    labels = c(
      "Higher deprivation towns" = "Higher deprivation",
      "Lower deprivation towns" = "Lower deprivation"
    )
  ) +
  scale_x_continuous(
    expand = expansion(
      mult = c(0.08, 0.08)
    )
  ) +
  labs(
    title = "Mean educational attainment by deprivation category",
    x = "Mean town education score",
    y = NULL
  ) +
  theme_bw(
    base_size = 12
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title.position = "plot",
    plot.caption = element_text(
      hjust = 0
    )
  )


#Version B: Accessible / Dynamic Outlet
#1.Select variables 
data <- raw_data |>
  select(
    size_flag,
    rgn11nm,
    income_flag,
    education_score
  )

#2.Prepare the new dataframe "regional_income"
regional_income <- data |>
  filter(                     #Keep only relevant 
    size_flag %in% c(         #Include three town-size
      "Small Towns",
      "Medium Towns",
      "Large Towns"
    ),
    !is.na(rgn11nm),          #Remove towns with missing
    income_flag %in% c(       #Keep the three deprivation
      "Higher deprivation towns",
      "Mid deprivation towns",
      "Lower deprivation towns"
    )
  ) |>
  count(                      #Count towns by region and 
    rgn11nm,
    income_flag,
    name = "n_towns"
  ) |>
  mutate(                                 #Create region-level variables
    region_total = sum(n_towns),          #Calculate total towns per region
    proportion = n_towns / region_total,  #Calculate each category's share
    .by = rgn11nm                         #Perform calculations within each region
  )


region_order <- regional_income |>
  filter(
    income_flag == "Higher deprivation towns"
  ) |>
  arrange(proportion) |>
  pull(rgn11nm)

income_order <- c(
  "Higher deprivation towns",
  "Mid deprivation towns",
  "Lower deprivation towns"
)

income_table <- regional_income |>
  select(
    rgn11nm,
    income_flag,
    n_towns
  ) |>
  pivot_wider(
    names_from = income_flag,
    values_from = n_towns,
    values_fill = 0
  ) |>
  mutate(
    Total = `Higher deprivation towns` +
      `Mid deprivation towns` +
      `Lower deprivation towns`,
    rgn11nm = factor(
      rgn11nm,
      levels = rev(region_order)
    )
  ) |>
  arrange(rgn11nm)

#Table 
income_table |>
  gt() |>
  cols_label(
    rgn11nm = "Region",
    `Higher deprivation towns` = "Higher",
    `Mid deprivation towns` = "Mid",
    `Lower deprivation towns` = "Lower",
    Total = "Total"
  ) |>
  tab_spanner(
    label = "Income deprivation category",
    columns = c(
      `Higher deprivation towns`,
      `Mid deprivation towns`,
      `Lower deprivation towns`
    )
  ) |>
  tab_header(
    title = "How many towns fall into each income deprivation category?",
    subtitle = "Number of towns in each English region"
  ) |>
  cols_align(
    align = "center",
    columns = -rgn11nm
  )


#Stacked Bar Chart
regional_income |>
  mutate(
    rgn11nm = factor(
      rgn11nm,
      levels = region_order
    ),
    income_flag = factor(
      income_flag,
      levels = income_order
    )
  ) |>
  ggplot(
    aes(
      x = rgn11nm,
      y = proportion,
      fill = income_flag
    )
  ) +
  geom_col(
    width = 0.65,
    position = "stack"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = label_percent(),
    breaks = seq(0, 1, by = 0.2),
    expand = expansion(
      mult = c(0, 0.02)
    )
  ) +
  scale_fill_manual(
    values = c(
      "Higher deprivation towns" = "#D95F5F",
      "Mid deprivation towns" = "#E6C84A",
      "Lower deprivation towns" = "#4C9BD2"
    )
  ) +
  labs(
    title = "Income deprivation is distributed differently across regions",
    subtitle = "Regions are ordered by the share of towns with higher deprivation",
    x = NULL,
    y = "Share of towns",
    fill = "Income deprivation category"
  ) +
  theme_minimal(
    base_size = 14
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title.position = "plot"
  )


#Scatter plot
regional_education <- data |>
  filter(
    size_flag %in% c(
      "Small Towns",
      "Medium Towns",
      "Large Towns"
    ),
    !is.na(rgn11nm),
    income_flag %in% income_order
  ) |>
  summarise(
    higher_proportion = mean(
      income_flag == "Higher deprivation towns"
    ),
    mean_education_score = mean(
      education_score,
      na.rm = TRUE
    ),
    .by = rgn11nm
  )

regional_education |>
  ggplot(
    aes(x = higher_proportion, y = mean_education_score)
  ) +
  geom_hline(
    yintercept = 0, linetype = "dashed", colour = "grey65"
  ) +
  geom_point(
    size = 3,
    colour = "#0000E3"
  ) +
  ggrepel::geom_text_repel(
    aes(label = rgn11nm),
    seed = 123,
    size = 4,
    max.overlaps = Inf
  ) +
  scale_x_continuous(
    labels = scales::label_percent(),
    breaks = seq(0, 1, by = 0.1)
  ) +
  labs(
    title = "Do regions with more income-deprived towns also have lower average educational attainment?",
    x = "Share of towns with higher income deprivation",
    y = "Mean town education score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title.position = "plot"
  )


