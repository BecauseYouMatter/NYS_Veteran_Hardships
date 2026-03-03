
library(tinytex)
library(tidyverse)
library(lubridate)
library(here)
library(skimr)
library(janitor)
library(ggplot2)
library(dplyr)

theme_set(theme_classic())   # <-- MATCH labor script

# Folder that contains the Census files
folder_path <- "C:/GitHub_Share/Vet_Hardship/NYS_Veteran_Hardships/Processed_Data"

# File path to the combined output you created earlier
file_path <- file.path(folder_path, "S2101_Veteran_Loop.csv")

# Output folder
Out_folder_path <- "C:/GitHub_Share/Vet_Hardship/NYS_Veteran_Hardships/Processed_Data"

# Read the combined CSV
df <- read_csv(file_path, show_col_types = FALSE)

#  only show names that contain labor
names(df)[str_detect(names(df), "labor")]

df_plot <- df %>%
  select(
    zip_code,
    YEAR,
    vets_labor_force_count,
    nonvets_labor_force_count
  ) %>%
  filter(YEAR %in% c(2019, 2024)) %>%
  mutate(
    YEAR = as.integer(YEAR),
    is_2024 = (YEAR == 2024)
  )

ggplot(df_plot, aes(x = nonvets_labor_force_count,
                    y = vets_labor_force_count,
                    group = zip_code)) +
  geom_line(alpha = 0.25) +
  geom_point(aes(size = is_2024, shape = is_2024), alpha = 0.8) +
  scale_size_manual(values = c(`FALSE` = 1.2, `TRUE` = 3.5), guide = "none") +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 17), guide = "none") +
  labs(
    title = "Nonveteran-to-Veteran Labor Force Imbalance by NYS ZIP Code",
    subtitle = "Lines connect 2019 to 2024 for each ZIP (larger marker = 2024)",
    x = "Nonveterans in labor force (count)",
    y = "Veterans in labor force (count)"
  ) +
  theme_minimal()

# build a direction flag for each ZIP
df_plot2 <- df %>%
  select(
    zip_code,
    YEAR,
    vets_labor_force_count,
    nonvets_labor_force_count
  ) %>%
  filter(YEAR %in% c(2019, 2024)) %>%
  mutate(
    zip_code = as.character(zip_code),
    YEAR = as.integer(YEAR),
    total_lf = vets_labor_force_count + nonvets_labor_force_count,
    is_2024 = (YEAR == 2024)
  ) %>%
  group_by(zip_code) %>%
  filter(n() == 2) %>%   # keeps only ZIPs that have both years
  ungroup()


# classify each ZIP as increase or decrease
zip_direction <- df_plot2 %>%
  select(zip_code, YEAR, total_lf) %>%
  pivot_wider(names_from = YEAR, values_from = total_lf, names_prefix = "y") %>%
  mutate(
    movement_group = case_when(
      y2024 > y2019 ~ "Increase",
      y2024 < y2019 ~ "Decrease",
      TRUE ~ "No Change"
    )
  )

# count ZIPs in each movement group
group_counts <- zip_direction %>%
  count(movement_group, name = "zip_count")

# create a label that combines the group name and count
zip_direction <- zip_direction %>%
  left_join(group_counts, by = "movement_group") %>%
  mutate(
    movement_label = paste0(movement_group, " (", zip_count, " ZIP codes)")
  )


# join that label back to the plotting data
df_plot3 <- df_plot2 %>%
  left_join(
    zip_direction %>% select(zip_code, movement_group, movement_label),
    by = "zip_code"
  )

# build the summary numbers for each panel
panel_summary <- df_plot3 %>%
  group_by(zip_code, movement_group, movement_label) %>%
  summarise(
    vets_2019    = vets_labor_force_count[YEAR == 2019],
    vets_2024    = vets_labor_force_count[YEAR == 2024],
    nonvets_2019 = nonvets_labor_force_count[YEAR == 2019],
    nonvets_2024 = nonvets_labor_force_count[YEAR == 2024],
    .groups = "drop"
  ) %>%
  mutate(
    vets_change    = vets_2024 - vets_2019,
    nonvets_change = nonvets_2024 - nonvets_2019,
    total_change   = (vets_2024 + nonvets_2024) - (vets_2019 + nonvets_2019)
  ) %>%
  group_by(movement_group, movement_label) %>%
  summarise(
    vets_change    = sum(vets_change, na.rm = TRUE),
    nonvets_change = sum(nonvets_change, na.rm = TRUE),
    total_change   = sum(total_change, na.rm = TRUE),
    .groups = "drop"
  )

#make the label text

panel_summary <- panel_summary %>%
  mutate(
    label_text = paste0(
      "Total LF change: ", scales::comma(total_change),
      "\nVet change: ", scales::comma(vets_change),
      "\nNonvet change: ", scales::comma(nonvets_change)
    )
  )




# 2 panels on one page
ggplot(df_plot3, aes(x = nonvets_labor_force_count,
                     y = vets_labor_force_count,
                     group = zip_code,
                     color = movement_group)) +
  geom_line(alpha = 0.25) +
  geom_point(aes(size = is_2024, shape = is_2024), alpha = 0.8) +
  geom_text(
    data = panel_summary,
    aes(x = Inf, y = Inf, label = label_text),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.1,
    size = 3.5
  ) +
  scale_color_manual(values = c(
    "Decrease" = "red3",
    "Increase" = "darkgreen",
    "No Change" = "goldenrod2"
  )) +
  scale_size_manual(values = c(`FALSE` = 1.2, `TRUE` = 3.5), guide = "none") +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 17), guide = "none") +
  facet_wrap(~ movement_label) +
  labs(
    title = "Labor Force Movement by NYS ZIP Code",
    subtitle = "ZIPs split by whether total veteran + nonveteran labor force increased or decreased from 2019 to 2024",
    x = "Nonveterans in labor force (count)",
    y = "Veterans in labor force (count)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


#create export
p_labor_move <- ggplot(df_plot3, aes(x = nonvets_labor_force_count,
                                     y = vets_labor_force_count,
                                     group = zip_code,
                                     color = movement_group)) +
  geom_line(alpha = 0.25) +
  geom_point(aes(size = is_2024, shape = is_2024), alpha = 0.8) +
  geom_label(
    data = panel_summary,
    aes(x = Inf, y = Inf, label = label_text),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.1,
    size = 3.5
  ) +
  scale_color_manual(values = c(
    "Decrease" = "red3",
    "Increase" = "darkgreen",
    "No Change" = "goldenrod2"
  )) +
  scale_size_manual(values = c(`FALSE` = 1.2, `TRUE` = 3.5), guide = "none") +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 17), guide = "none") +
  facet_wrap(~ movement_label) +
  labs(
    title = "Labor Force Movement by NYS ZIP Code",
    subtitle = "ZIPs split by whether total veteran + nonveteran labor force increased or decreased from 2019 to 2024",
    x = "Nonveterans in labor force (count)",
    y = "Veterans in labor force (count)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
ggsave(
  filename = file.path(Out_folder_path, "Labor_Force_Movement_NYS_ZIP_Landscape.pdf"),
  plot = p_labor_move,
  device = "pdf",
  width = 11,
  height = 8.5,
  units = "in"
)