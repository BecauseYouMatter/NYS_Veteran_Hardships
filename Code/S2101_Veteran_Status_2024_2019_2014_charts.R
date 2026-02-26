# Load the packages you’ll use in this script (must be installed already)
install.packages("tinytex")
tinytex::install_tinytex()

library(tinytex)
library(tidyverse)
library(lubridate)
library(here)
library(skimr)
library(janitor)
library(ggplot2)

theme_set(theme_classic())   # <-- MATCH labor script

# Folder that contains the Census files
folder_path <- "C:/GitHub_Share/Vet_Hardship/NYS_Veteran_Hardships/Processed_Data"

# File path to the combined output you created earlier
file_path <- file.path(folder_path, "S2101_Veteran_Loop.csv")

# Output folder
Out_folder_path <- "C:/GitHub_Share/Vet_Hardship/NYS_Veteran_Hardships/Processed_Data"

# Load dataset
kept <- read_csv(file_path, show_col_types = FALSE)

# Summarize total vets + nonvets by year (DISABILITY)
yr_totals <- kept %>%
  mutate(YEAR = as.character(YEAR)) %>%
  group_by(YEAR) %>%
  summarise(
    dis_total         = sum(total_dis_count, na.rm = TRUE),
    vets_dis_total    = sum(vet_dis_count, na.rm = TRUE),
    nonvets_dis_total = sum(nonvet_dis_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(dis_total, vets_dis_total, nonvets_dis_total),
    names_to = "group",
    values_to = "total"
  ) %>%
  mutate(
    group = recode(group,
                   dis_total = "Disabled (Total)",
                   vets_dis_total = "Veterans",
                   nonvets_dis_total = "Nonveterans")
  )

# Chart 1 as an object (so ggsave can save it)
p_year <- ggplot(yr_totals, aes(x = YEAR, y = total, fill = group)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = scales::comma(total)),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  ) +
  labs(
    title = "Disability: Total vs Veterans vs Nonveterans by Year",
    subtitle = "Source: ACS S2101 (combined ZIP output)",
    x = "Year",
    y = "Disabled count",
    fill = NULL
  )

# Save Chart 1 to its own PDF (MATCH labor script pattern)
ggsave(
  filename = file.path(folder_path, "disability_bars_2019_2024.pdf"),
  plot = p_year,
  width = 10,
  height = 6
)

# ---- Chart 2 (simpler): 4 bars per ZIP (2019/2024 × Vet/Nonvet), each labeled ----
zip_totals <- kept %>%
  mutate(
    YEAR = as.character(YEAR),
    zip_code = as.character(zip_code)
  ) %>%
  filter(YEAR %in% c("2019","2024")) %>%
  group_by(zip_code, YEAR) %>%
  summarise(
    Veterans    = sum(vet_dis_count, na.rm = TRUE),
    Nonveterans = sum(nonvet_dis_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    c(Veterans, Nonveterans),
    names_to = "group",
    values_to = "total"
  ) %>%
  mutate(
    zip_code = factor(zip_code, levels = sort(unique(zip_code))),
    year_group = factor(
      paste(YEAR, group),
      levels = c(
        "2019 Veterans", "2019 Nonveterans",
        "2024 Veterans", "2024 Nonveterans"
      )
    )
  )

max_tot <- max(zip_totals$total, na.rm = TRUE)

p_zip <- ggplot(zip_totals, aes(x = zip_code, y = total, fill = year_group)) +
  geom_col(position = position_dodge2(width = 0.8, preserve = "single", reverse = TRUE)) +
  geom_text(
    aes(label = paste0(year_group, ": ", scales::comma(total))),
    position = position_dodge2(width = 0.8, preserve = "single", reverse = TRUE),
    hjust = -0.1,
    size = 2.5
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(limits = c(0, max_tot * 1.15)) +
  theme(plot.margin = margin(10, 60, 10, 10)) +
  labs(
    title = "Veterans vs Nonveterans Disability by ZIP Code",
    subtitle = "Four bars per ZIP: 2019/2024 - Veterans/Nonveterans",
    x = "ZIP Code",
    y = "Disabled count",
    fill = NULL
  )+
  theme(
    legend.position = "top",
    legend.direction = "horizontal"
  )

p_zip


ggsave(
  filename = file.path(folder_path, "disability_zip_bars_2019_2024.pdf"),
  plot = p_zip,
  width = 16,
  height = 1200,
  limitsize = FALSE
)

# Summarize total vets + nonvets by year (POVERTY COUNTS)
yr_totals <- kept %>%
  mutate(YEAR = as.character(YEAR)) %>%
  group_by(YEAR) %>%
  summarise(
    pov_total         = sum(total_pov_count, na.rm = TRUE),
    vets_pov_total    = sum(vets_pov_count, na.rm = TRUE),
    nonvets_pov_total = sum(nonvets_pov_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(pov_total, vets_pov_total, nonvets_pov_total),
    names_to = "group",
    values_to = "total"
  ) %>%
  mutate(
    group = recode(group,
                   pov_total = "Low Income Total",
                   vets_pov_total = "Veterans"),
    nonvets_pov_total = "Nonveterans"
  )

# Chart Poverty 1 as an object (so ggsave can save it)
p_year <- ggplot(yr_totals, aes(x = YEAR, y = total, fill = group)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = scales::comma(total)),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  ) +
  labs(
    title = "Poverty (Count): Total vs Veterans vs Nonveterans by Year",
    subtitle = "Source: ACS S2101 (combined ZIP output)",
    x = "Year",
    y = "Below poverty count",
    fill = NULL
  )

# Save Chart 1 to its own PDF
ggsave(
  filename = file.path(folder_path, "poverty_bars_2019_2024.pdf"),
  plot = p_year,
  width = 10,
  height = 6
)

# ---- Chart Poverty 2 (simpler): 4 bars per ZIP (2019/2024 × Vet/Nonvet), each labeled ----

zip_totals <- kept %>%
  mutate(
    YEAR = as.character(YEAR),
    zip_code = as.character(zip_code)
  ) %>%
  filter(YEAR %in% c("2019","2024")) %>%
  group_by(zip_code, YEAR) %>%
  summarise(
    Veterans    = sum(vets_pov_count, na.rm = TRUE),
    Nonveterans = sum(nonvets_pov_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    c(Veterans, Nonveterans),
    names_to = "group",
    values_to = "total"
  ) %>%
  mutate(
    zip_code = factor(zip_code, levels = sort(unique(zip_code))),
    year_group = factor(
      paste(YEAR, group),
      levels = c(
        "2019 Veterans", "2019 Nonveterans",
        "2024 Veterans", "2024 Nonveterans"
      )
    )
  )

max_tot <- max(zip_totals$total, na.rm = TRUE)

p_zip <- ggplot(zip_totals, aes(x = zip_code, y = total, fill = year_group)) +
  geom_col(position = position_dodge2(width = 0.8, preserve = "single", reverse = TRUE)) +
  geom_text(
    aes(label = paste0(year_group, ": ", scales::comma(total))),
    position = position_dodge2(width = 0.8, preserve = "single", reverse = TRUE),
    hjust = -0.1,
    size = 2.5
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(limits = c(0, max_tot * 1.15)) +
  theme_minimal() +
  theme(
    plot.margin = margin(10, 60, 10, 10),
    panel.background  = element_rect(fill = "white", colour = "white"),
    plot.background   = element_rect(fill = "white", colour = "white"),
    legend.background = element_rect(fill = "white", colour = "white"),
    legend.key        = element_rect(fill = "white", colour = "white")
  ) +
  theme_classic() +   # KEEP THE + HERE (part of the chain)
  theme(
    panel.background = element_rect(fill = "white", colour = "white"),
    plot.background  = element_rect(fill = "white", colour = "white")
  ) +
  labs(
    title = "Veterans vs Nonveterans Poverty (Count) by ZIP Code",
    subtitle = "Four bars per ZIP: 2019/2024 - Veterans/Nonveterans",
    x = "ZIP Code",
    y = "Below poverty count",
    fill = NULL
  ) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal"
  )

p_zip

ggsave(
  filename = file.path(folder_path, "poverty_zip_bars_2019_2024.pdf"),
  plot = p_zip,
  width = 16,
  height = 1200,
  limitsize = FALSE
)

# Summarize total vets + nonvets by year (LABOR FORCE)
yr_totals <- kept %>%
  mutate(YEAR = as.character(YEAR)) %>%
  group_by(YEAR) %>%
  summarise(
    lf_total         = sum(total_labor_force_count, na.rm = TRUE),
    vets_lf_total    = sum(vets_labor_force_count, na.rm = TRUE),
    nonvets_lf_total = sum(nonvets_labor_force_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(lf_total, vets_lf_total, nonvets_lf_total),
    names_to = "group",
    values_to = "total"
  ) %>%
  mutate(
    group = recode(group,
                   lf_total = "Labor force (Total)",
                   vets_lf_total = "Veterans",
                   nonvets_lf_total = "Nonveterans")
  )

# Chart LF 1 as an object (so ggsave can save it)
p_year <- ggplot(yr_totals, aes(x = YEAR, y = total, fill = group)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = scales::comma(total)),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  ) +
  labs(
    title = "Labor Force: Total vs Veterans vs Nonveterans by Year",
    subtitle = "Source: ACS S2101 (combined ZIP output)",
    x = "Year",
    y = "Labor force count",
    fill = NULL
  )

# Save Chart 1 to its own PDF (put this before your p_zip ggsave)
ggsave(
  filename = file.path(folder_path, "labor_force_bars_2019_2024.pdf"),
  plot = p_year,
  width = 10,
  height = 6
)

# ---- Chart LF 2 (simpler): 4 bars per ZIP (2019/2024 × Vet/Nonvet), each labeled ----

zip_totals <- kept %>%
  mutate(
    YEAR = as.character(YEAR),
    zip_code = as.character(zip_code)
  ) %>%
  filter(YEAR %in% c("2019","2024")) %>%
  group_by(zip_code, YEAR) %>%
  summarise(
    Veterans    = sum(vets_labor_force_count, na.rm = TRUE),
    Nonveterans = sum(nonvets_labor_force_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    c(Veterans, Nonveterans),
    names_to = "group",
    values_to = "total"
  ) %>%
  mutate(
    zip_code = factor(zip_code, levels = sort(unique(zip_code))),
    year_group = factor(
      paste(YEAR, group),
      levels = c(
        "2019 Veterans", "2019 Nonveterans",
        "2024 Veterans", "2024 Nonveterans"
      )
    )
  )

max_tot <- max(zip_totals$total, na.rm = TRUE)

p_zip <- ggplot(zip_totals, aes(x = zip_code, y = total, fill = year_group)) +
  geom_col(position = position_dodge2(width = 0.8, preserve = "single", reverse = TRUE)) +
  geom_text(
    aes(label = paste0(year_group, ": ", scales::comma(total))),
    position = position_dodge2(width = 0.8, preserve = "single", reverse = TRUE),
    hjust = -0.1,
    size = 2.5
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(limits = c(0, max_tot * 1.15)) +
  theme_minimal() +
  theme(
    plot.margin = margin(10, 60, 10, 10),
    panel.background  = element_rect(fill = "white", colour = "white"),
    plot.background   = element_rect(fill = "white", colour = "white"),
    legend.background = element_rect(fill = "white", colour = "white"),
    legend.key        = element_rect(fill = "white", colour = "white")
  ) +
  theme_classic() +   # <-- KEEP THE + HERE (part of the chain)
  theme(
    panel.background = element_rect(fill = "white", colour = "white"),
    plot.background  = element_rect(fill = "white", colour = "white")
  ) +
  labs(
    title = "Veterans vs Nonveterans Labor Force by ZIP Code",
    subtitle = "Four bars per ZIP: 2019/2024 - Veterans/Nonveterans",
    x = "ZIP Code",
    y = "Labor force count",
    fill = NULL
  )+
  theme(
    legend.position = "top",
    legend.direction = "horizontal"
  )

p_zip

ggsave(
  filename = file.path(folder_path, "labor_force_zip_bars_2019_2024.pdf"),
  plot = p_zip,
  width = 16,
  height = 1200,
  limitsize = FALSE
)
