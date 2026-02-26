# ---- S2101 clean output ----
library(readr)
library(dplyr)
library(stringr)

# Folder that contains the Census files
folder_path <- "D:/COURSERA/Data_Google/Census Data/S2101_Veteran_Status_2024_2019_2014"

# Output folder
Out_folder_path <- "C:/GitHub_Share/Vet_Hardship/NYS_Veteran_Hardships/Processed_Data"

# Output file
out_path <- file.path(Out_folder_path, "S2101_Veteran_Loop.csv")

# Find all files that end with "Data.csv"
data_files <- list.files(
  path = folder_path,
  pattern = "Data\\.csv$",
  full.names = TRUE
)

stopifnot(length(data_files) > 0)


# ----------------------------
# Helper: Convert "1,234" / "" / "-" to numeric, then force NA -> 0
to_num0 <- function(x) {
  x <- trimws(x)
  x[x %in% c("", "-", "NA")] <- NA
  x <- as.numeric(gsub(",", "", x))
  x[is.na(x)] <- 0
  x
}

# ---------------------------- Opening Good
# ---------------------------- Step 1
# Process ONE file into df_small
process_one_file <- function(file_path) {
  
  # Extract year from filename
  file_name <- basename(file_path)
  year <- sub("^ACSST5Y([0-9]{4}).*$", "\\1", file_name)
  
  # Read top header row
  df <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
  
  # Drop the 2nd row (the labels row that becomes row 1 of data)
  df <- df[-1, , drop = FALSE]
  row.names(df) <- NULL
  
  # Make unique names + remove blanks
  names(df) <- make.unique(names(df))
  bad <- is.na(names(df)) | names(df) == ""
  df <- df[, !bad]
  
  print(names(df)[1:10])

  
  # Add YEAR + zip_code right after key fields
  df <- df %>%
    mutate(
      YEAR = year,
      zip_code = sprintf("%05d", as.integer(str_extract(.data$GEO_ID, "\\d{5}$")))
    ) %>%
    relocate(YEAR, zip_code, .after = NAME)
  
  
  print(df[1, c("YEAR","zip_code","GEO_ID","NAME")])
  
  # ---- Step 2: Key ACS S2101 columns by *code* (not by label text) + sanity checks ----
  
  vets_popcount                <- "S2101_C02_001E"  # Veterans: Estimate    Civilian population 18+
  nonvets_pop_count            <- "S2101_C03_001E"  # Nonveterans: Estimate    Civilian population 18+
  
  total_labor_force_percent         <- "S2101_C02_032E" # Estimate!!Percent!!EMPLOYMENT STATUS!!Civilian population 18 to 64 years!!Labor force participation rate
  vets_labor_force_percent          <- "S2101_C04_032E" # Estimate!!Percent Veterans!!EMPLOYMENT STATUS!!Civilian population 18 to 64 years!!Labor force participation rate
  nonvets_labor_force_percent       <- "S2101_C06_032E" # Estimate!!Percent Nonveterans!!EMPLOYMENT STATUS!!Civilian population 18 to 64 years!!Labor force participation rate
  
  total_labor_force_count         <- "S2101_C01_033E" # Estimate!!Total!!EMPLOYMENT STATUS!!Civilian labor force 18 to 64 years
  vets_labor_force_count          <- "S2101_C03_033E" # Estimate!!Veterans!!EMPLOYMENT STATUS!!Civilian labor force 18 to 64 years
  nonvets_labor_force_count       <- "S2101_C05_033E" # Estimate!!Nonveterans!!EMPLOYMENT STATUS!!Civilian labor force 18 to 64 years
  
  total_unemp_rate_percent         <- "S2101_C02_034E"  # Total unemployment rate (all people, not just vets), ages 18–64, in the civilian labor force.
  vets_unemp_rate_percent           <- "S2101_C04_034E"  # Veteran unemployment rate, ages 18–64, in the civilian labor force.
  nonvets_unemp_rate_percent       <- "S2101_C06_034E"  # Nonveteran unemployment rate, ages 18–64, in the civilian labor force.
  
  total_pov_count                 <- "S2101_C01_036E"  # Estimate!!Total!!POVERTY STATUS IN THE PAST 12 MONTHS!!Civilian population 18 years and over for whom poverty status is determined!!Income in the past 12 months below poverty level
  total_pov_percent               <- "S2101_C02_036E"  # Estimate!!Percent!!POVERTY STATUS IN THE PAST 12 MONTHS!!Civilian population 18 years and over for whom poverty status is determined!!Income in the past 12 months below poverty level
  vets_pov_count                  <- "S2101_C03_036E"  # Veterans: Estimate    Poverty Status in the Past 12 Months    Civilian population 18+ for whom poverty status is determined
  vets_pov_percent                <- "S2101_C04_036E"  # Estimate!!Percent Veterans!!POVERTY STATUS IN THE PAST 12 MONTHS!!Civilian population 18 years and over for whom poverty status is determined!!Income in the past 12 months below poverty level
  nonvets_pov_count               <- "S2101_C05_036E"  # Estimate!!Nonveterans!!POVERTY STATUS IN THE PAST 12 MONTHS!!Civilian population 18 years and over for whom poverty status is determined!!Income in the past 12 months below poverty level
  nonvets_pov_percent             <- "S2101_C06_036E"  # Estimate!!Percent Nonveterans!!POVERTY STATUS IN THE PAST 12 MONTHS!!Civilian population 18 years and over for whom poverty status is determined!!Income in the past 12 months below poverty level
 
  total_dis_count               <- "S2101_C01_039E" # Estimate!!Total!!DISABILITY STATUS!!Civilian population 18 years and over for whom poverty status is determined!!With any disability
  total_dis_percent             <- "S2101_C02_039E" # Estimate!!Percent!!DISABILITY STATUS!!Civilian population 18 years and over for whom poverty status is determined!!With any disability
  vet_dis_count                 <- "S2101_C03_039E"  # Estimate!!Veterans!!DISABILITY STATUS!!Civilian population 18 years and over for whom poverty status is determined!!With any disability
  vet_dis_percent               <-"S2101_C04_039E"  #Estimate!!Percent Veterans!!DISABILITY STATUS!!Civilian population 18 years and over for whom poverty status is determined!!With any disability  
  nonvet_dis_count              <- "S2101_C05_039E"  # Estimate!!Nonveterans!!DISABILITY STATUS!!Civilian population 18 years and over for whom poverty status is determined!!With any disability
  nonvet_dis_percent            <-"S2101_C06_039E"  # Estimate!!Percent Nonveterans!!DISABILITY STATUS!!Civilian population 18 years and over for whom poverty status is determined!!With any disability
  
  
  
  df <- df %>%
    mutate(
      # Population counts (18+ civilian)
      vets_popcount          = to_num0(.data[[vets_popcount]]),
      nonvets_pop_count      = to_num0(.data[[nonvets_pop_count]]),
      
      # Labor force participation rate (% of civilian pop 18–64)
      total_labor_force_percent   = to_num0(.data[[total_labor_force_percent]]),
      vets_labor_force_percent    = to_num0(.data[[vets_labor_force_percent]]),
      nonvets_labor_force_percent = to_num0(.data[[nonvets_labor_force_percent]]),
      
      # Civilian labor force counts (18–64)
      total_labor_force_count   = to_num0(.data[[total_labor_force_count]]),
      vets_labor_force_count    = to_num0(.data[[vets_labor_force_count]]),
      nonvets_labor_force_count = to_num0(.data[[nonvets_labor_force_count]]),
      
      # Unemployment rate (% of civilian labor force 18–64)
      total_unemp_rate_percent   = to_num0(.data[[total_unemp_rate_percent]]),
      vets_unemp_rate_percent    = to_num0(.data[[vets_unemp_rate_percent]]),
      nonvets_unemp_rate_percent = to_num0(.data[[nonvets_unemp_rate_percent]]),
      
      # Poverty: below poverty level (count + percent)
      total_pov_count       = to_num0(.data[[total_pov_count]]),
      total_pov_percent     = to_num0(.data[[total_pov_percent]]),
      
      vets_pov_count        = to_num0(.data[[vets_pov_count]]),
      vets_pov_percent      = to_num0(.data[[vets_pov_percent]]),
      
      nonvets_pov_count     = to_num0(.data[[nonvets_pov_count]]),
      nonvets_pov_percent   = to_num0(.data[[nonvets_pov_percent]]),
      
      # Disability (within poverty-status-determined universe): with any disability (count + percent)
      total_dis_count   = to_num0(.data[[total_dis_count]]),
      total_dis_percent = to_num0(.data[[total_dis_percent]]),
      
      vet_dis_count     = to_num0(.data[[vet_dis_count]]),
      vet_dis_percent   = to_num0(.data[[vet_dis_percent]]),
      nonvet_dis_count   = to_num0(.data[[nonvet_dis_count]]),
      nonvet_dis_percent = to_num0(.data[[nonvet_dis_percent]])
    )
  
  
  ##################################################################### ---- Step 5: checks + export ----     
  
  titles <- df %>%
    select(
      GEO_ID,
      NAME,
      YEAR,
      zip_code,
      
      vets_popcount,
      nonvets_pop_count,
      
      total_labor_force_percent,
      vets_labor_force_percent,
      nonvets_labor_force_percent,
      
      total_labor_force_count,
      vets_labor_force_count,
      nonvets_labor_force_count,
      
      total_unemp_rate_percent,
      vets_unemp_rate_percent,
      nonvets_unemp_rate_percent,
      
      total_pov_count,
      total_pov_percent,
      vets_pov_count,
      vets_pov_percent,
      nonvets_pov_count,
      nonvets_pov_percent,
      
      total_dis_count,
      total_dis_percent,
      vet_dis_count,
      vet_dis_percent,
      nonvet_dis_count,
      nonvet_dis_percent
    )
  
  titles
}  # closes process_one_file()

# ----------------------------
# Step 101: Loop over every file and append
combined <- bind_rows(lapply(sort(data_files), process_one_file))

# --- INSERT HERE (after combined is built) ---
combined_flagged <- combined %>%
  group_by(zip_code) %>%
  mutate(drop_zip = !any(vets_popcount > 0 | nonvets_pop_count > 0)) %>%
  ungroup()

kept    <- combined_flagged %>% filter(!drop_zip)
dropped <- combined_flagged %>% filter(drop_zip)
#add zero flag for zips missing population in only 1 or 2 years
kept <- kept %>%
  mutate(zero_flag = ifelse(coalesce(vets_popcount,0) > 0 | coalesce(nonvets_pop_count,0) > 0, 0, 1))


# --- then write two files ---
write.csv(kept,    out_path, row.names = FALSE)
write.csv(dropped, sub("\\.csv$", "_dropped.csv", out_path), row.names = FALSE)





# Write the combined output
#write.csv(combined, out_path, row.names = FALSE)

# Confirm output path + quick check
print(out_path)
print(dim(combined))

# Show which files were processed (sanity check)
print(tail(sort(data_files), 3))   # last 3 files (by filename sort)

# Peek at output
print(head(combined, 3))
print(tail(combined, 3))







#stopifnot(exists("data_files"))
#stopifnot(length(data_files) > 0)

#for (f in data_files) {
#  cat("\n---\n", basename(f), "\n", sep = "")
#  df_test <- process_one_file(f)
#  cat("Exists:", file.exists(f), "\n")
#}
#df_test <- process_one_file(data_files[1])
#stop("STOP HERE ON PURPOSE (print-only loop)")

