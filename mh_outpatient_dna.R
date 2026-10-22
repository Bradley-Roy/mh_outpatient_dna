
# Crib Sheet: https://publichealthscotland.scot/media/24924/2026-08-smr-crib-sheet-smr00-final.pdf

# DNA Rate for MH Outpatient appointments ---------------------------------


library(DBI)
library(dplyr)
library(tidyverse)
library(slfhelper)
library(odbc)
library(janitor)
library(lubridate)
library(writexl)
library(phsmethods)
library(plotly)
library(knitr)
library(kableExtra)
library(scales)
library(phstemplates)
library(phslookups)
library(purrr)
library(bslib)
library(readxl)
library(glue)
library(crosstalk)

data_folder <- "/conf/LIST_analytics/Borders/Missingness/Tweed/"


# Chart colours -----------------------------------------------------------

chart_cols <- c(
  "#005EB8",  # blue
  "#007F8B",  # teal
  "#41A4A0",  # lighter teal
  "#768692",  # blue-grey
  "#425563"   # dark grey
)

select_specialties <- c(
          'G1',
          'G1A',
          'G2',
          'G21',
          'G22',
          'G3',
          'G4',
          'G5',
          'G6',
          'G61',
          'G62',
          'G63'
        )

# Functions ---------------------------------------------------------------

tabs <- function(plots, names){
  do.call(
    navset_pill,
    pmap(
      plots, names,
      \(plot, name){
        if(inherits(plot, "ggplot")) plot <- ggiraph::girafe(plot)
        nav_panel(name, plot)
      }
    )
  )
}

# Lookups -----------------------------------------------------------------

prac_names <- read_csv("https://www.opendata.nhs.scot/dataset/f23655c3-6e23-4103-a511-a80d998adb90/resource/0032ce94-f7f6-44dd-879c-7d3074d8e2e8/download/practice_contact_details_20260701_opendata.csv")

simd_lookup <- get_simd_postcode() |> 
  select(
    pc8,
    simd2020v2_sc_decile,
    simd2020v2_sc_quintile
  ) |> 
  rename(postcode = pc8,
         simd_decile = simd2020v2_sc_decile,
         simd_quintile = simd2020v2_sc_quintile)

# simd_lookup <- get_simd_postcode(
#   postcode_version = "latest",
#   simd_version = "2020v2"
# )

specialty_lookup <- read_xlsx(glue(data_folder, "SMR00_Specialty_Lookup.xlsx")) |> 
  clean_names()

# Extraction --------------------------------------------------------------

channel <- dbConnect(odbc(), dsn="SMRA",
                     uid = Sys.getenv("USER"),
                     pwd=.rs.askForPassword("SMRA Password:"))

# odbcListObjects(channel, schema="ANALYSIS")

list_of_smr00_columns <- odbcPreviewObject(channel, table="ANALYSIS.SMR00_PI", rowLimit=3)

check_col <- data.frame(colnames(list_of_smr00_columns))



smr00_test <- dbGetQuery(
  channel,
  statement = "
    SELECT *
    FROM ANALYSIS.SMR00_PI
    WHERE CLINIC_DATE >= TO_DATE('01/01/2023', 'DD/MM/YYYY')
      AND CLINIC_DATE < TO_DATE('31/12/2025', 'DD/MM/YYYY')
    AND GP_PRACTICE_CODE IN (' 16211', '16211') 
    AND ROWNUM <= 500
  "
)


### all appointments

all_appointments_total_raw <- dbGetQuery(
  channel,
  statement = "
    SELECT 
      SPECIALTY,
      COUNT(*) AS TOTAL_APPOINTMENTS
    FROM ANALYSIS.SMR00_PI
    WHERE CLINIC_DATE >= TO_DATE('01/01/2023', 'DD/MM/YYYY')
      AND CLINIC_DATE < TO_DATE('31/12/2025', 'DD/MM/YYYY')
      AND GP_PRACTICE_CODE IN (' 16211', '16211')
      AND REFERRAL_SOURCE = '1'
    GROUP BY SPECIALTY
    ORDER BY SPECIALTY
  "
)



all_appointments_raw <- dbGetQuery(
  channel,
  statement = "
    SELECT 
      SPECIALTY,
      COUNT(*) AS TOTAL_APPOINTMENTS,
      CLINIC_DATE,
      POSTCODE,
      CLINIC_ATTENDANCE
    FROM ANALYSIS.SMR00_PI
    WHERE CLINIC_DATE >= TO_DATE('01/01/2023', 'DD/MM/YYYY')
      AND CLINIC_DATE < TO_DATE('31/12/2025', 'DD/MM/YYYY')
      AND GP_PRACTICE_CODE IN (' 16211', '16211')
      AND REFERRAL_SOURCE = '1'
    GROUP BY CLINIC_DATE, POSTCODE, SPECIALTY, CLINIC_ATTENDANCE
    ORDER BY SPECIALTY
  "
)



# Referral Source, 1 = GP
# Specialities:
    # "G1",   # General Psychiatry
    # "G1A",  # Community Psychiatry
    # "G2",   # Child & Adolescent Psychiatry
    # "G21",  # Child Psychiatry
    # "G22",  # Adolescent Psychiatry
    # "G3",   # Forensic Psychiatry
    # "G4",   # Psychiatry of Old Age
    # "G5",   # Learning Disability
    # "G6",   # Psychotherapy
    # "G61",  # Behavioural Psychotherapy
    # "G62",  # Child & Adolescent Psychotherapy
    # "G63"   # Adult Psychotherapy



# mh_appointments_raw <- dbGetQuery(
#   channel,
#   statement = "
#     SELECT 
#       GP_PRACTICE_CODE,
#       CLINIC_DATE,
#       REFERRAL_SOURCE,
#       SPECIALTY,
#       CLINIC_ATTENDANCE,
#       POSTCODE
#     FROM ANALYSIS.SMR00_PI
#     WHERE CLINIC_DATE >= TO_DATE('01/01/2023', 'DD/MM/YYYY')
#       AND CLINIC_DATE < TO_DATE('31/12/2025', 'DD/MM/YYYY')
#       AND GP_PRACTICE_CODE IN (' 16211', '16211') 
#       AND REFERRAL_SOURCE = '1'
#       AND SPECIALTY IN (
#         'G1',
#         'G1A',
#         'G2', 
#         'G21',
#         'G22',
#         'G3', 
#         'G4', 
#         'G5', 
#         'G6', 
#         'G61',
#         'G62',
#         'G63'
#       )
#   "
# )


# For details of SMR00 data definitions see: https://publichealthscotland.scot/resources-and-tools/health-intelligence-and-data-management/national-data-catalogue/data-dictionary/search-the-data-dictionary/attendance-status/?Search=A&ID=96&Title=Attendance%20Status
# Attendance Status
# 1	Patient was seen
# 5	Patient attended but was not seen (Could Not Wait - CNW)
# 8	Patient did not attend and gave no prior warning (Did Not Attend - DNA)

# mh_appointments_simd <- mh_appointments_raw |> 
#   clean_names() |> 
#   left_join(
#     simd_lookup,
#     by = "postcode"
#   ) |> 
#   left_join(specialty_lookup) |> 
#   mutate(date_year = year(clinic_date))
# 
# dna_summary <- mh_appointments_simd %>%
#   group_by(gp_practice_code) %>%
#   summarise(total_appointments = n(),
#             dna_count = sum(clinic_attendance == 8, na.rm = TRUE),
#             .groups = "drop") %>%
#   mutate(dna_percent = percent(dna_count / total_appointments, .1)) %>%
#   arrange(desc(dna_percent)) 
# 
# 
# spec_simd_summary <- mh_appointments_simd %>%
#   group_by(date_year, gp_practice_code, specialty, simd_quintile) %>%
#   summarise(total_appointments = n(),
#             dna_count = sum(clinic_attendance == 8, na.rm = TRUE),
#             .groups = "drop") %>%
#   mutate(dna_percent = percent(dna_count / total_appointments, .1)) %>%
#   arrange(desc(dna_percent)) 
# 
# #test table to check formatting
# spec_simd_summary_pivot <- spec_simd_summary |> 
#   filter(specialty == "G1") |> 
#   select(date_year, simd_quintile, dna_percent) |> 
#   pivot_wider(names_from = date_year, values_from = dna_percent) |> 
#   arrange(simd_quintile)

# Charts ------------------------------------------------------------------

# number

# create_spec_table <- function(spec) {
#   
#   spec_simd_summary |>
#     filter(specialty == spec) |>
#     select(date_year, simd_quintile, total_appointments) |>
#     pivot_wider(
#       names_from = date_year,
#       values_from = total_appointments
#     ) |>
#     arrange(simd_quintile) 
# }
# 
# spec_tables_ta <- unique(spec_simd_summary$specialty) |>
#   set_names() |>
#   map(create_spec_table)
# 
# spec_tables_ta <- purrr::map(
#   spec_tables_ta,
#   \(x) {
# 
#     # Add 2026 if missing
#     if (!"2026" %in% names(x)) {
#       x[["2026"]] <- 0
#     }
# 
#     # Put year columns in order
#     x |>
#       dplyr::select(-dplyr::any_of(c("2024", "2025", "2026")),
#                     `2024`, `2025`, `2026`)
#   }
# )
# 
# spec_tab_names_ta <- unique(spec_simd_summary$specialty)



# alternative table creation ----------------------------------------------

# table_creation <- function(spec) {
#   table_output <- spec_simd_summary |> 
#     filter(specialty == spec) |> 
#     select(date_year, simd_quintile, total_appointments) |>
#     pivot_wider(
#       names_from = date_year,
#       values_from = total_appointments
#     ) |>
#     arrange(simd_quintile) 
#   # table_output
# }

# table_creation <- function(spec) {
#   
#   spec_simd_summary |> 
#     filter(specialty == spec) |> 
#     select(date_year, simd_quintile, total_appointments) |>
#     pivot_wider(
#       names_from = date_year,
#       values_from = total_appointments
#     ) |>
#     arrange(simd_quintile)
# }
# 
# spec_tab_names_ta <- tibble(specialty = unique(spec_simd_summary$specialty))
# 
# tables <- map(spec_tab_names_ta, table_creation)
table_creation <- function(spec) {
  
  spec_simd_summary |> 
    filter(specialty == spec) |> 
    select(date_year, simd_quintile, total_appointments) |>
    pivot_wider(
      names_from = date_year,
      values_from = total_appointments
    ) |>
    arrange(simd_quintile)
}


spec_tab_names_ta <- unique(spec_simd_summary$specialty)


tables <- map(
  spec_tab_names_ta,
  table_creation
) |>
  set_names(spec_tab_names_ta)

#DNAs
create_spec_table <- function(spec) {
  
  spec_simd_summary |>
    filter(specialty == spec) |>
    select(date_year, simd_quintile, dna_count) |>
    pivot_wider(
      names_from = date_year,
      values_from = dna_count
    ) |>
    arrange(simd_quintile)|> 
    select(simd_quintile, `2024`, `2025`, `2026`)
}

spec_tables_dna <- unique(spec_simd_summary$specialty) |>
  set_names() |>
  map(create_spec_table)
spec_tables_dna <- purrr::map(
  spec_tables_dna,
  \(x) {
    
    # Add 2026 if missing
    if (!"2026" %in% names(x)) {
      x[["2026"]] <- 0
    }
    
    # Put year columns in order
    x |>
      dplyr::select(-dplyr::any_of(c("2024", "2025", "2026")),
                    `2024`, `2025`, `2026`)
  }
)



create_spec_table <- function(spec) {
  
  spec_simd_summary |>
    filter(specialty == spec) |>
    select(date_year, simd_quintile, dna_percent) |>
    pivot_wider(
      names_from = date_year,
      values_from = dna_percent
    ) |>
    arrange(simd_quintile)|> 
    select(simd_quintile, `2024`, `2025`, `2026`)
}

spec_tables_dna_per <- unique(spec_simd_summary$specialty) |>
  set_names() |>
  map(create_spec_table)
spec_tables_dna_per <- purrr::map(
  spec_tables_dna,
  \(x) {
    
    # Add 2026 if missing
    if (!"2026" %in% names(x)) {
      x[["2026"]] <- 0
    }
    
    # Put year columns in order
    x |>
      dplyr::select(-dplyr::any_of(c("2024", "2025", "2026")),
                    `2024`, `2025`, `2026`)
  }
)


sapply(spec_tables, nrow)

spec_tab_names <- unique(spec_simd_summary$specialty) 

# Markdown Rendering ----------------------------------------------------------------

# render_markdown("mh_op_dna_markdown.Rmd",
#                 "/conf/LIST_analytics/Borders/Missingness/Tweed/Mental Health Outpatient Appointment Rate.html")
rmarkdown::render(
  input = "mh_op_dna_markdown.Rmd",
  output_format = "html_document",
  output_file = "Mental Health Outpatient Appointment Rate.html",
  output_dir = data_folder
)
