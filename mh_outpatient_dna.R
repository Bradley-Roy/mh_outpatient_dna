
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
    WHERE CLINIC_DATE >= ADD_MONTHS(SYSDATE, -12)
    AND GP_PRACTICE_CODE IN (' 16211', '16211') 
    AND ROWNUM <= 500
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

mh_appointments <- dbGetQuery(
  channel,
  statement = "
    SELECT 
      GP_PRACTICE_CODE,
      CLINIC_DATE,
      REFERRAL_SOURCE,
      SPECIALTY,
      CLINIC_ATTENDANCE,
      POSTCODE
    FROM ANALYSIS.SMR00_PI
    WHERE CLINIC_DATE >= TO_DATE('01/04/2024', 'DD/MM/YYYY')
      AND GP_PRACTICE_CODE IN (' 16211', '16211') 
      AND REFERRAL_SOURCE = '1'
      AND SPECIALTY IN (
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
  "
)

clinic_attendance_codes <- mh_appointments |> 
  clean_names() |> 
  distinct(clinic_attendance)

# Wrangle -----------------------------------------------------------------

# For details of SMR00 data definitions see: https://publichealthscotland.scot/resources-and-tools/health-intelligence-and-data-management/national-data-catalogue/data-dictionary/search-the-data-dictionary/attendance-status/?Search=A&ID=96&Title=Attendance%20Status
# Attendance Status
# 1	Patient was seen
# 5	Patient attended but was not seen (Could Not Wait - CNW)
# 8	Patient did not attend and gave no prior warning (Did Not Attend - DNA)

mh_appointments_simd <- mh_appointments |> 
  clean_names() |> 
  left_join(
    simd_lookup,
    by = "postcode"
  ) |> 
  mutate(date_year = year(clinic_date))

dna_summary <- mh_appointments_simd %>%
  group_by(gp_practice_code) %>%
  summarise(total_appointments = n(),
            dna_count = sum(clinic_attendance == 8, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(dna_percent = percent(dna_count / total_appointments, .1)) %>%
  arrange(desc(dna_percent)) 


spec_simd_summary <- mh_appointments_simd %>%
  group_by(date_year, gp_practice_code, specialty, simd_quintile) %>%
  summarise(total_appointments = n(),
            dna_count = sum(clinic_attendance == 8, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(dna_percent = percent(dna_count / total_appointments, .1)) %>%
  arrange(desc(dna_percent)) 

#test table to check formatting
spec_simd_summary_pivot <- spec_simd_summary |> 
  filter(specialty == "G1") |> 
  select(date_year, simd_quintile, dna_percent) |> 
  pivot_wider(names_from = date_year, values_from = dna_percent) |> 
  arrange(simd_quintile)

# Charts ------------------------------------------------------------------

create_spec_table <- function(spec) {
  
  spec_simd_summary |>
    filter(specialty == spec) |>
    select(date_year, simd_quintile, dna_percent) |>
    pivot_wider(
      names_from = date_year,
      values_from = dna_percent
    ) |>
    arrange(simd_quintile)
}

spec_tables <- unique(spec_simd_summary$specialty) |>
  set_names() |>
  map(create_spec_table)


# Markdown Rendering ----------------------------------------------------------------

render_markdown("mh_op_dna_markdown.Rmd",
                "//conf/LIST_analytics/Lothian/Midlothian/Missingness/Tweed/Mental Health Outpatient Appointment Rate.html")

