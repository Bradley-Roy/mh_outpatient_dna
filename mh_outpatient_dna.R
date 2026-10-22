
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
library(DT)

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

# tabs <- function(plots, names){
#   do.call(
#     navset_pill,
#     pmap(
#       plots, names,
#       \(plot, name){
#         if(inherits(plot, "ggplot")) plot <- ggiraph::girafe(plot)
#         nav_panel(name, plot)
#       }
#     )
#   )
# }

pretty_date <- function(date) {
  
  d <- lubridate::day(date)
  
  suffix <- dplyr::case_when(
    d %in% 11:13 ~ "th",
    d %% 10 == 1 ~ "st",
    d %% 10 == 2 ~ "nd",
    d %% 10 == 3 ~ "rd",
    TRUE ~ "th"
  )
  
  paste0(
    format(date, "%B "),
    d,
    suffix,
    " ",
    lubridate::year(date)
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


#Source wrangling code
source("wrangle.R")



# Markdown Rendering ----------------------------------------------------------------

# render_markdown("mh_op_dna_markdown.Rmd",
#                 "/conf/LIST_analytics/Borders/Missingness/Tweed/Mental Health Outpatient Appointment Rate.html")
rmarkdown::render(
  input = "mh_op_dna_markdown.Rmd",
  output_format = "html_document",
  output_file = "Mental Health Outpatient Appointment Rate.html",
  output_dir = data_folder
)
