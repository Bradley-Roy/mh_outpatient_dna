
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


# Lookups -----------------------------------------------------------------

prac_names <- read_csv("https://www.opendata.nhs.scot/dataset/f23655c3-6e23-4103-a511-a80d998adb90/resource/0032ce94-f7f6-44dd-879c-7d3074d8e2e8/download/practice_contact_details_20260701_opendata.csv")



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
    AND ROWNUM <= 10
  "
)



mh_appointments <- dbGetQuery(
  channel,
  statement = "
    SELECT 
      GP_PRACTICE_CODE,
      CLINIC_DATE,
      REFERRAL_SOURCE,
      SPECIALTY
    FROM ANALYSIS.SMR00_PI
    WHERE CLINIC_DATE >= ADD_MONTHS(SYSDATE, -12)
    AND GP_PRACTICE_CODE IN (' 16211', '16211') 
    AND ROWNUM <= 10
  "
)


# Wrangle -----------------------------------------------------------------

dna_summary <- mh_appointments %>%
  group_by(GP_PRACTICE_CODE) %>%
  summarise(
    Total_Appointments = n(),
    DNA_Count = sum(ATTENDANCE_STATUS == "DNA_CODE", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    DNA_Percent = round(
      DNA_Count / Total_Appointments * 100,
      1
    )
  ) %>%
  arrange(desc(DNA_Percent))



# Charts ------------------------------------------------------------------



