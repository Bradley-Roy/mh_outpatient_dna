# Wrangle Data


clinic_attendance_codes <- all_appointments_raw |> 
  clean_names() |> 
  distinct(clinic_attendance)



# Wrangle -----------------------------------------------------------------

## all appoitnemnts for summar chart

# all_appointments <- all_appointments_total_raw |> 
#   clean_names() |> 
#   left_join(specialty_lookup) |> 
#   mutate(selections = case_when(specialty %in% select_specialties ~ "Mental Health",
#                                 TRUE ~ "Other"))
# 
# all_specialties_chart <- plot_ly(data = all_appointments,
#                                  x = ~total_appointments,
#                                  y = ~specialty_description,
#                                  color = ~selections,
#                                  colors = chart_cols,
#                                  type = 'bar') %>%
#   layout(xaxis = list(title = "Appointments"),
#          yaxis = list(title = "Specialty", 
#                       categoryorder = "total ascending"),
#          hovermode = "y unified")


## all appointments by Year and SIMD
all_appointments_simd <- all_appointments_raw |> 
  clean_names() |> 
  left_join(
    simd_lookup,
    by = "postcode"
  ) |> 
  left_join(specialty_lookup) |> 
  mutate(date_year = year(clinic_date)) |> 
  mutate(selections = case_when(specialty %in% select_specialties ~ "Mental Health",
                                TRUE ~ "Other"))

# Practice DNA Rate
all_dna_summary <- all_appointments_simd %>%
  group_by() %>%
  summarise(total_appointments = n(),
            dna_count = sum(clinic_attendance == 8, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(dna_percent = percent(dna_count / total_appointments, .1)) %>%
  arrange(desc(dna_percent)) 

#specialty
#mental health group
all_spec_simd_summary_mh <- all_appointments_simd %>%
  filter(specialty %in% select_specialties) |> 
  mutate(specialty_description = "Mental Health (Grouped)",
         selections = "Mental Health (Grouped)") |> 
  group_by(specialty_description, selections) %>%
  summarise(total_appointments = n(),
            dna_count = sum(clinic_attendance == 8, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(dna_percent_chart = dna_count / total_appointments,
         dna_percent = percent(dna_count / total_appointments, .1)) %>%
  arrange(desc(dna_percent)) 

#all specialties group
all_spec_simd_summary_all <- all_appointments_simd %>% 
  mutate(specialty_description = "All Specialties",
         selections = "All Specialties") |> 
  group_by(specialty_description, selections) %>%
  summarise(total_appointments = n(),
            dna_count = sum(clinic_attendance == 8, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(dna_percent_chart = dna_count / total_appointments,
         dna_percent = percent(dna_count / total_appointments, .1)) %>%
  arrange(desc(dna_percent)) 

#standard names group
all_spec_simd_summary <- all_appointments_simd %>%
  group_by(specialty_description, selections) %>%
  summarise(total_appointments = n(),
            dna_count = sum(clinic_attendance == 8, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(dna_percent_chart = dna_count / total_appointments,
         dna_percent = percent(dna_count / total_appointments, .1)) %>%
  arrange(desc(dna_percent)) |> 
  bind_rows(all_spec_simd_summary_mh, all_spec_simd_summary_all)

# all spec - appts
all_specialties_chart <- all_spec_simd_summary |> 
  filter(!specialty_description %in% c("All Specialties", "Mental Health")) |> 
  plot_ly(x = ~total_appointments,
          y = ~specialty_description,
          color = ~selections,
          colors = chart_cols,
          type = 'bar') %>%
  layout(xaxis = list(title = "Appointments"),
         yaxis = list(title = "Specialty", 
                      categoryorder = "total ascending"),
         hovermode = "y unified")

# all spec - DNAs
all_specialties_dna_chart <- all_spec_simd_summary |> 
  filter(!specialty_description %in% c("All Specialties", "Mental Health")) |> 
  plot_ly(x = ~dna_count,
          y = ~specialty_description,
          color = ~selections,
          colors = chart_cols,
          type = 'bar') %>%
  layout(xaxis = list(title = "Appointments"),
         yaxis = list(title = "Specialty", 
                      categoryorder = "total ascending"),
         hovermode = "y unified")


# DNA Percentage 
all_specialties_dnaper_chart <- plot_ly(data = all_spec_simd_summary,
                                        x = ~dna_percent_chart,
                                        y = ~specialty_description,
                                        color = ~selections,
                                        colors = chart_cols,
                                        type = 'bar') %>%
  layout(xaxis = list(title = "Appointments",
                      tickformat = ".0%" ),
         yaxis = list(title = "Specialty", 
                      categoryorder = "total ascending"),
         hovermode = "y unified")


# Specialty + Year + SIMD
all_spec_simd_year_mh <- all_appointments_simd %>%
  filter(specialty %in% select_specialties) |> 
  mutate(specialty_description = "Mental Health") |> 
  group_by(date_year, specialty_description, simd_quintile) %>%
  summarise(total_appointments = n(),
            dna_count = sum(clinic_attendance == 8, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(dna_percent = percent(dna_count / total_appointments, .1)) %>%
  arrange(desc(dna_percent)) 

all_spec_simd_year_all <- all_appointments_simd %>%
  mutate(specialty_description = "All Specialties") |> 
  group_by(date_year, specialty_description, simd_quintile) %>%
  summarise(total_appointments = n(),
            dna_count = sum(clinic_attendance == 8, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(dna_percent = percent(dna_count / total_appointments, .1)) %>%
  arrange(desc(dna_percent)) 


all_spec_simd_year <- all_appointments_simd %>%
  group_by(date_year, specialty_description, simd_quintile) %>%
  summarise(total_appointments = n(),
            dna_count = sum(clinic_attendance == 8, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(dna_percent = percent(dna_count / total_appointments, .1)) %>%
  arrange(desc(dna_percent)) |> 
  bind_rows(all_spec_simd_year_all, all_spec_simd_year_mh) 


#test table to check formatting
all_spec_simd_pivot <- all_spec_simd_year |>
  # filter(specialty_description == "General Psychiatry") |>
  select(specialty_description, date_year, simd_quintile, dna_percent) |>
  pivot_wider(names_from = date_year, values_from = dna_percent) |>
  arrange(specialty_description, simd_quintile) |>
  filter(!is.na(simd_quintile)) |>
  select(specialty_description, simd_quintile, `2023`, `2024`, `2025`) |> 
  rename(`Specialty Description` = specialty_description,
         `SIMD Quintile` = simd_quintile)



