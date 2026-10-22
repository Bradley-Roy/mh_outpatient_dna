
# archive code ------------------------------------------------------------


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
