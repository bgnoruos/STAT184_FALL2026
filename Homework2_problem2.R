reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)
customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)
# Problem 2, Part A: 

reports_labeled <- reports_raw |>
  arrange(form, line) |>
  group_by(form) |>
  fill(customer_id, .direction = "down") |>
  ungroup()

reports_labeled

reports_clean <- reports_labeled |>
  mutate(
    primary_clean = na_if(primary_amount, -999),
    amount        = coalesce(primary_clean, backup_amount)
  )

reports_clean |>
  select(form, line, customer_id, round,
         primary_amount, backup_amount, amount)

reports_complete <- reports_clean |>
  select(customer_id, round, amount) |>
  mutate(recorded_row = TRUE) |>
  complete(customer_id, round = 1:3,
           fill = list(recorded_row = FALSE))

reports_complete

nrow(reports_clean)                          
nrow(reports_complete)                       
sum(reports_complete$recorded_row)           
sum(!reports_complete$recorded_row)          
sum(is.na(reports_clean$amount))             
sum(is.na(reports_complete$amount))          


# ---------- Problem 2, Part B: Join and check ----------


customers |> filter(is.na(customer_id))            # missing IDs
reports_clean |> count(customer_id, round) |> filter(n > 1)



customers_marked <- customers |> mutate(customer_found = TRUE)

reports_joined <- reports_clean |>
  left_join(customers_marked,
            by = join_by(customer_id),
            relationship = "many-to-one",
            na_matches = "never") |>
  mutate(customer_found = coalesce(customer_found, FALSE))

reports_joined |>
  select(customer_id, round, amount, region, customer_found)



reports_clean |>
  anti_join(customers, by = join_by(customer_id), na_matches = "never") |>
  distinct(customer_id)


customers |>
  anti_join(reports_clean, by = join_by(customer_id), na_matches = "never") |>
  distinct(customer_id)

#PART C
customers_bad <- bind_rows(customers,
                           tibble(customer_id = "C2", region = "East")
reports_duplicated <- reports_clean |>
  left_join(customers_bad,
            by = join_by(customer_id),
            relationship = "many-to-many")
reports_duplicated
nrow(reports_clean)                            
nrow(reports_duplicated)                       
sum(reports_clean$amount, na.rm = TRUE)        
sum(reports_duplicated$amount, na.rm = TRUE) 

reports_wide <- reports_complete |>
  select(customer_id, round, amount) |>
  pivot_wider(names_from = round,
              names_prefix = "round_",
              values_from = amount)
reports_wide
