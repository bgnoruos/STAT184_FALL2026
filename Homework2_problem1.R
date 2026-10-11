library(dplyr); library(tidyr); library(tibble)
library(readr); library(stringr); library(forcats); library(purrr)
sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)
#PART A 
sales_clean <- sales_raw %>%
       mutate(
             item_clean = str_to_lower(str_squish(item_raw)),
             amount = parse_number(amount_raw),
             discount_rate = parse_number(discount_raw) / 100,
             net_amount = amount * (1 - discount_rate),
             size = case_when(
                   is.na(net_amount) ~ "unrecorded",
                   net_amount >= 20 ~ "large",
                   TRUE ~ "small"
               )
         )
sales_clean |>
  select(sale_id, item_clean, amount, discount_rate, net_amount, size)
#PART B
sales_codes <- sales_clean |>
  mutate(
    
    code_clean = str_to_upper(str_trim(code_raw)),
    
    
    valid_code = str_detect(code_clean, "^(ST|ON)-[0-9]{3}$")
  )

invalid_codes <- sales_codes |>
  filter(!valid_code) |>
  select(sale_id, code_raw, code_clean)

invalid_codes

valid_code_parts <- sales_codes |>
  filter(valid_code) |>
  separate_wider_delim(code_clean, delim = "-",
                       names = c("channel", "number"),
                       cols_remove = FALSE) |>
  select(sale_id, code_clean, channel, number)

valid_code_parts

sale_tags <- sales_clean |>
  select(sale_id, tags) |>
  separate_longer_delim(tags, delim = ";")

sale_tags
#PART C
customer_summary <- sales_clean |>
  summarise(
    sales_n     = n(),                          # all sales for the customer
    known_n     = sum(!is.na(net_amount)),      # sales with a known net amount
    large_n     = sum(net_amount >= 20, na.rm = TRUE),  # known net amounts >= 20
    # if there are no known amounts, return NA instead of 0/0 (NaN)
    large_share = if_else(known_n == 0, NA_real_, large_n / known_n),
    .by = customer_id
  ) |>
  arrange(customer_id)

sales_factors <- sales_clean |>
  mutate(
    item_factor = fct_infreq(factor(item_clean)),
    item_group  = fct_lump_n(factor(item_clean), n = 2, other_level = "Other")
  )

levels(sales_factors$item_factor)
levels(sales_factors$item_group)
sales_factors |> count(item_factor)
sales_factors |> count(item_group)