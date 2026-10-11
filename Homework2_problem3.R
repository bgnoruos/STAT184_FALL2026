#Problem 3, Part A: Write a t-test function 

two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  # Sample summaries
  nx <- length(x)
  ny <- length(y)
  mean_x <- mean(x)
  mean_y <- mean(y)
  var_x <- var(x)
  var_y <- var(y)
  
  if (equal_var) {
    # Pooled test
    method <- "Pooled"
    sp2 <- ((nx - 1) * var_x + (ny - 1) * var_y) / (nx + ny - 2)
    se <- sqrt(sp2 * (1 / nx + 1 / ny))
    df <- nx + ny - 2
  } else {
    # Welch test
    method <- "Welch"
    se <- sqrt(var_x / nx + var_y / ny)
    df <- (var_x / nx + var_y / ny)^2 /
      ((var_x / nx)^2 / (nx - 1) + (var_y / ny)^2 / (ny - 1))
  }
  
  t_stat <- (mean_x - mean_y) / se
  p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
  
  return(list(
    method = method,
    t_stat = t_stat,
    df = df,
    p_value = p_value,
    reject = p_value < alpha
  ))
}

# Verification against t.test() (used only for checking)
x_check <- c(12, 15, 14, 10, 13)
y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)

mine_welch  <- two_sample_t(x_check, y_check)
mine_pooled <- two_sample_t(x_check, y_check, equal_var = TRUE)
mine_welch
mine_pooled

ref_welch  <- t.test(x_check, y_check, var.equal = FALSE)
ref_pooled <- t.test(x_check, y_check, var.equal = TRUE)

abs(mine_welch$t_stat  - unname(ref_welch$statistic))  < 1e-8
abs(mine_welch$df      - unname(ref_welch$parameter))  < 1e-8
abs(mine_welch$p_value - ref_welch$p.value)            < 1e-8
abs(mine_pooled$t_stat  - unname(ref_pooled$statistic)) < 1e-8
abs(mine_pooled$df      - unname(ref_pooled$parameter)) < 1e-8
abs(mine_pooled$p_value - ref_pooled$p.value)           < 1e-8
# All six should be TRUE: the differences are smaller than 1e-8.

# ---------- Problem 3, Part B: Simulation ----------

B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05

scenarios <- data.frame(
  scenario = c("equal_null", "unequal_null", "equal_shift", "unequal_shift"),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x   = c(1, 3, 1, 3),
  sd_y   = c(1, 1, 1, 1)
)

set.seed(2026)  # once, before the loops

# Allocate storage in advance: 4 scenarios x 1000 replicates x 2 methods
total_rows <- nrow(scenarios) * B * 2
simulation_results <- data.frame(
  scenario  = character(total_rows),
  replicate = integer(total_rows),
  method    = character(total_rows),
  t_stat    = numeric(total_rows),
  df        = numeric(total_rows),
  p_value   = numeric(total_rows),
  reject    = logical(total_rows)
)

row_i <- 1
for (s in seq_len(nrow(scenarios))) {
  for (b in seq_len(B)) {
    # Fresh samples for this replicate
    x <- rnorm(n_x, mean = scenarios$mean_x[s], sd = scenarios$sd_x[s])
    y <- rnorm(n_y, mean = scenarios$mean_y[s], sd = scenarios$sd_y[s])
    
    # Same samples for both methods
    for (eq in c(FALSE, TRUE)) {
      res <- two_sample_t(x, y, equal_var = eq, alpha = alpha)
      simulation_results$scenario[row_i]  <- scenarios$scenario[s]
      simulation_results$replicate[row_i] <- b
      simulation_results$method[row_i]    <- res$method
      simulation_results$t_stat[row_i]    <- res$t_stat
      simulation_results$df[row_i]        <- res$df
      simulation_results$p_value[row_i]   <- res$p_value
      simulation_results$reject[row_i]    <- res$reject
      row_i <- row_i + 1
    }
  }
}

head(simulation_results, 6)
nrow(simulation_results)   # 8000

simulation_summary <- simulation_results |>
  group_by(scenario, method) |>
  summarise(
    mean_t = mean(t_stat),
    sd_t = sd(t_stat),
    rejection_rate = mean(reject),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

  # 8 rows
