install.packages(c("ggplot2", "dplyr", "tidyr", "readr", "sf"))
library(ggplot2)

vehicles <- ggplot2::mpg

p_A <- ggplot(vehicles, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv), alpha = 0.6, size = 1.8) +    # color only in the point layer
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +  # blue line, no confidence band
  facet_wrap(~ drv, nrow = 1) +                              # panels: 4 | f | r
  labs(x = "Engine displacement (litres)",
       y = "Highway fuel economy (mpg)",
       color = "drv") +
  theme_bw()

print(p_A)
ggsave("p1_A.png", p_A, width = 9, height = 4, dpi = 150)
#part B
library(ggplot2)
library(dplyr)

vehicles <- ggplot2::mpg

# Left plot: counts (stacked bars)
p_B_left <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar() +
  labs(x = "drv", y = "count", fill = "factor(cyl)") +
  theme_bw()

# Right plot: proportion on the y-axis, computed explicitly
prop_tbl <- vehicles %>%
  count(drv, cyl) %>%
  group_by(drv) %>%
  mutate(n_drv = sum(n), prop = n / n_drv) %>%
  ungroup()

p_B_right <- ggplot(prop_tbl, aes(x = drv, y = prop, fill = factor(cyl))) +
  geom_col() +
  labs(x = "drv", y = "Proportion within drive type", fill = "factor(cyl)") +
  theme_bw()

print(p_B_left)
print(p_B_right)

ggsave("p1_B_left.png",  p_B_left,  width = 4.5, height = 4, dpi = 150)
ggsave("p1_B_right.png", p_B_right, width = 4.5, height = 4, dpi = 150)

# Sample size per drive type (supports the explanation)
vehicles %>% count(drv, name = "n_cars")
# part C
prop_tbl <- vehicles %>%
  count(drv, cyl) %>%
  group_by(drv) %>%
  mutate(n_drv = sum(n), prop = n / n_drv) %>%
  ungroup()