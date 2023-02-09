# at age 30, 25% randomly retire
# aging curve remains the same
players <- unique(sim_df$player)
rand_players <- sample(players, length(players) / 4)

retire_30 <- sim_df |> 
  filter(player %in% rand_players, age >= 30)

out_random_30 <- sim_df |> 
  anti_join(retire_30)

# drop 1st half of career 
# when average between 20 and 25 is below 0.5
ops_avg_start <- sim_df |> 
  filter(age <= 25) |> 
  group_by(player) |> 
  summarise(avg = mean(ops)) |> 
  filter(avg < 0.55)

out_avg_start <- sim_df |> 
  mutate(ops = ifelse(player %in% ops_avg_start$player & age > 25, NA, ops),
         ops_trans = ifelse(is.na(ops), NA, ops_trans)) 

# drop if any 4-year avg is below 0.5
get_drop_4yr <- sim_df |> 
  group_by(player) |> 
  filter((ops + lag(ops) + lag(ops, 2) + lag(ops, 3)) / 4 < 0.55) |> 
  slice_min(age) |> 
  select(player, drop_age = age)

out_4yr_avg <- sim_df |> 
  left_join(get_drop_4yr) |> 
  mutate(drop_age = ifelse(is.na(drop_age), 40, drop_age),
         ops = ifelse(age > drop_age, NA, ops),
         ops_trans = ifelse(is.na(ops), NA, ops_trans)) |>
  select(-drop_age)

# drop at start of career
# sim_df |> 
#   mutate(ops = ifelse(ops < 0.55 & age <= 25, NA, ops),
#          type = "minor league") 

# drop 2nd half of career 
# drop if age > 30 and ops between any 2 years decreases by more than 0.2

# get_drop_2yr <- sim_df |> 
#   filter(age >= 30) |> 
#   group_by(player) |>
#   filter(ops - lag(ops) < -0.1) |> 
#   slice_min(age) |> 
#   select(player, drop_age = age)
# 
# out_2yr_avg <- sim_df |> 
#   left_join(get_drop_2yr) |> 
#   mutate(drop_age = ifelse(is.na(drop_age), 40, drop_age),
#          ops = ifelse(age > drop_age, NA, ops)) |>
#   select(-drop_age)

# compare original curve with different dropouts
drop_plot <- mutate(sim_df, type = "original") |>
  bind_rows(mutate(out_random_30, type = "drop_random_30")) |>
  bind_rows(mutate(out_avg_start, type = "drop_avg_25")) |>
  bind_rows(mutate(out_4yr_avg, type = "drop_4yr_avg")) |> 
  group_by(type, age) |> 
  summarise(avg = mean(ops, na.rm = TRUE)) |> 
  ggplot(aes(x = age, y = avg, color = type)) +
  #geom_point() +
  geom_smooth(method = "loess", se = FALSE, size = 1, span = 1.4) + 
  scale_color_manual(
    values = c("black", "gold", "purple3", "maroon"),
    labels = c("(1) Low 4-year OPS average",
               "(2) Low age 21-25 OPS average",
               "(3) Randomly retire at 30",
               "Original")
    ) +
  scale_x_continuous(breaks = seq(21, 39, 3)) +
  labs(x = "Age",
       y = "OPS",
       color = "Missingness Scenario")
  #ggformula::geom_spline()
  #gam:cubic spline smoother

drop_plot

# library(mosaic)
# library(Metrics)
# mae(mean(ops ~ age, data = sim_df), mean(ops ~ age, data = out_random_30))
# mae(mean(ops ~ age, data = sim_df), mean(ops ~ age, na.rm = TRUE, data = out_avg_start))
# mae(mean(ops ~ age, data = sim_df), mean(ops ~ age, na.rm = TRUE, data = out_4yr_avg))
