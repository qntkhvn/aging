mlb_real <- Batting |> 
  group_by(playerID, yearID) |> 
  summarise(across(G:GIDP, sum)) |> 
  battingStats() |> 
  left_join(People, by = "playerID") |> 
  mutate(age = ifelse(birthMonth >= 7, yearID - birthYear - 1, yearID - birthYear))

age_range <- 21:39
mlb_real_imp <- mlb_real |> 
  filter(PA >= 100, age %in% age_range, str_sub(debut, 1, 4) >= 1985) |> 
  mutate(ops_trans = asin(sqrt(OPS / max(OPS))),
         pnum = as.integer(as.factor(playerID)))

players <- unique(mlb_real_imp$playerID)
players_age <- tibble(playerID = rep(players, each = length(age_range))) |> 
  group_by(playerID) |> 
  mutate(age = age_range) |> 
  ungroup()

all_players <- players_age |> 
  left_join(mlb_real_imp) |> 
  mutate(player = as.integer(factor(playerID)))

set.seed(100)
# imp_mlb_age_only <- all_players |> 
#   imp_age_only()

# write_rds(imp_mlb_age_only, "cmsac/write_imp_mlb_age_only.rds")
# imp_mlb_age_only <- read_rds("cmsac/write_imp_mlb_age_only.rds")
imp_mlb_age_only <- read_rds("write_imp_mlb_age_only.rds")

mlb_imp_plot <- imp_mlb_age_only |> 
  imp_comp() |> 
  mutate(type = paste("Imputation", .imp),
         type = str_replace(type, "Imputation 0", "Missing")) |> 
  select(.id, age, ops, type) |> 
  group_by(type, age) |>
  summarise(ops = mean(ops, na.rm = TRUE)) |> 
  bind_rows(transmute(imp_comb(imp_mlb_age_only), type = "Combined", age, ops)) |> 
  ungroup() |> 
  mutate(type = fct_relevel(type, "Missing", "Combined")) |> 
  ggplot(aes(age, ops, color = type)) +
  #geom_point() + 
  geom_smooth(method = "loess", span = 1.5, se = FALSE) +
  #geom_point(aes(age, ops), data = mutate(imp_comb(imp_mlb_age_only), type = "Combined")) +
  geom_smooth(aes(age, ops), se = FALSE, method = "loess", span = 1.5,
              data = mutate(imp_comb(imp_mlb_age_only), type = "Combined")) +
  #geom_errorbar(aes(ymin = ops_lower, ymax = ops_upper)) + 
  scale_color_manual(values = c("green", "red", str_c("gray", seq(60, 80, 5)))) +
  scale_x_continuous(breaks = seq(21, 39, 3)) +
  labs(x = "Age",
       y = "OPS",
       color = "Curve")

mlb_imp_plot

# imputation with covariates
# ops
# obp
# hr rate (hr / pa)
# strikeouts rate
# walk rate

# mlb_cov <- all_players |> 
#   mutate(hr_rate = HR/PA,
#          so_rate = SO/PA,
#          bb_rate = BB/PA,
#          obp_trans = asin(sqrt(OBP)),
#          hr_trans = asin(sqrt(hr_rate)),
#          so_trans = asin(sqrt(so_rate)),
#          bb_trans = asin(sqrt(bb_rate))) |> 
#   select(player, age, contains("trans"))
# 
# pred_mat <- mlb_cov |> 
#   mice(maxit = 0) |> 
#   pluck("predictorMatrix")
# pred_mat["ops_trans", ] <- c(-2, 2, 0, 2, 2, 2, 2)
# pred_mat["obp_trans", ] <- c(-2, 2, 2, 0, 2, 2, 2)
# pred_mat["hr_trans", ] <- c(-2, 2, 2, 2, 0, 2, 2)
# pred_mat["so_trans", ] <- c(-2, 2, 2, 2, 2, 0, 2)
# pred_mat["bb_trans", ] <- c(-2, 2, 2, 2, 2, 2, 0)
# 
# set.seed(102)
# imp_cov <- mlb_cov |> 
#   mice(predictorMatrix = pred_mat,
#        method = "2l.norm")
# 
# imp_cov |> 
#   imp_comp() |> 
#   mutate(obp = sin(obp_trans)^2,
#          hr_rate = sin(hr_trans)^2,
#          so_rate = sin(so_trans)^2,
#          bb_rate = sin(bb_trans)^2) |> 
#   mutate(type = paste("Imputation", .imp)) |> 
#   select(age, type, ops:bb_rate) |> 
#   pivot_longer(ops:bb_rate) |> 
#   group_by(type, age, name) |>
#   summarise(val = mean(value, na.rm = TRUE)) |> 
#   ggplot(aes(x = age, y = val, color = type)) +
#   geom_point() + 
#   geom_smooth(method = "gam") +
#   facet_wrap(~ name, scales = "free")
  

