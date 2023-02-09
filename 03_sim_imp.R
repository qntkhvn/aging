library(mice)
set.seed(100)

imp_age_only <- function(df, nimp = 5, niter = 30) {
  
  imp_df <- df |> 
    select(player, age, ops_trans)
  
  ini <- imp_df |> 
    mice(maxit = 0)
  
  pred_mat <- ini |> 
    pluck("predictorMatrix")
  
  pred_mat["ops_trans", ] <- c(-2, 2, 0)
  
  imp <- imp_df |> 
    mice(predictorMatrix = pred_mat, 
         method = "2l.norm",
         m = nimp,
         maxit = niter,
         #print = FALSE
         )
  
  return(imp)
}

imp_comp <- function(imp) {
  comp <- imp |>
    complete("long", include = TRUE) |>
    mutate(ops = max_ops * (sin(ops_trans)) ^ 2)
  return(comp)
}

imp_comb <- function(imp, alpha = 0.05) {
  combined <- mice::complete(imp, "long") |> 
    dplyr::group_by(.imp, age) |>
    dplyr::summarize(est = mean(ops_trans),
                     v = var(ops_trans)) |> 
    dplyr::ungroup() |> 
    dplyr::group_by(age) |> 
    dplyr::summarize(avg = mean(est),
                     vw = sum(v) / imp$m, # Within imputation var
                     vb = sum((est - avg) ^ 2) / (imp$m - 1), # Between imputation var
                     vt = vw + vb + vb / imp$m, # total variability
                     se_pooled = sqrt(vt), # pooled se
                     lambda = (vb + vb / imp$m) / vt,
                     riv = (vb + vb / imp$m) / vw,
                     df = (imp$m - 1) / (lambda ^ 2), 
                     upper = avg + qt(1 - alpha / 2, df) * se_pooled,
                     lower = avg - qt(1 - alpha / 2, df) * se_pooled) |> 
    dplyr::transmute(age,
                     ops = max_ops * (sin(avg)) ^ 2,
                     ops_upper = max_ops * (sin(upper)) ^ 2,
                     ops_lower = max_ops * (sin(lower)) ^ 2)
  return(combined)
}

# doParallel::registerDoParallel(cores = 5)

# imp_4yr_avg <- out_4yr_avg |>
#   imp_age_only()
# write_rds(imp_4yr_avg, "cmsac/write_imp_4yr_avg.rds")

# imp_avg_start <- out_avg_start |>
#   imp_age_only()
# write_rds(imp_avg_start, "cmsac/write_imp_avg_start.rds")

# imp_4yr_avg <- read_rds("cmsac/write_imp_4yr_avg.rds")
# imp_avg_start <- read_rds("cmsac/write_imp_avg_start.rds")

# imp_4yr_avg <- read_rds("write_imp_4yr_avg.rds")
imp_avg_start <- read_rds("write_imp_avg_start.rds")

bind_vars <- c("player", "age", "ops", "curve")

sim_df_bind <- sim_df |> 
  mutate(curve = "Original") |> 
  select(all_of(bind_vars))

# comp_4yr_avg <- imp_4yr_avg |> 
#   imp_comp() |> 
#   mutate(curve = paste("Imputation", .imp),
#          curve = str_replace(curve, "Imputation 0", "Missing")) |> 
#   select(all_of(bind_vars)) |> 
#   bind_rows(sim_df_bind) |> 
#   mutate(drop = "Low 4-year average")
# 
# comb_4yr_avg <- imp_4yr_avg |> 
#   imp_comb() |> 
#   mutate(curve = "Combined",
#          drop = "Low 4-year average")

comp_avg_start <- imp_avg_start |> 
  imp_comp() |> 
  mutate(curve = paste("Imputation", .imp),
         curve = str_replace(curve, "Imputation 0", "Missing")) |> 
  select(all_of(bind_vars)) |> 
  bind_rows(sim_df_bind) |> 
  mutate(drop = "Low age 21-25 average")

comb_avg_start <- imp_avg_start |> 
  imp_comb() |> 
  mutate(curve = "Combined",
         drop = "Low age 21-25 average")

# mae(mean(ops ~ age, data = sim_df), mean(ops ~ age, data = comb_4yr_avg))
# Metrics::mae(mean(ops ~ age, data = sim_df), mean(ops ~ age, data = comb_avg_start))

# drop_imp_plot <- comp_4yr_avg |> 
#   bind_rows(comp_avg_start) |> 
#   group_by(curve, age, drop) |>
#   summarise(ops = mean(ops, na.rm = TRUE)) |> 
#   bind_rows(comb_4yr_avg) |> 
#   bind_rows(comb_avg_start) |> 
#   mutate(curve = factor(curve, levels = c(str_c("Imputation ", 1:5), "Combined", "Missing", "Original"))) |> 
#   ggplot(aes(x = age, y = ops, color = curve)) +
#   geom_point() + 
#   geom_smooth(method = "loess", span = 1.1, se = FALSE) +
#   geom_errorbar(aes(ymin = ops_lower, ymax = ops_upper)) + 
#   scale_color_manual(values = c(str_c("gray", seq(10, 50, 10)), "blue", "green", "red")) +
#   #scale_x_continuous(breaks = seq(21, 39, 3)) +
#  # scale_y_continuous(breaks = seq(0.59, 0.71, 0.02)) +
#   facet_wrap(~ drop) +
#   labs(x = "Age",
#        y = "OPS",
#        color = "Curve") +
#   theme_bw() +
#   theme(panel.grid.minor = element_blank())
# 
# drop_imp_plot

# convergence

imp_converge <- function(imp) {
  mn <- imp$chainMean[3, , ] |> 
    as_tibble() |> 
    mutate(iter = row_number()) |>
    pivot_longer(!iter,
                 names_to = "chain",
                 names_prefix = "Chain ",
                 values_to = "mean")
  std <- imp$chainVar[3, , ] |> 
    as_tibble() |> 
    mutate(iter = row_number()) |>
    pivot_longer(!iter,
                 names_to = "chain",
                 names_prefix = "Chain ",
                 values_to = "sd",
                 values_transform = list(sd = sqrt))
  mn |> 
    full_join(std) |> 
    pivot_longer(mean:sd)
}

# trace plot
# converge_trace <- imp_4yr_avg |> 
#   imp_converge() |> 
#   mutate(method = "Low 4-year average") |> 
#   bind_rows(
#     imp_avg_start |> 
#       imp_converge() |> 
#       mutate(method = "Low age 21-25 average") 
#   ) |> 
#   ggplot(aes(iter, value, color = chain)) +
#   geom_line() +
#   ggh4x::facet_grid2(method ~ name, scales = "free", independent = "y")
# 
# converge_trace


# density curve
# converge_density <- comp_4yr_avg |> 
#   filter(curve != "Missing") |> 
#   bind_rows(filter(comp_avg_start, curve != "Missing")) |> 
#   ggplot(aes(ops, color = curve)) +
#   geom_density(size = 1) +
#   scale_color_manual(values = c(palette()[2:6], "black")) +
#   facet_wrap(~ drop)
# 
# converge_density
