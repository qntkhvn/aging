library(Lahman)
library(tidyverse)

mlb_sim <- Batting |> 
  group_by(playerID, yearID) |> 
  summarise(across(G:GIDP, sum)) |> 
  battingStats() |> 
  left_join(People, by = "playerID") |> 
  filter(PA >= 100) |> 
  mutate(age = ifelse(birthMonth >= 7, yearID - birthYear - 1, yearID - birthYear),
         ops_trans = asin(sqrt(OPS / max(OPS))))

max_ops <- max(mlb_sim$OPS)

library(lme4)
set.seed(100)

mlb_fit <- lmer(ops_trans ~ poly(age, 3, raw = TRUE) + (1 | playerID),
                data = mlb_sim)

var_cor <- mlb_fit |> 
  VarCorr() |> 
  as_tibble()

age_sim <- 21:39
sim_career <- function() {
  # baseline
  pred <- predict(mlb_fit, tibble(age = age_sim), re.form = NA)
  # deviation from baseline
  shift <- rnorm(1, 0, var_cor$sdcor[1])
  # variability across seasons
  eps <- rnorm(length(age_sim), 0, var_cor$sdcor[2])
  
  return(pred + shift + eps)
}

sim_list <- c()
nsim <- 1000
for (i in 1:nsim) {
  sim_list[[i]] <- tibble(player = i,
                          ops_trans = sim_career(),
                          age = age_sim)
}

sim_df <- sim_list |> 
  bind_rows() |> 
  mutate(ops = max_ops * sin(ops_trans) ^ 2)
