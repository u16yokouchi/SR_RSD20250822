# 必要パッケージ
library(readxl)
library(dplyr)
library(rstan)
library(loo)

# 1. データ読み込み＆前処理
df <- readxl::read_excel("Rater6_v2.xlsx") %>%
  rename(
    examinee = StudentID,
    item     = ItemID,
    time     = TimeID,
    X        = Score   # Stanモデルの変数名に合わせる
  ) %>%
  mutate(
    examinee = as.integer(as.factor(examinee)),
    item     = as.integer(as.factor(item)),
    time     = as.integer(as.factor(time)),
    # Stanが期待する「1始まり」のカテゴリに変換（1, 2, ..., K）
    X        = as.integer(X - min(X, na.rm = TRUE) + 1)
  )

# 2. ETinter（例: item × time のID化。必要に応じて設計）
df <- df %>%
  mutate(
    ETinter = as.integer(as.factor(item * time))
  )

# 3. Stanデータリスト作成
stan_data <- list(
  N          = nrow(df),
  J          = length(unique(df$examinee)),
  T          = length(unique(df$time)),
  K          = length(unique(df$X)),         # 1, ..., K
  I          = length(unique(df$ETinter)),
  ExamineeID = df$examinee,
  TimeID     = df$time,
  ETinter    = df$ETinter,
  X          = df$X
)

# 念のため確認（これが 1〜K になっていればOK）
cat("カテゴリ最小値:", min(stan_data$X), "最大値:", max(stan_data$X), "\n")
# table(stan_data$X)

# 4. Stan推論
fit <- stan(
  file = "Yokouchi et al.stan",
  data = stan_data,
  iter = 4000, warmup = 2000, chains = 4, seed = 123,
  control = list(adapt_delta = 0.99)
)

# 5. パラメータ抽出・表示
print(fit, pars = c("theta", "beta_t", "alpha_t", "alpha_i", "beta_i", "category_est"))

sims <- extract(fit)

# 6. β_tの事後平均プロット
if(!is.null(sims$beta_t)){
  beta_mean <- apply(sims$beta_t, 2, mean)
  plot(beta_mean, type = "o", xlab = "Time (t)", ylab = expression(beta[t]),
       main = expression(paste("Posterior Mean of ", beta[t])))
}

# 7. PPP（Posterior Predictive p-value: 一致率）
if(!is.null(sims$X_pre)){
  obs_score <- stan_data$X
  score_rep <- sims$X_pre
  ppp <- mean(score_rep == matrix(obs_score, nrow = nrow(score_rep), ncol = ncol(score_rep), byrow = TRUE))
  cat("PPP (Posterior Predictive p-value: 観測とレプリケート一致率) =", round(ppp, 4), "\n")
}

# 8. WAIC・LOO
if(!is.null(sims$log_lik)){
  log_lik <- sims$log_lik # [iterations, N]
  waic_result <- waic(log_lik)
  loo_result <- loo(log_lik)
  print(waic_result)
  print(loo_result)
}

# 9. RMSE
if(!is.null(sims$X_pre)){
  pred_mean <- apply(sims$X_pre, 2, mean)
  obs_score <- stan_data$X
  rmse <- sqrt(mean((pred_mean - obs_score)^2))
  cat("RMSE (Root Mean Squared Error) =", round(rmse, 4), "\n")
}

# 10. RhatとESSの収束診断
# Yokouchi et al. (2024) モデルのパラメータ名に合わせて指定

summary_fit <- summary(fit, pars = c("theta", "beta_t", "alpha_t", "alpha_i", "beta_i", "category_est"))$summary

# Rhat > 1.1 パラメータ名表示
high_rhat_idx <- which(summary_fit[,"Rhat"] > 1.1)
if(length(high_rhat_idx) > 0) {
  cat("!!! The following parameters have Rhat > 1.1 (not converged):\n")
  print(rownames(summary_fit)[high_rhat_idx])
} else {
  cat("All monitored parameters have Rhat <= 1.1 (converged).\n")
}

# ESSの統計
ess_vals <- summary_fit[,"n_eff"]
cat("Effective Sample Size (ESS):\n")
print(ess_vals)

cat("Minimum ESS:", min(ess_vals, na.rm=TRUE), "\n")
cat("Median  ESS:", median(ess_vals, na.rm=TRUE), "\n")
cat("Maximum ESS:", max(ess_vals, na.rm=TRUE), "\n")# 10. RhatとESSの収束診断
# Yokouchi et al. (2024) モデルのパラメータ名に合わせて指定

summary_fit <- summary(fit, pars = c("theta", "beta_t", "alpha_t", "alpha_i", "beta_i", "category_est"))$summary

# Rhat > 1.1 パラメータ名表示
high_rhat_idx <- which(summary_fit[,"Rhat"] > 1.1)
if(length(high_rhat_idx) > 0) {
  cat("!!! The following parameters have Rhat > 1.1 (not converged):\n")
  print(rownames(summary_fit)[high_rhat_idx])
} else {
  cat("All monitored parameters have Rhat <= 1.1 (converged).\n")
}

# ESSの統計
ess_vals <- summary_fit[,"n_eff"]
cat("Effective Sample Size (ESS):\n")
print(ess_vals)

cat("Minimum ESS:", min(ess_vals, na.rm=TRUE), "\n")
cat("Median  ESS:", median(ess_vals, na.rm=TRUE), "\n")
cat("Maximum ESS:", max(ess_vals, na.rm=TRUE), "\n")
