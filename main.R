# 初期設定 --------------------------------------------------------------------

set.seed(42)

# 設定の読み込み
source("setting.R")

# 出力画像を保存しておくフォルダをリセットする
fs::dir_delete("figure")
fs::dir_create("figure")

# 関数群 ---------------------------------------------------------------------

# シミュレーションデータを生成する
generate_data <- function(n_instances = 2000, rho = 0) {
  X <- MASS::mvrnorm(
    n_instances,
    mu = c(0, 0),
    Sigma = matrix(c(1, rho, rho, 1), 2, 2)
  )

  colnames(X) <- c("X1", "X2")

  df <- X %>%
    as_tibble() %>%
    mutate(
      across(c(X1, X2), pnorm, mean = 0, sd = 1),
      Y = X1 + X2^2 + rnorm(n_instances, mean = 0, sd = 0.01)
    )

  return(df)
}

# 予測値の等高線プロットを作る
# 理論値との比較もできる
draw_prediction <- function(model, X_df, title) {
  df_grid <- crossing(
    X1 = seq(0, 1, 0.01), X2 = seq(0, 1, 0.01)
  ) %>%
    mutate(theo = X1 + X2^2)

  g <- df_grid %>%
    mutate(predict(rf, new_data = .)) %>%
    ggplot(aes(X1, X2)) +
    geom_point(data = df, color = cols[7], alpha = 0.5) +
    geom_contour(aes(z = theo), color = cols[4], size = 1) +
    geom_label_contour(aes(z = theo), color = cols[4], family = base_family) +
    geom_contour(aes(z = .pred), color = cols[2], size = 1) +
    geom_label_contour(aes(z = .pred), color = cols[2], family = base_family) +
    scale_x_continuous(breaks = breaks_width(0.2)) +
    scale_y_continuous(breaks = breaks_width(0.2)) +
    labs(title = title) +
    theme_scatter()

  save_plot(g, fname = glue("figure/{title}.png"), width = 6, height = 6)
  return(g)
}

# 値を置き換えて予測の平均値を計算する（あとで使う）
predict_average <- function(model, X_df, var_name, xj) {
  pred <- X_df %>%
    mutate({{ var_name }} := xj) %>%
    predict(model, new_data = .) %>%
    pull(.pred) %>%
    mean()

  return(pred)
}

# グリッドを計算する（あとで使う）
get_grids <- function(X_df, var_name, n_grids) {
  min_max <- X_df %>%
    summarise(
      min = min({{ var_name }}),
      max = max({{ var_name }})
    )
  grids <- seq(min_max$min, min_max$max, length.out = n_grids)

  return(grids)
}

partial_dependence <- function(model, X_df, var_name, n_grids = 30) {
  # ターゲットの変数を、取りうる値の最大値から最小値まで動かせるように
  grids <- get_grids(X_df, {{ var_name }}, n_grids)

  # グリッドごとに全データで平均的な予測値を求める
  partial_dependences <- numeric(n_grids)
  for (k in 1:n_grids) {
    partial_dependences[k] <- predict_average(
      model = model,
      X_df = X_df,
      var_name = {{ var_name }},
      xj = grids[k]
    )
  }

  # データフレームとして出力
  df_pred <- tibble(
    var_name = as_label(enquo(var_name)),
    x = grids,
    pred = partial_dependences
  )
  return(df_pred)
}

conditional_dependence <- function(model, X_df, var_name, n_grids = 30) {
  # ターゲットの変数を、取りうる値の最大値から最小値まで動かせるように
  grids <- get_grids(X_df, {{ var_name }}, n_grids)

  # 区間ごとにデータを限定して平均的な予測値を計算
  xjs <- numeric(n_grids - 1)
  conditional_dependences <- numeric(n_grids - 1)
  for (k in 2:n_grids) {
    tmp_df <- X_df %>%
      filter({{ var_name }} %>% between(grids[k - 1], grids[k]))

    xjs[k - 1] <- (grids[k - 1] + grids[k]) / 2
    conditional_dependences[k - 1] <- predict_average(
      model = model,
      X_df = tmp_df,
      var_name = {{ var_name }},
      xj = xjs[k - 1]
    )
  }

  # データフレームとして出力
  df_pred <- tibble(
    var_name = as_label(enquo(var_name)),
    x = xjs,
    pred = conditional_dependences
  )

  return(df_pred)
}


accumulated_local_effects <- function(model, X_df, var_name, n_grids = 30) {
  # ターゲットの変数を、取りうる値の最大値から最小値まで動かせるようにする
  grids <- get_grids(X_df, {{ var_name }}, n_grids)

  # 区間ごとに両端での予測値の平均的な差分を求める
  local_effects <- rep(0, n_grids)
  for (k in 2:n_grids) {
    tmp_df <- X_df %>%
      filter({{ var_name }} %>% between(grids[k - 1], grids[k]))

    pred_upper <- predict_average(
      model = model,
      X_df = tmp_df,
      var_name = {{ var_name }},
      xj = grids[k]
    )
    pred_lower <- predict_average(
      model,
      X_df = tmp_df,
      var_name = {{ var_name }},
      xj = grids[k - 1]
    )
    local_effects[k] <- pred_upper - pred_lower
  }
  accumulated_local_effects <- cumsum(local_effects)

  # データフレームとして出力
  df_pred <- tibble(
    var_name = as_label(enquo(var_name)),
    x = grids,
    pred = accumulated_local_effects
  )
  return(df_pred)
}

draw_dependence <- function(dependence_df, f1, f2, ylabel, title) {
  g <- dependence_df %>%
    ggplot(aes(x, pred)) +
    geom_function(
      data = filter(dependence_df, var_name == "X1"),
      fun = f1,
      color = cols[7],
      size = 1
    ) +
    geom_function(
      data = filter(dependence_df, var_name == "X2"),
      fun = f2,
      color = cols[7],
      size = 1
    ) +
    geom_line(color = cols[2], size = 1) +
    scale_x_continuous(breaks = breaks_width(0.2)) +
    scale_y_continuous(breaks = breaks_width(0.2)) +
    facet_wrap(~var_name) +
    labs(x = "X", y = ylabel, title = title) +
    theme_line()

  save_plot(g, fname = glue("figure/{title}.png"), width = 8)
  return(g)
}


# PD: 相関係数 = 0 ----------------------------------------------------------------
rho <- 0

# データを生成
df <- generate_data(rho = rho)

# モデルの学習
rf <- rand_forest(
  mode = "regression",
  engine = "ranger",
  trees = 500,
  mtry = 2,
  min_n = 10
) %>%
  fit(Y ~ X1 + X2, data = df)

# 予測値を可視化
draw_prediction(
  model = rf,
  X_df = df,
  title = glue("理論値と予測値の比較（相関係数={rho}）")
)

# PDの計算
df_dependence <- bind_rows(
  partial_dependence(rf, df, X1),
  partial_dependence(rf, df, X2)
)

# PDを可視化
draw_dependence(
  dependence_df = df_dependence,
  f1 = ~ . + 1 / 3,
  f2 = ~ .**2 + 0.5,
  ylabel = "Partial Dependence",
  title = glue("理論的な関係と実際のPDの比較（相関係数={rho}）")
)

# PD: 相関係数 = 0.99 ---------------------------------------------------------------
rho <- 0.99

# データを生成
df <- generate_data(rho = rho)

# モデルの学習
rf <- rand_forest(
  mode = "regression",
  engine = "ranger",
  trees = 500,
  mtry = 2,
  min_n = 10
) %>%
  fit(Y ~ X1 + X2, data = df)

# 予測値を可視化
draw_prediction(
  model = rf,
  X_df = df,
  title = glue("理論値と予測値の比較（相関係数={rho}）")
)


df_dependence <- bind_rows(
  partial_dependence(rf, df, X1),
  partial_dependence(rf, df, X2)
)

draw_dependence(
  dependence_df = df_dependence,
  f1 = ~ . + 1 / 3,
  f2 = ~ .^2 + 0.5,
  ylabel = "Partial Dependence",
  title = glue("理論的な関係と実際のPDの比較（相関係数={rho}）")
)

# CD: 相関係数 = 0.99 ----------------------------------------------------------

df_dependence <- bind_rows(
  conditional_dependence(rf, df, X1),
  conditional_dependence(rf, df, X2)
)

draw_dependence(
  dependence_df = df_dependence,
  f1 = ~.,
  f2 = ~ .^2,
  ylabel = "Conditional Dependence",
  title = glue("理論的な関係と実際のCDの比較（相関係数={rho}）")
)

# ALE: 相関係数 = 0.95 ----------------------------------------------------------

df_dependence <- bind_rows(
  accumulated_local_effects(rf, df, X1),
  accumulated_local_effects(rf, df, X2)
)

draw_dependence(
  dependence_df = df_dependence,
  f1 = ~.,
  f2 = ~ .^2,
  ylabel = "Accumulated Local Effects",
  title = glue("理論的な関係と実際のALEの比較（相関係数={rho}）")
)
