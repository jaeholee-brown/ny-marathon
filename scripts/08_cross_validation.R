# cross-validation: 5-fold cv comparison of all models (optimized)
#
# requires: all model scripts (02-07) to be sourced first
# outputs: cv_results, visualization

#' Compute quantile regression check loss (pinball loss)
#'
#' Calculates the check loss (pinball loss) for quantile regression,
#' which is the asymmetric loss function minimized by quantile regression.
#'
#' @param y Numeric vector of observed values.
#' @param yhat Numeric vector of predicted values.
#' @param tau Numeric scalar, the quantile level (0 < tau < 1).
#'
#' @return Numeric vector of check loss values for each observation.
#'
#' @examples
#' \dontrun{
#' y <- c(1, 2, 3, 4, 5)
#' yhat <- c(1.1, 2.2, 2.8, 4.1, 4.9)
#' loss <- CheckLoss(y, yhat, tau = 0.5)
#' }
#'
#' @export
CheckLoss <- function(y, yhat, tau) {
  u <- y - yhat
  return((tau - (u < 0)) * u)
}

#' Parallelized K-fold cross-validation for quantile regression
#'
#' Performs K-fold cross-validation for a quantile regression model,
#' computing mean check loss across all specified quantiles. The function
#' parallelizes across all tau-fold combinations for maximum efficiency.
#'
#' @param df Data frame containing the response and predictors.
#' @param formula Formula specifying the quantile regression model.
#' @param taus_vec Numeric vector of quantile levels to evaluate.
#' @param K Integer, number of folds for cross-validation (default 5).
#' @param seed Integer, random seed for reproducible fold assignment.
#'
#' @return Numeric scalar, the mean check loss averaged across all
#'   quantiles and folds.
#'
#' @details
#' This function creates a grid of all tau-fold combinations (e.g., 25
#' combinations for 5 taus and 5 folds) and evaluates them in parallel
#' using \code{future_mapply}. This is significantly faster than nested
#' sequential loops.
#'
#' @examples
#' \dontrun{
#' loss <- CvQrMeanLoss(df_m1, formula(qr_fit), taus, K = 5)
#' }
#'
#' @export
CvQrMeanLoss <- function(df, formula, taus_vec, K = 5, seed = 123) {
  n <- nrow(df)

  # create reproducible folds
  set.seed(seed)
  folds <- sample(rep(1:K, length.out = n))

  # pre-compute fold indices once (avoids repeated computation)
  fold_indices <- lapply(seq_len(K), function(k) which(folds == k))

  # create grid of all tau-fold combinations
  # this allows parallelizing across all 25 combinations (5 taus x 5 folds)
  grid <- expand.grid(tau_idx = seq_along(taus_vec), fold = seq_len(K))

  # parallel computation across all tau-fold combinations
  results <- future.apply::future_mapply(
    function(tau_idx, k) {
      tau <- taus_vec[tau_idx]
      test_idx <- fold_indices[[k]]
      train_idx <- setdiff(seq_len(n), test_idx)

      # fit model on training fold
      fit_k <- rq(
        formula,
        tau = tau,
        data = df[train_idx, , drop = FALSE],
        method = RQ_METHOD
      )

      # predict on test fold
      pred_k <- predict(fit_k, newdata = df[test_idx, , drop = FALSE])

      # return total check loss for this fold (sum, not mean)
      sum(CheckLoss(df$FinishSeconds[test_idx], pred_k, tau))
    },
    grid$tau_idx,
    grid$fold,
    future.packages = c("quantreg", "splines"),
    future.seed = TRUE,
    SIMPLIFY = TRUE
  )

  # reshape results: rows = folds, cols = taus
  loss_matrix <- matrix(results, nrow = K, ncol = length(taus_vec), byrow = FALSE)

  # mean across folds for each tau, then mean across taus
  tau_means <- colSums(loss_matrix) / n
  return(mean(tau_means))
}

#' Run cross-validation for a single model
#'
#' Wrapper function that runs K-fold cross-validation for a single model
#' and returns results as a tibble.
#'
#' @param df Data frame for this model.
#' @param form Formula for the quantile regression model.
#' @param label Character string identifying the model.
#'
#' @return A tibble with columns \code{model} and \code{mean_check_loss}.
#'
#' @export
CvOneModel <- function(df, form, label) {
  return(
    tibble::tibble(
      model = label,
      mean_check_loss = CvQrMeanLoss(df, form, taus, K = 5)
    )
  )
}

# extract formulas from the fitted models
form_m1 <- formula(qr_fit_base)
form_m2 <- formula(qr_fit_cont)
form_m3 <- formula(qr_fit_18)
form_m4 <- formula(qr_fit_since1975)
form_m5 <- formula(qr_fit_5)
form_m6 <- formula(qr_fit_4)

# run cv for all 6 models in parallel
# each model's cv is internally parallelized across tau-fold combinations
message("starting cross-validation for 6 models...")

f1 <- future::future(
  CvOneModel(df_m1, form_m1, "M1: 3-level factor"),
  packages = c("quantreg", "splines", "tibble", "future.apply")
)

f2 <- future::future(
  CvOneModel(df_m2, form_m2, "M2: Continuous 0-17 yrs"),
  packages = c("quantreg", "splines", "tibble", "future.apply")
)

f3 <- future::future(
  CvOneModel(df_m3, form_m3, "M3: 18-level discrete"),
  packages = c("quantreg", "splines", "tibble", "future.apply")
)

f4 <- future::future(
  CvOneModel(df_m4, form_m4, "M4: Trend since 1975"),
  packages = c("quantreg", "splines", "tibble", "future.apply")
)

f5 <- future::future(
  CvOneModel(df_m5, form_m5, "M5: School stage (5-level)"),
  packages = c("quantreg", "splines", "tibble", "future.apply")
)

f6 <- future::future(
  CvOneModel(df_m6, form_m6, "M6: Generational cohorts (4-level)"),
  packages = c("quantreg", "splines", "tibble", "future.apply")
)

# collect results
cv_results <- dplyr::bind_rows(
  future::value(f1),
  future::value(f2),
  future::value(f3),
  future::value(f4),
  future::value(f5),
  future::value(f6)
) %>%
  arrange(mean_check_loss)

print(cv_results)
write.csv(cv_results, "output/cv_results.csv", row.names = FALSE)

# visualization: model ranking by cv loss
p_cv <- cv_results %>%
  mutate(model = forcats::fct_reorder(model, mean_check_loss)) %>%
  ggplot(aes(x = model, y = mean_check_loss)) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL,
    y = "5-fold mean check loss (lower is better)",
    title = "Cross-validated quantile loss by Title IX specification"
  ) +
  theme_minimal(base_size = 12)

print(p_cv)
ggsave("output/cv_comparison.png", p_cv, width = 10, height = 6, dpi = 300)

message("cross-validation complete")
