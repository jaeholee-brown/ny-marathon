# setup: libraries, parallel backend, and helper functions

# data manipulation
library(dplyr)
library(forcats)
library(tidyr)
library(stringr)
library(tibble)

# visualization
library(ggplot2)
library(scales)

# time handling
library(hms)

# statistical modeling
library(quantreg)
library(splines)
library(broom)

# parallel processing
library(future)
library(future.apply)

# parallel backend configuration
# use a reasonable number of workers
# availableCores() respects system / RStudio limits and environment settings
available <- future::availableCores()
workers <- max(1L, min(as.integer(available) - 1L, 8L))

plan(multisession, workers = workers)

message("parallel workers: ", workers)

# global quantile levels
taus <- c(0.10, 0.25, 0.50, 0.75, 0.90)

# global rq method
RQ_METHOD <- "fn"

#' Fit quantile regression across multiple taus in parallel
#'
#' Fits separate quantile regression models for each tau level in parallel
#' using the fast "pfn" method, then combines results.
#'
#' @param formula A formula specifying the model.
#' @param data A data frame containing the variables.
#' @param taus_vec Numeric vector of quantile levels.
#' @param method The rq method to use (default: "pfn").
#'
#' @return A list of rq fit objects, one per tau.
#'
#' @examples
#' \dontrun{
#' fits <- FitRqParallel(y ~ x, df, c(0.25, 0.5, 0.75))
#' }
#'
#' @export
FitRqParallel <- function(formula, data, taus_vec, method = "pfn") {
  fits <- future.apply::future_lapply(
    taus_vec,
    function(tau) {
      rq(formula, tau = tau, data = data, method = method)
    },
    future.packages = c("quantreg", "splines"),
    future.seed = TRUE
  )
  names(fits) <- as.character(taus_vec)
  return(fits)
}

#' Extract tidy coefficients from quantile regression objects
#'
#' Extracts and tidies coefficient estimates, standard errors, and p-values
#' from fitted quantile regression models across multiple quantiles.
#'
#' @param fit_model Either a fitted \code{rq} object with multiple taus,
#'   or a list of single-tau \code{rq} objects from \code{FitRqParallel}.
#' @param taus_vec Numeric vector of quantile levels used in the fit.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{term}{Character. Name of the coefficient.}
#'     \item{tau}{Numeric. The quantile level.}
#'     \item{estimate}{Numeric. Point estimate of the coefficient.}
#'     \item{std.error}{Numeric. Standard error of the estimate.}
#'     \item{p.value}{Numeric. P-value for the coefficient.}
#'   }
#'   Year fixed effects (terms matching "factor(Year)") are excluded.
#'
#' @examples
#' \dontrun{
#' fits <- FitRqParallel(y ~ x, df, c(0.25, 0.5, 0.75))
#' coefs <- GetQrCoefs(fits, c(0.25, 0.5, 0.75))
#' }
#'
#' @export
GetQrCoefs <- function(fit_model, taus_vec) {
  # helper to extract coefficients from a single summary.rq object
  extract_coefs <- function(s, tau_val) {
    # get coefficients matrix
    coef_mat <- s$coefficients
    if (is.null(coef_mat)) {
      stop("No coefficients found in summary object")
    }
    tb <- as.data.frame(coef_mat)
    tibble::tibble(
      term = rownames(tb),
      tau = tau_val,
      estimate = tb[, "Value"],
      std.error = tb[, "Std. Error"],
      p.value = tb[, "Pr(>|t|)"]
    )
  }

  # handle list of single-tau fits (from FitRqParallel)
  if (is.list(fit_model) && !inherits(fit_model, "rq") && !inherits(fit_model, "rqs")) {
    result <- dplyr::bind_rows(lapply(seq_along(taus_vec), function(i) {
      fit <- fit_model[[i]]
      s <- summary(fit, se = "nid", covariance = FALSE)
      extract_coefs(s, taus_vec[i])
    }))
  } else {
    # handle single or multi-tau rq/rqs object
    model_sum <- summary(fit_model, se = "nid", covariance = FALSE)

    # summary() returns "summary.rq" for 1 tau, "summary.rqs" for multiple taus
    if (inherits(model_sum, "summary.rqs")) {
      # multiple tau case - model_sum is a list of summary.rq objects
      result <- dplyr::bind_rows(lapply(seq_along(taus_vec), function(i) {
        extract_coefs(model_sum[[i]], taus_vec[i])
      }))
    } else {
      # single tau case
      result <- extract_coefs(model_sum, taus_vec[1])
    }
  }

  # filter out year fixed effects
  result <- result %>%
    dplyr::filter(!grepl("^factor\\(Year\\)", term))

  return(result)
}
