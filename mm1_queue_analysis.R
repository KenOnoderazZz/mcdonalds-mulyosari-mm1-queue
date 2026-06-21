# =============================================================================
# M/M/1 Queueing Theory Analysis — McDonald's Mulyosari Self-Service Kiosks
# Course: Operations Research | Institut Teknologi Sepuluh Nopember (ITS)
# Data: 71 observations, 2 independent kiosks, Idul Adha period 2025
# =============================================================================
# This script reads the actual observational data and reproduces:
#   1. Data cleaning & derived variables
#   2. Descriptive statistics per counter
#   3. Exponential distribution fitting (KS test) — interarrival & service time
#   4. Poisson arrival assumption check
#   5. M/M/1 performance metrics: λ, μ, ρ, P0, Wq, W, Lq, L
#   6. Visualizations: histograms, ECDFs, QQ-plots, performance summary
# =============================================================================

# ---- 0. Packages ------------------------------------------------------------
required <- c("readxl", "dplyr", "ggplot2", "tidyr", "MASS",
              "fitdistrplus", "gridExtra", "scales", "viridis")
invisible(lapply(required, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}))

library(readxl); library(dplyr); library(ggplot2); library(tidyr)
library(MASS); library(fitdistrplus); library(gridExtra)
library(scales); library(viridis)

dir.create("output", showWarnings = FALSE)


# =============================================================================
# 1. LOAD & CLEAN DATA
# =============================================================================
# Place "Cleaned-Data-Research-Operation.xlsx" in your working directory
raw <- read_excel("Cleaned-Data-Research-Operation.xlsx",
                  sheet = "Final Clean Data")

# Parse time columns (stored as HH:MM:SS strings or duration)
parse_hms <- function(x) {
  x <- as.character(x)
  x <- gsub("0 days ", "", x)
  parts <- strsplit(x, ":")
  sapply(parts, function(p) {
    if (length(p) < 2 || is.na(p[1])) return(NA_real_)
    h <- as.numeric(p[1])
    m <- as.numeric(p[2])
    s <- as.numeric(gsub("[^0-9.]", "", p[3]))
    h * 3600 + m * 60 + s
  })
}

df <- raw %>%
  rename(
    No            = No,
    Counter       = Counter,
    Arrival       = `Arrival Time`,
    SysStart      = `System Start`,
    SysEnd        = `System End`,
    Pickup        = Pickup,
    TotalSys_raw  = `Total Time System`,
    WaitRaw       = `Waiting Time`,
    PickupDelay   = `Pickup Delay`,
    IAT_raw       = `InterArrival Time`,
    TBS_raw       = `Time Between System Start`
  ) %>%
  mutate(
    # Convert time-of-day strings to seconds since midnight
    arr_sec   = parse_hms(Arrival),
    start_sec = parse_hms(SysStart),
    end_sec   = parse_hms(SysEnd),
    # Derived variables in seconds
    service_time  = end_sec - start_sec,
    waiting_time  = start_sec - arr_sec,
    total_in_sys  = end_sec - arr_sec,
    iat_sec       = parse_hms(IAT_raw),
    pickup_delay  = parse_hms(PickupDelay)
  ) %>%
  filter(service_time > 0)   # remove invalid rows

c1 <- df %>% filter(Counter == 1)
c2 <- df %>% filter(Counter == 2)

cat("=== DATA SUMMARY ===\n")
cat(sprintf("Total observations : %d\n", nrow(df)))
cat(sprintf("Counter 1          : %d observations\n", nrow(c1)))
cat(sprintf("Counter 2          : %d observations\n", nrow(c2)))


# =============================================================================
# 2. DESCRIPTIVE STATISTICS
# =============================================================================
summarise_counter <- function(d, label) {
  iat   <- na.omit(d$iat_sec)
  st    <- na.omit(d$service_time)
  cat(sprintf("\n--- %s ---\n", label))
  cat(sprintf("  N                         : %d\n",  nrow(d)))
  cat(sprintf("  Mean interarrival time    : %.1f s (%.2f min)\n",
              mean(iat), mean(iat)/60))
  cat(sprintf("  Mean service time         : %.1f s (%.2f min)\n",
              mean(st), mean(st)/60))
  cat(sprintf("  SD interarrival time      : %.1f s\n", sd(iat)))
  cat(sprintf("  SD service time           : %.1f s\n", sd(st)))
  cat(sprintf("  Min / Max service time    : %.0f s / %.0f s\n",
              min(st), max(st)))
}

cat("\n=== DESCRIPTIVE STATISTICS ===")
summarise_counter(c1, "Counter 1")
summarise_counter(c2, "Counter 2")


# =============================================================================
# 3. PARAMETER ESTIMATION
# =============================================================================
compute_params <- function(d) {
  iat <- na.omit(d$iat_sec)
  st  <- na.omit(d$service_time)
  lambda <- 1 / mean(iat) * 3600          # customers per hour
  mu     <- 1 / mean(st)  * 3600          # customers per hour
  list(lambda = lambda, mu = mu,
       mean_iat = mean(iat), mean_st = mean(st))
}

p1 <- compute_params(c1)
p2 <- compute_params(c2)

cat("\n=== TABLE: PARAMETER ESTIMATES ===\n")
params_df <- data.frame(
  Metric            = c("Mean IAT (s)", "Mean Service Time (s)",
                        "Arrival Rate λ (cust/hr)", "Service Rate μ (cust/hr)"),
  Counter_1         = round(c(p1$mean_iat, p1$mean_st, p1$lambda, p1$mu), 2),
  Counter_2         = round(c(p2$mean_iat, p2$mean_st, p2$lambda, p2$mu), 2)
)
print(params_df, row.names = FALSE)


# =============================================================================
# 4. DISTRIBUTION TESTING — Exponential (KS test)
# =============================================================================
ks_exp <- function(x, label) {
  x   <- na.omit(x)
  fit <- fitdist(x, "exp")
  ks  <- ks.test(x, "pexp", rate = fit$estimate["rate"])
  cat(sprintf("  %-40s: D = %.4f, p = %.4f  → %s\n",
              label, ks$statistic, ks$p.value,
              ifelse(ks$p.value > 0.05,
                     "Fail to Reject H0 (Exponential fits)",
                     "Reject H0")))
  invisible(list(fit = fit, ks = ks))
}

cat("\n=== EXPONENTIAL DISTRIBUTION FIT (KS Test, α=0.05) ===\n")
cat("H0: data follows Exponential distribution\n\n")
fit_iat1 <- ks_exp(c1$iat_sec,      "Counter 1 — Interarrival Time")
fit_st1  <- ks_exp(c1$service_time, "Counter 1 — Service Time")
fit_iat2 <- ks_exp(c2$iat_sec,      "Counter 2 — Interarrival Time")
fit_st2  <- ks_exp(c2$service_time, "Counter 2 — Service Time")


# =============================================================================
# 5. POISSON ARRIVAL CHECK (variance ≈ mean for Poisson counts)
# =============================================================================
cat("\n=== POISSON ARRIVAL CHECK ===\n")
poisson_check <- function(d, label) {
  iat <- na.omit(d$iat_sec)
  # Group arrivals into 10-minute bins and count
  d2 <- d %>%
    mutate(bin = floor(arr_sec / 600)) %>%
    group_by(bin) %>%
    summarise(count = n(), .groups = "drop")
  v <- var(d2$count); m <- mean(d2$count)
  cat(sprintf("  %s: Mean arrivals/10min = %.2f, Variance = %.2f, VMR = %.3f %s\n",
              label, m, v, v/m,
              ifelse(abs(v/m - 1) < 0.5, "(≈1, Poisson plausible)", "(check manually)")))
}
poisson_check(c1, "Counter 1")
poisson_check(c2, "Counter 2")


# =============================================================================
# 6. M/M/1 PERFORMANCE METRICS
# =============================================================================
mm1 <- function(lambda, mu) {
  rho <- lambda / mu
  P0  <- 1 - rho
  Lq  <- rho^2 / (1 - rho)
  L   <- rho / (1 - rho)
  Wq  <- Lq / lambda         # hours
  W   <- L  / lambda         # hours
  list(rho = rho, P0 = P0, Lq = Lq, L = L,
       Wq_hr = Wq, W_hr = W,
       Wq_min = Wq * 60, W_min = W * 60)
}

m1 <- mm1(p1$lambda, p1$mu)
m2 <- mm1(p2$lambda, p2$mu)

cat("\n=== TABLE: M/M/1 PERFORMANCE METRICS ===\n")
metrics_df <- data.frame(
  Metric     = c("Utilization ρ", "P(system idle) P0",
                 "Avg queue length Lq (customers)",
                 "Avg number in system L (customers)",
                 "Avg wait in queue Wq (hours)",
                 "Avg time in system W (hours)",
                 "Avg wait in queue Wq (minutes)",
                 "Avg time in system W (minutes)"),
  Counter_1  = round(c(m1$rho, m1$P0, m1$Lq, m1$L,
                       m1$Wq_hr, m1$W_hr, m1$Wq_min, m1$W_min), 4),
  Counter_2  = round(c(m2$rho, m2$P0, m2$Lq, m2$L,
                       m2$Wq_hr, m2$W_hr, m2$Wq_min, m2$W_min), 4)
)
print(metrics_df, row.names = FALSE)

cat(sprintf("\nSteady-state check: ρ1 = %.3f < 1 → %s\n",
            m1$rho, ifelse(m1$rho < 1, "STABLE ✓", "UNSTABLE ✗")))
cat(sprintf("Steady-state check: ρ2 = %.3f < 1 → %s\n",
            m2$rho, ifelse(m2$rho < 1, "STABLE ✓", "UNSTABLE ✗")))


# =============================================================================
# 7. VISUALIZATIONS
# =============================================================================

# Helper: histogram + exponential overlay
plot_hist_exp <- function(x, rate, title, xlab, fill_col) {
  x  <- na.omit(x)
  df_plot <- data.frame(x = x)
  ggplot(df_plot, aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)), bins = 12,
                   fill = fill_col, color = "white", alpha = 0.85) +
    stat_function(fun = dexp, args = list(rate = rate),
                  color = "#e07b54", linewidth = 1.2, linetype = "dashed") +
    labs(title = title, x = xlab, y = "Density",
         subtitle = sprintf("Fitted Exp(rate = %.4f/s | mean = %.1f s)",
                            rate, 1/rate)) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
}

p_iat1 <- plot_hist_exp(c1$iat_sec,
                        fit_iat1$fit$estimate["rate"],
                        "Interarrival Time — Counter 1",
                        "Seconds", "#4f98a3")
p_st1  <- plot_hist_exp(c1$service_time,
                        fit_st1$fit$estimate["rate"],
                        "Service Time — Counter 1",
                        "Seconds", "#4f98a3")
p_iat2 <- plot_hist_exp(c2$iat_sec,
                        fit_iat2$fit$estimate["rate"],
                        "Interarrival Time — Counter 2",
                        "Seconds", "#7a39bb")
p_st2  <- plot_hist_exp(c2$service_time,
                        fit_st2$fit$estimate["rate"],
                        "Service Time — Counter 2",
                        "Seconds", "#7a39bb")

g_hist <- arrangeGrob(p_iat1, p_st1, p_iat2, p_st2, ncol = 2)
ggsave("output/01_histograms_exp_fit.png", g_hist,
       width = 12, height = 9, dpi = 150)
cat("\nSaved: output/01_histograms_exp_fit.png\n")

# ECDF vs theoretical exponential
plot_ecdf <- function(x, rate, title, color) {
  x <- na.omit(x)
  df_plot <- data.frame(x = sort(x),
                        empirical = seq_along(x) / length(x),
                        theoretical = pexp(sort(x), rate = rate))
  ggplot(df_plot, aes(x = x)) +
    geom_step(aes(y = empirical, color = "Empirical ECDF"), linewidth = 1) +
    geom_line(aes(y = theoretical, color = "Theoretical Exp"), linewidth = 1,
              linetype = "dashed") +
    scale_color_manual(values = c("Empirical ECDF" = color,
                                  "Theoretical Exp" = "#e07b54"),
                       name = NULL) +
    labs(title = title, x = "Seconds", y = "Cumulative Probability") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "bottom")
}

p_ecdf1_iat <- plot_ecdf(c1$iat_sec, fit_iat1$fit$estimate["rate"],
                         "ECDF: Interarrival Time — Counter 1", "#4f98a3")
p_ecdf1_st  <- plot_ecdf(c1$service_time, fit_st1$fit$estimate["rate"],
                         "ECDF: Service Time — Counter 1", "#4f98a3")
p_ecdf2_iat <- plot_ecdf(c2$iat_sec, fit_iat2$fit$estimate["rate"],
                         "ECDF: Interarrival Time — Counter 2", "#7a39bb")
p_ecdf2_st  <- plot_ecdf(c2$service_time, fit_st2$fit$estimate["rate"],
                         "ECDF: Service Time — Counter 2", "#7a39bb")

g_ecdf <- arrangeGrob(p_ecdf1_iat, p_ecdf1_st,
                      p_ecdf2_iat, p_ecdf2_st, ncol = 2)
ggsave("output/02_ecdf_exp_fit.png", g_ecdf,
       width = 12, height = 9, dpi = 150)
cat("Saved: output/02_ecdf_exp_fit.png\n")

# M/M/1 performance bar chart
perf_long <- metrics_df %>%
  filter(Metric %in% c("Utilization ρ", "P(system idle) P0",
                       "Avg wait in queue Wq (minutes)",
                       "Avg time in system W (minutes)",
                       "Avg queue length Lq (customers)",
                       "Avg number in system L (customers)")) %>%
  pivot_longer(cols = c(Counter_1, Counter_2),
               names_to = "Counter", values_to = "Value")

p_perf <- ggplot(perf_long, aes(x = Metric, y = Value, fill = Counter)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("Counter_1" = "#4f98a3",
                               "Counter_2" = "#7a39bb"),
                    labels = c("Counter 1", "Counter 2")) +
  labs(title = "M/M/1 Performance Metrics — Both Counters",
       subtitle = "McDonald's Mulyosari Self-Service Kiosks",
       x = NULL, y = "Value", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "top")

ggsave("output/03_mm1_performance.png", p_perf,
       width = 10, height = 6, dpi = 150)
cat("Saved: output/03_mm1_performance.png\n")

# Arrival timeline
df_time <- df %>%
  mutate(hour = floor(arr_sec / 3600),
         Counter = paste0("Counter ", Counter)) %>%
  group_by(hour, Counter) %>%
  summarise(arrivals = n(), .groups = "drop")

p_time <- ggplot(df_time, aes(x = hour, y = arrivals, fill = Counter)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Counter 1" = "#4f98a3",
                               "Counter 2" = "#7a39bb")) +
  scale_x_continuous(breaks = 13:18,
                     labels = paste0(13:18, ":00")) +
  labs(title = "Customer Arrivals by Hour",
       subtitle = "McDonald's Mulyosari | Observation period",
       x = "Hour of Day", y = "Number of Arrivals", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "top")

ggsave("output/04_arrivals_by_hour.png", p_time,
       width = 9, height = 5, dpi = 150)
cat("Saved: output/04_arrivals_by_hour.png\n")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Outputs saved to ./output/\n")
cat("  01_histograms_exp_fit.png   — histograms with exponential curve overlay\n")
cat("  02_ecdf_exp_fit.png         — ECDF vs theoretical exponential\n")
cat("  03_mm1_performance.png      — M/M/1 performance metrics comparison\n")
cat("  04_arrivals_by_hour.png     — arrival patterns by hour\n")