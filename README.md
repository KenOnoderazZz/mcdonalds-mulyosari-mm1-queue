# M/M/1 Queueing Analysis — McDonald's Mulyosari Self-Service Kiosks

> Applying **M/M/1 queueing theory** to evaluate the performance of two independent self-service kiosks at McDonald's Mulyosari, Surabaya — testing whether inter-arrival and service times follow exponential distributions, and computing steady-state performance metrics from 71 real observations.

***

## Key Results

| Metric | Counter 1 | Counter 2 |
|---|---|---|
| Arrival Rate λ | 7.58 customers/hr | 6.17 customers/hr |
| Service Rate μ | 22.80 customers/hr | 21.36 customers/hr |
| Utilization ρ | **0.33** | **0.29** |
| P(system idle) P₀ | 0.67 | 0.71 |
| Avg wait in queue Wq | 0.02 hr (~1.2 min) | 0.02 hr (~1.2 min) |
| Avg time in system W | 0.07 hr (~4.2 min) | 0.07 hr (~4.2 min) |
| Avg queue length Lq | 0.17 customers | 0.12 customers |
| Avg customers in system L | 0.50 | 0.41 |

Both counters are **stable** (ρ < 1). Inter-arrival and service times pass the Kolmogorov-Smirnov test for exponential distribution at α = 0.05, validating the M/M/1 model assumption.

***

## Project Overview

Data was collected through direct observation at McDonald's Mulyosari branch (Jl. Mulyosari, Surabaya) during weekends and national holidays around the Idul Adha period — a low-traffic window due to the *pulang kampung* tradition. A total of **71 cashless kiosk transactions** were recorded across two independent self-service counters.

Each transaction captured:
- Arrival time
- System start time (kiosk interaction begins)
- System end time (payment complete)
- Pickup time (food collected)

From these, service time, waiting time, interarrival time, and pickup delay were derived.

***

## Methodology

```
Raw Data (Excel) → Parse timestamps → Derive service/interarrival times
      ↓
Exponential distribution fitting (MLE via fitdistrplus)
      ↓
KS goodness-of-fit test (H0: Exponential distribution)
      ↓
Poisson arrival check (variance-to-mean ratio of arrival counts)
      ↓
M/M/1 steady-state formulas → λ, μ, ρ, P0, Lq, L, Wq, W
      ↓
4 output plots saved to ./output/
```

***

## M/M/1 Formulas Used

| Metric | Formula |
|---|---|
| Utilization | ρ = λ/μ |
| P(idle) | P₀ = 1 − ρ |
| Avg queue length | Lq = ρ² / (1 − ρ) |
| Avg in system | L = ρ / (1 − ρ) |
| Avg wait in queue | Wq = Lq / λ |
| Avg time in system | W = L / λ = 1 / (μ − λ) |

Assumes: Poisson arrivals, Exponential service times, FCFS discipline, single server per kiosk, unlimited queue capacity.

***

## How to Run

```r
# 1. Place Cleaned-Data-Research-Operation.xlsx in your working directory
# 2. Open mm1_queue_analysis.R in RStudio
# 3. Session > Set Working Directory > To Source File Location
# 4. Run All (Ctrl+Shift+Enter)
```

### Required Packages

```r
install.packages(c("readxl", "dplyr", "ggplot2", "tidyr",
                   "MASS", "fitdistrplus", "gridExtra", "scales", "viridis"))
```

Packages auto-install on first run. Plots are saved to `./output/`.

***

## Output Files

| File | Description |
|---|---|
| `01_histograms_exp_fit.png` | Histograms with fitted exponential overlay |
| `02_ecdf_exp_fit.png` | Empirical vs theoretical ECDF |
| `03_mm1_performance.png` | Performance metrics bar chart |
| `04_arrivals_by_hour.png` | Arrivals by hour of day |

***

## File Structure

```
mcdonalds-mulyosari-mm1-queue/
├── README.md
├── mm1_queue_analysis.R
├── Cleaned-Data-Research-Operation.xlsx
└── report/
    └── Group-1-Research-Operation-Final-Project_Report.pdf
```

***

## Project Info

- **Course:** Operations Research — Institut Teknologi Sepuluh Nopember (ITS)
- **Department:** Statistics, Faculty of Science and Analytical Data
- **Observation Site:** McDonald's Mulyosari, Jl. Mulyosari, Surabaya
- **Data Collection:** Weekends & Idul Adha holiday period, 2025
- **Tools:** R · readxl · fitdistrplus · ggplot2
