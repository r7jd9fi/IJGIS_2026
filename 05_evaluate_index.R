#-------------------------------------------------------------------------------
source("01_loading.R")

load("data/final_results_random.Rdata")

original_scale <- final_results[, .N/100, by = county]
#-------------------------------------------------------------------------------


n_traj <- 100L

I2 <- function(
  total_areas,
  county_trajectories,
  n_traj = 100L,
  cut = NULL
  ) {
  x <- rep(1:total_areas, n_traj)
  data <- data.table(x = x, V1 = county_trajectories)
  data <- drop_na(data)
  data_M <- data[, .(m = min(V1), M = max(V1)), by = x]

  if (!is.null(cut)) {
    data_M <- data_M[!(x %in% c(1:cut))]
    data <- data[!(x %in% c(1:cut))]
  }

  data$x <- data$x + 1
  data_with_M <- left_join(data, data_M)
  differences <- data_with_M[
    ,
    .(diff_m = abs(V1 - m),
    diff_M = abs(V1 - M))
  ]
  max_differences <- apply(drop_na(differences), 1, max)

  return(
    mean(max_differences)
  )
}

I1 <- function(
  total_areas,
  county_trajectories,
  statistic,
  n_traj = 100L,
  cut = NULL
  ) {
  
  x <- rep(1:total_areas, n_traj)
  data <- data.table(x = x, V1 = county_trajectories)
  data <- drop_na(data)
  # M <- data[, max(V1), by = x]$V1
  # m <- data[, min(V1), by = x]$V1
  M <- data[, quantile(V1, 1 - 0.05), by = x]$V1
  m <- data[, quantile(V1, 0.05), by = x]$V1

  if (!is.null(cut)) {
    M <- M[-c(1:cut)]
    m <- m[-c(1:cut)]
  }

  if (statistic == "r2" | statistic == "gini") {
    denominator <- 1
  }

  if (startsWith(statistic, "cor")) {
    denominator <- 2
  }

  if (startsWith(statistic, "beta") | 
  startsWith(statistic, "var") | 
  startsWith(statistic, "mean")) {
    denominator <- max(M) - min(m)
  }

  mean((M - m) / denominator)
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
is <- vector("list", length(unique(final_results$county)))

p <- ncol(final_results) - 2
stat_names <- sub("\\..*$", "", colnames(final_results)[1:p])
idx <- 1
for (.county in unique(final_results$county)) {
  for (k in 1:p) {
    statistic <- stat_names[k]
    colname <- colnames(final_results)[k]
    if (statistic == "beta" | statistic == "var" | statistic == "mean") {
      cut <- 10
    } else {
      cut <- NULL
    }

    state <- unique(final_results[county == .county, state])
    total_areas <- original_scale[county == .county, V1]
    county_trajectories <- unlist(
      final_results[county == .county, ..colname]
    )
    
    i1 <- I1(
      county_trajectories = county_trajectories,
      total_areas = total_areas,
      statistic = statistic,
      cut = cut
    )
    i2 <- I2(
      total_areas = total_areas,
      county_trajectories = county_trajectories,
      cut = cut
    )

    is[[idx]] <- data.frame(
      i1 = i1,
      i2 = i2,
      county = .county,
      state = state,
      colname = colname
    )
    idx <- idx + 1
  }
}

da <- do.call(rbind, is)

saveRDS(da, "data/index_random_quantile_90.rds")
