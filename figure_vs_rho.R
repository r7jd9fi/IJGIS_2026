##------------------------------------------------------------------------------
require(tidyverse)
require(ggplot2)
require(data.table)
require(cubature)
require(reshape2)
require(spdep)
require(magick)
require(latex2exp)

source("00_functions.R")
load("data/final_results_random.Rdata")
index_random <- readRDS("data/index_random_quantile_90.rds")
original_scale <- final_results[, .N/100, by = county]
#------------------------------------------------------------------------------

cols <- colnames(final_results)[1:(ncol(final_results)- 2)]
tau <- data.table(
  matrix(0, nr = length(unique(final_results$county)), nc = ncol(final_results))
)
colnames(tau) <- colnames(final_results)
tau$county <- as.character(tau$county)
tau$state <- as.character(tau$state)

k <- 1
for (i in unique(final_results$county)) {
  da <- final_results[county == i, ]
  tau[k, ] <- da[nrow(da), ]
  k <- k + 1
}

tau_melt <- melt(tau)
tau_melt$variable <- as.character(tau_melt$variable)
colnames(tau_melt) <- c("county", "state", "colname", "tau")

index_random_with_tau <- left_join(tau_melt, index_random, by = c("state", "county", "colname"))

head(index_random_with_tau)

a <- subset(
  index_random_with_tau, 
  !(
    startsWith(colname, "var") |
      startsWith(colname, "mean") |
      startsWith(colname, "beta")
  )
)

a[startsWith(a$colname, "cor"), ]$colname <- "cor"

label_map <- c(
  "cor" = "Correlation",
  "gini" = "Gini*' '*index",
  "r2" = "R^2"
)

a <- a %>%
  mutate(colname = recode(colname, !!!label_map))

ggplot(a, aes(x = tau, y = i1)) +
  geom_point(size = 0.1) +
  geom_smooth(se = FALSE, method = "loess", linewidth = 1.2) +
  facet_wrap(~colname, labeller = label_parsed, scales = "free") +
  ylab(TeX("$I_m$")) +
  xlab("Statistic value") +
  theme_bw(base_size = 16)
ggsave("figures/I_vs_rho.jpg", width = 8, height = 3, units = "in")

##------------------------------------------------------------------------------
