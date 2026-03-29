#-------------------------------------------------------------------------------
source("01_loading.R")
load("data/final_results_adv.Rdata")
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
da_adv <- do.call(rbind, lapply(final_adv_results, function(x) {
  i1 <- mean(abs(x$M - x$m)) / 2
  df <- data.frame(
    county = x$County[1],
    state = x$State[1],
    colname = rename_var(paste0(x$Var1[1], ".", x$Var2[1])),
    I1_ADV = i1
  )
}))

head(da_adv)

da <- readRDS("data/index_random_quantile_90.rds")

da_adv <- left_join(da_adv, da)

saveRDS(da_adv, "data/index_adv.rds")
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
da_adv_with_m_M <- do.call(rbind, lapply(final_adv_results, function(x) {
  df <- data.frame(
    county = x$County[1],
    state = x$State[1],
    colname = rename_var(paste0(x$Var1[1], ".", x$Var2[1])),
    m = x$m,
    M = x$M
  )
}))

da <- readRDS("data/index_random_quantile_90.rds")

da_adv_with_m_M <- left_join(da_adv_with_m_M, da)

saveRDS(da_adv_with_m_M, "data/index_adv_with_m_M.rds")
#-------------------------------------------------------------------------------