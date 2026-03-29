source("01_loading.R")

parser <- ArgumentParser()
parser$add_argument("--n_threads", type = "integer", default = 1L)
args <- parser$parse_args()

n_threads <- args$n_threads

cluster <- makeCluster(n_threads)
registerDoSNOW(cluster)

message(sprintf("Starting evaluating simulation with ADVERSARIAL model. Using %d threads.\n", n_threads))

vars <- t(combn(c("time_travel", "earning", "race", "rooms"), 2))
vars <- as.data.frame(vars)
counties <- unique(data_corr$County)

result <- expand.grid(County = counties, idx = 1:nrow(vars))
final_result <- merge(
    result, vars,
    by.x = "idx", by.y = "row.names"
)
final_result <- final_result[, -1]

colnames(final_result)[2:3] <- c("Var1", "Var2")

table_counties <- as.data.frame(table(data_corr$County))
colnames(table_counties) <- c("County", "Freq")

sim_setup <- left_join(final_result, table_counties)
sim_setup <- sim_setup[order(sim_setup$Freq, decreasing = TRUE), ]

saveRDS(sim_setup, "data/sim_setup.rds")

N <- nrow(sim_setup)
adv_results <- foreach(
	i = 1:N,
	.packages = c("sf", "spdep", "tidyverse"),
	.verbose = TRUE
	) %dopar% {
		idx_county <- data_corr$County == sim_setup$County[i]
		dc <- data_corr[idx_county, ]
		dc <- drop_na(dc)
		if (nrow(dc) < 20) {
			return(NULL)
		}
		dc <- dc[!st_is_empty(dc), ]
		if (nrow(dc) < 20) {
			return(NULL)
		}
		dc_nb <- poly2nb(dc)
		search <- n.comp.nb(dc_nb)
		idmax <- as.numeric(names(which.max(table(search$comp.id))))
		dc <- dc[search$comp.id == idmax, ]
		if (nrow(dc) < 20) {
			return(NULL)
		}
		data <- dc[, c(sim_setup$Var1[i], sim_setup$Var2[i])]
		data <- st_drop_geometry(data)
		data <- as.data.frame(scale(data))
		geometry <- st_geometry(dc)
		skater_adv <- eval_maup_skater(
			model = "adversarial",
			data = data,
			geometry = geometry
		)
		
		results_max <- skater_adv$skater_max$stat_correlation_adversarial
		results_min <- skater_adv$skater_min$stat_correlation_adversarial

		return(list(
			Results = data.frame(
				M = results_max,
				m = results_min
			),
			County = unique(dc$County),
			State = unique(dc$State)
		))
}

save(adv_results, file = "data/results_adv.Rdata")