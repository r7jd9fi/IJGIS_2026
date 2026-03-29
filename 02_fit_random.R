source("01_loading.R")

parser <- ArgumentParser()
parser$add_argument("--n_threads", type = "integer", default = 1L)
parser$add_argument("--n_sample", type = "integer", default = 100L)
parser$add_argument("--n_counties", type = "integer", default = 783L)
parser$add_argument("--verbose", type = "logical", default = FALSE)

args <- parser$parse_args()

formula <- earning ~ race + time_travel + rooms

n_sample <- args$n_sample
n_counties <- args$n_counties
n_threads <- args$n_threads
verbose <- FALSE

set.seed(2024)
counties <- unique(data_corr$County)
un <- sample(counties, size = n_counties)
sim_results <- list()

cluster <- makeCluster(n_threads)
registerDoSNOW(cluster)

message(sprintf("Starting evaluating simulation with RANDOM model. Using %d threads, generating %d samples for each county, running for %d counties.\n",
n_threads, n_sample, n_counties))

idx <- 1
for (k in 1:n_counties) {
	message(paste(k, "(k) out of", n_counties))
	dc <- data_corr[data_corr$County == un[k], ]
	dc <- drop_na(dc)
	if (nrow(dc) < 20) {
		next
	}
	dc <- dc[!st_is_empty(dc), ]
	if (nrow(dc) < 20) {
		next
	}
	dc_nb <- poly2nb(dc)
	search <- n.comp.nb(dc_nb)
	idmax <- as.numeric(names(which.max(table(search$comp.id))))
	dc <- dc[search$comp.id == idmax, ]
	if (nrow(dc) < 20) {
		next
	}
	data <- dc[, c("earning", "race", "estimate", "time_travel", "rooms")]
	data <- st_drop_geometry(data)
	data <- as.data.frame(scale(data))
	geometry <- st_geometry(dc)
	sim_results[[idx]] <- foreach(
		i = 1:n_sample,
		.packages = c(
			"sf", "spdep",
			"cubature",
			"numDeriv",
			"tidyverse"
		), .verbose = verbose
	) %dopar% {
		trajectory <- eval_maup_skater(
			model = "random",
			data = data,
			geometry = geometry,
			stat_function = eval_stats,
			formula = formula,
			regression_vars = c("earning", "race", "time_travel", "rooms"),
			cor_vars = c("earning", "race", "time_travel", "rooms"),
			mean_vars = c("earning", "race", "time_travel", "rooms"),
			var_vars = c("earning", "race", "time_travel", "rooms")
		)
		r2 <- sapply(trajectory$stat, function(x) x$r2)
		beta <- t(sapply(trajectory$stat, function(x) x$beta))
		mean_values <- t(sapply(trajectory$stat, function(x) x$mean))
		var_values <- t(sapply(trajectory$stat, function(x) x$var))
		cor_values <- t(sapply(trajectory$stat, function(x) x$cor))

		results <- data.frame(
			r2 = r2,
			beta = beta,
			mean = mean_values,
			var = var_values,
			cor = cor_values
		)

		ret <- list(
			county = un[k],
			state = unique(dc$State),
			results = results
		)

		return(ret)
	}
	idx <- idx + 1
}

save(sim_results, file = "data/results_random.Rdata")