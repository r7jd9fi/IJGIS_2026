N_THREADS ?= 1
N_SAMPLE ?= 100
N_COUNTIES ?= 5

all: data/index_random_quantile_90.rds data/index_adv.rds

.PHONY: random advs
random: data/index_random_quantile_90.rds
advs: data/index_adv.rds data/index_adv_with_m_M.rds

.PHONY: extract
extract:
	@echo "Extracting results from simulated data."
	Rscript 04_extract_results.R
	Rscript 04_extract_results_gini.R
	Rscript 04_extract_results_adv.R
	@echo "Evaluating indices."
	Rscript 05_evaluate_index.R
	Rscript 05_evaluate_index_adv.R
	@echo "Generating figures."
	Rscript 06_graphs.R
	Rscript figure_typical.R
	Rscript figure_vs_rho.R

.PHONY: fit_random fit_random_gini
fit_random: 02_fit_random.R
	@echo "Fitting the random model."
	Rscript 02_fit_random.R --n_threads $(N_THREADS) --n_sample $(N_SAMPLE) \
	--n_counties $(N_COUNTIES)

fit_random_gini: 02_fit_random_gini.R
	@echo "Fitting the random model for gini index."
	Rscript 02_fit_random_gini.R --n_threads $(N_THREADS) --n_sample $(N_SAMPLE) \
	--n_counties $(N_COUNTIES)

# Paper/review default: if these exist, don't rebuild just because scripts changed.
data/results_random.Rdata: | 02_fit_random.R
	@echo "Fitting the random model."
	Rscript 02_fit_random.R --n_threads $(N_THREADS) --n_sample $(N_SAMPLE) \
	--n_counties $(N_COUNTIES)

data/results_random_gini.Rdata: | 02_fit_random_gini.R
	@echo "Fitting the random model for gini index."
	Rscript 02_fit_random_gini.R --n_threads $(N_THREADS) --n_sample $(N_SAMPLE) \
	--n_counties $(N_COUNTIES)

.PHONY: fit_adv
fit_adv: 03_fit_adv.R
	@echo "Fitting adversarial model (this can take a long time)."
	Rscript 03_fit_adv.R --n_threads $(N_THREADS)

# For paper/review builds we ship data/results_adv.Rdata precomputed.
# Don't re-run the expensive fit just because the script timestamp changed.
data/results_adv.Rdata:
	@test -f $@ || (echo "Missing $@. Provide the precomputed file or run 'make fit_adv' to generate it."; exit 1)

data/final_results_random.Rdata: data/results_random.Rdata data/results_random_gini.Rdata
	@echo "Extracting the results from the simulated data."
	Rscript 04_extract_results.R
	Rscript 04_extract_results_gini.R

data/final_results_adv.Rdata: data/results_adv.Rdata
	@echo "Extracting the results from the simulated data (ADV)."
	Rscript 04_extract_results_adv.R

data/index_random_quantile_90.rds: data/final_results_random.Rdata
	@echo "Evaluating index for random scenario"
	Rscript 05_evaluate_index.R

data/index_adv.rds: data/final_results_adv.Rdata data/index_random_quantile_90.rds
	@echo "Evaluating index for adversarial scenario"
	Rscript 05_evaluate_index_adv.R

# Produced by 05_evaluate_index_adv.R in the same run as data/index_adv.rds.
data/index_adv_with_m_M.rds: data/index_adv.rds
	@test -f $@ || (echo "Missing $@ (should be produced by 05_evaluate_index_adv.R)."; exit 1)
