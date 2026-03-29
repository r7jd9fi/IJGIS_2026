#-------------------------------------------------------------------------------
source("01_loading.R")

da <- readRDS("data/index_random_quantile_90.rds")
da_adv <- readRDS("data/index_adv.rds")

load("data/final_results_random.Rdata")

original_scale <- final_results[, .N/100, by = county]
#-------------------------------------------------------------------------------

da <- left_join(da, original_scale)
da_adv <- left_join(da_adv, original_scale)
da_adv <- da_adv[, -5]
da_adv <- left_join(da_adv, da[, c("i1", "county", "colname")], by = c("county", "colname"))

da_adv <- left_join(da_adv, da[, c("county", "colname")], by = c("county", "colname"))

da$colname <- factor(
  da$colname,
  levels = unique(da$colname)
)

require(ggplot2)

ggplot(da) +
  geom_boxplot(aes(x = colname, y = i1)) +
  facet_wrap(~state, nrow = 5) +
  theme_bw(base_size = 17) +
  scale_x_discrete(labels = c(
    "beta.1" = expression(beta[" 0"]),
    "beta.2" = expression(beta[" BP"]),
    "beta.3" = expression(beta[" T"]),
    "beta.4" = expression(beta[" RO"]),
    "cor.earning.race" = expression(rho[" E,BP"]),
    "cor.earning.rooms" = expression(rho[" E,RO"]),
    "cor.earning.time_travel" = expression(rho[" E,TI"]),
    "cor.race.rooms" = expression(rho[" BP,RO"]),
    "cor.race.time_travel" = expression(rho[" BP,T"]),
    "cor.time_travel.rooms" = expression(rho[" T,RO"]),
    "gini" = "GI",
    "mean.earning" = expression(mu[" E"]),
    "mean.race" = expression(mu[" BP"]),
    "mean.rooms" = expression(mu[" RO"]),
    "mean.time_travel" = expression(mu[" T"]),
    "r2" = expression(R^2),
    "var.earning" = expression(S["E"]),
    "var.race" = expression(S["BP"]),
    "var.rooms" = expression(S["RO"]),
    "var.time_travel" = expression(S["T"])
  )) +
  xlab("Statistic") +
  ylab(expression(I["m"])) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggsave("figures/Iz_boxplot.jpg", width = 12, height = 12, units = "in")


ggplot(da) +
  geom_boxplot(aes(x = colname, y = i2)) +
  facet_wrap(~state, nrow = 5) +
  theme_bw(base_size = 18) +
  theme(axis.text.x = element_text(size = 18, angle = 90, hjust = 1, vjust = 0.5)) +
  scale_x_discrete(labels = c(
    "beta.1" = expression(beta[0]),
    "beta.2" = expression(beta[1]),
    "beta.3" = expression(beta[2]),
    "beta.4" = expression(beta[3]),
    "cor.earning.race" = expression(rho["E,BP"]),
    "cor.earning.rooms" = expression(rho["E,RO"]),
    "cor.earning.time_travel" = expression(rho["E,TI"]),
    "cor.race.rooms" = expression(rho["BP,RO"]),
    "cor.race.time_travel" = expression(rho["BP,T"]),
    "cor.time_travel.rooms" = expression(rho["T,RO"]),
    "gini" = "GI",
    "mean.earning" = expression(mu["E"]),
    "mean.race" = expression(mu["BP"]),
    "mean.rooms" = expression(mu["RO"]),
    "mean.time_travel" = expression(mu["T"]),
    "r2" = expression(R^2),
    "var.earning" = expression(S["E"]),
    "var.race" = expression(S["BP"]),
    "var.rooms" = expression(S["RO"]),
    "var.time_travel" = expression(S["T"])
  )) +
  xlab("Statistic") +
  ylab(expression(I[2]))

label_map <- c(
  "beta.1" = "beta*' '*phantom()[0]",
  "beta.2" = "beta*' '*phantom()[BP]",
  "beta.3" = "beta*' '*phantom()[T]",
  "beta.4" = "beta*' '*phantom()[RO]",
  "cor.earning.race" = "rho*' '*phantom()['E, BP']",
  "cor.earning.rooms" = "rho*' '*phantom()['E, RO']",
  "cor.earning.time_travel" = "rho*' '*phantom()['E, T']",
  "cor.race.rooms" = "rho*' '*phantom()['BP, RO']",
  "cor.race.time_travel" = "rho*' '*phantom()['BP, T']",
  "cor.time_travel.rooms" = "rho*' '*phantom()['T, RO']",
  "gini" = "GI",
  "mean.earning" = "mu*' '*phantom()[E]",  # Add space after mu
  "mean.race" = "mu*' '*phantom()[BP]",   # Add space after mu
  "mean.rooms" = "mu*' '*phantom()[RO]",  # Add space after mu
  "mean.time_travel" = "mu*' '*phantom()[T]",  # Add space after mu
  "r2" = "R^2",
  "var.earning" = "S[E]",
  "var.race" = "S[BP]",
  "var.rooms" = "S[RO]",
  "var.time_travel" = "S[T]"
)

da <- da %>%
  mutate(colname = recode(colname, !!!label_map))


ggplot(da) +
  geom_point(aes(x = i1, y = i2)) +
  facet_wrap(~colname, labeller = label_parsed) + # Use label_parsed to interpret expressions
  theme_bw(base_size = 22) +
  xlab(expression(I["z"])) +
  ylab(expression(I["s"]))

ggsave("figures/I1_vs_I2.jpg", width = 10, height = 9, units = "in", dpi = 100)

cor(da$i1, da$i2)

## TO-DO

## 1. Boxplot together is for both methods

da_adv_melt <- reshape2::melt(da_adv, measure.vars = c("I1_ADV", "i1"))
da_adv_melt$colname <- factor(da_adv_melt$colname)

label_map <- c(
  "cor.earning.race" = "rho*' '*phantom()['E, BP']",
  "cor.earning.rooms" = "rho*' '*phantom()['E, RO']",
  "cor.earning.time_travel" = "rho*' '*phantom()['E, T']",
  "cor.race.rooms" = "rho*' '*phantom()['BP, RO']",
  "cor.race.time_travel" = "rho*' '*phantom()['BP, T']",
  "cor.time_travel.rooms" = "rho*' '*phantom()['T, RO']"
)

da_adv_melt <- da_adv_melt %>%
  mutate(colname = recode(colname, !!!label_map))

ggplot(da_adv_melt) +
  geom_boxplot(aes(x = variable, y = value)) +
  facet_wrap(~colname, labeller = label_parsed) + # Use label_parsed to interpret expressions
  theme_bw(base_size = 6) +
  scale_x_discrete(labels = c(
    "i1" = expression(I["m"]),
    "I1_ADV" = expression(I["z"])
  )) +
  xlab("Metric") +
  ylab("Index")
ggsave("figures/boxplot_I1_vs_I1.jpg", width = 3, height = 2, units = "in")


da_adv <- da_adv %>%
  mutate(colname = recode(colname, !!!label_map))

ggplot(da_adv) +
  geom_point(aes(x = I1_ADV, y = i1)) +
  # geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~colname, labeller = label_parsed) +
  theme_bw(base_size = 18) +
  xlab(expression(I["z"])) +
  ylab(expression(I["m"]))
ggsave("figures/I1_vs_I1.jpg", width = 8, height = 5, units = "in", dpi = 100)

cor(da_adv$I1_ADV, da_adv$i1)

idx <- da_adv$I1_ADV < da_adv$i1

da_adv[idx, ]

## 2. ? If we make a plot of Iz versus the absolute number of areas, I think there will be a negative trend

ggplot(da) +
  geom_point(aes(x = log(V1), y = i1)) +
  facet_wrap(~ colname)

## 3. Recreate all graphs in the paper for all measureaments.
## 4. When decide which graph we will include in the paper, update makefile



#-------------------------------------------------------------------------------
get_area <- function(dc) {
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
  area <- median(st_area(dc))

  pop <- sum(dc$estimate)
  area_km <- as.numeric(units::set_units(area, km^2))
  county <- unique(dc$County)
  data.frame(pop = pop, area_km = area_km, county = county)
}

split_data_corr <- split(data_corr, data_corr$County)
area_per_county <- sapply(split_data_corr, get_area)
area_per_county <- do.call(rbind, area_per_county)
head(area_per_county)

db <- left_join(area_per_county, da)
db$pop_density <- db$pop / db$area_km

ggplot(db) +
  geom_point(aes(x = log(area_km), y = i1)) +
  facet_wrap(~colname)

label_map <- c(
  "beta.1" = "beta*' '*phantom()[0]",
  "beta.2" = "beta*' '*phantom()[BP]",
  "beta.3" = "beta*' '*phantom()[T]",
  "beta.4" = "beta*' '*phantom()[RO]",
  "cor.earning.race" = "rho*' '*phantom()['E, BP']",
  "cor.earning.rooms" = "rho*' '*phantom()['E, RO']",
  "cor.earning.time_travel" = "rho*' '*phantom()['E, T']",
  "cor.race.rooms" = "rho*' '*phantom()['BP, RO']",
  "cor.race.time_travel" = "rho*' '*phantom()['BP, T']",
  "cor.time_travel.rooms" = "rho*' '*phantom()['T, RO']",
  "gini" = "GI",
  "mean.earning" = "mu*' '*phantom()[E]",  # Add space after mu
  "mean.race" = "mu*' '*phantom()[BP]",   # Add space after mu
  "mean.rooms" = "mu*' '*phantom()[RO]",  # Add space after mu
  "mean.time_travel" = "mu*' '*phantom()[T]",  # Add space after mu
  "r2" = "R^2",
  "var.earning" = "S[E]",
  "var.race" = "S[BP]",
  "var.rooms" = "S[RO]",
  "var.time_travel" = "S[T]"
)

db <- db %>%
  mutate(colname = recode(colname, !!!label_map))

ggplot(db) +
  geom_point(size = 0.5, aes(x = log(pop_density), y = i1)) +
  facet_wrap(~colname, labeller = label_parsed) + # Use label_parsed to interpret expressions
  theme_bw(base_size = 15) +
  xlab("Population density") +
  ylab(expression(I["m"])) +
  theme(strip.text.x = element_text(size = 15))
ggsave("figures/pop_density.jpg", width = 15, height = 9, units = "in")

ggplot(db) +
  geom_point(aes(x = log(pop_density), y = i2)) +
  facet_wrap(~colname, labeller = label_parsed) + # Use label_parsed to interpret expressions
  theme_bw(base_size = 20) +
  xlab("Population density") +
  ylab(expression(I[2])) +
  theme(strip.text.x = element_text(size = 15))
ggsave("figures/pop_density_i2.jpg", width = 15, height = 9, units = "in")

ggplot(db) +
  geom_point(aes(x = log(V1), y = i1)) +
  facet_wrap(~colname, labeller = label_parsed) + # Use label_parsed to interpret expressions
  theme_bw(base_size = 20) +
  xlab("Population density") +
  ylab(expression(I["m"])) +
  theme(strip.text.x = element_text(size = 15))
ggsave("figures/areas_vs_Im.jpg", width = 15, height = 9, units = "in", dpi = 100)

ggplot(db) +
  geom_point(aes(x = log(pop), y = i1)) +
  facet_wrap(~ colname)

load("data/final_results_random.Rdata")
N <- final_results[, .N, by = county]$N

original_scale_value <- melt(final_results[cumsum(N), ])
colnames(original_scale_value)[3] <- "colname"

db2 <- left_join(original_scale_value, db)

ggplot(db2) +
  geom_point(aes(x = pop, y = value)) +
  facet_wrap(~ colname, scales = "free")

ggplot(db2) +
  geom_point(aes(x = pop, y = V1))
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
db <- left_join(area_per_county, da_adv)
db$pop_density <- db$pop / db$area_km

ggplot(db) +
  geom_point(aes(x = log(area_km), y = i1)) +
  facet_wrap(~ colname)

ggplot(db) +
  geom_point(aes(x = log(pop_density), y = i1)) +
  facet_wrap(~ colname)

ggplot(db) +
  geom_point(aes(x = log(pop), y = i1)) +
  facet_wrap(~ colname)
#-------------------------------------------------------------------------------