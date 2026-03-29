require(tidyverse)
require(stringr)
require(data.table)
require(usdata)
require(tidycensus)
require(sf)
require(spdep)
require(ggplot2)
require(ggrepel)
require(gridExtra)
require(dplyr)
require(cubature)
require(numDeriv)
require(foreach)
require(doSNOW)
require(argparse)

source("00_functions.R")

load("data/data_corr.Rdata")


data_corr$earning <- data_corr$earning / data_corr$hu
data_corr$race <- data_corr$race / data_corr$estimate
data_corr$time_travel <- data_corr$time_travel / data_corr$estimate
data_corr$rooms <- data_corr$rooms / data_corr$hu