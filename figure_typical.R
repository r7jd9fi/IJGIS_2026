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
original_scale <- final_results[, .N / 100, by = county]
da_adv_with_m_M <- readRDS("data/index_adv_with_m_M.rds")
##------------------------------------------------------------------------------

##------------------------------------------------------------------------------
                                        # Figure 12
plot_typical_random <- function(
  this_county, var, ylab_name, print_i1 = TRUE, print_i2 = TRUE, plot_adv = FALSE, plot_grey = FALSE, y_min_grey = NULL, y_max_grey = NULL,x_max_grey = 10, plot_rect = FALSE, y_anot = NULL
  ) {
    # county_traj <- do.call(cbind, lapply(dc_sk_random[[idx]], function(x) x$ssw))
    # da <- as.data.table(cbind(x = 1:nrow(county_traj), county_traj))
    da <- final_results[county == this_county, ..var]
    da$x <- rep(1:(nrow(da) / 100), 100)
    da$group <- factor(rep(1:100, each = nrow(da) / 100))
    data_m <- as.data.table(melt(da,
      id.vars = c("x", "group"),
      value.name = "value"
    ))
    maxmin_AUX <- data_m[, .(
      max = max(value),
      min = min(value),
      avg = mean(value)
    ),
    by = x
    ]
    maxmin <- data_m[, .(
      max = quantile(value, 0.975, na.rm = TRUE),
      min = quantile(value, 0.025, na.rm = TRUE),
      avg = mean(value)
    ),
    by = x
    ]
    tau <- as.numeric(da[nrow(da), ..var])
    idx <- index_random$county == this_county & index_random$colname == var

    anot <- TeX(paste("$I_m$ =",
                      format(round(index_random[idx, ]$i1, 2),
                             nsmall = 2)))
    anot2 <- TeX(paste("$I_s$ =",
                       format(round(index_random[idx, ]$i2, 2),
                              nsmall = 2)))
    x_anot <- original_scale[county == this_county, ]$V1 * 0.9
    if (is.null(y_anot)) {
      y_anot <- max(maxmin_AUX$max, na.rm = TRUE) * 0.9
    }
    y_anot_2 <- min(maxmin$min, na.rm = TRUE) * 0.8
    p <- ggplot(data_m)
    if (plot_grey) {
      p <- p + annotate("rect",
        ymin = -Inf, ymax = Inf, xmin = 0, xmax = x_max_grey,
        fill = "grey", alpha = 0.3
      )
    } else {
      y_anot <- y_max_grey * 0.9
    }
    if (plot_rect) {
      if (is.null(y_min_grey)) {
        y_min_grey <- as.numeric(maxmin_AUX[maxmin_AUX$x == x_max_grey, "min"])
        y_max_grey <- as.numeric(maxmin_AUX[maxmin_AUX$x == x_max_grey, "max"])
      }
      
      p <- p + geom_hline(yintercept = y_min_grey, linewidth = 2, linetype = "dotted") + geom_hline(yintercept = y_max_grey, linewidth = 2, linetype = "dotted")
    }
    if (print_i1) {
      p <- p + annotate("text",
        x = x_anot, y = y_anot,
        label = anot, size = 15
      )
    }
    p <- p +
        geom_path(aes(x = x, y = value, group = group),
                  linewidth = 1,
                  alpha = 0.4) +
        geom_line(data = maxmin,
                  mapping = aes(x = x, y = max),
                  color = "red", linewidth = 1.5) +
        geom_line(data = maxmin,
                  mapping = aes(x = x, y = min),
                  color = "red", linewidth = 1.5) +
        geom_hline(yintercept = tau,
                   linewidth = 1.3,
                   color = "blue") +
        ylab(ylab_name) +
        xlab("Number of clusters") +
        ## labs(linetype = "Zoning system") +
        theme_bw(base_size = 40) +
        theme(legend.key.size = unit(3.5, "line"),
              legend.position = "top")
    if (print_i2) {
      p <- p + annotate("text",
        x = x_anot, y = y_anot_2,
        label = anot2, size = 15
      )
    }
    if (plot_adv) {
      aux <- da_adv_with_m_M[da_adv_with_m_M$county == this_county &
        da_adv_with_m_M$colname == var, ]
      aux$x <- 1:nrow(aux)
      aux <- left_join(data_m, aux, by = c("x" = "x"))
      p <- p + geom_line(
        data = aux,
        mapping = aes(x = x, y = m, linetype = "Line for m", color = "Line for m"), # Map linetype and color
        linewidth = 1.7
      ) +
        geom_line(
          data = aux,
          mapping = aes(x = x, y = M, linetype = "Line for M", color = "Line for M"), # Map linetype and color
          linewidth = 1.7
        ) +
        scale_linetype_manual(
          name = "Zoning system", # Title for the legend
          values = c("Line for m" = "solid", "Line for M" = "solid"), # Specify linetypes for each mapped value
          labels = c("Line for m" = "m", "Line for M" = "M") # Labels in the legend for each mapped value
        ) +
        scale_color_manual(
          name = "Zoning system", # Use the same name to merge legends
          values = c("Line for m" = "#009B0D", "Line for M" = "#009B0D"), # Specify colors for each mapped value
          labels = c("Line for m" = "m", "Line for M" = "M") # Use the same labels to merge legends
        ) +
        theme(legend.position = "none")
        #guides(linetype = guide_legend(override.aes = list(color = c("#009B0D", "#009B0D"))))
    } 

    return(p)
}



p1 <- plot_typical_random(
  "DuPage County", "cor.race.time_travel", "Correlation", print_i2 = FALSE,
  plot_grey = FALSE, y_min_grey = -1, y_max_grey = 1, plot_rect = TRUE
) + ylim(-1, 1)

p2 <- plot_typical_random("Wilson County", "gini", "Gini Index", print_i2 = FALSE, y_min_grey = 0, y_max_grey = 1, plot_rect = TRUE) + ylim(0, 1)

p3 <- plot_typical_random("Los Angeles County", "r2", "Coeficient of determination", print_i2 = FALSE, y_min_grey = 0, y_max_grey = 1, plot_rect = TRUE) + ylim(0, 1)

p4 <- plot_typical_random("Travis County", "var.earning", "Variance", print_i2 = FALSE, plot_grey = TRUE, x_max_grey = 10, plot_rect = TRUE)

ggsave(plot = p1, file = "figures/typical_28.jpg",
       bg = "white", width = 15, height = 10)
ggsave(plot = p2, file = "figures/typical_50.jpg",
       bg = "white", width = 15, height = 10)
ggsave(plot = p3, file = "figures/typical_20.jpg",
       bg = "white", width = 15, height = 10)
ggsave(plot = p4, file = "figures/typical_19.jpg",
       bg = "white", width = 15, height = 10)
## ------------------------------------------------------------------------------



p1 <- plot_typical_random(
  "Lenawee County", "cor.race.time_travel", "Correlation",
  print_i2 = FALSE,
  print_i1 = FALSE,
  plot_grey = FALSE,
  y_min_grey = -1,
  y_max_grey = 1,
  plot_rect = TRUE,
  plot_adv = TRUE
) + ylim(-1, 1)

p_oak <- plot_typical_random(
  "Oakland County", "cor.earning.time_travel", "Correlation", print_i1 = FALSE,
  print_i2 = FALSE, plot_adv = TRUE
)
ggsave(plot = p_oak, file = "figures/oakland.jpg",
       bg = "white", width = 12, height = 8)

p_river <- plot_typical_random(
  "Riverside County", "cor.earning.rooms", "Correlation", print_i1 = FALSE,
  print_i2 = FALSE, plot_adv = TRUE
)
ggsave(plot = p_river, file = "figures/riverside.jpg",
       bg = "white", width = 12, height = 8)

p_la <- plot_typical_random(
  "Los Angeles County", "cor.race.rooms", "Correlation", print_i1 = FALSE,
  print_i2 = FALSE, plot_adv = TRUE
)
ggsave(plot = p_la, file = "figures/la.jpg",
       bg = "white", width = 12, height = 8)


p_sd <- plot_typical_random(
  "New York County", "cor.earning.time_travel", "Correlation", print_i1 = FALSE,
  print_i2 = FALSE, plot_adv = TRUE
)
p_sd


p_sd <- plot_typical_random(
  "New York County", "cor.race.rooms", "Correlation", print_i1 = FALSE,
  print_i2 = FALSE, plot_adv = TRUE
)
p_sd


ggsave(plot = p_sd, file = "figures/ny.jpg",
       bg = "white", dpi = 100, width = 12, height = 8)


#-------------------------------------------------------------------------------
require(tidyverse)
require(ggplot2)
require(data.table)
require(cubature)
require(reshape2)
require(spdep)
require(magick)
require(latex2exp)

source("00_functions.R")
index_adv <- readRDS("data/index_adv_with_m_M.rds")
  
plot_typical <- function(county, state, colname) {
    idx <- which(index_adv$county == county & index_adv$state == state & index_adv$colname == colname)
    county_m <- index_adv[idx, ]$m
    county_M <- index_adv[idx, ]$M
    data_m <- melt(data.frame(x = 1:length(county_m),
                              m = county_m,
                              M = county_M),
                   id.vars = "x", variable.name = "zs", value.name = "value")

    tau <- county_m[length(county_m)]
    anot <- TeX(paste("$I_z$ =", round(index_adv[idx[1], "i1"], 2)))
    x_anot <- length(county_m) * 0.9
    y_anot = 0.8
    p <- ggplot(data_m) +
        geom_line(aes(x = x, y = value, linetype = zs),
                  linewidth = 1.5) +
        geom_hline(yintercept = tau,
                   linewidth = 1.3,
                   color = "blue") +
        ylab("Correlation") +
        xlab("Number of clusters") +
        labs(linetype = "Zoning system") +
        annotate("text", x = x_anot, y = y_anot,
                 label = anot, size = 10) +
        theme_bw(base_size = 20) +
        theme(legend.key.size = unit(3.5, "line"),
              legend.position = "top")
    return(p)
}

p_texas <- plot_typical("Harris County", "Texas", "cor.earning.race")
p_franklin <- plot_typical("Franklin County", "Ohio", "cor.race.rooms")
p_san <- plot_typical("San Joaquin County", "California", "cor.earning.race")
p_washtenaw <- plot_typical("Washtenaw County", "Michigan", "cor.earning.time_travel")


ggsave(plot = p_texas,
       file = "figures/texas.jpg",
       bg = "white", width = 10, height = 6)
ggsave(plot = p_franklin,
       file = "figures/franklin.jpg",
       bg = "white", width = 10, height = 6)
ggsave(plot = p_san,
       file = "figures/san.jpg",
       bg = "white", width = 10, height = 6)
ggsave(plot = p_washtenaw,
       file = "figures/washtenaw.jpg",
       bg = "white", width = 10, height = 6)

