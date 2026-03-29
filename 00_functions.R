# Ensure scripts are runnable from any working directory when executed via Rscript.
.local_setwd_to_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) < 1) return(invisible(FALSE))
  script_path <- sub("^--file=", "", file_arg[1])
  script_dir <- dirname(normalizePath(script_path))
  if (dir.exists(script_dir)) {
    setwd(script_dir)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}
.local_setwd_to_script_dir()
rm(.local_setwd_to_script_dir)

rename_var <- function(input_string) {
  switch(input_string,
    "time_travel.earning" = "cor.earning.time_travel",
    "time_travel.race" = "cor.race.time_travel",
    "time_travel.rooms" = "cor.time_travel.rooms",
    "earning.race" = "cor.earning.race",
    "earning.rooms" = "cor.earning.rooms",
    "race.rooms" = "cor.race.rooms",
    stop("Input not recognized")
  )
}


cor_vector <- function(cor_matrix) {
  result <- c()
  
  var_names <- colnames(cor_matrix)
  
  for (i in 1:(ncol(cor_matrix) - 1)) {
    for (j in (i + 1):ncol(cor_matrix)) {
      name <- paste(var_names[i], var_names[j], sep = ",")
      
      result[name] <- cor_matrix[i, j]
    }
  }
  
  return(result)
}

gini_index <- function(income, population) {
  weighted_income <- income * population
  
  gini <- ineq::ineq(weighted_income)
  
  return(gini)
}


cost_random <- function(data, id, id.neigh) {
  return(rnorm(length(id.neigh)))
}

create_mstree <- function(geometry, data, cost_function) {
  neighbourhood <- poly2nb(geometry, snap = 1e-5)
  costs <- nbcosts(neighbourhood, data, cost_function)
  neighbourhood_weights <- nb2listw(
    neighbourhood,
    costs,
    style = "B"
  )
  return(mstree(neighbourhood_weights))
}

eval_maup_skater <- function(
    model,
    data,
    geometry,
    cost_function_mstree = NULL,
    stat_function = NULL,
    n_cuts = NULL, ...) {
  if (!(model %in% c("adversarial", "random", "adversarial_random"))) {
    stop("Wrong model argument")
  }

  if (is.null(n_cuts)) {
    n_cuts <- nrow(data) - 1
  }

  if (model == "adversarial") {
    global_correlation <- cor(data)[1, 2]

    if (global_correlation > 0) {
      min_mstree <- create_mstree(
        geometry = geometry,
        data = data,
        cost_function = cost_correlation_adversarial_minus_identity
      )
      max_mstree <- create_mstree(
        geometry = geometry,
        data = data,
        cost_function = cost_correlation_adversarial_identity
      )
      edges_min_mstree <- min_mstree[, 1:2]
      edges_max_mstree <- max_mstree[, 1:2]
      skater_min <- maup_skater_adv(
        edges = edges_min_mstree,
        data = data,
        n_cuts = n_cuts,
        type = "positive"
      )
      skater_max <- maup_skater_adv(
        edges = edges_max_mstree,
        data = data,
        n_cuts = n_cuts,
        type = "negative"
      )
      skater_min$stat_correlation_adversarial <- -skater_min$stat_correlation_adversarial

      return(list(skater_max = skater_max, skater_min = skater_min))
    } else {
      max_mstree <- create_mstree(
        geometry = geometry,
        data = data,
        cost_function = cost_correlation_adversarial_minus_identity
      )
      min_mstree <- create_mstree(
        geometry = geometry,
        data = data,
        cost_function = cost_correlation_adversarial_identity
      )
      edges_min_mstree <- min_mstree[, 1:2]
      edges_max_mstree <- max_mstree[, 1:2]
      skater_min <- maup_skater_adv(
        edges = edges_min_mstree,
        data = data,
        n_cuts = n_cuts,
        type = "negative"
      )
      skater_max <- maup_skater_adv(
        edges = edges_max_mstree,
        data = data,
        n_cuts = n_cuts,
        type = "positive"
      )
      skater_max$stat_correlation_adversarial <- -skater_max$stat_correlation_adversarial
      return(list(skater_max = skater_max, skater_min = skater_min))
    }
  }

  if (model == "random") {
    if (is.null(stat_function)) {
      stop("stat_function must be provided if the model is random")
    }

    if (is.null(cost_function_mstree)) {
      cost_function_mstree <- cost_random
    }

    random_mstree <- create_mstree(
      geometry = geometry,
      data = data,
      cost_function = cost_function_mstree
    )
    edges <- random_mstree[, 1:2]
    ret <- maup_skater_random(
      edges = edges,
      data = data,
      stat_function = stat_function,
      n_cuts = n_cuts, ...
    )

    return(ret)
  }

  if (model == "adversarial_random") {
    global_correlation <- cor(data)[1, 2]

    if (is.null(cost_function_mstree)) {
      cost_function_mstree <- cost_random
    }

    random_mstree <- create_mstree(
      geometry = geometry,
      data = data,
      cost_function = cost_function_mstree
    )
    edges <- random_mstree[, 1:2]

    if (global_correlation > 0) {
      skater_min <- maup_skater_adv(
        edges = edges,
        data = data,
        n_cuts = n_cuts,
        type = "positive"
      )
      skater_max <- maup_skater_adv(
        edges = edges,
        data = data,
        n_cuts = n_cuts,
        type = "negative"
      )
      skater_min$stat_correlation_adversarial <- -skater_min$stat_correlation_adversarial

      return(list(skater_max = skater_max, skater_min = skater_min))
    } else {
      skater_min <- maup_skater_adv(
        edges = edges,
        data = data,
        n_cuts = n_cuts,
        type = "negative"
      )
      skater_max <- maup_skater_adv(
        edges = edges,
        data = data,
        n_cuts = n_cuts,
        type = "positive"
      )
      skater_max$stat_correlation_adversarial <- -skater_max$stat_correlation_adversarial
      return(list(skater_max = skater_max, skater_min = skater_min))
    }
  }
}

eval_gini <- function(means_groups, ...) {
  means_groups <- as.data.frame(do.call(rbind, means_groups))
  additional_args <- list(...)
  gini <- gini_index(
    income = means_groups[, additional_args$income_col],
    population = means_groups[, additional_args$population_col]
  )
 
 return(unlist(gini))
}

eval_stats <- function(means_groups, ...) {
  additional_args <- list(...)
  means_groups <- as.data.frame(do.call(rbind, means_groups))
  regression_data <- means_groups[, additional_args$regression_vars]

  mean_data <- means_groups[, additional_args$mean_vars]
  var_data <- means_groups[, additional_args$var_vars]
  cor_data <- means_groups[, additional_args$cor_vars]

  if (ncol(regression_data) > nrow(regression_data)) {
    r2 <- NA
    beta <- rep(NA, ncol(regression_data))
  } else {
    model <- lm(additional_args$formula, data = regression_data)
    summary_model <- summary(model)
    r2 <- summary_model$r.squared
    beta <- coef(model)
  }

  mean_value <- colMeans(mean_data)
  var_value <- apply(var_data, 2, var)
  cor_matrix <- cor(cor_data)
  cor_value <- cor_vector(cor_matrix)

  return(list(
    r2 = r2,
    beta = beta,
    mean = mean_value,
    var = var_value,
    cor = cor_value
  ))
}

univariate_stat_cor <- function(means_groups) {
  means_groups <- as.data.frame(do.call(rbind, means_groups))
  cor_matrix <- cor(means_groups)
  
  return(cor_matrix[1, 2])
}

univariate_stat_mean <- function(means_groups, ...) {
  return(mean(sapply(means_groups, mean)))
}

eval_stat_function <- function(data, id_groups, stat_function, ...) {
  newdata_group <- lapply(id_groups, function(x) {
    if (length(x) == 0) {
      return()
    }
    data[x, ]
  })
  newdata_group <- newdata_group[lengths(newdata_group) != 0]
  means_groups <- lapply(newdata_group, function(x) colMeans(rbind(x)))
  return(do.call(
    stat_function, 
    list(means_groups = means_groups, ...)
  ))
}

maup_skater_random <- function(edges, data, stat_function, n_cuts, ...) {
    n <- nrow(edges) + 1
    res <- list(groups = rep(1, n), edges.groups = list(list(node = 1:n,
                                                             edge = edges)),
                not.prune = NULL, candidates = 1)
    res$edges.groups[[1]]$edge = cbind(res$edges.groups[[1]]$edge,
                                       rnorm(nrow(res$edges.groups[[1]]$edge)))
    cuts <- length(res$edges.groups)
    res$candidates <- setdiff(1:length(res$edges.groups), res$not.prune)
    res$stat <- vector("list", n_cuts - 1)
    repeat {
        if (cuts > n_cuts)
            break
        if (length(res$candidates) == 0)
            break
        l.costs.ord <- lapply(res$edges.groups[res$candidates],
                              function(x) x$edge[, 3])
        t.id <- rep(res$candidates, sapply(l.costs.ord, length))
        t.cost <- unlist(l.costs.ord)
        t.idi <- unlist(lapply(l.costs.ord, function(x) {
            if (length(x) > 0)
                1:length(x)
            else NULL
        }))
        dc <- cbind(t.id, t.cost, t.idi)
        if (nrow(dc) > 1) dc <- dc[order(rnorm(nrow(dc))), ]
        toprun <- rbind(res$edges.groups[[dc[1, 1]]]$edge[dc[1, 3], 1:2],
                        res$edges.groups[[dc[1, 1]]]$edge[-dc[1, 3], 1:2])
        g.pruned <- prunemst(toprun, only.nodes = FALSE)
        groups <- lapply(res$edges.groups[-dc[1, 1]],
                         function(x) list(node = x$node,
                                          edge = x$edge))
        groups <- append(groups, g.pruned[1])
        groups <- append(groups, g.pruned[2])
        gc.pruned <- lapply(groups, function(e) {
            list(node = e$node,
                 edge = cbind(e$edge, rnorm(nrow(e$edge))))
        })
        res$edges.groups[[dc[1, 1]]] <- gc.pruned[[length(gc.pruned) - 1]]
        cuts <- cuts + 1
        res$edges.groups[[cuts]] <- gc.pruned[[length(gc.pruned)]]
        ids <- lapply(res$edges.groups, function(x) x$node)
        res$stat[[cuts - 1]] <- eval_stat_function(
            data = data,
            id_groups = ids,
            stat_function = stat_function,
            ...
            )
        res$candidates <- setdiff(1:length(res$edges.groups),
                                  res$not.prune)
    }
    for (i in 1:length(res$edges.groups)) res$groups[res$edges.groups[[i]]$node] <- i
    res$n_group <- unlist(lapply(ids, length))
    attr(res, "class") <- "skater"
    return(res)
}



cost_correlation_adversarial_identity <- function(data, id, id.neigh) {
  ret <- vector("list", length(id.neigh))
  k <- 1
  aux <- as.matrix(data)

  for (i in id.neigh) {        
      mean_xy_ij <- (aux[i, ] + aux[id, ]) / 2
      ret[[k]] <- abs(diff(mean_xy_ij))
      k <- k + 1
  }
  unlist(ret)
}

cost_correlation_adversarial_minus_identity <- function(data, id, id.neigh) {
  ret <- vector("list", length(id.neigh))
  k <- 1
  aux <- as.matrix(data)
  for (i in id.neigh) {
      mean_xy_ij <- (aux[i, ] + aux[id, ]) / 2
      ret[[k]] <- abs(sum(mean_xy_ij))
      k <- k + 1
  }
  unlist(ret)
}

stat_correlation_adversarial <- function (data, id_groups, type) {
    newdata_group <- lapply(id_groups, function(x) {
        if (length(x) == 0) return()
        data[x, ]
    })
    newdata_group <- newdata_group[lengths(newdata_group) != 0]
    means_group <- lapply(newdata_group, function(x) colMeans(rbind(x)))
    if (type == "positive") return(-cor(do.call(rbind, means_group))[1, 2])
    if (type == "negative") return(cor(do.call(rbind, means_group))[1, 2])
}

prunecost_corrrelation_first <- function(edges, data, type) {
  sapply(1:nrow(edges),
          function(i) {
              pruned.ids <- prunemst(
                rbind(edges[i, ], edges[-i, ]),
                only.nodes = TRUE
              )
              newdata_group <- lapply(pruned.ids, function(x) {
                data[x, ]
              })
              means_group <- lapply(
                newdata_group,
                function(x) colMeans(rbind(x))
              )
              da <- do.call(rbind, means_group)
              if (type == "positive") {
                return(-diff(c(da[1, 1], da[2, 1])) /
                      diff(c(da[1, 2], da[2, 2])))
              }
              if (type == "negative") {
                return(diff(c(da[1, 1], da[2, 1])) /
                      diff(c(da[1, 2], da[2, 2])))
              }
          })
}

prunecost_corrrelation <- function(edges, data, id_groups, type) {
    sapply(1:nrow(edges), function(i) {
        pruned.ids <- prunemst(rbind(edges[i, ],
                                     edges[-i,]),
                               only.nodes = TRUE)
        aux <- lapply(id_groups, function(x) {
            if (length(x) == 0) return()
            l <- setdiff(x, pruned.ids[[1]])
            setdiff(l, pruned.ids[[2]])
        })
        aux <- append(aux, unname(pruned.ids[1]))
        aux <- append(aux, unname(pruned.ids[2]))
        stat_correlation_adversarial(data, aux, type)
    })
}


maup_skater_adv <- function(edges, data, n_cuts, type) {
    n <- nrow(edges) + 1
    res <- list(groups = rep(1, n), edges.groups = list(list(node = 1:n,
                                                             edge = edges)), candidates = 1)
    tmp <- sort(prunecost_corrrelation_first(res$edges.groups[[1]]$edge[, 1:2,
                                                               drop = FALSE],
                                    data, type),
                decreasing = TRUE, method = "quick", index.return = TRUE)
    res$edges.groups[[1]]$edge = cbind(res$edges.groups[[1]]$edge[tmp$ix,],
                                       tmp$x)
    cuts <- length(res$edges.groups)
    res$candidates <- setdiff(1:length(res$edges.groups), res$not.prune)
    repeat {
        # print(cuts)
        if (cuts > n_cuts)
            break
        if (length(res$candidates) == 0)
            break
        l.costs.ord <- lapply(res$edges.groups[res$candidates],
                              function(x) x$edge[, 3])
        t.id <- rep(res$candidates, sapply(l.costs.ord, length))
        t.cost <- unlist(l.costs.ord)
        t.idi <- unlist(lapply(l.costs.ord, function(x) {
            if (length(x) > 0)
                1:length(x)
            else NULL
        }))
        dc <- cbind(t.id, t.cost, t.idi)
        if (nrow(dc) == 0) break
        dc <- dc[sort(dc[, 2], method = "quick", decreasing = TRUE,
                      index.return = TRUE)$ix, , drop = FALSE]
        toprun <- rbind(res$edges.groups[[dc[1, 1]]]$edge[dc[1, 3], 1:2],
                        res$edges.groups[[dc[1, 1]]]$edge[-dc[1, 3], 1:2])
        g.pruned <- prunemst(toprun, only.nodes = FALSE)
        groups <- lapply(res$edges.groups[-dc[1, 1]],
                         function(x) list(node = x$node,
                                          edge = x$edge))
        groups <- append(groups, g.pruned[1])
        groups <- append(groups, g.pruned[2])

        gc.pruned <- lapply(groups, function(e) {
            if (nrow(e$edge) == 0) {
                return(list(node = e$node, edge = matrix(0, 0, 3),
                            stat_correlation_adversarial = Inf))
            }
            else {
                id_groups <- lapply(res$edges.groups[-dc[1, 1]],
                                    function(x) x$node)
                id_groups <- append(id_groups, list(g.pruned[[1]]$node))
                id_groups <- append(id_groups, list(g.pruned[[2]]$node))
                tmp <- sort(prunecost_corrrelation(edges = e$edge[, 1:2, drop = FALSE],
                                           data,
                                           id_groups = id_groups,
                                           type),
                            decreasing = TRUE, method = "quick",
                            index.return = TRUE)
                list(node = e$node,
                     edge = cbind(e$edge[tmp$ix, , drop = FALSE], tmp$x))
            }
        })
        res$edges.groups[[dc[1, 1]]] <- gc.pruned[[length(gc.pruned) - 1]]
        cuts <- cuts + 1
        res$edges.groups[[cuts]] <- gc.pruned[[length(gc.pruned)]]
        ids <- lapply(res$edges.groups, function(x) x$node)
        res$stat_correlation_adversarial <- c(
            res$stat_correlation_adversarial,
             stat_correlation_adversarial(data, ids, type)
        )
        res$candidates <- setdiff(1:length(res$edges.groups),
                                  res$not.prune)
    }
    for (i in 1:length(res$edges.groups)) res$groups[res$edges.groups[[i]]$node] <- i
    res$n_group <- unlist(lapply(ids, length))
    attr(res, "class") <- "skater"
    return(res)
}
