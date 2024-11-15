keep_nodes <- function(vp, n, more) {
  max_n <- length(vp$children)
  if(n < max_n) {
    idx <- max_n %/% 2 + rev(seq.int(n %/% 2, by = -1, length.out = n))
  } else {
    idx <- seq_len(max_n)
    if(more) idx <- idx[!idx %in% (max_n %/% 2 + -1:2)]
  }
  idx
}

apply_ramp <- function(x, colors) {
  grDevices::rgb(grDevices::colorRamp(colors)(x), maxColorValue = 255)
}

node <- function(n) {
  grid::viewport(layout.pos.row = n, layout.pos.col = 1)
}

layer <- function(n_layer, n_nodes) {
  grid::vpTree(
    grid::viewport(
      layout = grid::grid.layout(n_nodes, 1, respect = TRUE),
      layout.pos.col = n_layer, layout.pos.row = 1,
    ),
    do.call(grid::vpList, lapply(seq_len(n_nodes), node))
  )
}

plot_network <- function(ns_nodes, activators, weights, max_nodes, main, colors) {
  stopifnot(max_nodes > 5)
  grid::grid.newpage()
  more <- ns_nodes > max_nodes
  n_orig <- ns_nodes
  ns_nodes[more] <- max_nodes
  n_layers <- length(ns_nodes)
  layers <- seq_len(n_layers)
  gap <- grid::unit(1, "strheight", paste(activators, collapse = ""))
  grid::pushViewport(vp_total <- grid::vpList(
    grid::viewport(
      y = 0.9, width = 0.9, height = grid::unit(0.05, "npc") - gap, just = "top"
    ),
    grid::vpStack(
      grid::viewport(y = 0.1, width = 0.9, height = 0.7, just = "bottom"),
      grid::vpTree(
        grid::viewport(layout = grid::grid.layout(1, n_layers)),
        do.call(grid::vpList, lapply(layers, layer, n_nodes = max(ns_nodes)))
      )
    )
  ))
  grid::seekViewport(vp_total[[1]]$name)
  grid::grid.text(main, gp = grid::gpar(fontface = "bold"))
  vp <- vp_total[[2]][[2]]
  keep <- .mapply(keep_nodes, list(vp$children, ns_nodes, more), NULL)
  which_nodes <- vector("list", n_layers)
  which_nodes[more] <- .mapply(\(k, n) {
    start <- which(diff(k) != 1)
    end <- length(k) - start
    c(seq_len(start), rev(seq.int(n, by = -1, length.out = end)))
  }, list(k = keep[more], n = n_orig[more]), NULL)
  middle <- grid::unit(0.5, "npc")
  width <- grid::unit(0, "npc")
  height <- grid::unit(1, "npc")
  z <- lapply(layers, \(i) {
    lapply(vp$children[[i]]$children[keep[[i]]], \(v) {
      grid::seekViewport(v$name)
      radius <- grid::deviceDim(width, height)$h / 2
      list(
        r = grid::deviceLoc(middle + radius, middle),
        l = grid::deviceLoc(middle - radius, middle)
      )
    })
  })
  l <- do.call(rbind, lapply(layers[-1], \(i) {
    z0 <- z[[i-1]]
    z1 <- z[[i]]
    idx <- expand.grid(seq_along(z1), seq_along(z0))
    idx_weights <- idx
    if(more[i]) idx_weights$Var1 <- which_nodes[[i]][idx_weights$Var1]
    if(more[i-1]) idx_weights$Var2 <- which_nodes[[i-1]][idx_weights$Var2]
    w <- weights[[i-1]][as.matrix(idx_weights)]
    data.frame(
      x0 = vapply(idx$Var2, \(j) z0[[j]]$r$x, numeric(1)),
      y0 = vapply(idx$Var2, \(j) z0[[j]]$r$y, numeric(1)),
      x1 = vapply(idx$Var1, \(j) z1[[j]]$l$x, numeric(1)),
      y1 = vapply(idx$Var1, \(j) z1[[j]]$l$y, numeric(1)),
      weight = w / max(abs(weights[[i-1]])),
      weight = w
    )
  }))
  negative <- l$weight < 0
  col <- character(length(l$weight))
  col[negative] <- apply_ramp(-l$weight[negative], colors[c(2,1)])
  col[!negative] <- apply_ramp(l$weight[!negative], colors[c(2,3)])
  grid::upViewport(0)
  grid::grid.polyline(
    x = grid::convertX(grid::unit(c(l$x0, l$x1), "inches"), "npc"),
    y = grid::convertY(grid::unit(c(l$y0, l$y1), "inches"), "npc"),
    id = rep.int(seq_len(nrow(l)), 2), gp = grid::gpar(col = col)
  )
  for(i in seq_len(n_layers)) {
    child <- vp$children[[i]]
    grid::seekViewport(child$parent$name)
    for(j in keep[[i]]) {
      grid::grid.circle(name = "c", vp = child$children[[j]])
    }
    if(more[i]) {
      for(j in seq_len(child$parent$layout$nrow)[-keep[[i]]][-1] - 1) {
        grid::grid.circle(
          y = 0, r = 0.15, vp = child$children[[j]], gp = grid::gpar(fill = "black")
        )
      }
    }
  }
  for(i in layers[-1]) {
    y = grid::unit(1.5, "npc")
    pad <- grid::unit(0.5, "npc")
    grid::seekViewport(vp$children[[i]]$children[[keep[[i]][1]]]$name)
    t <- grid::textGrob(activators[i-1], y = y, just = "bottom")
    grid::grid.rect(
      y = y - pad/2, width = grid::grobWidth(t) + pad,
      height = grid::grobHeight(t) + pad, just = "bottom",
      gp = grid::gpar(col = NA)
    )
    grid::grid.draw(t)
  }
  invisible()
}
