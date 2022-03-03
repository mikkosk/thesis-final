library(Matrix)

find_overlaps <- function(qstart, qend, starts, ends, norm='both') {
  denom <- switch(which(norm == c('left', 'right', 'both')),
                  qend-qstart,
                  ends-starts,
                  pmax(qend, ends)-pmin(qstart, starts))
  sim <- pmax(0, pmin(qend, ends)-pmax(qstart, starts)) / denom
  idx <- which(sim > 0)
  list(idx = idx, sim = sim[idx])
}

pairwise_overlaps <- function(starts, ends, norm='both') {
  n <- length(starts)
  so <- order(starts)
  eo <- order(ends)
  ss <- sort(starts)
  es <- sort(ends)
  maxlen <- max(ends-starts)

  y <- lapply(
    1:n,
    function(x) intersect(so[(findInterval(starts[x]-maxlen, ss)):(findInterval(ends[x], ss))],
                          eo[(findInterval(starts[x], es)):(findInterval(ends[x]+maxlen, es))]))
  i <- unlist(lapply(1:(length(y)), function(x) rep(x, length(y[[x]]))))
  j <- unlist(y)

  ov <- pmin(ends[i], ends[j])-pmax(starts[i], starts[j])
  ov[ov < 0] <- 0
  denom <- switch(which(norm == c('left', 'right', 'both')),
                  ends[i]-starts[i],
                  ends[j]-starts[j],
                  pmax(ends[i], ends[j])-pmin(starts[i], starts[j]))
  sparseMatrix(i = i, j = j, x = ov / denom, dims = c(n, n))
}

sparse_mtx_to_list <- function(m) {
  m2 <- t(m)
  idx <- lapply(1:(length(m2@p)-1),
                function(x) {
                  if (m2@p[x] < m2@p[x+1]) {
                    return(m2@i[(m2@p[x]+1):m2@p[x+1]]+1)
                  } else {
                    return(integer(0))
                  } })
  sim <- lapply(1:(length(m2@p)-1),
                function(x) {
                  if (m2@p[x] < m2@p[x+1]) {
                    return(m2@x[(m2@p[x]+1):m2@p[x+1]])
                  } else {
                    return(numeric(0))
                  } })
  list(idx = idx, sim = sim)
}

chinwhisp <- function(m) {
  changed <- TRUE
  clust <- 1:(nrow(m))
  iter <- 1
  # convert the sparse matrix to lists for optimization purposes
  nb <- sparse_mtx_to_list(m)
  while (changed) {
    message('iteration ', iter, ' ', format(Sys.time(), "%a %b %d %X %Y"))
    changed <- FALSE
    for (i in sample(1:(nrow(m)))) {
      v <- ave(nb$sim[[i]], clust[nb$idx[[i]]], FUN=sum)
      if (any(v > 0)) {
        newcl <- clust[nb$idx[[i]][which.max(v)]]
        changed <- changed | (clust[i] != newcl)
        if (clust[i] != newcl) {
          clust[i] <- newcl
        }
      }
    }
    iter <- iter + 1
  }
  clust
}
