a.findcorrelations <- function(df, vars1=names(df), vars2=vars1, min.cor=0.5) {
  diagonal <- missing(vars2)
  n1 <- length(vars1)
  n2 <- length(vars2)
  corrs <- NULL
  name <- NULL
  for (first in 1:n1) {
    v1 <- df[, vars1[first]]  # select a column
    rv1 <- rank(v1)
    if (!diagonal) {  
      range <- 1:n2
    } else if (first < n2) {  
        range <- (first+1):n2  # leave out duplicates and self-correlations
    } else {
        range <- NULL  # avoid crash
    }
    for (second in range) {
      v2 <- df[, vars2[second]]  # select a column
      rv2 <- rank(v2)
      where <- !is.na(v1) & !is.na(v2)
      # now compute the correlations:
      corrs <- c(corrs, cor(v1[where], v2[where]))
      name <- c(name, paste(vars1[first],"/",vars2[second], sep=""))
      corrs <- c(corrs, cor(rv1[where], rv2[where]))
      name <- c(name, 
                paste("r(", vars1[first],")/r(",vars2[second], ")", sep=""))
    }
  }
  which <- abs(corrs) >= min.cor
  corrs <- corrs[which]
  name <- name[which]
  ord <- order(-abs(corrs))
  result <- corrs[ord]
  names(result) <- name[ord]
  result
}

a.iqr <- function(x) {
  qq <- quantile(x, c(0.25,0.75))
  attr(qq, "names") <- NULL
  qq[2] - qq[1]
}

a.proportion.test <- function(x1, x2, y1, y2, totals=FALSE) {
  if (totals) {
    x2 <- x2-x1
    y2 <- y2-y1
  }
  m <- rbind(c(x1, x2), c(y1, y2))
  if (max(as.vector(m)) < 50) {
    fisher <- fisher.test(m)
    fisher.p <- fisher$p.value
  }
  else
    fisher.p <- "---   "
  chi <- chisq.test(m)
  chi.p <- chi$p.value
  cat(x1,":",x2," <=> ",y1,":",y2,
      "  Fisher-p: ",format(fisher.p,digits=4),
      "  Chi-square-p: ",format(chi.p,digits=4),
      "\n", sep = "")
  invisible(chi)
}

a.qr <- function(x) {
  qq <- quantile(x, c(0.25,0.75))
  attr(qq, "names") = NULL
  ifelse(qq[1] < 0, NA, ifelse(qq[1] == qq[2], 1, qq[2]/qq[1]))  # 0/0 == 1
}

a.rankval <- function(x) {
  where <- !is.na(x)
  r <- rank(x[where])           # rank only existing values
  result <- numeric(length(x))  # create result of proper length
  result[TRUE] <- NA            # set all result values to NA
  result[where] <- r            # assign ranks where they exist
  result
}

a.showextremes <- function(df, vars, largest=5, showalso=NULL) {
  show <- c(vars,showalso)
  ordering <- rep(ifelse(largest > 0, -1, 1), length(vars))
  kind <- ifelse(largest > 0, "largest", "smallest")
  n <- abs(largest)
  count <- NULL
  for (i in 1:length(vars)) {
    # show extremes for this variable
    name <- vars[i]
    values <- df[,name]
    cat(kind, n, "of", name)  # print header
    nas <- sum(is.na(values))
    if (nas > 0) cat ("  ", nas, "x NA")
    cat(":\n")
    which <- order(ordering[i]*values)[1:n]  # select extremes
    print(df[which, show])                   # print extremes
    count <- c(count,row.names(df[which,]))
  }
  cat("summary:\n");                         # print row frequency table
  invisible(print(-sort(-table(count))))
}
