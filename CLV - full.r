###########################################################################
# customer lifetime value
# 8/9/16
###########################################################################

#unload all previously loaded packages
pkgs = names(sessionInfo()$otherPkgs) 
pkgs = paste('package:', pkgs, sep = "")
lapply(pkgs, detach, character.only = TRUE, unload = TRUE)
rm(pkgs)


#set working directory
setwd("C:\\Users\\Babbenante\\documents\\my stuff\\code\\clv\\")

#load required libraries
if (!require(readr)) {
  install.packages('readr') # read preformatted dates
  require(read)
}

if (!require(BTYD)) {
  install.packages('BTYD') # read preformatted dates
  require(BTYD)
}

if (!require(dplyr)) {
  install.packages('dplyr') # consistent data.frame operations
  require(dplyr)
}


## clear the console of all variables
rm(list = ls())
## free up unused memory
gc()


############################
## start functions        ##
############################

h2f1 <- function(a,b,c,z){
  lenz <- length(z)
  j = 0
  uj <- 1:lenz
  uj <- uj/uj
  y <- uj
  lteps <- 0
  while (lteps<lenz){
    lasty <- y
    j <- j+1
    uj <- uj*(a+j-1)*(b+j-1)/(c+j-1)*z/j
    y <- y + uj
    lteps <- sum(y==lasty)
  }
  y
}

pnbd2.cbs.LL <- function(params, cal.cbs) {
  
  dc.check.model.params(c("r", "alpha", "s", "beta"), params, "pnbd2.cbs.LL")
  
  tryCatch(x <- cal.cbs[, "x"], error = function(e) stop("Error in pnbd2.cbs.LL: cal.cbs must have a frequency column labelled \"x\""))
  tryCatch(t.x <- cal.cbs[, "t.x"], error = function(e) stop("Error in pnbd2.cbs.LL: cal.cbs must have a recency column labelled \"t.x\""))
  tryCatch(T.cal <- cal.cbs[, "T.cal"], error = function(e) stop("Error in pnbd2.cbs.LL: cal.cbs must have a column for length of time observed labelled \"T.cal\""))
  
  if ("custs" %in% colnames(cal.cbs)) {
    custs <- cal.cbs[, "custs"]
  } else {
    custs <- rep(1, length(x))
  }
  return(sum(custs * pnbd2.LL(params, x, t.x, T.cal)))
}

pnbd2.LL= function (params, x, t.x, T.cal) {
  max.length <- max(length(x), length(t.x), length(T.cal))
  if (max.length%%length(x)) 
    warning("Maximum vector length not a multiple of the length of x")
  if (max.length%%length(t.x)) 
    warning("Maximum vector length not a multiple of the length of t.x")
  if (max.length%%length(T.cal)) 
    warning("Maximum vector length not a multiple of the length of T.cal")
  dc.check.model.params(c("r", "alpha", "s", "beta"), params, 
                        "pnbd2.LL")
  if (any(x < 0) || !is.numeric(x)) 
    stop("x must be numeric and may not contain negative numbers.")
  if (any(t.x < 0) || !is.numeric(t.x)) 
    stop("t.x must be numeric and may not contain negative numbers.")
  if (any(T.cal < 0) || !is.numeric(T.cal)) 
    stop("T.cal must be numeric and may not contain negative numbers.")
  x <- rep(x, length.out = max.length)
  t.x <- rep(t.x, length.out = max.length)
  T.cal <- rep(T.cal, length.out = max.length)
  r <- params[1]
  alpha <- params[2]
  s <- params[3]
  beta <- params[4]
  maxab <- max(alpha, beta)
  absab <- abs(alpha - beta)
  param2 <- s + 1
  if (alpha < beta) {
    param2 <- r + x
  }
  part1 <- r * log(alpha) + s * log(beta) - lgamma(r) + lgamma(r + x)
  part2 <- -(r + x) * log(alpha + T.cal) - s * log(beta + T.cal)
  if (absab == 0) {    
    part2_times_F1_min_F2 <- ( (alpha+T.cal)/(maxab+t.x) )^(r+x) * (beta+T.cal)^s / 
      ((maxab+t.x)^s) -
      ( (alpha+T.cal)/(maxab+T.cal) )^(r+x) * (beta+T.cal)^s / 
      ((maxab+t.x)^s) 
  }
  else {
    part2_times_F1_min_F2 = h2f1(r+s+x, param2, r+s+x+1, absab / (maxab+t.x)) * 
      ( (alpha+T.cal)/(maxab+t.x) )^(r+x) * (beta+T.cal)^s / 
      ((maxab+t.x)^s) -
      h2f1(r+s+x, param2, r+s+x+1, absab / (maxab+T.cal)) * 
      ( (alpha+T.cal)/(maxab+T.cal) )^(r+x) * (beta+T.cal)^s / 
      ((maxab+t.x)^s)
  }
  return(part1 + part2 + log(1+(s/(r+s+x))*part2_times_F1_min_F2) )
}

pnbd2.compress.cbs <- function(cbs, rounding = 3) {
  
  if (!("x" %in% colnames(cbs))) 
    stop("Error in pnbd2.compress.cbs: cbs must have a frequency column labelled \"x\"")
  if (!("t.x" %in% colnames(cbs))) 
    stop("Error in pnbd2.compress.cbs: cbs must have a recency column labelled \"t.x\"")
  if (!("T.cal" %in% colnames(cbs))) 
    stop("Error in pnbd2.compress.cbs: cbs must have a column for length of time observed labelled \"T.cal\"")
  
  orig.rows <- nrow(cbs)
  
  if (!("custs" %in% colnames(cbs))) {
    custs <- rep(1, nrow(cbs))
    cbs <- cbind(cbs, custs)
  }
  
  other.colnames <- colnames(cbs)[!(colnames(cbs) %in% c("x", "t.x", "T.cal"))]
  
  ## Round x, t.x and T.cal to the desired level
  cbs[, c("x", "t.x", "T.cal")] <- round(cbs[, c("x", "t.x", "T.cal")], rounding)
  
  ## Aggregate every column that is not x, t.x or T.cal by those columns. Do this
  ## by summing entries which have the same x, t.x and T.cal.
  cbs <- as.matrix(aggregate(cbs[, !(colnames(cbs) %in% c("x", "t.x", "T.cal"))], 
                             by = list(x = cbs[, "x"], t.x = cbs[, "t.x"], T.cal = cbs[, "T.cal"]), sum))
  
  colnames(cbs) <- c("x", "t.x", "T.cal", other.colnames)
  final.rows <- nrow(cbs)
  message("Data reduced from ", orig.rows, " rows to ", final.rows, " rows.")
  return(cbs)
}

pnbd2.EstimateParameters <- function(cal.cbs, par.start = c(1, 1, 1, 1), 
                                     max.param.value = 10000) {
  
  dc.check.model.params(c("r", "alpha", "s", "beta"), par.start, "pnbd2.EstimateParameters")
  
  ## helper function to be optimized
  pnbd2.eLL <- function(params, cal.cbs, max.param.value) {
    params <- exp(params)
    params[params > max.param.value] <- max.param.value
    return(-1 * pnbd2.cbs.LL(params, cal.cbs))
  }
  logparams <- log(par.start)
  results <- optim(logparams, pnbd2.eLL, cal.cbs = cal.cbs, max.param.value = max.param.value, 
                   method = "L-BFGS-B")
  estimated.params <- exp(results$par)
  estimated.params[estimated.params > max.param.value] <- max.param.value
  return(estimated.params)
}

pnbd2.pmf <- function(params, t, x) {
  
  max.length <- max(length(t), length(x))
  if (max.length%%length(t)) 
    warning("Maximum vector length not a multiple of the length of t")
  if (max.length%%length(x)) 
    warning("Maximum vector length not a multiple of the length of x")
  
  dc.check.model.params(c("r", "alpha", "s", "beta"), params, "pnbd2.pmf")
  
  if (any(t < 0) || !is.numeric(t)) 
    stop("t must be numeric and may not contain negative numbers.")
  if (any(x < 0) || !is.numeric(x)) 
    stop("x must be numeric and may not contain negative numbers.")
  
  t. <- rep(t, length.out = max.length)
  x <- rep(x, length.out = max.length)
  
  return(pnbd2.pmf.General(params, 0, t, x))
}

pnbd2.pmf.General <- function(params, t.start, t.end, x) {
  
  max.length <- max(length(t.start), length(t.end), length(x))
  
  if (max.length%%length(t.start)) 
    warning("Maximum vector length not a multiple of the length of t.start")
  if (max.length%%length(t.end)) 
    warning("Maximum vector length not a multiple of the length of t.end")
  if (max.length%%length(x)) 
    warning("Maximum vector length not a multiple of the length of x")
  
  dc.check.model.params(c("r", "alpha", "s", "beta"), params, "pnbd2.pmf.General")
  
  if (any(t.start < 0) || !is.numeric(t.start)) 
    stop("t.start must be numeric and may not contain negative numbers.")
  if (any(t.end < 0) || !is.numeric(t.end)) 
    stop("t.end must be numeric and may not contain negative numbers.")
  if (any(x < 0) || !is.numeric(x)) 
    stop("x must be numeric and may not contain negative numbers.")
  
  
  t.start <- rep(t.start, length.out = max.length)
  t.end <- rep(t.end, length.out = max.length)
  x <- rep(x, length.out = max.length)
  
  if (any(t.start > t.end)) {
    stop("Error in pnbd2.pmf.General: t.start > t.end.")
  }
  
  r <- params[1]
  alpha <- params[2]
  s <- params[3]
  beta <- params[4]
  
  equation.part.0 <- rep(0, max.length)
  equation.part.0[x == 0] <- 1 - exp(s * log(beta) - s * log(beta + t.start))
  
  ## (t.end - t.start)^x is left outside the exp() for this reason: exp(0 *
  ## log(0))=NaN; 0^0=1.
  equation.part.1 <- exp(lgamma(r + x) - lgamma(r) - lfactorial(x) + r * log(alpha) - 
                           r * log(alpha + t.end - t.start) - x * log(alpha + t.end - t.start) + s * 
                           log(beta) - s * log(beta + t.end)) * (t.end - t.start)^x
  
  equation.part.2 <- r * log(alpha) + s * log(beta) + lbeta(r + x, s + 1) - lbeta(r, 
                                                                                  s)
  
  B.1 <- rep(NA, max.length)
  B.1[alpha > beta + t.start] <- hyperg_2F1(r + s, s + 1, r + s + x + 1, (alpha - 
                                                                            beta - t.start)/alpha)/(alpha^(r + s))
  B.1[alpha <= beta + t.start] <- hyperg_2F1(r + s, r + x, r + s + x + 1, (beta + 
                                                                             t.start - alpha)/(beta + t.start))/((beta + t.start)^(r + s))
  
  B.2 <- function(r, alpha, s, beta, t.start, t.end, x, ii) {
    if (alpha > (beta + t.start)) {
      hyperg_2F1(r + s + ii, s + 1, r + s + x + 1, (alpha - beta - t.start)/(alpha + 
                                                                               t.end - t.start))/((alpha + t.end - t.start)^(r + s + ii))
    } else {
      hyperg_2F1(r + s + ii, r + x, r + s + x + 1, (beta + t.start - alpha)/(beta + 
                                                                               t.end))/((beta + t.end)^(r + s + ii))
    }
  }
  
  equation.part.2.summation <- rep(NA, max.length)
  ## In the paper, for i=0 we have t^i / i * B(r+s, i). the denominator reduces
  ## to: i * Gamma (r+s) * Gamma(i) / Gamma (r+s+i) : Gamma (r+s) * Gamma(i+1) /
  ## Gamma (r+s+i) : Gamma (r+s) * Gamma(1) / Gamma(r+s) : 1 The 1 represents
  ## this reduced piece of the equation.
  
  for (i in 1:max.length) {
    ii <- c(1:x[i])
    equation.part.2.summation[i] <- B.2(r, alpha, s, beta, t.start[i], t.end[i], 
                                        x[i], 0)
    if (x[i] > 0) {
      equation.part.2.summation[i] = equation.part.2.summation[i] + sum((t.end[i] - 
                                                                           t.start[i])^ii/(ii * beta(r + s, ii)) * B.2(r, alpha, s, beta, t.start[i], 
                                                                                                                       t.end[i], x[i], ii))
    }
  }
  return(equation.part.0 + equation.part.1 + exp(equation.part.2 + log(B.1 - equation.part.2.summation)))
}


pnbd2.ConditionalExpectedTransactions <- function(params, T.star, x, t.x, 
                                                  T.cal) {
  
  max.length <- max(length(T.star), length(x), length(t.x), length(T.cal))
  
  if (max.length%%length(T.star)) 
    warning("Maximum vector length not a multiple of the length of T.star")
  if (max.length%%length(x)) 
    warning("Maximum vector length not a multiple of the length of x")
  if (max.length%%length(t.x)) 
    warning("Maximum vector length not a multiple of the length of t.x")
  if (max.length%%length(T.cal)) 
    warning("Maximum vector length not a multiple of the length of T.cal")
  
  dc.check.model.params(c("r", "alpha", "s", "beta"), params, "pnbd2.ConditionalExpectedTransactions")
  
  if (any(T.star < 0) || !is.numeric(T.star)) 
    stop("T.star must be numeric and may not contain negative numbers.")
  if (any(x < 0) || !is.numeric(x)) 
    stop("x must be numeric and may not contain negative numbers.")
  if (any(t.x < 0) || !is.numeric(t.x)) 
    stop("t.x must be numeric and may not contain negative numbers.")
  if (any(T.cal < 0) || !is.numeric(T.cal)) 
    stop("T.cal must be numeric and may not contain negative numbers.")
  
  
  T.star <- rep(T.star, length.out = max.length)
  x <- rep(x, length.out = max.length)
  t.x <- rep(t.x, length.out = max.length)
  T.cal <- rep(T.cal, length.out = max.length)
  
  r <- params[1]
  alpha <- params[2]
  s <- params[3]
  beta <- params[4]
  
  P1 <- (r + x) * (beta + T.cal)/((alpha + T.cal) * (s - 1))
  P2 <- (1 - ((beta + T.cal)/(beta + T.cal + T.star))^(s - 1))
  P3 <- pnbd2.PAlive(params, x, t.x, T.cal)
  return(P1 * P2 * P3)
}

# pnbd2.PAlive was contributed by Ricardo Pereira
pnbd2.PAlive <- function(params, x, t.x, T.cal) {
  
  max.length <- max(length(x), length(t.x), length(T.cal))
  
  if (max.length%%length(x)) 
    warning("Maximum vector length not a multiple of the length of x")
  if (max.length%%length(t.x)) 
    warning("Maximum vector length not a multiple of the length of t.x")
  if (max.length%%length(T.cal)) 
    warning("Maximum vector length not a multiple of the length of T.cal")
  
  dc.check.model.params(c("r", "alpha", "s", "beta"), params, "pnbd2.PAlive")
  
  if (any(x < 0) || !is.numeric(x)) 
    stop("x must be numeric and may not contain negative numbers.")
  if (any(t.x < 0) || !is.numeric(t.x)) 
    stop("t.x must be numeric and may not contain negative numbers.")
  if (any(T.cal < 0) || !is.numeric(T.cal)) 
    stop("T.cal must be numeric and may not contain negative numbers.")
  
  
  x <- rep(x, length.out = max.length)
  t.x <- rep(t.x, length.out = max.length)
  T.cal <- rep(T.cal, length.out = max.length)
  
  r <- params[1]
  alpha <- params[2]
  s <- params[3]
  beta <- params[4]
  
  A0 <- 0
  if (alpha >= beta) {
    F1 <- hyperg_2F1(r + s + x, s + 1, r + s + x + 1, (alpha - beta)/(alpha + 
                                                                        t.x))
    F2 <- hyperg_2F1(r + s + x, s + 1, r + s + x + 1, (alpha - beta)/(alpha + 
                                                                        T.cal))
    #        A0 <- F1/((alpha + t.x)^(r + s + x)) - F2/((alpha + T.cal)^(r + s + x))
    X1 <- F1*((alpha+T.cal)/(alpha+t.x))^(r+x)*((beta+T.cal)/(alpha+t.x))^s
    X2 <- F2*((beta+T.cal)/(alpha+T.cal))^s
    
  } else {
    F1 <- hyperg_2F1(r + s + x, r + x, r + s + x + 1, (beta - alpha)/(beta + 
                                                                        t.x))
    F2 <- hyperg_2F1(r + s + x, r + x, r + s + x + 1, (beta - alpha)/(beta + 
                                                                        T.cal))
    #       A0 <- F1/((beta + t.x)^(r + s + x)) - F2/((beta + T.cal)^(r + s + x))
    X1 <- F1*((alpha+T.cal)/(beta+t.x))^(r+x)*((beta+T.cal)/(beta+t.x))^s
    X2 <- F2*((alpha+T.cal)/(beta+T.cal))^(r+x)
    return((1 + s/(r + s + x) * (X1-X2))^(-1))
  }
  #    return((1 + s/(r + s + x) * (alpha + T.cal)^(r + x) * (beta + T.cal)^s * A0)^(-1))
  return((1 + s/(r + s + x) * (X1-X2))^(-1))
}

pnbd2.Expectation <- function(params, t) {
  
  dc.check.model.params(c("r", "alpha", "s", "beta"), params, "pnbd2.Expectation")
  
  if (any(t < 0) || !is.numeric(t)) 
    stop("t must be numeric and may not contain negative numbers.")
  
  r = params[1]
  alpha = params[2]
  s = params[3]
  beta = params[4]
  
  return((r * beta)/(alpha * (s - 1)) * (1 - (beta/(beta + t))^(s - 1)))
}

pnbd2.PlotFrequencyInCalibration <- function(params, cal.cbs, censor, plotZero = TRUE, 
                                             xlab = "Calibration period transactions", ylab = "Customers", title = "Frequency of Repeat Transactions") {
  
  tryCatch(x <- cal.cbs[, "x"], error = function(e) stop("Error in pnbd2.PlotFrequencyInCalibration: cal.cbs must have a frequency column labelled \"x\""))
  tryCatch(T.cal <- cal.cbs[, "T.cal"], error = function(e) stop("Error in pnbd2.PlotFrequencyInCalibration: cal.cbs must have a column for length of time observed labelled \"T.cal\""))
  
  dc.check.model.params(c("r", "alpha", "s", "beta"), params, "pnbd2.PlotFrequencyInCalibration")
  if (censor > max(x)) 
    stop("censor too big (> max freq) in PlotFrequencyInCalibration.")
  
  n.x <- rep(0, max(x) + 1)
  custs = nrow(cal.cbs)
  
  for (ii in unique(x)) {
    n.x[ii + 1] <- sum(ii == x)
  }
  
  n.x.censor <- sum(n.x[(censor + 1):length(n.x)])
  n.x.actual <- c(n.x[1:censor], n.x.censor)
  
  T.value.counts <- table(T.cal)
  T.values <- as.numeric(names(T.value.counts))
  n.T.values <- length(T.values)
  
  total.probability <- 0
  
  n.x.expected <- rep(0, length(n.x.actual))
  
  for (ii in 1:(censor)) {
    this.x.expected <- 0
    for (T.idx in 1:n.T.values) {
      T <- T.values[T.idx]
      if (T == 0) 
        next
      n.T <- T.value.counts[T.idx]
      expected.given.x.and.T <- n.T * pnbd2.pmf(params, T, ii - 1)
      this.x.expected <- this.x.expected + expected.given.x.and.T
      total.probability <- total.probability + expected.given.x.and.T/custs
    }
    n.x.expected[ii] <- this.x.expected
  }
  n.x.expected[censor + 1] <- custs * (1 - total.probability)
  
  col.names <- paste(rep("freq", length(censor + 1)), (0:censor), sep = ".")
  col.names[censor + 1] <- paste(col.names[censor + 1], "+", sep = "")
  censored.freq.comparison <- rbind(n.x.actual, n.x.expected)
  colnames(censored.freq.comparison) <- col.names
  
  cfc.plot <- censored.freq.comparison
  if (plotZero == FALSE) 
    cfc.plot <- cfc.plot[, -1]
  
  n.ticks <- ncol(cfc.plot)
  if (plotZero == TRUE) {
    x.labels <- 0:(n.ticks - 1)
    x.labels[n.ticks] <- paste(n.ticks - 1, "+", sep = "")
  } else {
    x.labels <- 1:(n.ticks)
    x.labels[n.ticks] <- paste(n.ticks, "+", sep = "")
  }
  
  ylim <- c(0, ceiling(max(cfc.plot) * 1.1))
  barplot(cfc.plot, names.arg = x.labels, beside = TRUE, ylim = ylim, main = title, 
          xlab = xlab, ylab = ylab, col = 1:2)
  
  legend("topright", legend = c("Actual", "Model"), col = 1:2, lwd = 2)
  
  return(censored.freq.comparison)
}


pnbd2.PlotFreqVsConditionalExpectedFrequency <- function(params, T.star, 
                                                         cal.cbs, x.star, censor, xlab = "Calibration period transactions", ylab = "Holdout period transactions", 
                                                         xticklab = NULL, title = "Conditional Expectation") {
  
  tryCatch(x <- cal.cbs[, "x"], error = function(e) stop("Error in pnbd2.PlotFreqVsConditionalExpectedFrequency: cal.cbs must have a frequency column labelled \"x\""))
  tryCatch(t.x <- cal.cbs[, "t.x"], error = function(e) stop("Error in pnbd2.PlotFreqVsConditionalExpectedFrequency: cal.cbs must have a recency column labelled \"t.x\""))
  tryCatch(T.cal <- cal.cbs[, "T.cal"], error = function(e) stop("Error in pnbd2.PlotFreqVsConditionalExpectedFrequency: cal.cbs must have a column for length of time observed labelled \"T.cal\""))
  
  dc.check.model.params(c("r", "alpha", "s", "beta"), params, "pnbd2.PlotFreqVsConditionalExpectedFrequency")
  if (censor > max(x)) 
    stop("censor too big (> max freq) in PlotFreqVsConditionalExpectedFrequency.")
  
  if (any(T.star < 0) || !is.numeric(T.star)) 
    stop("T.star must be numeric and may not contain negative numbers.")
  if (any(x.star < 0) || !is.numeric(x.star)) 
    stop("x.star must be numeric and may not contain negative numbers.")
  
  n.bins <- censor + 1
  transaction.actual <- rep(0, n.bins)
  transaction.expected <- rep(0, n.bins)
  bin.size <- rep(0, n.bins)
  
  for (cc in 0:censor) {
    if (cc != censor) {
      this.bin <- which(cc == x)
    } else if (cc == censor) {
      this.bin <- which(x >= cc)
    }
    n.this.bin <- length(this.bin)
    bin.size[cc + 1] <- n.this.bin
    
    transaction.actual[cc + 1] <- sum(x.star[this.bin])/n.this.bin
    transaction.expected[cc + 1] <- sum(pnbd2.ConditionalExpectedTransactions(params, 
                                                                              T.star, x[this.bin], t.x[this.bin], T.cal[this.bin]))/n.this.bin
  }
  
  col.names <- paste(rep("freq", length(censor + 1)), (0:censor), sep = ".")
  col.names[censor + 1] <- paste(col.names[censor + 1], "+", sep = "")
  comparison <- rbind(transaction.actual, transaction.expected, bin.size)
  colnames(comparison) <- col.names
  
  if (is.null(xticklab) == FALSE) {
    x.labels <- xticklab
  } else {
    if (censor < ncol(comparison)) {
      x.labels <- 0:(censor)
      x.labels[censor + 1] <- paste(censor, "+", sep = "")
    } else {
      x.labels <- 0:(ncol(comparison))
    }
  }
  
  actual <- comparison[1, ]
  expected <- comparison[2, ]
  
  ylim <- c(0, ceiling(max(c(actual, expected)) * 1.1))
  plot(actual, type = "l", xaxt = "n", col = 1, ylim = ylim, xlab = xlab, ylab = ylab, 
       main = title)
  lines(expected, lty = 2, col = 2)
  
  axis(1, at = 1:ncol(comparison), labels = x.labels)
  legend("topleft", legend = c("Actual", "Model"), col = 1:2, lty = 1:2, lwd = 1)
  
  return(comparison)
  
}

pnbd2.PlotRecVsConditionalExpectedFrequency <- function(params, cal.cbs, 
                                                        T.star, x.star, xlab = "Calibration period recency", ylab = "Holdout period transactions", 
                                                        xticklab = NULL, title = "Actual vs. Conditional Expected Transactions by Recency") {
  
  dc.check.model.params(c("r", "alpha", "s", "beta"), params, "pnbd2.PlotRecVsConditionalExpectedFrequency")
  
  if (any(T.star < 0) || !is.numeric(T.star)) 
    stop("T.star must be numeric and may not contain negative numbers.")
  if (any(x.star < 0) || !is.numeric(x.star)) 
    stop("x.star must be numeric and may not contain negative numbers.")
  
  tryCatch(x <- cal.cbs[, "x"], error = function(e) stop("Error in pnbd2.PlotRecVsConditionalExpectedFrequency: cal.cbs must have a frequency column labelled \"x\""))
  tryCatch(t.x <- cal.cbs[, "t.x"], error = function(e) stop("Error in pnbd2.PlotRecVsConditionalExpectedFrequency: cal.cbs must have a recency column labelled \"t.x\""))
  tryCatch(T.cal <- cal.cbs[, "T.cal"], error = function(e) stop("Error in pnbd2.PlotRecVsConditionalExpectedFrequency: cal.cbs must have a column for length of time observed labelled \"T.cal\""))
  
  t.values <- sort(unique(t.x))
  n.recs <- length(t.values)
  transaction.actual <- rep(0, n.recs)
  transaction.expected <- rep(0, n.recs)
  rec.size <- rep(0, n.recs)
  
  for (tt in 1:n.recs) {
    this.t.x <- t.values[tt]
    this.rec <- which(t.x == this.t.x)
    n.this.rec <- length(this.rec)
    rec.size[tt] <- n.this.rec
    transaction.actual[tt] <- sum(x.star[this.rec])/n.this.rec
    transaction.expected[tt] <- sum(pnbd2.ConditionalExpectedTransactions(params, 
                                                                          T.star, x[this.rec], t.x[this.rec], T.cal[this.rec]))/n.this.rec
  }
  
  comparison <- rbind(transaction.actual, transaction.expected, rec.size)
  colnames(comparison) <- round(t.values, 3)
  
  bins <- seq(1, ceiling(max(t.x)))
  n.bins <- length(bins)
  actual <- rep(0, n.bins)
  expected <- rep(0, n.bins)
  bin.size <- rep(0, n.bins)
  
  x.labels <- NULL
  if (is.null(xticklab) == FALSE) {
    x.labels <- xticklab
  } else {
    x.labels <- 1:(n.bins)
  }
  point.labels <- rep("", n.bins)
  point.y.val <- rep(0, n.bins)
  for (ii in 1:n.bins) {
    if (ii < n.bins) {
      this.bin <- which(as.numeric(colnames(comparison)) >= (ii - 1) & as.numeric(colnames(comparison)) < 
                          ii)
    } else if (ii == n.bins) {
      this.bin <- which(as.numeric(colnames(comparison)) >= ii - 1)
    }
    actual[ii] <- sum(comparison[1, this.bin])/length(comparison[1, this.bin])
    expected[ii] <- sum(comparison[2, this.bin])/length(comparison[2, this.bin])
    bin.size[ii] <- sum(comparison[3, this.bin])
  }
  
  ylim <- c(0, ceiling(max(c(actual, expected)) * 1.1))
  plot(actual, type = "l", xaxt = "n", col = 1, ylim = ylim, xlab = xlab, ylab = ylab, 
       main = title)
  lines(expected, lty = 2, col = 2)
  
  axis(1, at = 1:n.bins, labels = x.labels)
  legend("topleft", legend = c("Actual", "Model"), col = 1:2, lty = 1:2, lwd = 1)
  
  return(rbind(actual, expected, bin.size))
}

pnbd2.PlotTransactionRateHeterogeneity <- function(params, lim = NULL) {
  
  dc.check.model.params(c("r", "alpha", "s", "beta"), params, "pnbd2.PlotTransactionRateHeterogeneity")
  
  shape <- params[1]
  rate <- params[2]
  rate.mean <- round(shape/rate, 4)
  rate.var <- round(shape/rate^2, 4)
  if (is.null(lim)) {
    lim = qgamma(0.99, shape = shape, rate = rate)
  }
  x.axis.ticks <- seq(0, lim, length.out = 100)
  heterogeneity <- dgamma(x.axis.ticks, shape = shape, rate = rate)
  plot(x.axis.ticks, heterogeneity, type = "l", xlab = "Transaction Rate", ylab = "Density", 
       main = "Heterogeneity in Transaction Rate")
  mean.var.label <- paste("Mean:", rate.mean, "    Var:", rate.var)
  mtext(mean.var.label, side = 3)
  return(rbind(x.axis.ticks, heterogeneity))
}

pnbd2.PlotDropoutRateHeterogeneity <- function(params, lim = NULL) {
  
  dc.check.model.params(c("r", "alpha", "s", "beta"), params, "pnbd2.PlotDropoutRateHeterogeneity")
  
  shape <- params[3]
  rate <- params[4]
  rate.mean <- round(shape/rate, 4)
  rate.var <- round(shape/rate^2, 4)
  if (is.null(lim)) {
    lim = qgamma(0.99, shape = shape, rate = rate)
  }
  x.axis.ticks <- seq(0, lim, length.out = 100)
  heterogeneity <- dgamma(x.axis.ticks, shape = shape, rate = rate)
  plot(x.axis.ticks, heterogeneity, type = "l", xlab = "Dropout rate", ylab = "Density", 
       main = "Heterogeneity in Dropout Rate")
  mean.var.label <- paste("Mean:", rate.mean, "    Var:", rate.var)
  mtext(mean.var.label, side = 3)
  return(rbind(x.axis.ticks, heterogeneity))
}

pnbd2.ExpectedCumulativeTransactions <- function(params, T.cal, T.tot, 
                                                 n.periods.final) {
  
  dc.check.model.params(c("r", "alpha", "s", "beta"), params, "pnbd2.ExpectedCumulativeTransactions")
  
  if (any(T.cal < 0) || !is.numeric(T.cal)) 
    stop("T.cal must be numeric and may not contain negative numbers.")
  
  if (length(T.tot) > 1 || T.tot < 0 || !is.numeric(T.tot)) 
    stop("T.cal must be a single numeric value and may not be negative.")
  if (length(n.periods.final) > 1 || n.periods.final < 0 || !is.numeric(n.periods.final)) 
    stop("n.periods.final must be a single numeric value and may not be negative.")
  
  ## Divide up time into equal intervals
  intervals <- seq(T.tot/n.periods.final, T.tot, length.out = n.periods.final)
  
  cust.birth.periods <- max(T.cal) - T.cal
  
  expected.transactions <- sapply(intervals, function(interval) {
    if (interval <= min(cust.birth.periods)) 
      return(0)
    sum(pnbd2.Expectation(params, interval - cust.birth.periods[cust.birth.periods <= 
                                                                  interval]))
  })
  
  return(expected.transactions)
}


pnbd2.PlotTrackingCum <- function(params, T.cal, T.tot, actual.cu.tracking.data, 
                                  xlab = "Week", ylab = "Cumulative Transactions", xticklab = NULL, title = "Tracking Cumulative Transactions") {
  
  dc.check.model.params(c("r", "alpha", "s", "beta"), params, "pnbd2.Plot.PlotTrackingCum")
  
  if (any(T.cal < 0) || !is.numeric(T.cal)) 
    stop("T.cal must be numeric and may not contain negative numbers.")
  if (any(actual.cu.tracking.data < 0) || !is.numeric(actual.cu.tracking.data)) 
    stop("actual.cu.tracking.data must be numeric and may not contain negative numbers.")
  
  if (length(T.tot) > 1 || T.tot < 0 || !is.numeric(T.tot)) 
    stop("T.cal must be a single numeric value and may not be negative.")
  
  actual <- actual.cu.tracking.data
  expected <- pnbd2.ExpectedCumulativeTransactions(params, T.cal, T.tot, length(actual))
  
  cu.tracking.comparison <- rbind(actual, expected)
  
  ylim <- c(0, max(c(actual, expected)) * 1.05)
  plot(actual, type = "l", xaxt = "n", xlab = xlab, ylab = ylab, col = 1, ylim = ylim, 
       main = title)
  lines(expected, lty = 2, col = 2)
  if (is.null(xticklab) == FALSE) {
    if (ncol(cu.tracking.comparison) != length(xticklab)) {
      stop("Plot error, xticklab does not have the correct size")
    }
    axis(1, at = 1:ncol(cu.tracking.comparison), labels = xticklab)
  } else {
    axis(1, at = 1:length(actual), labels = 1:length(actual))
  }
  abline(v = max(T.cal), lty = 2)
  
  legend("bottomright", legend = c("Actual", "Model"), col = 1:2, lty = 1:2, lwd = 1)
  
  return(cu.tracking.comparison)
}

pnbd2.PlotTrackingInc <- function(params, T.cal, T.tot, actual.inc.tracking.data, 
                                  xlab = "Week", ylab = "Transactions", xticklab = NULL, title = "Tracking Weekly Transactions") {
  
  dc.check.model.params(c("r", "alpha", "s", "beta"), params, "pnbd2.Plot.PlotTrackingCum")
  
  if (any(T.cal < 0) || !is.numeric(T.cal)) 
    stop("T.cal must be numeric and may not contain negative numbers.")
  if (any(actual.inc.tracking.data < 0) || !is.numeric(actual.inc.tracking.data)) 
    stop("actual.inc.tracking.data must be numeric and may not contain negative numbers.")
  
  if (length(T.tot) > 1 || T.tot < 0 || !is.numeric(T.tot)) 
    stop("T.cal must be a single numeric value and may not be negative.")
  
  actual <- actual.inc.tracking.data
  expected <- dc.CumulativeToIncremental(pnbd2.ExpectedCumulativeTransactions(params, 
                                                                              T.cal, T.tot, length(actual)))
  
  ylim <- c(0, max(c(actual, expected)) * 1.05)
  plot(actual, type = "l", xaxt = "n", xlab = xlab, ylab = ylab, col = 1, ylim = ylim, 
       main = title)
  lines(expected, lty = 2, col = 2)
  if (is.null(xticklab) == FALSE) {
    if (length(actual) != length(xticklab)) {
      stop("Plot error, xticklab does not have the correct size")
    }
    axis(1, at = 1:length(actual), labels = xticklab)
  } else {
    axis(1, at = 1:length(actual), labels = 1:length(actual))
  }
  abline(v = max(T.cal), lty = 2)
  
  legend("topright", legend = c("Actual", "Model"), col = 1:2, lty = 1:2, lwd = 1)
  
  return(rbind(actual, expected))
}


pnbd2.DERT <- function(params, x, t.x, T.cal, d) {
  
  max.length <- max(length(x), length(t.x), length(T.cal))
  
  if (max.length%%length(x)) 
    warning("Maximum vector length not a multiple of the length of x")
  if (max.length%%length(t.x)) 
    warning("Maximum vector length not a multiple of the length of t.x")
  if (max.length%%length(T.cal)) 
    warning("Maximum vector length not a multiple of the length of T.cal")
  
  dc.check.model.params(c("r", "alpha", "s", "beta"), params, "pnbd2.DERT")
  
  if (any(x < 0) || !is.numeric(x)) 
    stop("x must be numeric and may not contain negative numbers.")
  if (any(t.x < 0) || !is.numeric(t.x)) 
    stop("t.x must be numeric and may not contain negative numbers.")
  if (any(T.cal < 0) || !is.numeric(T.cal)) 
    stop("T.cal must be numeric and may not contain negative numbers.")
  
  
  x <- rep(x, length.out = max.length)
  t.x <- rep(t.x, length.out = max.length)
  T.cal <- rep(T.cal, length.out = max.length)
  
  r <- params[1]
  alpha <- params[2]
  s <- params[3]
  beta <- params[4]
  
  maxab = max(alpha, beta)
  absab = abs(alpha - beta)
  param2 = s + 1
  if (alpha < beta) {
    param2 = r + x
  }
  part1 <- (alpha^r * beta^s/gamma(r)) * gamma(r + x)
  part2 <- 1/((alpha + T.cal)^(r + x) * (beta + T.cal)^s)
  if (absab == 0) {
    F1 <- 1/((maxab + t.x)^(r + s + x))
    F2 <- 1/((maxab + T.cal)^(r + s + x))
  } else {
    F1 <- hyperg_2F1(r + s + x, param2, r + s + x + 1, absab/(maxab + t.x))/((maxab + 
                                                                                t.x)^(r + s + x))
    F2 <- hyperg_2F1(r + s + x, param2, r + s + x + 1, absab/(maxab + T.cal))/((maxab + 
                                                                                  T.cal)^(r + s + x))
  }
  
  likelihood = part1 * (part2 + (s/(r + s + x)) * (F1 - F2))
  
  z <- d * (beta + T.cal)
  
  tricomi.part.1 = ((z)^(1 - s))/(s - 1) * hyperg_1F1(1, 2 - s, z)
  tricomi.part.2 = gamma(1 - s) * hyperg_1F1(s, s, z)
  tricomi = tricomi.part.1 + tricomi.part.2
  
  result <- exp(r * log(alpha) + s * log(beta) + (s - 1) * log(d) + lgamma(r + 
                                                                             x + 1) + log(tricomi) - lgamma(r) - (r + x + 1) * log(alpha + T.cal) - log(likelihood))
  
  return(result)
}

pnbd2.Plot.DERT <- function(params, x, t.x, T.cal, d, type = "wireframe") {
  
  dc.check.model.params(c("r", "alpha", "s", "beta"), params, "pnbd2.Plot.DERT")
  if (any(x < 0) || !is.numeric(x)) 
    stop("x must be numeric and may not contain negative numbers.")
  if (any(t.x < 0) || !is.numeric(t.x)) 
    stop("t.x must be numeric and may not contain negative numbers.")
  if (length(T.cal) > 1 || T.cal < 0 || !is.numeric(T.cal)) 
    stop("T.cal must be a single numeric value and may not be negative.")
  if (!(type == "persp" || type == "contour")) {
    stop("The plot type in pnbd2.Plot.DERT must be either 'wireframe' or 'contour'.")
  }
  
  DERT <- matrix(NA, length(t.x), length(x))
  rownames(DERT) <- t.x
  colnames(DERT) <- x
  for (i in 1:length(t.x)) {
    for (j in 1:length(x)) {
      DERT[i, j] <- pnbd2.DERT(params, x[j], t.x[i], T.cal, d)
    }
  }
  
  if (type == "contour") {
    if (max(DERT, na.rm = TRUE) <= 10) {
      levels <- 1:max(DERT, na.rm = TRUE)
    } else if (max(DERT, na.rm = TRUE) <= 20) {
      levels <- c(1, seq(2, max(DERT, na.rm = TRUE), 2))
    } else {
      levels <- c(1, 2, seq(5, max(DERT, na.rm = TRUE), 5))
    }
    contour(x = t.x, y = x, z = DERT, levels = levels, xlab = "Recency", ylab = "Frequency", 
            main = "Iso-Value Representation of DERT")
  }
  
  if (type == "persp") {
    persp(x = t.x, y = x, z = DERT, theta = -30, phi = 20, axes = TRUE, ticktype = "detailed", 
          nticks = 5, main = "DERT as a Function of Frequency and Recency", shade = 0.5, 
          xlab = "Recency", ylab = "Frequency", zlab = "Discounted expected residual transactions")
  }
  return(DERT)
}

############################
## end functions        ##
############################


if(!exists("getCLVReservationData", mode="function")) source("C:\\Users\\babbenante\\documents\\My Stuff\\code\\utils\\load data.r")


#reservations.all<-getCLVReservationData("Reservations-CLV-2012-FEB.txt")
reservations.all<-getCLVReservationData("Reservations-CLV.txt")

members.all<-getMemberSurvivalData("Members - 2011-13 Joiners.txt")
members.all<-members.all%>%
  select(MEMBER_ID,LEVEL_5_KEY,SEGMENT_CLASS,FEE_FREQ)

reservation.subset<-reservations.all%>%
  filter(JOIN_COHORT<=as.Date("2011-08-01") & JOIN_COHORT<=as.Date("2013-07-31"))

reservation.subset<-merge(reservation.subset,members.all,all.x=TRUE,by="MEMBER_ID")

reservation.subset<-reservation.subset%>%
  filter(SEGMENT_CLASS=='Consumer' & FEE_FREQ=='annual')

ZipELog<-data.frame(reservation.subset)
colnames(ZipELog)[1]<-'cust'
colnames(ZipELog)[3]<-'date'
colnames(ZipELog)[4]<-'sales'
ZipELog<-ZipELog%>%
  select(cust,date,sales)
elog <- dc.MergeTransactionsOnSameDate(ZipELog)
#end.of.cal.period <- as.Date("2013-01-01")
#elog.cal <- elog[which(elog$date <= end.of.cal.period), ]
#split.data <- dc.SplitUpElogForRepeatTrans(elog.cal)
#clean.elog <- split.data$repeat.trans.elog
#freq.cbt <- dc.CreateFreqCBT(clean.elog)
#tot.cbt <- dc.CreateFreqCBT(elog.cal)
#cal.cbt <- dc.MergeCustomers(tot.cbt, freq.cbt)
#birth.periods <- split.data$cust.data$birth.per
#last.dates <- split.data$cust.data$last.date
#cal.cbs.dates <- data.frame(birth.periods, last.dates, end.of.cal.period)
#cal.cbs <- dc.BuildCBSFromCBTAndDates(cal.cbt, cal.cbs.dates, per="month")


cal.cbs = dc.ElogToCbsCbt(elog
                          ,per="month"
                          ,T.cal=as.Date("2013-12-31")
                          ,T.tot=as.Date("2016-07-31")
                          ,cohort.birth.per = as.Date("2013-07-31")
#                          ,statistic="average.spend"
                          )
#params = pnbd2.EstimateParameters(cal.cbs$cal$cbs)

#############################################################
# BG/NBD
#############################################################

params <- bgnbd.EstimateParameters(cal.cbs$cal$cbs);
params
LL <- bgnbd.cbs.LL(params, cal.cbs$cal$cbs);
p.matrix <- c(params, LL);
for (i in 1:9){
  params <- bgnbd.EstimateParameters(cal.cbs$cal$cbs, params);
  LL <- bgnbd.cbs.LL(params, cal.cbs$cal$cbs);
  p.matrix.row <- c(params, LL);
  p.matrix <- rbind(p.matrix, p.matrix.row);
}
colnames(p.matrix) <- c("r", "alpha", "a", "b", "LL");
rownames(p.matrix) <- 1:10;
p.matrix;
bgnbd.PlotTransactionRateHeterogeneity(params)
bgnbd.PlotDropoutRateHeterogeneity(params)
bgnbd.Expectation(params, t=36);
my.member<-"106562366"
cal.cbs$cal$cbs[my.member,]
x <- cal.cbs$cal$cbs[my.member, "x"]
t.x <- cal.cbs$cal$cbs[my.member, "t.x"]
T.cal <- cal.cbs$cal$cbs[my.member, "T.cal"]
bgnbd.ConditionalExpectedTransactions(params, T.star = 36,
                                      x, t.x, T.cal)
bgnbd.PlotFrequencyInCalibration(params, cal.cbs$cal$cbs, 9)

#elog2 <- dc.SplitUpElogForRepeatTrans(elog)$repeat.trans.elog;
#x.star <- rep(0, nrow(cal.cbs));
#cal.cbs <- cbind(cal.cbs, x.star);
#elog2.custs <- elog2$cust;
#for (i in 1:nrow(cal.cbs)){
#  current.cust <- rownames(cal.cbs)[i]
#  tot.cust.trans <- length(which(elog2.custs == current.cust))
#  cal.trans <- cal.cbs[i, "x"]
#  cal.cbs[i, "x.star"] <- tot.cust.trans - cal.trans
#}

T.star <- 30 # length of the holdout period
censor <- 6 # This censor serves the same purpose described above
#x.star <- cal.cbs[,"x.star"]
comp <- bgnbd.PlotFreqVsConditionalExpectedFrequency(params, T.star,
                                                     cal.cbs$cal$cbs, cal.cbs$holdout$cbs, censor)
rownames(comp) <- c("act", "exp", "bin")
comp

bgnbd.Expectation(params, t=1:36)
bgnbd.pmf.General(params, 1,9,7)


tot.cbt <- dc.CreateFreqCBT(elog)
d.track.data <- rep(0, 1080)
origin <- as.Date("2010-01-01")
for (i in colnames(tot.cbt)){
  date.index <- difftime(as.Date(i), origin) + 1;
  d.track.data[date.index] <- sum(tot.cbt[,i]);
}
m.track.data <- rep(0, 72)
for (j in 1:72){
  m.track.data[j] <- sum(d.track.data[(j*30-29):(j*30)])
}

T.cal <- cal.cbs$cal$cbs[,"T.cal"]
T.tot <- 72
#n.periods.final <- 36
inc.tracking <- bgnbd.PlotTrackingInc(params, T.cal,
                                      T.tot, m.track.data)

cum.tracking.data <- cumsum(m.track.data)
cum.tracking <- bgnbd.PlotTrackingCum(params, T.cal,
                                      T.tot, cum.tracking.data)

avg.spend.df<-reservation.subset%>%
  dplyr::filter(RESERVATION_DATE>=as.Date("2010-02-01") & RESERVATION_DATE<=as.Date("2012-07-31"))%>%
  group_by(MEMBER_ID)%>%
  dplyr::summarise(trans=n()
                   ,rev=mean(REVENUE))
avg.spend<-avg.spend.df[["rev"]]
tot.trans<-avg.spend.df[["trans"]]
names(avg.spend)<-avg.spend.df[["MEMBER_ID"]]
tot.trans.df<-reservations.all%>%
  dplyr::filter(RESERVATION_DATE>=as.Date("2010-04-01") & RESERVATION_DATE<=as.Date("2010-09-30"))%>%
  group_by(MEMBER_ID)%>%
  dplyr::summarise(trans=n())
tot.trans<-tot.trans.df[["trans"]]
names(tot.trans)<-tot.trans.df[["MEMBER_ID"]]

ave.spend <- cal.cbs$m.x
tot.trans <- cal.cbs[,"x"]
# There will be many warnings due to the zeroes that are
# included in the data above. To avoid them, use the following:
# (see example for spend.LL)
avg.spend <- avg.spend[which(tot.trans > 0)]
tot.trans <- tot.trans[which(tot.trans > 0)]
# We will let the spend function use default starting parameters
params<-spend.EstimateParameters(avg.spend,tot.trans)

spend.expected.value(params, m.x=75, x=6)
spend.marginal.likelihood(params, m.x=75, x=6)

m.x.vector<-avg.spend
x.vector<-tot.trans
spend.plot.average.transaction.value(params, m.x.vector, x.vector,
                                     xlab = "Average Transaction Value",
                                     ylab = "Marginal Distribution of Average Transaction Value",
                                     title = "Actual vs. Expected Average Transaction Value Across Customers")





#############################################################
#paretto/nbd 
#############################################################

params <- pnbd2.EstimateParameters(cal.cbs$cal$cbs)
params
LL <- pnbd2.cbs.LL(params, cal.cbs$cal$cbs)

p.matrix <- c(params, LL);
for (i in 1:5){
  params <- pnbd2.EstimateParameters(cal.cbs$cal$cbs, params);
  LL <- pnbd2.cbs.LL(params, cal.cbs$cal$cbs);
  p.matrix.row <- c(params, LL);
  p.matrix <- rbind(p.matrix, p.matrix.row);
}
colnames(p.matrix) <- c("r", "alpha", "s", "beta", "LL");
rownames(p.matrix) <- 1:6;
p.matrix
pnbd2.PlotTransactionRateHeterogeneity(params)
pnbd2.PlotDropoutRateHeterogeneity(params)
pnbd2.Expectation(params, t=36)
my.member<-"102419983"
cal.cbs$cal$cbs[my.member,]
x <- cal.cbs$cal$cbs[my.member, "x"]
t.x <- cal.cbs$cal$cbs[my.member, "t.x"]
T.cal <- cal.cbs$cal$cbs[my.member, "T.cal"]
pnbd.ConditionalExpectedTransactions(params
                                     ,T.star = 36
                                     ,x, t.x, T.cal)
pnbd.PAlive(params, x, t.x, T.cal)

pnbd.PlotFrequencyInCalibration(params, cal.cbs$cal$cbs, 9)


T.star <- 30 # length of the holdout period
censor <- 6 # This censor serves the same purpose described above
#x.star <- cal.cbs[,"x.star"]
comp <- pnbd.PlotFreqVsConditionalExpectedFrequency(params
                                                     ,T.star,
                                                     cal.cbs$cal$cbs
                                                     ,cal.cbs$holdout$cbs
                                                     ,censor)



T.star <- 31 # length of the holdout period
censor <- 6 # This censor serves the same purpose described above
x.star <- cal.cbs[,"x.star"]
comp <- pnbd.PlotFreqVsConditionalExpectedFrequency(params
                                                    ,T.star
                                                    ,cal.cbs
                                                    ,x.star
                                                    ,censor)
rownames(comp) <- c("act", "exp", "bin")
comp
tot.cbt <- dc.CreateFreqCBT(elog)
# ...Completed Freq CBT
d.track.data <- rep(0, 1080)
origin <- as.Date("2012-02-01")
for (i in colnames(tot.cbt)){
  date.index <- difftime(as.Date(i), origin) + 1;
  d.track.data[date.index] <- sum(tot.cbt[,i]);
}
w.track.data <- rep(0, 36)
for (j in 1:36){
  w.track.data[j] <- sum(d.track.data[(j*30-29):(j*30)])
}

T.cal <- cal.cbs$cal$cbs[,"T.cal"]
T.tot <- 36
n.periods.final <- 36
inc.tracking <- pnbd.PlotTrackingInc(params, T.cal,
                                     T.tot, w.track.data,
                                     n.periods.final)

cum.tracking.data <- cumsum(w.track.data)
cum.tracking <- pnbd.PlotTrackingCum(params, T.cal,
                                     T.tot, cum.tracking.data,
                                     n.periods.final)


