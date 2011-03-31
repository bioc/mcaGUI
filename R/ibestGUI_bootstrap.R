bootstrap = function(abundance_table, confidence_level, iterations, bs_mean, bs_median, bs_standard_deviation, bs_coefficient_of_variation) {

  #Functions usual.jack and norm.enter used from sources cited below:
  #Angelo Canty and Brian Ripley (2010). boot: Bootstrap R (S-Plus) Functions. R package version 1.2-43.
  #Davison, A. C. & Hinkley, D. V. (1997) Bootstrap Methods and Their Applications. Cambridge University Press, Cambridge. ISBN 0-521-57391-2

  data = abundance_table

  usual.jack <- function(data, stat, stype="w", index=1, strata=rep(1,n),...)
  #
  #  Function to use the normal (delete 1) jackknife method to estimate the
  #  empirical influence values
  #
  {
      n <- NROW(data)
      l <- rep(0,n)
      strata <- tapply(strata,as.numeric(strata))
      if (stype == "w") {
          w0 <- rep(1,n)/table(strata)[strata]
          tobs <- stat(data, w0, ...)[index]
          for (i in seq_len(n)) {
              w1 <- w0
              w1[i] <- 0
              gp <- strata==strata[i]
              w1[gp] <- w1[gp]/sum(w1[gp])
              l[i] <- (sum(gp)-1)*(tobs - stat(data,w1, ...)[index])
          }
      }
      else if (stype == "f") {
          f0 <- rep(1,n)
          tobs <- stat(data, f0,...)[index]
          for (i in seq_len(n)) {
              f1 <- f0
              f1[i] <- 0
              gp <- strata==strata[i]
              l[i] <- (sum(gp)-1)*(tobs - stat(data, f1, ...)[index])
          }
      }
      else {
          i0 <- seq_len(n)
          tobs <- stat(data, i0,...)[index]
          for (i in seq_len(n)) {
              i1 <- i0[-i]
              gp <- strata==strata[i]
              l[i] <- (sum(gp)-1)*(tobs - stat(data, i1, ...)[index])
          }
      }
      l
  }

  norm.inter <- function(t,alpha)
  #
  #  Interpolation on the normal quantile scale.  For a non-integer
  #  order statistic this function interpolates between the surrounding
  #  order statistics using the normal quantile scale.  See equation
  #  5.8 of Davison and Hinkley (1997)
  #
  {
      t <- t[is.finite(t)]
      R <- length(t)
      rk <- (R+1)*alpha
      if (!all(rk>1 & rk<R))
          warning("extreme order statistics used as endpoints")
      k <- trunc(rk)
      inds <- seq_along(k)
      out <- inds
      kvs <- k[k>0 & k<R]
      tstar <- sort(t,partial=sort(union(c(1,R),c(kvs,kvs+1))))
      ints <- (k == rk)
      if (any(ints)) out[inds[ints]] <- tstar[k[inds[ints]]]
        out[k==0] <- tstar[1L]
        out[k==R] <- tstar[R]
      not <- function(v) xor(rep(TRUE,length(v)),v)
      temp <- inds[not(ints) & k!=0 & k!=R]
      temp1 <- qnorm(alpha[temp])
      temp2 <- qnorm(k[temp]/(R+1))
      temp3 <- qnorm((k[temp]+1)/(R+1))
      tk <- tstar[k[temp]]
      tk1 <- tstar[k[temp]+1L]
      out[temp] <- tk + (temp1-temp2)/(temp3-temp2)*(tk1 - tk)
      cbind(round(rk,2),out)
  }

#--------------------------------------------------#

#mean
  bca.mean <- function(data, num, conf, distrbution_histogram) {
    data <- as.vector(data)
    resamples <- lapply(1:num, function(i) sample(data, replace=T))
    r.stat <- sapply(resamples, mean)
    std.err <- sqrt(var(r.stat))

    theta_hat = mean(data)
    B = num
    theta_hat_b = r.stat

    alpha <- (1+c(-conf,conf))/2
    alpha
    zalpha <- qnorm(alpha)
    zalpha

    z0_hat = qnorm(sum(theta_hat_b < theta_hat)/length(theta_hat_b))
    z0_hat
  
    statistic <- function(x, d) {
      return(mean(x[d]))
    }

    jack_knife<- usual.jack(data=data, stat=statistic, stype="i", index=1, strata=rep(1,length(data)))
    a_hat <- sum(jack_knife^3)/(6*sum(jack_knife^2)^1.5)
    adj_alpha = pnorm((z0_hat+(z0_hat+zalpha))/(1-a_hat*(z0_hat+zalpha)))
    bca_ci <- norm.inter(theta_hat_b,adj_alpha)

    print("Bootstap estimate of the mean")
    print(mean(r.stat))
    print(paste(100*conf, "% BCa confidence intervals"))
    print(c(bca_ci[1,2], bca_ci[2,2]))
  
    if(distrbution_histogram=="TRUE") {
      dev.new()
      hist(r.stat, , main="Boootstrap Distribution of the Mean")
    }
  
    #return(list(std.err=std.err, resamples=resamples, stats=r.stat, bootstrap_mean = mean(r.stat), bca_conf=c(bca_ci[1,2], bca_ci[2,2])))
  }

  if(bs_mean == "TRUE") {
    #data = c(0.77, 1.17, 2.79, 3.13, 3.31, 3.70, 4.13, 5.28, 5.84, 7.38, 7.75, 8.25, 9.00, 10.12, 11.51, 15.82, 18.28, 21.73, 32.74, 38.28)
    try(mean <- bca.mean(data=data, num=iterations, conf=confidence_level, distrbution_histogram="TRUE"))
    #names(mean)
  }

  #Median
  bca.median <- function(data, num, conf, distrbution_histogram) {
    data <- as.vector(data)
    resamples <- lapply(1:num, function(i) sample(data, replace=T))
    r.stat <- sapply(resamples, median)
    std.err <- sqrt(var(r.stat))

    theta_hat = median(data)
    B = num
    theta_hat_b = r.stat

    alpha <- (1+c(-conf,conf))/2
    alpha
    zalpha <- qnorm(alpha)
    zalpha

    z0_hat = qnorm(sum(theta_hat_b < theta_hat)/length(theta_hat_b))
    z0_hat

    statistic <- function(x, d) {
      return(median(x[d]))
    }

    jack_knife<- usual.jack(data=data, stat=statistic, stype="i", index=1, strata=rep(1,length(data)))
    a_hat <- sum(jack_knife^3)/(6*sum(jack_knife^2)^1.5)
    adj_alpha = pnorm((z0_hat+(z0_hat+zalpha))/(1-a_hat*(z0_hat+zalpha)))
    bca_ci <- norm.inter(theta_hat_b,adj_alpha)

    print("Bootstap estimate of the Median")
    print(mean(r.stat))
    print(paste(100*conf, "% BCa confidence intervals"))
    print(c(bca_ci[1,2], bca_ci[2,2]))

    if(distrbution_histogram=="TRUE") {
      dev.new()
      hist(r.stat, , main="Boootstrap Distribution of the Median")
    }

    #return(list(std.err=std.err, resamples=resamples, stats=r.stat, bca_conf=c(bca_ci[1,2], bca_ci[2,2])))
  }

  if(bs_median == "TRUE") {
    #data = c(0.77, 1.17, 2.79, 3.13, 3.31, 3.70, 4.13, 5.28, 5.84, 7.38, 7.75, 8.25, 9.00, 10.12, 11.51, 15.82, 18.28, 21.73, 32.74, 38.28)
    #data=c(7,11,15,16,20,22,24,25,29,33,34,37,41,42,49,57,66,71,84,90)
    try(median <- bca.median(data=data, num=iterations, conf=confidence_level, distrbution_histogram="TRUE"))
    #names(median)
  }

  #Standard Error
  bca.sd <- function(data, num, conf, distrbution_histogram) {
    data <- as.vector(data)
    resamples <- lapply(1:num, function(i) sample(data, replace=T))
    r.stat <- sapply(resamples, sd)
    std.err <- sqrt(var(r.stat))

    theta_hat = sd(data)
    B = num
    theta_hat_b = r.stat

    alpha <- (1+c(-conf,conf))/2
    alpha
    zalpha <- qnorm(alpha)
    zalpha

    z0_hat = qnorm(sum(theta_hat_b < theta_hat)/length(theta_hat_b))
    z0_hat

    statistic <- function(x, d) {
      return(sd(x[d]))
    }

    jack_knife<- usual.jack(data=data, stat=statistic, stype="i", index=1, strata=rep(1,length(data)))
    a_hat <- sum(jack_knife^3)/(6*sum(jack_knife^2)^1.5)
    adj_alpha = pnorm((z0_hat+(z0_hat+zalpha))/(1-a_hat*(z0_hat+zalpha)))
    bca_ci <- norm.inter(theta_hat_b,adj_alpha)

    print("Bootstap estimate of the Standard Deviation")
    print(mean(r.stat))
    print(paste(100*conf, "% BCa confidence intervals"))
    print(c(bca_ci[1,2], bca_ci[2,2]))

    if(distrbution_histogram=="TRUE") {
      dev.new()
      hist(r.stat, , main="Boootstrap Distribution of the Standard Deviation")
    }

    #return(list(std.err=std.err, resamples=resamples, stats=r.stat, bca_conf=c(bca_ci[1,2], bca_ci[2,2])))
  }

  if(bs_standard_deviation == "TRUE") {
    #data = c(0.77, 1.17, 2.79, 3.13, 3.31, 3.70, 4.13, 5.28, 5.84, 7.38, 7.75, 8.25, 9.00, 10.12, 11.51, 15.82, 18.28, 21.73, 32.74, 38.28)
    #data=c(7,11,15,16,20,22,24,25,29,33,34,37,41,42,49,57,66,71,84,90)
    try(sd <- bca.sd(data=data, num=iterations, conf=confidence_level, distrbution_histogram="TRUE"))
    #names(sd)
  }

  #Coefficient of Variation
  bca.cv <- function(data, num, conf, distrbution_histogram) {

    cv = function(data) {
      return(100*sd(data)/mean(data))
    }

    data <- as.vector(data)
    resamples <- lapply(1:num, function(i) sample(data, replace=T))
    r.stat <- sapply(resamples, cv)
    std.err <- sqrt(var(r.stat))

    theta_hat = cv(data)
    B = num
    theta_hat_b = r.stat

    alpha <- (1+c(-conf,conf))/2
    alpha
    zalpha <- qnorm(alpha)
    zalpha

    z0_hat = qnorm(sum(theta_hat_b < theta_hat)/length(theta_hat_b))
    z0_hat

    statistic <- function(x, d) {
      return(cv(x[d]))
    }

    jack_knife<- usual.jack(data=data, stat=statistic, stype="i", index=1, strata=rep(1,length(data)))
    a_hat <- sum(jack_knife^3)/(6*sum(jack_knife^2)^1.5)
    adj_alpha = pnorm((z0_hat+(z0_hat+zalpha))/(1-a_hat*(z0_hat+zalpha)))
    bca_ci <- norm.inter(theta_hat_b,adj_alpha)

    print("Bootstap estimate of the Coefficient of Variation")
    print(mean(r.stat))
    print(paste(100*conf, "% BCa confidence intervals"))
    print(c(bca_ci[1,2], bca_ci[2,2]))

    if(distrbution_histogram=="TRUE") {
      dev.new()
      hist(r.stat, main="Boootstrap Distribution of the Coefficient of Variation")
    }

    #return(list(std.err=std.err, resamples=resamples, stats=r.stat, bca_conf=c(bca_ci[1,2], bca_ci[2,2])))
  }

  if(bs_coefficient_of_variation=="TRUE") {
    #data = c(0.77, 1.17, 2.79, 3.13, 3.31, 3.70, 4.13, 5.28, 5.84, 7.38, 7.75, 8.25, 9.00, 10.12, 11.51, 15.82, 18.28, 21.73, 32.74, 38.28)
    #data=c(7,11,15,16,20,22,24,25,29,33,34,37,41,42,49,57,66,71,84,90)
    try(cv <- bca.cv(data=data, num=iterations, conf=confidence_level, distrbution_histogram="TRUE"))
    #names(cv)
  }

}

#data = c(0.77, 1.17, 2.79, 3.13, 3.31, 3.70, 4.13, 5.28, 5.84, 7.38, 7.75, 8.25, 9.00, 10.12, 11.51, 15.82, 18.28, 21.73, 32.74, 38.28)
#data = abundance2
#bootstrap(abundance_table=data, confidence_level=.95, iterations=5000, bs_mean="TRUE", bs_median="TRUE", bs_standard_deviation="TRUE", bs_coefficient_of_variation="TRUE")
#bootstrap(abundance_table=abundance2, confidence_level=0.95, iterations=5000, bs_mean=TRUE, bs_median=TRUE, bs_standard_deviation=TRUE, bs_coefficient_of_variation=TRUE)