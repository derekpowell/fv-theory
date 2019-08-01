# helpers file for nutrition fv-theory project
devtools::source_gist(id = "f1994c0f8325abbc5d300600744af39d", filename="cbrm.R")

read_qualtrics <- function(fname, skip=3){
  # load raw qualtrics data
  header <- colnames(read.csv(fname, header = TRUE))
  df <- read.csv(fname, skip = skip, header = FALSE, col.names = header)
  
  # do some preliminary tidying
  df <- df %>%
    as_tibble()
  
  return(df)
}


rescale_beta <- function(x, lower, upper) {
  # rescales onto the open interval (0,1)
  # rescales over theoretical bounds of measurement, specified by "upper" and "lower"
  
  N <- length(x)
  res <- (x - lower) / (upper - lower)
  res <- (res * (N - 1) + .5) / N
  
  return(as.vector(res))
}


cumulative_intercept_prior <- function(k,
                                       sd = 2,
                                       alpha = 1, beta = 1,
                                       shape = c("flat", "middle", "rightskewed", "leftskewed")) {
  ## create priors on intercepts for cumulative() family regression
  ## assumes that probability of response options follow beta distribution 
  ## specified by a and b or by "shape" argument
  ## defaults to uniform distribution over response categories
  ##
  ## k = number of categories
  ## sd = std dev of normal over intercept
  ## a, b = alpha and beta specifying shape of distribution
  ## shape = string specifying pre-defined distribution shape
  
  shapes <- list("rightskewed" = c(2, 4), 
                 "leftskewed" = c(4, 2), 
                 "middle" = c(3, 3), 
                 "flat" = c(1, 1))
  
  if (length(shape) == 1) {
    alpha <- shapes[[shape]][1]
    beta <- shapes[[shape]][2]
  }
  
  intercepts <- seq(1, k - 1)
  prior_list <- lapply(intercepts, function(x) {
    center <- qlogis(pbeta(x / k, alpha, beta))
    p <- paste0("normal(", center, ",", sd, ")")
    
    return(set_prior(p, class = "Intercept", coef = as.character(x)))
  })
  
  return(Reduce(c,prior_list))
}


cohens_D <- function(x,y=NULL, group=NULL) {
  # calculate cohen's d comparing X and Y vectors, or for X grouped by Y (with two levels)
  if (!is.null(group)){
    vals <- unique(group)
    y <- x[group==vals[2]]
    x <- x[group==vals[1]]
  }
  
  m1 <- mean(x, na.rm=TRUE)
  s1 <- sd(x, na.rm=TRUE)
  m2 <- mean(y, na.rm=TRUE)
  s2 <- sd(y, na.rm=TRUE)
  
  s_pooled <- sqrt((s1^2 + s2^2)/2)
  d <- (m1 - m2)/s_pooled
  
  return(d)
}


## printing helpers

make_percent <- function(x){
  x <- x*100
  return(round(x,0))
}


rcor <- function(x,y=NULL, digits=3){
  # rounded correlation
  round(cor(x,y),digits)
}


get_term <- function(df, coefficient, statistic, round=TRUE){
  # get term from broom::tidy df
  value <- select(filter(df, term==coefficient), statistic)[[1]]
  
  if(!round){ return(value)} else {return(signif(value, 3))}
  
}


get_CI <- function(df, coefficient, round=TRUE){
  # get CI from broom::tidy df and format
  upper <- get_term(df, coefficient, "upper", round)
  lower <- get_term(df, coefficient, "lower", round)
  
  output <- paste0("[",lower,", ",upper,"]")
  return(output)
}

## Plotting helpers

# corr plot code
## ----------------------------------------

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}