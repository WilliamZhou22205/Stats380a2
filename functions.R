# functions.R
# STATS 380 Assignment 2

log_likelihood <- function(params, the_data) {
  if (!is.numeric(params))
    stop("Argument 'params' must be numeric.")
  if (length(params) != 3)
    stop("Argument 'params' must be of length 3.")
  if (!is.data.frame(the_data))
    stop("Argument 'the_data' must be a data frame.")
  if (!all(c("year", "deaths") %in% names(the_data)))
    stop("Argument 'the_data' must have columns 'year' and 'deaths'.")
  
  lambda1    <- params[1]
  lambda2    <- params[2]
  tc         <- params[3]
  lambda_vec <- ifelse(the_data$year < tc, lambda1, lambda2)
  sum(dpois(the_data$deaths, lambda = lambda_vec, log = TRUE))
}

guess <- function() {
  c(runif(1, 0, 5), runif(1, 0, 5), runif(1, 1851, 1962))
}

guess_and_check <- function(the_data) {
  params <- guess()
  logl   <- log_likelihood(params, the_data)
  list(params = params, logl = logl)
}


data    <- read.csv("coal.csv")
set.seed(380)
guesses <- lapply(1:10000, function(i) guess_and_check(data))
logls   <- sapply(guesses, function(g) g$logl)
top     <- max(logls)

t_c   <- numeric(1000)
found <- 0

while (found < 1000) {
  k <- sample(length(guesses), 1)
  if (runif(1) < exp(logls[k] - top)) {
    found      <- found + 1
    t_c[found] <- guesses[[k]]$params[3]
    cat("Good guesses found:", found, "\n")
  }
}

hist(t_c, breaks = 100, xlab = "t_c", main = "Plausible t_c values")