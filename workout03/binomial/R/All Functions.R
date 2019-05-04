###Private Checker Functions###

# title: check_prob
# description: test if an input "prob" is a valid probability value (i.e. 0 <= p <= 1)
# param: prob - probability value
# return: TRUE if prob is valid; Error message if prob is invalid
check_prob <- function(prob) {
  if (0 <= prob & prob <= 1 & length(prob) == 1) {
    return(TRUE)
  }
  else {
    stop("invalid prob value")
  }
}

# title: check_trials
# description: test if an input "trials" is a valid value for number of trials (i.e. n is a non-negative integer)
# param: trials - number of trials
# return: TRUE if trials is valid; Error message if trials is invalid
check_trials <- function(trials) {
  if((trials >= 0) & (trials %% 1 == 0)) {
    return(TRUE)
  }
  else {
    stop("invalid trials value")
  }
}

# title: check_success
# description: test if an input success is a valid value for number of successes (i.e. 0 <= k <= n)
# param: success - vector of non-negative integer(s) less than or equal to trials
# param: trials - number of trials
# return: TRUE if success is valid; Error message if success is invalid
check_success <- function(success, trials){
  for (s in 1:length(trials)){
    if (success[s] < 0 || success[s] > trials){
      stop("invalid success value")}

    else if (success[s] >= 0 && success[s]<= trials)
      return(TRUE)}
}


###Private Auxillary Functions###

# title: aux_mean
# description: determine the mean
# param: trials - number of trials
# param: prob - probability value
# return: mean value
aux_mean <- function(trials, prob) {
  return(trials * prob)
}

# title: aux_variance
# description: determine the variance
# param: trials - number of trials
# param: prob - probability value
# return: variance value
aux_variance <- function(trials, prob) {
  return(trials * prob * (1 - prob))
}
aux_variance(10, 0.3)

# title: aux_mode
# description: determine the mode
# param: trials - number of trials
# param: prob - probability value
# return: mode value
aux_mode <- function(trials, prob) {
  mode <- as.integer(trials * prob + prob)
  return(mode)
}

# title: aux_skewness
# description: determine the skewness
# param: trials - number of trials
# param: prob - probability value
# return: skewness value
aux_skewness <- function(trials, prob) {
  return((1 - 2 * prob) / (sqrt(trials * prob * (1 - prob))))
}

# title: aux_kurtosis
# description: determine the kurtosis
# param: trials - number of trials
# param: prob - probability value
# return: kurtosis value
aux_kurtosis <- function(trials, prob) {
  return((1 - (6 * prob) * (1 - prob)) / (trials * prob * (1 - prob)))
}


###Main Functions###

#' @title bin_choose
#' @description calculates the number of combinations in which k successes can occur in n trials
#' @param n number of trials (numeric)
#' @param k number of successes (numeric)
#' @return number of combinations
#' @examples
#' bin_choose(5, 2)
#' bin_choose(5, 0)
#' bin_choose(5, 1:3)
#' @export
bin_choose <- function(n, k) {
  if(any(k > n)) {
    stop("k cannot be greater than n")
  }
  else {
    combo <- factorial(n) / (factorial(k) * factorial(n - k))
    return(combo)
  }
}

#' @title bin_probability
#' @description check for validity and calculuate probability
#' @param success vector of non-negative integer(s) less than or equal to trials (numeric)
#' @param trials number of trials (numeric)
#' @param prob probability value (numeric)
#' @return probability value
#' @examples
#' bin_probability(2, 5, 0.5)
#' bin_probability(0:2, 5, 0.5)
#' bin_probability(55, 100, 0.45)
#' @export
bin_probability <- function(success, trials, prob) {
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)
  probability <- bin_choose(trials, success) * (prob^success) * (1 - prob)^(trials - success)
  return(probability)
}

#' @title bin_distribution
#' @description create a data frame with the probability distribution
#' @param trials number of trials (numeric)
#' @param prob probability value (numeric)
#' @return data frame with the probability distribution
#' @examples
#' bin_distribution(5, 0.5)
#' @export
bin_distribution <- function(trials, prob) {
  success_value <- (0:trials)
  probability <- bin_probability(success_value, trials, prob)
  dat <- data.frame(success = success_value, probability = probability)
  class(dat) <- c("bindis", "data.frame")
  return(dat)
}

#' @export
plot.bindis <- function(dat, ...) {
  barplot(names.arg = dat$success, height = dat$probability, xlab = "success", ylab = "probability", col = "red3")
  title(main = "Success and Probability in Binominal Distribution", font.main = 4)
}

#' @title bin_cumulative
#' @description create a data frame with probability distribution and cumulative probabilities
#' @param trials number of trials (numeric)
#' @param prob probability value (numeric)
#' @return data frame with probability distribution and cumulative probabilities
#' @examples
#' bin_cumulative(5, 0.5)
#' @export
bin_cumulative <- function(trials, prob) {
  success_value <- (0:trials)
  probability <- bin_probability(success_value, trials, prob)
  dat <- data.frame(success = success_value, probability = probability)
  for (s in success_value + 1) {
    dat$cumulative[s] = sum(dat$probability[1:s])
  }
  return(structure(dat, class = c("bincum", "data.frame")))
}

#' @export
plot.bincum <- function(dat, ...) {
  plot(x = dat$success, y = dat$cumulative, type = "o", xlab = "successes", ylab = "probability", col = "red3")
  title(main = "Success and Probability in Cumulative Distribution", font.main = 4)
}

#' @title bin_variable
#' @description create a binomial random variable object
#' @param trials number of trials (numeric)
#' @param prob probability value (numeric)
#' @return binomial random variable project
#' @examples
#' bin_variable(5, 0.5)
#' @export
bin_variable <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  binvar <- list(trials = trials, prob = prob)
  class(binvar) <- c("binvar")
  return(binvar)
}

#' @export
print.binvar <- function(x) {
  print("Binomial Variable")
  print(" ")
  print("Parameters")
  print(paste("- number of trials:", x$trials))
  print(paste("- prob of succcess:", x$prob))
}

#' @export
summary.binvar <- function(x) {
  summary = list(trials = x$trials, prob = x$prob, mean = aux_mean(x$trials, x$prob),
                 variance = aux_variance(x$trials, x$prob), mode = aux_mode(x$trials, x$prob),
                 skewness = aux_skewness(x$trials, x$prob), kurtosis = aux_kurtosis(x$trials, x$prob))
  class(summary) = "summary.binvar"
  return(summary)
}

#' @export
print.summary.binvar <- function(x) {
  print("Summary Binomial")
  print("")
  print("Parameters")
  print(paste("- number of trials:", x$trials))
  print(paste("- prob of success :", x$prob))
  print("")
  print("Measures")
  print(paste("- mean    :", x$mean))
  print(paste("- variance:", x$variance))
  print(paste("- mode    :", x$mode))
  print(paste("- skewness:", x$skewness))
  print(paste("- kurtosis:", x$kurtosis))
}

#' @title bin_mean
#' @description determine the mean
#' @param trials number of trials (numeric)
#' @param prob probabiltiy value (numeric)
#' @return mean
#' @examples
#' bin_mean(10, 0.3)
#' @export
bin_mean <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials, prob))
}

#' @title bin_variance
#' @description determine the variance
#' @param trials number of trials (numeric)
#' @param prob probabiltiy value (numeric)
#' @return variance
#' @examples
#' bin_mean(10, 0.3)
#' @export
bin_variance <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials, prob))
}

#' @title bin_mode
#' @description determine the mode
#' @param trials number of trials (numeric)
#' @param prob probabiltiy value (numeric)
#' @return mode
#' @examples
#' bin_mode(10, 0.3)
#' @export
bin_mode <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials, prob))
}

#' @title bin_skewness
#' @description determine the skewness
#' @param trials number of trials (numeric)
#' @param prob probabiltiy value (numeric)
#' @return skewness
#' @examples
#' bin_skewness(10, 0.3)
#' @export
bin_skewness <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials, prob))
}

#' @title bin_kurtosis
#' @description determine the kurtosis
#' @param trials number of trials (numeric)
#' @param prob probabiltiy value (numeric)
#' @return kurtosis
#' @examples
#' bin_kurtosis(10, 0.3)
#' @export
bin_kurtosis <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials, prob))
}
