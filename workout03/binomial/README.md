**About the Binomial Package**
"The binomial distribution is a probability distribution that summarizes the likelihood that a value will take one of two independent values under a given set of parameters or assumptions. The underlying assumptions of the binomial distribution are that there is only one outcome for each trial, that each trial has the same probability of success, and that each trial is mutually exclusive, or independent of each other." (Investopedia)


"The binomial distribution is a common discrete distribution used in statistics, as opposed to a continuous distribution, such as the normal distribution. This is because the binomial distribution only counts two states, typically represented as 1 (for a success) or 0 (for a failure) given a number of trials in the data. The binomial distribution, therefore, represents the probability for x successes in n trials, given a success probability p for each trial." (Investopedia)

In essence, the binomial distribution is extremely significant in probability theory and statistics. It is widely used as a model to calculate probabilities about the number of successes in a number of identical trials.

This package provides functions to be used when assessing the properties of the binomial distribution variable. 

**Functions**
*bin_choose()*
*bin_probability()*
*bin_distribution()*
*plot.bindis()*
*bin_cumulative()*
*plot.bincum()*
*bin_var()*
*print.binvar()*
*summary.binvar()*
*print.summary.binvar()*
*bin_mean()*
*bin_variance()*
*bin_mode()*
*bin_skewness()*
*bin_kurtosis()*

**Usage**

```{r}
bin_choose(n = 5, k = 2)

bin_probability(success = 5, trials = 10, prob = 0.25)

prob_distribution <- bin_distribution(trials = 10, prob = 0.25)
plot(prob_distribution)

cum_distribution <- bin_cumulative(trials = 10, prob = 0.25)
plot(cum_distribution)

list <- bin_variable (trials = 10, prob = 0.25)
list <- summary(list)

bin_mean(10, 0.25)
bin_variance(10, 0.25)
bin_mode(10, 0.25)
bin_skewness(10, 0.25)
bin_kurtosis(10, 0.25)
```



