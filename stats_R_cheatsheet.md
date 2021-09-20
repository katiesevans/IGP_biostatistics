# R stats cheatsheet

## Summary statistics and general math
- `mean(x)`: calculate the mean of a vector `x`
    - `mean(c(1,2,3,4,5))`
    - `mean(c(1,2,3,4,NA), na.rm = T)`: make sure to use `na.rm = T` if your sample has NA values
- `sd(x)`: calculate the standard deviation of a vector `x`
    - `sd(c(1,2,3,4,5))`
- `summary(x)`: calculate six number summary statistics of a vector `x` (min, q1, median, mean, q3, max)
    - `summary(c(1,2,3,4,5))`
- `sum(x)`: calculate sum of vector `x`
    - `sum(c(1,2,3,4,5))`
- `prod(x)`: calculate product of vector `x`
    - `prod(c(4,5,2))`
- `sqrt(x)`: calculate the sqare root of a value
    - `sqrt(100)`

## Sampling from a distribution
- `seq(from, to, by)`: Generate a sequence of values
    - `seq(1, 10, 1)`
    - `seq(10, 1000, 20)`
- `rep(x, times = 1)`: generates a vector of `x` repeating `times` times
    - `rep(1, 10)`
- `set.seed(x)`: sets seed for random number generator. If you provide the same seed number before your analysis you will get the same 'random' sample each time.
    - `set.seed(1753)`
- `sample(x, size, replace = FALSE, prob = NULL)`: takes a sample of `size` from vector `x` with our without replacement
    - `sample(seq(1,10,1), 3)`
    - `sample(seq(1,10,1), 3, replace = T)`
    - Can use `prob` as a vector of probabilities for each element of `x`: `sample(c("H", "T"), 10, replace = T, prob = c(0.6, 0.4))`

## Generating a distribution
**Binomial distribution**
- `dbinom(x, size, prob)`: generates a binomial distribution of `size` from vector `x` with `prob` probability of success for each trial
    - `dbinom(seq(1,10), 10, 0.37)`

**Normal distribution**
- `rnorm(n, mean = 0, sd = 1)`: generates a normal distribution of size `n` with given `mean` and `sd`
    - `rnorm(20)`
    - `rnorm(100, mean = 3, sd = 2.5)`
- `dnorm(x, mean = 0, sd = 1)`: gives the density of a normal distribution of values `x`
    - *example*
- `pnorm(q, mean = 0, sd = 1)`: calculates the distribution function of a normal distribution with given `mean` and `sd` and returns the area under the curve at z value `q`
    - `pnorm(1.96)`

**Skewed distribution**
- `rbeta()`

## Hypothesis testing
**Calculate the t statistic**
- `qt(prob, df)`: calculates the Student's t statistic given the area under the curve `prob` and the degrees of freedom `df`
    - `qt(0.975, 13)`
**t test**
- `t.test(x)`: performs a one or two sample Student's t-test on vectors of data
    - *examples*


## Outside links:
* [Statistical Analysis with R For Dummies Cheat Sheet](https://www.dummies.com/programming/r/statistical-analysis-with-r-for-dummies-cheat-sheet/)
* [Basic statistics with R](https://cheatography.com/xeonkai/cheat-sheets/basic-statistics-with-r/)
* [General R cheatsheet](http://nicolascampione.weebly.com/uploads/1/9/4/1/19411255/r_cheat_sheet.pdf)
* [Essential stats cheatsheet](https://bioconnector.github.io/workshops/handouts/r-stats-cheatsheet.pdf)
* [RStudio cheatsheets](https://www.rstudio.com/resources/cheatsheets/)


