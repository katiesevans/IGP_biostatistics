#########################
# Permutation example   #
#########################

# create data
set.seed(19)

dist1 <- rnorm(10, -1, 1)
dist2 <- rnorm(10, 1, 1)

# calculate p-value
t.test(dist1, dist2)

# create empty vector for randomization
random_p <- c()

# randomize data - many ways this can be achieved, this is just one way
for(i in 1:1000) {
    # create vector of all data from both distributions
    all_data <- c(dist1, dist2)
    
    # select 10 random observations for new dist1
    random_dist1 <- sample(all_data, 10)
    
    # remaining 10 observations for new dist2
    random_dist2 <- all_data[!all_data %in% random_dist1]
    
    # calculate ttest with randomized data and get p-value
    random_p <- c(random_p, t.test(random_dist1, random_dist2)$p.value)
}

# look at distribution of random_p
head(random_p)
summary(random_p)
hist(random_p)

# choose new thresh so that 5% are significant by chance
thresh <- quantile(random_p, 0.05)
thresh

# is our value significant?
t.test(dist1, dist2)$p.value < thresh
