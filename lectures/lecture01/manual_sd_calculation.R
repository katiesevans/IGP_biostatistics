calc_sd <- function(data) {
    # calculate the mean
    data_mean <- mean(data)
    
    # calculate the deviations
    deviations <- data - data_mean
    
    # square the deviations
    squared_deviations <- deviations**2
    
    # sum of the squared deviations
    sum_variance <- sum(squared_deviations)
    
    # population SD: divide by N
    pop_sd <- sqrt(sum_variance/length(data))
    
    # sample SD: divide by N-1 (degrees of freedom)
    samp_sd <- sqrt(sum_variance/(length(data)-1))
    
    return(paste0("Population SD: ", pop_sd, "; Sample SD: ", samp_sd))
}

# calcualte sd manually
calc_sd(ice_cream$puzzle)

# calculate sd with function from R
sd(ice_cream$puzzle)
