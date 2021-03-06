#################################
# Intro to R - quick
#################################

# create a vector
x <- c(3, 5, 10)

# create a dataframe
data.frame(color = c("red", "white"),
           count = c(45, 80))

# types of variables
# numeric
x <- 5
mode(x)

# character
x <- "5"
mode(x)

# logical/boolean
x <- TRUE
mode(x)


# R logic expressions
x <- 5
x == 5
x != 4
x > 2
x <= 10
x > 4 & x < 6
x > 6 | x < 7

# more logical expressions with vectors
x <- c(4, 6, 10)
x == 6
x > 5
10 %in% x
x[1]
x[2:3]
x[-1]
x[4] <- 15
which(x > 5)

##### Working with vectors
x <- c(2,5,10,12)

# Get the first element
x[1]

# Change the first element to a 9
x[1] <- 9

# Remove the second element (rename a new vector y)
y <- x[-2]

# keep the second and third elements (rename a new vector z)
z <- x[2:3]

# Get the elements that are > 5
x[x > 5]

# IFELSE statements
x <- c(2, 5, 10, 12)

# write an ifelse statement to print “less than 9” if the element is less than 9 
# or “greater than 9” if the element is greater than 9

ifelse(x < 9, "less than 9", "greater than 9")

# now look at if statements (different than ifelse)
x <- 10

# Write an ‘if' statement to print “less than 9” if the variable is less than 9 or 
# “greater than 9” if the variable is greater than 9

if(x < 9) {
  print("Less than 9")
} else {
  print("Greater than 9")
}

# FOR loops
x <- c(2, 5, 10, 12)

# Write an ‘for loop’ to add 5 to each element of vector x
for(i in 1:length(x)) {
  print(x[i] + 5)
}


#################################
# load data
#################################
# load dplyr package
library(dplyr)

# loads starwars dataframe from dplyr package
data(starwars)

# look at starwars dataframe in Rstudio
View(starwars)

#################################
# dplyr::filter()
#################################
# Filter the starwars dataframe to keep only humans
dplyr::filter(starwars, species == "Human")

# Filter the starwars dataframe to keep only male humans
dplyr::filter(starwars, species == "Human", gender == "male")

# Filter the starwars dataframe to keep humans and droids
dplyr::filter(starwars, species %in% c("Human", "Droid"))

# Filter the starwars dataframe to keep characters with a height less than 100
dplyr::filter(starwars, height < 100)

# Filter the starwars dataframe to keep all characters that are non-human
dplyr::filter(starwars, species != "Human")

# Filter the starwars dataframe to remove unknown species (i.e. species == NA)
dplyr::filter(starwars, !is.na(species))

#################################
# piping in tidyverse
#################################
# Filter the starwars dataframe to keep only male humans
starwars %>%
    dplyr::filter(species == "Human") %>%
    dplyr::filter(gender == "male")

#################################
# dplyr::select()
#################################
# Select only name, species, and films variables from the starwars dataframe
dplyr::select(starwars, name, species, films)

# Select all columns except the gender
dplyr::select(starwars, -gender)

# Select columns name, height, mass, hair color, and skin color
dplyr::select(starwars, name:skin_color)

# Select all columns except hair color, skin color, and eye color
dplyr::select(starwars, -hair_color, -skin_color, -eye_color)
dplyr::select(starwars, -(hair_color:eye_color))

# Select columns name, mass, skin color, hair color, and height (in order)
dplyr::select(starwars, name, mass, skin_color, hair_color, height)

# rename the "name" column to "character" and the "mass" column to "weight" using select()
dplyr::select(character = name, weight = mass)

# Keep:  height greater than 100 &
# Keep: humans &
# Remove: brown hair color &
# Remove: vehicles &
# Keep: name, homeworld, height, species, hair color

df <- starwars %>%
    dplyr::filter(height > 100) %>%
    dplyr::filter(species == "Human") %>%
    dplyr::filter(hair_color != "brown") %>%
    dplyr::select(-vehicles) %>%
    dplyr::select(name, homeworld, height, species, hair_color)

#################################
# dplyr::mutate()
#################################

# check out the starwars dataframe help page to see what the units are for mass and height
?starwars

# Calculate the BMI of all starwars characters
# Step 1: convert height in cm to height in m (save as new dataframe)
new_starwars <- dplyr::mutate(starwars, height_m = height / 100)

# Step 2: calculate BMI with formula: BMI = weight (kg) / [height (m)]^2
new_starwars <- starwars %>%
    dplyr::mutate(height_m = height / 100) %>%
    dplyr::mutate(bmi = mass / height_m^2)

# select only name, height, mass, height_m, and bmi and view dataframe
new_starwars <- new_starwars %>%
    dplyr::select(name, height, mass, height_m, bmi)
View(new_starwars)

# re-calculate the bmi by changing the height column to meters instead of creating a new column
new_starwars <- starwars %>%
    dplyr::mutate(height = height / 100) %>%
    dplyr::mutate(bmi = mass / height^2) %>%
    dplyr::select(name, height, mass, bmi)

# Make a new column to find the average height (in meters)
test <- starwars %>%
    dplyr::mutate(avg_height = mean(height)/100) %>%
    dplyr::select(name, height, mass, avg_height)

# check the help page for mean
?mean

# use na.rm = TRUE in the mean() function
test <- starwars %>%
    dplyr::mutate(avg_height = mean(height, na.rm = TRUE)/100) %>%
    dplyr::select(name, height, mass, avg_height)

# Standardize the heights of the starwars characters (height / avg_height)
test <- starwars %>%
    dplyr::mutate(height = height / 100) %>%
    dplyr::mutate(avg_height = mean(height, na.rm = TRUE)) %>%
    dplyr::mutate(std_height = height / avg_height) %>%
    dplyr::select(name, height, mass, avg_height, std_height)

# Make a new column to see if each character is above or below the average height
test <- starwars %>%
    dplyr::mutate(height = height / 100) %>%
    dplyr::mutate(avg_height = mean(height, na.rm = TRUE)) %>%
    dplyr::mutate(std_height = height / avg_height) %>%
    dplyr::select(name, height, mass, avg_height, std_height) %>%
    dplyr::mutate(relative_height = ifelse(std_height > 1, "above", "below"))

# Add on to previous code to filter to keep only characters with heights below average
test <- starwars %>%
    dplyr::mutate(height = height / 100) %>%
    dplyr::mutate(avg_height = mean(height, na.rm = TRUE)) %>%
    dplyr::mutate(std_height = height / avg_height) %>%
    dplyr::select(name, height, mass, avg_height, std_height) %>%
    dplyr::mutate(relative_height = ifelse(std_height > 1, "above", "below")) %>%
    dplyr::filter(relative_height == "below")

# OR dplyr::filter(std_height < 1)
# OR dplyr::filter(height > avg_height)

#################################
# dplyr::group_by()
#################################
# Group starwars dataframe by gender
grouped_starwars <- starwars %>%
    dplyr::group_by(gender)

# Calculate average height PER GENDER (hint: group by gender FIRST)
grouped_starwars <- starwars %>%
    dplyr::mutate(height = height / 100) %>%
    dplyr::group_by(gender) %>%
    dplyr::mutate(avg_height = mean(height, na.rm = TRUE)) %>%
    dplyr::select(name, gender, height, avg_height)

# Ungroup your grouped dataframe and re-calculate average height
ungrouped_starwars <- grouped_starwars %>%
    dplyr::ungroup() %>%
    dplyr::mutate(avg_height = mean(height, na.rm = TRUE))

# Calculate the average height per gender AND eye color
grouped_starwars <- starwars %>%
    dplyr::group_by(gender, eye_color) %>%
    dplyr::mutate(avg_height = mean(height, na.rm = TRUE)) %>%
    dplyr::select(name, gender, eye_color, height, avg_height)


#################################
# dplyr::summarize()
# dplyr::summarise()
#################################

summarized <- starwars %>%
    dplyr::group_by(gender) %>%
    dplyr::summarize(avg_height = mean(height, na.rm = TRUE))

# look at the difference between summarize() and mutate() to calculate average height by gender
mutated <- starwars %>%
    dplyr::group_by(gender) %>%
    dplyr::mutate(avg_height = mean(height, na.rm = TRUE))


#################################
# Extra practice
#################################

# practice 1
# calculate the birth year of all characters (who have an age) given the "birth_year" column is actually AGE in 2019
# Hint: rename "birth_year" as "age" then calculate birth year
practice1 <- starwars %>%
    dplyr::select(name, age = birth_year) %>%
    dplyr::mutate(birth_year = 2019 - age) %>%
    dplyr::filter(!is.na(age))

# practice 2
# calculate the number of characters with each hair color combination
# Note: "blonde, brown" is different than "blonde" and "brown"
# hint: the function n() counts the number of observations in a group.
# BONUS: Combine 'blond' and 'blonde' as one category and re-calculate
practice2 <- starwars %>%
    dplyr::select(name, hair_color) %>%
    dplyr::group_by(hair_color) %>%
    dplyr::summarize(number = n())

# BONUS:
practice2 <- starwars %>%
    dplyr::mutate(hair_color = ifelse(hair_color == "blond", "blonde", hair_color)) %>%
    dplyr::select(name, hair_color) %>%
    dplyr::group_by(hair_color) %>%
    dplyr::summarize(number = n())

# practice 3
# calculate the number of characters with each hair color combination, keep ALL characters
# Note: "blonde, brown" is different than "blonde" and "brown"
# hint: the function n() counts the number of observations in a group.
practice3 <- starwars %>%
    dplyr::select(name, hair_color) %>%
    dplyr::group_by(hair_color) %>%
    dplyr::mutate(number = n())

# practice 4
# select all characters and their homeworlds in Return of the Jedi
# try using 'grepl()' 
practice4 <- starwars %>%
    dplyr::filter(grepl("Return of the Jedi", films)) %>%
    dplyr::select(name, homeworld)


#################################
# R statistics intro
#################################

####### Summary stats

# create vector "vec"
vec <- c(1, 1, 3, 4, 5, 5, 7, 8, 9, 9)

# calculate sum of vector
sum(vec)

# calculate the mean of vector
mean(vec)

# calculate the standard deviation of vector
sd(vec)

# calculate the product of vector
prod(vec)

# calculate summary statistics
summary(vec)

# calculate table of values
table(vec)


####### Sampling from a distribution
# sample without replacement
sample(vec, 3, replace = FALSE)

# sample with replacement
sample(vec, 3, replace = TRUE)

# set random generator seed
set.seed(192)

# sample with set probabilities
sample(c("Yes", "No"), 10, replace = TRUE, prob = c(0.7, 0.3))

####### Generating distributions
# generate NORMAL distribution
rnorm(5, mean = 0, sd = 1)


# generate UNIFORM distribution
runif(5, min = 1, max = 10)


# generate BINOMIAL distribution
rbinom(10, size = 1, prob = 0.5)


#################################
# Plotting with base R and ggplot
#################################

# generate NORMAL distribution to plot
x <- rnorm(100, mean = 0, sd = 1)

# plot with base R (histogram)
hist(x)

# plot with ggplot2 - basic
ggplot2::ggplot(data.frame(x)) +
  ggplot2::aes(x = x) +
  ggplot2::geom_histogram()

# show extra ggplot
ggplot2::ggplot(data.frame(x)) +
  ggplot2::aes(x = x) +
  ggplot2::geom_histogram(bins = 20, fill = "blue") +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "variable", y = "frequency")



