#################################
# Intro to R - quick
#################################

# create a vector


# create a dataframe



# types of variables
# numeric



# character



# logical/boolean




# R logic expressions







# more logical expressions with vectors







##### Working with vectors



# Get the first element



# Change the first element to a 9



# Remove the second element (rename a new vector y)



# keep the second and third elements (rename a new vector z)



# Get the elements that are > 5



# IFELSE statements




# write an ifelse statement to print “less than 9” if the element is less than 9 
# or “greater than 9” if the element is greater than 9



# now look at if statements (different than ifelse)


# Write an ‘if' statement to print “less than 9” if the variable is less than 9 or 
# “greater than 9” if the variable is greater than 9






# FOR loops


# Write an ‘for loop’ to add 5 to each element of vector x





#################################
# Data wrangling with dplyr
#################################

#################################
# load data
#################################
# load dplyr package


# loads starwars dataframe from dplyr package


# look at starwars dataframe in Rstudio


#################################
# dplyr::filter()
#################################
# Filter the starwars dataframe to keep only humans


# Filter the starwars dataframe to keep only male humans


# Filter the starwars dataframe to keep humans and droids


# Filter the starwars dataframe to keep characters with a height less than 100


# Filter the starwars dataframe to keep all characters that are non-human


# Filter the starwars dataframe to remove unknown species (i.e. species == NA)


#################################
# piping in tidyverse
#################################
# Filter the starwars dataframe to keep only male humans



#################################
# dplyr::select()
#################################
# Select only name, species, and films variables from the starwars dataframe


# Select all columns except the gender


# Select columns name, height, mass, hair color, and skin color


# Select all columns except hair color, skin color, and eye color



# Select columns name, mass, skin color, hair color, and height (in order)


# rename the "name" column to "character" and the "mass" column to "weight" using select()


# Keep:  height greater than 100 &
# Keep: humans &
# Remove: brown hair color &
# Remove: vehicles &
# Keep: name, homeworld, height, species, hair color








#################################
# dplyr::mutate()
#################################

# check out the starwars dataframe help page to see what the units are for mass and height


# Calculate the BMI of all starwars characters
# Step 1: convert height in cm to height in m (save as new dataframe)


# Step 2: calculate BMI with formula: BMI = weight (kg) / [height (m)]^2




# select only name, height, mass, height_m, and bmi and view dataframe




# re-calculate the bmi by changing the height column to meters instead of creating a new column




# Make a new column to find the average height (in meters)



# check the help page for mean


# use na.rm = TRUE in the mean() function




# Standardize the heights of the starwars characters (height / avg_height)





# Make a new column to see if each character is above or below the average height





# Add on to previous code to filter to keep only characters with heights below average








#################################
# dplyr::group_by()
#################################
# Group starwars dataframe by gender



# Calculate average height PER GENDER (hint: group by gender FIRST)





# Ungroup your grouped dataframe and re-calculate average height




# Calculate the average height per gender AND eye color





#################################
# dplyr::summarize()
# dplyr::summarise()
#################################

# Summarize height by gender



# look at the difference between summarize() and mutate() to calculate average height by gender




#################################
# Extra practice
#################################

# practice 1
# calculate the birth year of all characters (who have an age) given the "birth_year" column is actually AGE in 2019
# Hint: rename "birth_year" as "age" then calculate birth year





# practice 2
# calculate the number of characters with each hair color combination
# Note: "blonde, brown" is different than "blonde" and "brown"
# hint: the function n() counts the number of observations in a group.
# BONUS: Combine 'blond' and 'blonde' as one category and re-calculate






# BONUS:






# practice 3
# calculate the number of characters with each hair color combination, keep ALL characters
# Note: "blonde, brown" is different than "blonde" and "brown"
# hint: the function n() counts the number of observations in a group.






# practice 4
# select all characters and their homeworlds in Return of the Jedi
# try using 'grepl()' 






#################################
# R statistics intro
#################################

####### Summary stats

# create vector "vec"


# calculate sum of vector


# calculate the mean of vector


# calculate the standard deviation of vector


# calculate the product of vector


# calculate summary statistics


# calculate table of values



####### Sampling from a distribution
# sample without replacement


# sample with replacement


# set random generator seed


# sample with set probabilities


####### Generating distributions
# generate NORMAL distribution



# generate UNIFORM distribution



# generate BINOMIAL distribution



#################################
# Plotting with base R and ggplot
#################################

# generate NORMAL distribution to plot


# plot with base R (histogram)


# plot with ggplot2 - basic





# show extra ggplot




