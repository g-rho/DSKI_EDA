library(nycflights13)
library(tidyverse)

flights
# Question: What kind of information does tha data contain?
# ...What does a row correspond to?
# ...note: it is a 'tibble' (and not just a 'data.frame')
str(flights)



### I) Filtering rows:
jan1 <- filter(flights, month == 1, day == 1) # REM: use double '==' for comparisons!

# ...logical or:
nov_dec <- filter(flights, month == 11 | month == 12)
# ...or alternatively: 
nov_dec <- filter(flights, month %in% c(11, 12))
nov_dec

# not (De Morgan's law)
no_delay <- filter(flights, !(arr_delay > 120 | dep_delay > 120))
# ...or alternatively: 
no_delay <- filter(flights, arr_delay <= 120, dep_delay <= 120)
no_delay

# Exercises: Find all flights that...
# 1) ...Had an arrival delay of two or more hours
# 2) ...Flew to Houston (IAH or HOU)
# 3) ... Were operated by United, American, or Delta
# 4) ...Departed in summer (July, August, and September)
# 5) ...Arrived more than two hours late, but didn’t leave late
# 6) ...Were delayed by at least an hour, but made up over 30 minutes in flight
# 7) ...Departed between midnight and 6am (inclusive)

# Another useful dplyr filtering helper is between(). 
# Can you use it to simplify the code needed to answer the previous challenges?
  
# How many flights have a missing dep_time? 
# What other variables are missing? 
# What might these rows represent?
  


### II) Re-arranging rows
# sort data according to one or several variables
arrange(flights, year, month, day)
# change ordert
arrange(flights, desc(dep_delay))

# Missings values (I): 
x <- c(5, 2, NA) # creat a sequence of numbers where one value is missing  
x
is.na(x) 

df <- tibble(x = c(5, 2, NA)) # transform into a tibble 
filter(df, x > 1)
filter(df, is.na(x) | x > 2)
# Question: which rows are selected and why?
# ...how are missings  treated when sorting data?
arrange(df, x)
arrange(df, desc(x))

# Exercises: 
# 1) Sort flights to find the most delayed flights. 
#    Find the flights that left earliest.
# 2) Sort flights to find the fastest (highest speed) flights.
# 3) Which flights travelled the farthest? Which travelled the shortest?



### III) Filtering (/selecting) columns
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))

# Some helpful options for select():
# - starts_with("abc"): matches names that begin with “abc”.
# - ends_with("xyz"): matches names that end with “xyz”.
# - contains("ijk"): matches names that contain “ijk”.
# - matches("(.)\\1"): selects variables that match a regular expression. 
# - num_range("x", 1:3): matches x1, x2 and x3.

# rename variables
rename(flights, tail_num = tailnum)
# move variables to the beginning
select(flights, time_hour, air_time, everything())


### IV) Add new variables:
flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time)
flights_sml

mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60
       hours = air_time / 60,
       gain_per_hour = gain / hours
       )
# What kind of information do the variables gain and speed represent?

# ...only keep the new variables:
transmute(flights,
            gain = dep_delay - arr_delay,
            hours = air_time / 60,
            gain_per_hour = gain / hours
          )


transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100
          )

# sometimes helpful: 
x <- 1:10
xlag(x)
lead(x)
cumsum(x)
cummean(x)

y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
min_rank(desc(y))
# Question: What does min_rank() return?

# Exercises:
# - Currently dep_time and sched_dep_time are convenient to look at, but hard to compute with because they’re not really continuous numbers. Convert them to a more convenient representation of number of minutes since midnight.
# - Compare dep_time, sched_dep_time, and dep_delay. How would you expect those three numbers to be related?
# - Find the 10 most delayed flights using a ranking function. How do you want to handle ties? Carefully read the documentation for min_rank().
# - What does 1:3 + 1:10 return? Why?


### V) (Grouped) summaries:
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))
  
# define gouping variables:  
by_day <- group_by(flights, year, month, day)
by_day # same data but grouping variables arre stored as meta information
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE)) # ...same as before but different result


# Missings (II):
# ...What is the role of the na.rm argument ?
by_day <- group_by(flights, year, month, day)
summarise(by_day, mean = mean(dep_delay))

# ...In this case missing values represent cancelled flights, we may want ro remove the first: 
not_cancelled <-  filter(flights, !is.na(dep_delay), !is.na(arr_delay))
by_day_not_cancelled <- group_by(not_cancelled, year, month, day) 
summarise(by_day_not_cancelled, mean = mean(dep_delay))



### VI) ...Using pipes
# run the following code...
by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)

delay <- filter(delay, count > 20, dest != "HNL")

ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)
# Exercise: Understand the code, what is analyzed here?

# ...the same code using pipes %>%:
delays2<- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL") %>%
  ggplot(mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)
# Note: After a pipe the first argument is skipped and given by the result of the preceeding function. 


### VII) Counts and other useful summary functions:
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )
delays
# Question: What information does the column n contain?

ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)
# Question: how is the variation in average delay related to the number of flights? 


# ..combining aggregation with logical subsetting 
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) 
  )
# Question: Why is avg_delay1 smaller than avg_delay2?

# computing the first / last departure for each day
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )

# count missings
not_cancelled %>% 
  group_by(dest,year, month, day) %>% 
  summarise(n_cancelled = sum(!is.na(arr_delay))) %>% 
  arrange(desc(n_cancelled))

# count the number of distinct (unique) values
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))

# How many flights left before 5am? (these usually indicate delayed flights from the previous day)
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500))

# What proportion of flights are delayed by more than an hour?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_prop = mean(arr_delay > 60))


## ungrouping
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE)) 

by_day %>% ungroup() %>% summarise(delay = mean(dep_delay, na.rm = TRUE))
# ...no longer groupe wise

# Exercise:
arrange(flights, flight)
group_by(flights, flight) %>% summarize(, nflights = n())
# How many flights are always 10+ minutes late


###  IX) Identifying typical / unusual values 
# diamond data
diamonds

smaller <- diamonds %>% 
  filter(carat < 3)

# - Which values are the most common? Why?
# - Which values are rare? Why? Does that match your expectations?
# - Can you see any unusual patterns? What might explain them?

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)
# ...some interesting questions:
# - Why are there more diamonds at whole carats and common fractions of carats?
# - Why are there more diamonds slightly to the right of each peak than there are slightly to the left of each peak?
# - Why are there no diamonds bigger than 3 carats?


# ..identifying unusual values:
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) 
# Question: Why is x-axes (here representing the variable 'y') as long? 

# ...try modyfing the code:
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

# ...three unusual values: 0, ~30, and ~60.
unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  select(price, x, y, z) %>%
  arrange(y)

# 1) y measures one of the three dimensions of these diamonds, in mm. 
#    diamonds can’t have a width of 0mm, so these values must be incorrect. 
# 2) measurements of 32mm and 59mm are supicions (implausible): 
#    those diamonds are over an inch long, but don’t cost hundreds of thousands of dollars!
  

# Exercises:
# - Explore the distribution of each of the x, y, and z variables in diamonds. 
#   What do you learn? 
#   Think about a diamond and how you might decide which dimension is the length, width, and depth.
# 
# - Explore the distribution of price. 
#   Do you discover anything unusual or surprising? 
#   (Hint: Carefully think about the binwidth and make sure you try a wide range of values.)
#
# - How many diamonds are 0.99 carat? How many are 1 carat? 
#   What do you think is the cause of the difference?



###  X) Long and wide tables 
us_rent_income
us_rent_income_wide <- pivot_wider(us_rent_income, 
                                   names_from = variable,
                                   values_from = c(estimate, moe))
us_rent_income_wide


billboard
billboard %>%
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    names_prefix = "wk",
    values_to = "rank",
    values_drop_na = TRUE
  )
