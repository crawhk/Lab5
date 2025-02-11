Lab 05 - La Quinta is Spanish for next to Denny’s, Pt. 2
================
Hannah Crawley
2.11.25

### Load packages and data

``` r
library(tidyverse) 
library(dsbox) 
```

``` r
states <- read_csv("data/states.csv")
```

### Exercise 1

> Filter the Denny’s data frame for Alaska (AK) and save the result as
> dn_ak. How many Denny’s locations are there in Alaska?

3 Dennys

``` r
dn_ak <- dennys %>% 
  filter(state == "AK")
nrow(dn_ak)
```

    ## [1] 3

> Now, do the same for La Quinta data frame for Alaska (AK) and save the
> result as lq_ak. How many La Quinta locations are there in Alaska?

2

``` r
lq_ak <- laquinta %>% 
  filter(state == "AK")
nrow(lq_ak)
```

    ## [1] 2

### Exercise 2

> Next we will be calculating the distance between all Denny’s and all
> La Quinta locations in Alaska.

> How many pairings are there between all Denny’s and all La Quinta
> locations in Alaska, i.e., how many distances do we need to calculate
> between the locations of these establishments in Alaska? Calculate the
> number of pairings using the data frames you have already made

We will need to calculate 6 different distances (2x3)

### Exercise 3

> In order to calculate these distances, we need to first restructure
> our data to pair the Denny’s and La Quinta locations. To do so, we
> will join the two data frames. We have six join options in R. Each of
> these join functions take at least three arguments: x, y, and by.  
> x and y are data frames to join by is the variable(s) to join by

Four of these join functions combine variables from the two data frames:

Note: These functions are called mutating joins.

inner_join(): return all rows from x where there are matching values in
y, and all columns from x and y.

left_join(): return all rows from x, and all columns from x and y. Rows
in x with no match in y will have NA values in the new columns.

right_join(): return all rows from y, and all columns from x and y. Rows
in y with no match in x will have NA values in the new columns.

full_join(): return all rows and all columns from both x and y, where
there are not matching values, returns NA for the one missing.

``` r
dn_lq_ak <- full_join(dn_ak, lq_ak, by = "state")
```

    ## Warning in full_join(dn_ak, lq_ak, by = "state"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 1 of `x` matches multiple rows in `y`.
    ## ℹ Row 1 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
dn_lq_ak
```

    ## # A tibble: 6 × 11
    ##   address.x     city.x state zip.x longitude.x latitude.x address.y city.y zip.y
    ##   <chr>         <chr>  <chr> <chr>       <dbl>      <dbl> <chr>     <chr>  <chr>
    ## 1 2900 Denali   Ancho… AK    99503       -150.       61.2 3501 Min… "\nAn… 99503
    ## 2 2900 Denali   Ancho… AK    99503       -150.       61.2 4920 Dal… "\nFa… 99709
    ## 3 3850 Debarr … Ancho… AK    99508       -150.       61.2 3501 Min… "\nAn… 99503
    ## 4 3850 Debarr … Ancho… AK    99508       -150.       61.2 4920 Dal… "\nFa… 99709
    ## 5 1929 Airport… Fairb… AK    99701       -148.       64.8 3501 Min… "\nAn… 99503
    ## 6 1929 Airport… Fairb… AK    99701       -148.       64.8 4920 Dal… "\nFa… 99709
    ## # ℹ 2 more variables: longitude.y <dbl>, latitude.y <dbl>

### Exercise 4

> How many observations are in the joined dn_lq_ak data frame? What are
> the names of the variables in this data frame.

Number of observations: 6 Variables: address.x city.x, state, zip.x,
longitude.x, latitude.x address.y city.y, zip.y, longitude.y, latitude.y

> .x in the variable names means the variable comes from the x data
> frame (the first argument in the full_join call, i.e. dn_ak), and .y
> means the variable comes from the y data frame. These variables are
> renamed to include .x and .y because the two data frames have the same
> variables and it’s not possible to have two variables in a data frame
> with the exact same name.

Now that we have the data in the format we wanted, all that is left is
to calculate the distances between the pairs.

### Exercise 5

> What function from the tidyverse do we use the add a new variable to a
> data frame while keeping the existing variables?

MUTATE!

### Exercise 6

…

Add exercise headings as needed.
