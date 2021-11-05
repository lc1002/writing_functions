Writing Functions
================

## Z scores

``` r
x_vec = rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -1.27448153  0.06881709 -1.18906852  1.77868936 -1.35847190 -1.34265378
    ##  [7] -0.68956613 -1.02647405  0.13044478  1.83759028  0.49882426 -0.25808298
    ## [13]  0.32348915  1.25803249  0.28740373  0.44644933 -1.20811472  0.06201571
    ## [19] -1.43812446  1.17497859  0.98396657 -0.52545822  0.57757401  0.61202216
    ## [25]  0.27019877

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  return(z)
}

z_scores(x = x_vec)
```

    ##  [1] -1.27448153  0.06881709 -1.18906852  1.77868936 -1.35847190 -1.34265378
    ##  [7] -0.68956613 -1.02647405  0.13044478  1.83759028  0.49882426 -0.25808298
    ## [13]  0.32348915  1.25803249  0.28740373  0.44644933 -1.20811472  0.06201571
    ## [19] -1.43812446  1.17497859  0.98396657 -0.52545822  0.57757401  0.61202216
    ## [25]  0.27019877

``` r
y_vec = rnorm(40, mean = 12, sd = 0.3)

z_scores(y_vec)
```

    ##  [1]  0.41366558 -1.09104401  0.63766781  0.95505836  1.48235246 -0.31808374
    ##  [7] -1.30299695 -0.54317703  0.03980837 -0.27696655 -0.19662541  0.79740465
    ## [13] -1.16872254 -1.45442361 -2.80084790  0.23486235  0.79181422  2.27098970
    ## [19]  0.13196398 -0.96708130  1.58597690  0.55921102  0.59654234  0.03386978
    ## [25] -1.78760050  1.60687203 -0.58235446  0.38045847  0.75344170 -0.41683392
    ## [31]  0.97898568  0.37507978 -0.48961791  0.19605507  0.25632496 -0.78613075
    ## [37]  0.08798962 -0.85028784 -0.35733326  0.22373287

How great is this??

Only kinda great.

Let’s try again.

``` r
z_scores = function(x) {
  
  if(!is.numeric(x)){
    stop("x needs to be numeric")
  }
  
  if(length(x) < 3){
    stop("x should have at least 3 numbers")
  }
  z = (x - mean(x)) / sd(x)
  return(z)
}
```

``` r
z_scores(3)
```

    ## Error in z_scores(3): x should have at least 3 numbers

``` r
z_scores(c("My", "name", "is", "lynn"))
```

    ## Error in z_scores(c("My", "name", "is", "lynn")): x needs to be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): x needs to be numeric

``` r
z_scores(x = y_vec)
```

    ##  [1]  0.41366558 -1.09104401  0.63766781  0.95505836  1.48235246 -0.31808374
    ##  [7] -1.30299695 -0.54317703  0.03980837 -0.27696655 -0.19662541  0.79740465
    ## [13] -1.16872254 -1.45442361 -2.80084790  0.23486235  0.79181422  2.27098970
    ## [19]  0.13196398 -0.96708130  1.58597690  0.55921102  0.59654234  0.03386978
    ## [25] -1.78760050  1.60687203 -0.58235446  0.38045847  0.75344170 -0.41683392
    ## [31]  0.97898568  0.37507978 -0.48961791  0.19605507  0.25632496 -0.78613075
    ## [37]  0.08798962 -0.85028784 -0.35733326  0.22373287

## Multiple outputs

give some collections of numbers will produce the mean and sd.

``` r
mean_and_sd = function(x) {
  
  if(!is.numeric(x)){
    stop("x needs to be numeric")
  }
  
  if(length(x) < 3){
    stop("x should have at least 3 numbers")
  }
  mean_x = mean(x)
  sd_x = sd(x)
  
  output_df = 
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  
  return(output_df)
}

mean_and_sd(y_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.249

``` r
mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.50  3.98

## Different sample sizes, means, sds

``` r
sim_data = 
  tibble(
    x = rnorm(30, mean = 2, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.18  2.52

Let’s write a fucntion that simulates data, computes the mean and sd.

``` r
sim_mean_sd = function(n, mu = 4, sigma = 3){
  # provide default values
  # do checks on inputs 
  
      sim_data = 
      tibble(
        x = rnorm(n, mean = mu, sd = sigma)
      )
    
    sim_data %>% 
      summarize(
        mean = mean(x),
        sd = sd(x)
      )
  
}

sim_mean_sd(30,4,3) # r assumes its in order; position matching 
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.23  3.64

``` r
sim_mean_sd(n = 30, sigma = 3, mu = 40)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  40.3  2.69

``` r
sim_mean_sd(30)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.51  2.77
