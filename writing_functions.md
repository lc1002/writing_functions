Writing Functions
================

## Z scores

``` r
x_vec = rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.574243696 -1.806688731 -0.845314611  1.041799880  1.008855785
    ##  [6]  1.069790587  0.009921126 -0.144435933  0.241243806 -0.408123418
    ## [11]  0.306257106 -0.581783808 -0.165326194  1.126776948 -0.806378753
    ## [16]  1.638231348  0.059525260 -0.386080828  0.867366453 -2.178726752
    ## [21] -1.893628338  0.651319418  0.830136639  0.512839514 -0.721820201

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  return(z)
}

z_scores(x = x_vec)
```

    ##  [1]  0.574243696 -1.806688731 -0.845314611  1.041799880  1.008855785
    ##  [6]  1.069790587  0.009921126 -0.144435933  0.241243806 -0.408123418
    ## [11]  0.306257106 -0.581783808 -0.165326194  1.126776948 -0.806378753
    ## [16]  1.638231348  0.059525260 -0.386080828  0.867366453 -2.178726752
    ## [21] -1.893628338  0.651319418  0.830136639  0.512839514 -0.721820201

``` r
y_vec = rnorm(40, mean = 12, sd = 0.3)

z_scores(y_vec)
```

    ##  [1] -0.12160310 -0.64048200  1.60482290  1.33354749  1.78344497  0.10743243
    ##  [7]  1.03252158 -0.23896289  1.08735242 -0.89335663 -2.45699387  0.90176002
    ## [13] -0.22125977 -1.36859861  0.33552310 -0.10609317 -1.24933940  1.06850194
    ## [19] -0.64505682  1.46709473  0.68428773  0.36057129  0.09446123 -0.04949746
    ## [25] -0.87392505  0.85713751 -0.36171347  1.09912692 -1.14849943 -1.54001887
    ## [31] -0.51383706  0.50958585  0.06341476  0.08657274 -0.26810338  0.55219925
    ## [37]  1.16533691 -0.80757084 -1.14348259 -1.54630137

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

    ##  [1] -0.12160310 -0.64048200  1.60482290  1.33354749  1.78344497  0.10743243
    ##  [7]  1.03252158 -0.23896289  1.08735242 -0.89335663 -2.45699387  0.90176002
    ## [13] -0.22125977 -1.36859861  0.33552310 -0.10609317 -1.24933940  1.06850194
    ## [19] -0.64505682  1.46709473  0.68428773  0.36057129  0.09446123 -0.04949746
    ## [25] -0.87392505  0.85713751 -0.36171347  1.09912692 -1.14849943 -1.54001887
    ## [31] -0.51383706  0.50958585  0.06341476  0.08657274 -0.26810338  0.55219925
    ## [37]  1.16533691 -0.80757084 -1.14348259 -1.54630137

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
    ## 1  12.0 0.279

``` r
mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.82  3.55

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
    ## 1  1.85  2.76

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
    ## 1  4.12  3.13

``` r
sim_mean_sd(n = 30, sigma = 3, mu = 40)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  39.7  2.98

``` r
sim_mean_sd(30)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.06  3.02

## Napoleon Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

Okay but there are a lot of pages of reviews.

Write a function that gets reviews based on page url.

``` r
get_page_reviews = function(page_url) {
  
  page_html = read_html(page_url)

  review_titles = 
    page_html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    page_html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()
  
  review_text = 
    page_html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim()
  
  reviews = tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
  
  return(reviews)
}

base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

urls = str_c(base_url, 1:5)

bind_rows(
  get_page_reviews(urls[1]),
  get_page_reviews(urls[2]),
  get_page_reviews(urls[3]),
  get_page_reviews(urls[4]),
  get_page_reviews(urls[5]))
```

    ## # A tibble: 50 x 3
    ##    title                                                 stars text             
    ##    <chr>                                                 <dbl> <chr>            
    ##  1 Good movie                                                5 Weird story, goo~
    ##  2 I Just everyone to know this....                          5 VOTE FOR PEDRO !~
    ##  3 the cobweb in his hair during the bike ramp scene lol     5 5 stars for bein~
    ##  4 Best quirky movie ever                                    5 You all know the~
    ##  5 Classic Film                                              5 Had to order thi~
    ##  6 hehehehe                                                  5 goodjobboys      
    ##  7 Painful                                                   1 I think I sneeze~
    ##  8 GRAND                                                     5 GRAND            
    ##  9 Hello, 90s                                                5 So nostalgic mov~
    ## 10 Cult Classic                                              5 Watched it with ~
    ## # ... with 40 more rows

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4
