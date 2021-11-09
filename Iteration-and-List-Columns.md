Iteration and List Columns
================

## Lists

``` r
l <-  
  list(
    vec_numeric = 5:8,
    vec_logical = c(TRUE,FALSE),
    summary = summary(rnorm(1000,5,3))
     )

l[[3]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -7.065   2.938   4.845   4.970   7.090  14.229

``` r
l[["summary"]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -7.065   2.938   4.845   4.970   7.090  14.229

``` r
l$summary
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -7.065   2.938   4.845   4.970   7.090  14.229

## list of normals

``` r
list_norms <-  
  list(
    a = rnorm(50, 2, 1),
    b = rnorm(50, 5, 3),
    c = rnorm(50, 10, .2),
    d = rnorm(50, -3, 1)
  )

is.list(list_norms)
```

    ## [1] TRUE

## define the function

``` r
mean_and_sd = function(x){
  if(!is.numeric(x)){
    stop("x needs to be numeric")
  }
  
  if(length(x)<3){
    stop("x should have at least 3 numbers")
  }
  mean_x <-  mean(x)
  sd_x <-  sd(x)
  
  output_df <- 
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  
  return(output_df)
}

mean_and_sd(list_norms[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.16  1.17

## for loop

let’s use a for loop to iterate over my list of normals

``` r
output <- vector("list",length = 4)

output[[1]] = mean_and_sd(list_norms[[1]])

for (i in 1:4){
  output[[i]] = mean_and_sd(list_norms[[i]])
}
```

let’s use map instead

``` r
output_meansd <- map(list_norms,mean_and_sd)
output_summart <- map(list_norms,summary)

output_median <- map_dbl(list_norms,median)
```

## List Columns!!!1

``` r
listcol_df <- 
  tibble(
    name = c("a","b","c","d"),
    norms = list_norms
  )

listcol_df %>% 
  filter( name =="a")
```

    ## # A tibble: 1 × 2
    ##   name  norms       
    ##   <chr> <named list>
    ## 1 a     <dbl [50]>

``` r
listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>%  pull(norms)
```

    ## $a
    ##  [1]  2.3220987  3.1027121  3.4368689  3.7659748  2.5263781  2.3567713
    ##  [7]  0.4465193  2.2209890  2.7723872  2.7905145  4.6541146  0.6772944
    ## [13]  1.3357098  3.0714695  2.5390671  1.3123394  3.5143914  1.4302643
    ## [19]  2.9183359  2.6184292  0.1288396  2.8512614  2.8826652  2.0255282
    ## [25]  0.8405779  3.5114919  2.6706033  2.9502818  3.1165883  2.6246918
    ## [31]  2.4369809  3.0260986  0.8338536  4.1760770  1.9855176  1.6858172
    ## [37]  1.3288048  2.8789910  0.5743406  2.0977974  0.9938736  0.9124527
    ## [43] -0.1388960  1.0098442  3.9351629  0.6861456 -0.3831535  1.2820188
    ## [49]  2.7804023  2.4050589
    ## 
    ## $b
    ##  [1]  1.4034818  5.4881600  3.8876490  2.1997229  6.8870157 -0.5108353
    ##  [7]  2.8506766  3.8801399 -0.9278635  4.6959586  4.2603666  8.5832026
    ## [13]  8.5869891  7.5362194  3.2756974  4.8823107  9.1309302  1.4012297
    ## [19]  1.3845126  5.7858729  3.4118092  8.0122242  4.2593634  5.6039988
    ## [25]  3.6693362  3.2981457  3.4788782  4.9358508 -0.3055242  7.7873711
    ## [31]  0.2072843 -2.3466693  1.4454602  9.2794221  6.8070256  2.6678927
    ## [37]  0.4685361  3.6329325  3.9716641  3.2394703 -0.1401888  7.9147115
    ## [43]  6.8827584  2.8931523  6.6951254  1.6843842 10.7854928  0.7468922
    ## [49]  2.4973775  1.5868655
    ## 
    ## $c
    ##  [1] 10.263377 10.019474  9.715011  9.855664 10.357099 10.474379  9.875447
    ##  [8] 10.113090  9.949240  9.664816  9.979044  9.802573  9.503092  9.715631
    ## [15]  9.982344  9.834807  9.888463 10.039292  9.990947 10.627949  9.974051
    ## [22] 10.030349  9.758516  9.991035 10.039920 10.009050 10.217080  9.602450
    ## [29]  9.892337 10.144719 10.183648  9.882466  9.907670  9.618172  9.902179
    ## [36]  9.819996 10.141994  9.807683  9.918822  9.883766  9.953517 10.150802
    ## [43] 10.039699  9.831893  9.850768 10.149363  9.911622  9.943525 10.154551
    ## [50]  9.680111
    ## 
    ## $d
    ##  [1] -1.704819 -2.957075 -3.069145 -4.962236 -3.323547 -1.833531 -1.904034
    ##  [8] -3.209062 -3.311182 -2.733501 -2.635201 -3.703762 -3.408307 -2.817642
    ## [15] -2.948859 -3.877511 -3.347027 -2.518317 -2.681059 -3.222345 -1.372539
    ## [22] -2.082929 -4.495294 -4.613918 -3.741926 -1.669604 -3.869094 -2.630262
    ## [29] -1.314082 -4.832840 -3.944701 -2.699642 -4.449209 -2.577308 -2.798446
    ## [36] -3.383191 -2.619757 -3.248280 -4.005351 -2.608167 -3.977865 -4.607740
    ## [43] -1.358029 -3.413852 -3.236381 -2.542503 -2.119741 -1.369133 -2.072783
    ## [50] -3.657215

``` r
mean_and_sd(listcol_df$norms[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.16  1.17

``` r
listcol_df %>% 
  mutate(summary <- map(listcol_df$norms,mean_and_sd))
```

    ## # A tibble: 4 × 3
    ##   name  norms        `summary <- map(listcol_df$norms, mean_and_sd)`
    ##   <chr> <named list> <named list>                                   
    ## 1 a     <dbl [50]>   <tibble [1 × 2]>                               
    ## 2 b     <dbl [50]>   <tibble [1 × 2]>                               
    ## 3 c     <dbl [50]>   <tibble [1 × 2]>                               
    ## 4 d     <dbl [50]>   <tibble [1 × 2]>

## Nested data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2021-09-09 10:44:54 (7.599)

    ## file min/max dates: 1869-01-01 / 2021-09-30

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2021-09-09 10:44:58 (1.697)

    ## file min/max dates: 1965-01-01 / 2020-02-29

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2021-09-09 10:45:00 (0.909)

    ## file min/max dates: 1999-09-01 / 2021-09-30

Nest data within location

``` r
#nest and unest
weather_nest <- nest(weather_df,data = date:tmin)
unnest
```

    ## function (data, cols, ..., keep_empty = FALSE, ptype = NULL, 
    ##     names_sep = NULL, names_repair = "check_unique", .drop = deprecated(), 
    ##     .id = deprecated(), .sep = deprecated(), .preserve = deprecated()) 
    ## {
    ##     deprecated <- FALSE
    ##     if (!missing(.preserve)) {
    ##         lifecycle::deprecate_warn("1.0.0", "unnest(.preserve = )", 
    ##             details = "All list-columns are now preserved")
    ##         deprecated <- TRUE
    ##         .preserve <- tidyselect::vars_select(tbl_vars(data), 
    ##             !!enquo(.preserve))
    ##     }
    ##     else {
    ##         .preserve <- NULL
    ##     }
    ##     if (missing(cols) && missing(...)) {
    ##         list_cols <- names(data)[map_lgl(data, is_list)]
    ##         cols <- expr(c(!!!syms(setdiff(list_cols, .preserve))))
    ##         warn(paste0("`cols` is now required when using unnest().\n", 
    ##             "Please use `cols = ", expr_text(cols), "`"))
    ##         deprecated <- TRUE
    ##     }
    ##     if (missing(...)) {
    ##         cols <- enquo(cols)
    ##     }
    ##     else {
    ##         dots <- enquos(cols, ..., .named = TRUE, .ignore_empty = "all")
    ##         data <- dplyr::mutate(data, !!!dots)
    ##         cols <- expr(c(!!!syms(names(dots))))
    ##         unnest_call <- expr(unnest(!!cols))
    ##         warn(paste0("unnest() has a new interface. See ?unnest for details.\n", 
    ##             "Try `df %>% ", expr_text(unnest_call), "`, with `mutate()` if needed"))
    ##         deprecated <- TRUE
    ##     }
    ##     if (!is_missing(.drop)) {
    ##         lifecycle::deprecate_warn("1.0.0", "unnest(.drop = )", 
    ##             details = "All list-columns are now preserved.")
    ##         deprecated <- TRUE
    ##     }
    ##     if (!is_missing(.id)) {
    ##         lifecycle::deprecate_warn("1.0.0", "unnest(.id = )", 
    ##             details = "Manually create column of names instead.")
    ##         deprecated <- TRUE
    ##         first_col <- tidyselect::vars_select(tbl_vars(data), 
    ##             !!cols)[[1]]
    ##         data[[.id]] <- names(data[[first_col]])
    ##     }
    ##     if (!is_missing(.sep)) {
    ##         lifecycle::deprecate_warn("1.0.0", "unnest(.sep = )", 
    ##             details = glue("Use `names_sep = '{.sep}'` instead."))
    ##         deprecated <- TRUE
    ##         names_sep <- .sep
    ##     }
    ##     if (deprecated) {
    ##         return(unnest(data, cols = !!cols, names_sep = names_sep, 
    ##             keep_empty = keep_empty, ptype = ptype, names_repair = tidyr_legacy))
    ##     }
    ##     UseMethod("unnest")
    ## }
    ## <bytecode: 0x7fae6aad1070>
    ## <environment: namespace:tidyr>

``` r
weather_nest %>% 
  filter(name == "CentralPark_NY")
```

    ## # A tibble: 1 × 3
    ##   name           id          data              
    ##   <chr>          <chr>       <list>            
    ## 1 CentralPark_NY USW00094728 <tibble [365 × 4]>

``` r
## regression
weather_lm = function(df){
  lm(tmax ~ tmin,data = df)
}

lm(tmax ~ tmin, data = weather_nest$data[[3]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest$data[[3]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

``` r
weather_lm(weather_nest$data[[3]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

``` r
map(weather_nest$data,weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

``` r
weather_nest %>% 
  mutate(lm_results = map(data,weather_lm))
```

    ## # A tibble: 3 × 4
    ##   name           id          data               lm_results
    ##   <chr>          <chr>       <list>             <list>    
    ## 1 CentralPark_NY USW00094728 <tibble [365 × 4]> <lm>      
    ## 2 Waikiki_HA     USC00519397 <tibble [365 × 4]> <lm>      
    ## 3 Waterhole_WA   USS0023B17S <tibble [365 × 4]> <lm>

``` {r，eval=false}
read_page_reviews <- function(url) {
  
  html = read_html(url)
  
  review_titles = 
    html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()
  
  review_text = 
    html %>%
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

url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
vec_urls = str_c(url_base,1:5)

map(vec_urls,read_page_reviews )

napoleon_df <- 
  tibble(
    urls = vec_urls
  )

napoleon_df %>% 
  mutate(reviews =map(urls,read_page_reviews )) %>% 
  select(reviews) %>% 
  unnest(map(vec_urls,read_page_reviews ))
```
