P8105_hw1_REN XIN
================
2022-09-23

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(readxl)
library(readr)
library(dplyr)
```

Problem1

``` r
trans_ent = 
  read_excel(
    "NYC_Transit_Subway_Entrance_And_Exit_Data.xlsx")
col_types = cols(Route8 = "c", Route9 = "c", Route10 = "c", Route11 = "c")
trans_ent=janitor::clean_names(trans_ent)
data_c=select(trans_ent,line,station_name,station_latitude,station_longitude,route1:route11,entry,vending,entrance_type,ada) %>% 
  mutate(entry = ifelse(entry == "YES", TRUE, FALSE))
```

``` r
trans_ent %>% 
  select(station_name, line) %>% 
  distinct
```

    ## # A tibble: 465 × 2
    ##    station_name             line    
    ##    <chr>                    <chr>   
    ##  1 25th St                  4 Avenue
    ##  2 36th St                  4 Avenue
    ##  3 45th St                  4 Avenue
    ##  4 53rd St                  4 Avenue
    ##  5 59th St                  4 Avenue
    ##  6 77th St                  4 Avenue
    ##  7 86th St                  4 Avenue
    ##  8 95th St                  4 Avenue
    ##  9 9th St                   4 Avenue
    ## 10 Atlantic Av-Barclays Ctr 4 Avenue
    ## # … with 455 more rows

``` r
trans_ent %>% 
  filter(ada == TRUE) %>% 
  select(station_name, line) %>% 
  distinct
```

    ## # A tibble: 84 × 2
    ##    station_name                   line           
    ##    <chr>                          <chr>          
    ##  1 Atlantic Av-Barclays Ctr       4 Avenue       
    ##  2 DeKalb Av                      4 Avenue       
    ##  3 Pacific St                     4 Avenue       
    ##  4 Grand Central                  42nd St Shuttle
    ##  5 34th St                        6 Avenue       
    ##  6 47-50th Sts Rockefeller Center 6 Avenue       
    ##  7 Church Av                      6 Avenue       
    ##  8 21st St                        63rd Street    
    ##  9 Lexington Av                   63rd Street    
    ## 10 Roosevelt Island               63rd Street    
    ## # … with 74 more rows

``` r
trans_ent %>% 
  filter(vending == "NO") %>% 
  pull(entry) 
```

    ##   [1] "NO"  "NO"  "YES" "NO"  "NO"  "NO"  "YES" "YES" "YES" "YES" "YES" "YES"
    ##  [13] "YES" "YES" "YES" "NO"  "NO"  "YES" "NO"  "NO"  "NO"  "NO"  "NO"  "NO" 
    ##  [25] "NO"  "NO"  "YES" "NO"  "NO"  "NO"  "NO"  "NO"  "NO"  "YES" "NO"  "YES"
    ##  [37] "NO"  "NO"  "NO"  "YES" "YES" "NO"  "YES" "NO"  "NO"  "NO"  "YES" "NO" 
    ##  [49] "NO"  "YES" "YES" "YES" "NO"  "NO"  "NO"  "YES" "YES" "NO"  "NO"  "YES"
    ##  [61] "YES" "NO"  "NO"  "NO"  "NO"  "YES" "NO"  "YES" "NO"  "YES" "YES" "NO" 
    ##  [73] "NO"  "NO"  "NO"  "YES" "NO"  "NO"  "NO"  "NO"  "NO"  "YES" "YES" "YES"
    ##  [85] "NO"  "YES" "YES" "NO"  "YES" "YES" "NO"  "NO"  "NO"  "NO"  "NO"  "NO" 
    ##  [97] "YES" "NO"  "NO"  "NO"  "NO"  "NO"  "NO"  "NO"  "NO"  "NO"  "NO"  "NO" 
    ## [109] "YES" "YES" "YES" "NO"  "NO"  "NO"  "NO"  "NO"  "NO"  "YES" "NO"  "NO" 
    ## [121] "NO"  "NO"  "NO"  "YES" "NO"  "NO"  "NO"  "NO"  "NO"  "NO"  "NO"  "YES"
    ## [133] "YES" "YES" "YES" "YES" "NO"  "NO"  "YES" "YES" "NO"  "YES" "NO"  "NO" 
    ## [145] "NO"  "NO"  "NO"  "NO"  "NO"  "YES" "YES" "NO"  "YES" "YES" "YES" "YES"
    ## [157] "NO"  "YES" "YES" "YES" "NO"  "NO"  "NO"  "NO"  "NO"  "YES" "YES" "YES"
    ## [169] "YES" "YES" "NO"  "NO"  "NO"  "NO"  "NO"  "YES" "YES" "YES" "YES" "NO" 
    ## [181] "NO"  "NO"  "NO"

``` r
trans_ent=mutate(trans_ent,route8=as.character(route8))
trans_ent=mutate(trans_ent,route9=as.character(route9))
trans_ent=mutate(trans_ent,route10=as.character(route10))
trans_ent=mutate(trans_ent,route11=as.character(route11))
trans_ent %>% 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") %>% 
  filter(route == "A") %>% 
  select(station_name, line) %>% 
  distinct
```

    ## # A tibble: 60 × 2
    ##    station_name                  line           
    ##    <chr>                         <chr>          
    ##  1 Times Square                  42nd St Shuttle
    ##  2 125th St                      8 Avenue       
    ##  3 145th St                      8 Avenue       
    ##  4 14th St                       8 Avenue       
    ##  5 168th St - Washington Heights 8 Avenue       
    ##  6 175th St                      8 Avenue       
    ##  7 181st St                      8 Avenue       
    ##  8 190th St                      8 Avenue       
    ##  9 34th St                       8 Avenue       
    ## 10 42nd St                       8 Avenue       
    ## # … with 50 more rows

``` r
trans_ent %>% 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") %>% 
  filter(route == "A", ada == TRUE) %>% 
  select(station_name, line) %>% 
  distinct
```

    ## # A tibble: 17 × 2
    ##    station_name                  line            
    ##    <chr>                         <chr>           
    ##  1 14th St                       8 Avenue        
    ##  2 168th St - Washington Heights 8 Avenue        
    ##  3 175th St                      8 Avenue        
    ##  4 34th St                       8 Avenue        
    ##  5 42nd St                       8 Avenue        
    ##  6 59th St                       8 Avenue        
    ##  7 Inwood - 207th St             8 Avenue        
    ##  8 West 4th St                   8 Avenue        
    ##  9 World Trade Center            8 Avenue        
    ## 10 Times Square-42nd St          Broadway        
    ## 11 59th St-Columbus Circle       Broadway-7th Ave
    ## 12 Times Square                  Broadway-7th Ave
    ## 13 8th Av                        Canarsie        
    ## 14 Franklin Av                   Franklin        
    ## 15 Euclid Av                     Fulton          
    ## 16 Franklin Av                   Fulton          
    ## 17 Howard Beach                  Rockaway

Problem2

``` r
Trash_WCD_df = 
    read_excel("Trash Wheel Collection Data (1).xlsx",sheet = "Mr. Trash Wheel") 
```

    ## New names:
    ## • `` -> `...15`
    ## • `` -> `...16`

``` r
Trash_WCD_df=janitor::clean_names(Trash_WCD_df) 
Trash_WCD_df%>%     
drop_na(dumpster) %>% 
    mutate(sports_balls = as.integer(round(sports_balls)),)
```

    ## # A tibble: 547 × 16
    ##    dumpster month year  date                weight_tons volume…¹ plast…² polys…³
    ##       <dbl> <chr> <chr> <dttm>                    <dbl>    <dbl>   <dbl>   <dbl>
    ##  1        1 May   2014  2014-05-16 00:00:00        4.31       18    1450    1820
    ##  2        2 May   2014  2014-05-16 00:00:00        2.74       13    1120    1030
    ##  3        3 May   2014  2014-05-16 00:00:00        3.45       15    2450    3100
    ##  4        4 May   2014  2014-05-17 00:00:00        3.1        15    2380    2730
    ##  5        5 May   2014  2014-05-17 00:00:00        4.06       18     980     870
    ##  6        6 May   2014  2014-05-20 00:00:00        2.71       13    1430    2140
    ##  7        7 May   2014  2014-05-21 00:00:00        1.91        8     910    1090
    ##  8        8 May   2014  2014-05-28 00:00:00        3.7        16    3580    4310
    ##  9        9 June  2014  2014-06-05 00:00:00        2.52       14    2400    2790
    ## 10       10 June  2014  2014-06-11 00:00:00        3.76       18    1340    1730
    ## # … with 537 more rows, 8 more variables: cigarette_butts <dbl>,
    ## #   glass_bottles <dbl>, grocery_bags <dbl>, chip_bags <dbl>,
    ## #   sports_balls <int>, homes_powered <dbl>, x15 <lgl>, x16 <lgl>, and
    ## #   abbreviated variable names ¹​volume_cubic_yards, ²​plastic_bottles,
    ## #   ³​polystyrene

``` r
Trash_WCD_df
```

    ## # A tibble: 548 × 16
    ##    dumpster month year  date                weight_tons volume…¹ plast…² polys…³
    ##       <dbl> <chr> <chr> <dttm>                    <dbl>    <dbl>   <dbl>   <dbl>
    ##  1        1 May   2014  2014-05-16 00:00:00        4.31       18    1450    1820
    ##  2        2 May   2014  2014-05-16 00:00:00        2.74       13    1120    1030
    ##  3        3 May   2014  2014-05-16 00:00:00        3.45       15    2450    3100
    ##  4        4 May   2014  2014-05-17 00:00:00        3.1        15    2380    2730
    ##  5        5 May   2014  2014-05-17 00:00:00        4.06       18     980     870
    ##  6        6 May   2014  2014-05-20 00:00:00        2.71       13    1430    2140
    ##  7        7 May   2014  2014-05-21 00:00:00        1.91        8     910    1090
    ##  8        8 May   2014  2014-05-28 00:00:00        3.7        16    3580    4310
    ##  9        9 June  2014  2014-06-05 00:00:00        2.52       14    2400    2790
    ## 10       10 June  2014  2014-06-11 00:00:00        3.76       18    1340    1730
    ## # … with 538 more rows, 8 more variables: cigarette_butts <dbl>,
    ## #   glass_bottles <dbl>, grocery_bags <dbl>, chip_bags <dbl>,
    ## #   sports_balls <dbl>, homes_powered <dbl>, x15 <lgl>, x16 <lgl>, and
    ## #   abbreviated variable names ¹​volume_cubic_yards, ²​plastic_bottles,
    ## #   ³​polystyrene

``` r
Professor_TW_df = 
    read_excel("Trash Wheel Collection Data (1).xlsx",sheet = "Professor Trash Wheel") 
Professor_TW_df=janitor::clean_names(Professor_TW_df) 
Professor_TW_df%>% 
drop_na(dumpster) %>% 
    mutate(
      dumpster=as.character(dumpster)
    )
```

    ## # A tibble: 94 × 13
    ##    dumpster month     year date                weight_…¹ volum…² plast…³ polys…⁴
    ##    <chr>    <chr>    <dbl> <dttm>                  <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 1        January   2017 2017-01-02 00:00:00      1.79      15    1950    6080
    ##  2 2        January   2017 2017-01-30 00:00:00      1.58      15    9540   11230
    ##  3 3        February  2017 2017-02-26 00:00:00      2.32      18    8350    9210
    ##  4 4        February  2017 2017-02-26 00:00:00      3.72      15    8590    1030
    ##  5 5        February  2017 2017-02-28 00:00:00      1.45      15    7830    9950
    ##  6 6        March     2017 2017-03-30 00:00:00      1.71      15    8210   10340
    ##  7 7        April     2017 2017-04-01 00:00:00      1.82      15    9830   11020
    ##  8 8        April     2017 2017-04-20 00:00:00      2.37      15    9240    8760
    ##  9 9        May       2017 2017-05-10 00:00:00      2.64      15    9540    8810
    ## 10 10       May       2017 2017-05-26 00:00:00      2.78      15    8230    7800
    ## # … with 84 more rows, 5 more variables: cigarette_butts <dbl>,
    ## #   glass_bottles <dbl>, grocery_bags <dbl>, chip_bags <dbl>,
    ## #   homes_powered <dbl>, and abbreviated variable names ¹​weight_tons,
    ## #   ²​volume_cubic_yards, ³​plastic_bottles, ⁴​polystyrene

``` r
Professor_TW_df
```

    ## # A tibble: 95 × 13
    ##    dumpster month     year date                weight_…¹ volum…² plast…³ polys…⁴
    ##       <dbl> <chr>    <dbl> <dttm>                  <dbl>   <dbl>   <dbl>   <dbl>
    ##  1        1 January   2017 2017-01-02 00:00:00      1.79      15    1950    6080
    ##  2        2 January   2017 2017-01-30 00:00:00      1.58      15    9540   11230
    ##  3        3 February  2017 2017-02-26 00:00:00      2.32      18    8350    9210
    ##  4        4 February  2017 2017-02-26 00:00:00      3.72      15    8590    1030
    ##  5        5 February  2017 2017-02-28 00:00:00      1.45      15    7830    9950
    ##  6        6 March     2017 2017-03-30 00:00:00      1.71      15    8210   10340
    ##  7        7 April     2017 2017-04-01 00:00:00      1.82      15    9830   11020
    ##  8        8 April     2017 2017-04-20 00:00:00      2.37      15    9240    8760
    ##  9        9 May       2017 2017-05-10 00:00:00      2.64      15    9540    8810
    ## 10       10 May       2017 2017-05-26 00:00:00      2.78      15    8230    7800
    ## # … with 85 more rows, 5 more variables: cigarette_butts <dbl>,
    ## #   glass_bottles <dbl>, grocery_bags <dbl>, chip_bags <dbl>,
    ## #   homes_powered <dbl>, and abbreviated variable names ¹​weight_tons,
    ## #   ²​volume_cubic_yards, ³​plastic_bottles, ⁴​polystyrene

``` r
Trash_WCD_df=Trash_WCD_df[,-c(15:16)]
Trash_WCD_df=mutate(Trash_WCD_df,name=as.character("Mr. Trash Wheel"))
Professor_TW_df=mutate(Professor_TW_df,name=as.character("Professor Trash Wheel"))
Trash_WCD_df=mutate(Trash_WCD_df,year=as.double(year))
Trash_Professor= bind_rows(Trash_WCD_df,Professor_TW_df)
Trash_Professor
```

    ## # A tibble: 643 × 15
    ##    dumpster month  year date                weight_tons volume…¹ plast…² polys…³
    ##       <dbl> <chr> <dbl> <dttm>                    <dbl>    <dbl>   <dbl>   <dbl>
    ##  1        1 May    2014 2014-05-16 00:00:00        4.31       18    1450    1820
    ##  2        2 May    2014 2014-05-16 00:00:00        2.74       13    1120    1030
    ##  3        3 May    2014 2014-05-16 00:00:00        3.45       15    2450    3100
    ##  4        4 May    2014 2014-05-17 00:00:00        3.1        15    2380    2730
    ##  5        5 May    2014 2014-05-17 00:00:00        4.06       18     980     870
    ##  6        6 May    2014 2014-05-20 00:00:00        2.71       13    1430    2140
    ##  7        7 May    2014 2014-05-21 00:00:00        1.91        8     910    1090
    ##  8        8 May    2014 2014-05-28 00:00:00        3.7        16    3580    4310
    ##  9        9 June   2014 2014-06-05 00:00:00        2.52       14    2400    2790
    ## 10       10 June   2014 2014-06-11 00:00:00        3.76       18    1340    1730
    ## # … with 633 more rows, 7 more variables: cigarette_butts <dbl>,
    ## #   glass_bottles <dbl>, grocery_bags <dbl>, chip_bags <dbl>,
    ## #   sports_balls <dbl>, homes_powered <dbl>, name <chr>, and abbreviated
    ## #   variable names ¹​volume_cubic_yards, ²​plastic_bottles, ³​polystyrene

``` r
TW_trash=sum(Professor_TW_df$weight_tons[1:94])
filter_TWCD=filter(Trash_WCD_df,year==2020)
TN_sport=sum(filter_TWCD$sports_balls)
TW_trash
```

    ## [1] 190.12

``` r
TN_sport
```

    ## [1] 856

Description： According to the data, the key variables are
month,year,date weights_tons,and so on, which totally has 14 columns and
95 entries.the total weight of trash collected by Professor Trash Wheel
is `sum(Professor_TW_df$weight_tons[1:94])`, and the total number of
sports balls collected by Mr. Trash Wheel in 2020 is
`=sum(filter_TWCD$sports_balls)`, but before we sum that, we need to
select the tumber of data at 2020,and then do the sum step.

Problem3

``` r
Une=read_csv("unemployment.csv")
```

    ## Rows: 68 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (13): Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pols_month=read_csv("pols-month.csv")
```

    ## Rows: 822 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (8): prez_gop, gov_gop, sen_gop, rep_gop, prez_dem, gov_dem, sen_dem, r...
    ## date (1): mon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
S=read_csv("snp.csv")
```

    ## Rows: 787 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): date
    ## dbl (1): close
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
Une
```

    ## # A tibble: 68 × 13
    ##     Year   Jan   Feb   Mar   Apr   May   Jun   Jul   Aug   Sep   Oct   Nov   Dec
    ##    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ##  1  1948   3.4   3.8   4     3.9   3.5   3.6   3.6   3.9   3.8   3.7   3.8   4  
    ##  2  1949   4.3   4.7   5     5.3   6.1   6.2   6.7   6.8   6.6   7.9   6.4   6.6
    ##  3  1950   6.5   6.4   6.3   5.8   5.5   5.4   5     4.5   4.4   4.2   4.2   4.3
    ##  4  1951   3.7   3.4   3.4   3.1   3     3.2   3.1   3.1   3.3   3.5   3.5   3.1
    ##  5  1952   3.2   3.1   2.9   2.9   3     3     3.2   3.4   3.1   3     2.8   2.7
    ##  6  1953   2.9   2.6   2.6   2.7   2.5   2.5   2.6   2.7   2.9   3.1   3.5   4.5
    ##  7  1954   4.9   5.2   5.7   5.9   5.9   5.6   5.8   6     6.1   5.7   5.3   5  
    ##  8  1955   4.9   4.7   4.6   4.7   4.3   4.2   4     4.2   4.1   4.3   4.2   4.2
    ##  9  1956   4     3.9   4.2   4     4.3   4.3   4.4   4.1   3.9   3.9   4.3   4.2
    ## 10  1957   4.2   3.9   3.7   3.9   4.1   4.3   4.2   4.1   4.4   4.5   5.1   5.2
    ## # … with 58 more rows

``` r
pols_month
```

    ## # A tibble: 822 × 9
    ##    mon        prez_gop gov_gop sen_gop rep_gop prez_dem gov_dem sen_dem rep_dem
    ##    <date>        <dbl>   <dbl>   <dbl>   <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 1947-01-15        0      23      51     253        1      23      45     198
    ##  2 1947-02-15        0      23      51     253        1      23      45     198
    ##  3 1947-03-15        0      23      51     253        1      23      45     198
    ##  4 1947-04-15        0      23      51     253        1      23      45     198
    ##  5 1947-05-15        0      23      51     253        1      23      45     198
    ##  6 1947-06-15        0      23      51     253        1      23      45     198
    ##  7 1947-07-15        0      23      51     253        1      23      45     198
    ##  8 1947-08-15        0      23      51     253        1      23      45     198
    ##  9 1947-09-15        0      23      51     253        1      23      45     198
    ## 10 1947-10-15        0      23      51     253        1      23      45     198
    ## # … with 812 more rows

``` r
S
```

    ## # A tibble: 787 × 2
    ##    date    close
    ##    <chr>   <dbl>
    ##  1 7/1/15  2080.
    ##  2 6/1/15  2063.
    ##  3 5/1/15  2107.
    ##  4 4/1/15  2086.
    ##  5 3/2/15  2068.
    ##  6 2/2/15  2104.
    ##  7 1/2/15  1995.
    ##  8 12/1/14 2059.
    ##  9 11/3/14 2068.
    ## 10 10/1/14 2018.
    ## # … with 777 more rows

``` r
pols_month = separate(pols_month,col=mon, into= c('year','month','day'))
pols_month = janitor::clean_names(pols_month)
pols_month=pols_month%>%
  mutate(month = recode(month, "01" = "Jan", "02" = "Feb", "03"="Mar", "04"="Apr","05"="May","06"="Jun", "07"="Jul", "08"="Aug","09"="Sep","10"="Oct","11"="Nov", "12"="Dec")
)
pols_month=pols_month%>%
  mutate(president=ifelse(prez_gop==1, "prez_gop",ifelse(prez_gop==0,"prez_dem","gop_2") ))
pols_month
```

    ## # A tibble: 822 × 12
    ##    year  month day   prez_gop gov_gop sen_gop rep_gop prez_dem gov_dem sen_dem
    ##    <chr> <chr> <chr>    <dbl>   <dbl>   <dbl>   <dbl>    <dbl>   <dbl>   <dbl>
    ##  1 1947  Jan   15           0      23      51     253        1      23      45
    ##  2 1947  Feb   15           0      23      51     253        1      23      45
    ##  3 1947  Mar   15           0      23      51     253        1      23      45
    ##  4 1947  Apr   15           0      23      51     253        1      23      45
    ##  5 1947  May   15           0      23      51     253        1      23      45
    ##  6 1947  Jun   15           0      23      51     253        1      23      45
    ##  7 1947  Jul   15           0      23      51     253        1      23      45
    ##  8 1947  Aug   15           0      23      51     253        1      23      45
    ##  9 1947  Sep   15           0      23      51     253        1      23      45
    ## 10 1947  Oct   15           0      23      51     253        1      23      45
    ## # … with 812 more rows, and 2 more variables: rep_dem <dbl>, president <chr>

``` r
pols_month=select(pols_month, -day,-prez_gop,-prez_dem)
pols_month
```

    ## # A tibble: 822 × 9
    ##    year  month gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president
    ##    <chr> <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>    
    ##  1 1947  Jan        23      51     253      23      45     198 prez_dem 
    ##  2 1947  Feb        23      51     253      23      45     198 prez_dem 
    ##  3 1947  Mar        23      51     253      23      45     198 prez_dem 
    ##  4 1947  Apr        23      51     253      23      45     198 prez_dem 
    ##  5 1947  May        23      51     253      23      45     198 prez_dem 
    ##  6 1947  Jun        23      51     253      23      45     198 prez_dem 
    ##  7 1947  Jul        23      51     253      23      45     198 prez_dem 
    ##  8 1947  Aug        23      51     253      23      45     198 prez_dem 
    ##  9 1947  Sep        23      51     253      23      45     198 prez_dem 
    ## 10 1947  Oct        23      51     253      23      45     198 prez_dem 
    ## # … with 812 more rows

``` r
S = separate(S,date, into= c("month","day",'year'),sep= "/")
S=mutate(S, month = recode(month, "1" = "Jan", "2" = "Feb", "3"="Mar", "4"="Apr","5"="May","6"="Jun", "7"="Jul", "8"="Aug","9"="Sep","10"="Oct","11"="Nov", "12"="Dec")
)
```

``` r
S=mutate(S,year=as.integer(year))
S=mutate(S,year=as.integer(year))
S
```

    ## # A tibble: 787 × 4
    ##    month day    year close
    ##    <chr> <chr> <int> <dbl>
    ##  1 Jul   1        15 2080.
    ##  2 Jun   1        15 2063.
    ##  3 May   1        15 2107.
    ##  4 Apr   1        15 2086.
    ##  5 Mar   2        15 2068.
    ##  6 Feb   2        15 2104.
    ##  7 Jan   2        15 1995.
    ##  8 Dec   1        14 2059.
    ##  9 Nov   3        14 2068.
    ## 10 Oct   1        14 2018.
    ## # … with 777 more rows

``` r
A_df=pivot_longer(Une,Jan:Dec,names_to="month",values_to="amount")
A_df=select(A_df, year=Year,month,amount)
A_df=mutate(A_df,year=as.integer(year))
A_df=mutate(A_df,month=as.integer(month))
```

    ## Warning in mask$eval_all_mutate(quo): 强制改变过程中产生了NA

``` r
A_df
```

    ## # A tibble: 816 × 3
    ##     year month amount
    ##    <int> <int>  <dbl>
    ##  1  1948    NA    3.4
    ##  2  1948    NA    3.8
    ##  3  1948    NA    4  
    ##  4  1948    NA    3.9
    ##  5  1948    NA    3.5
    ##  6  1948    NA    3.6
    ##  7  1948    NA    3.6
    ##  8  1948    NA    3.9
    ##  9  1948    NA    3.8
    ## 10  1948    NA    3.7
    ## # … with 806 more rows

``` r
S=mutate(S, year = ifelse(year<20,year + 2000,year + 1900)
)
S=arrange(S,year,month)
S=relocate(S, year, month,close)
S = mutate(S,year=as.character(year))
```

``` r
unemploynment = janitor::clean_names(Une)
unemploynment = pivot_longer(unemploynment, jan:dec, names_to = "month", values_to = "amount")
unemploynment = mutate(unemploynment,month = recode(month, 'jan'='Jan', 'feb'='Feb', 'mar'= 'Mar', 'apl'= 'Apr', 'jun' = 'Jun', 'jul'= 'Jul', 'aug'= 'Aug', 'sep' = 'Sep', 'oct'= 'Oct', 'nov' = 'Nov', 'dec' = 'Dec'))
S=select(S,-day)
pols_month=mutate(pols_month,year=as.double(year))
S=mutate(S,year=as.double(year))
```

``` r
join_datasets = left_join(pols_month,S)
```

    ## Joining, by = c("year", "month")

``` r
join_datasets = left_join(join_datasets,unemploynment)
```

    ## Joining, by = c("year", "month")

``` r
join_datasets
```

    ## # A tibble: 822 × 11
    ##     year month gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president close
    ##    <dbl> <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>     <dbl>
    ##  1  1947 Jan        23      51     253      23      45     198 prez_dem     NA
    ##  2  1947 Feb        23      51     253      23      45     198 prez_dem     NA
    ##  3  1947 Mar        23      51     253      23      45     198 prez_dem     NA
    ##  4  1947 Apr        23      51     253      23      45     198 prez_dem     NA
    ##  5  1947 May        23      51     253      23      45     198 prez_dem     NA
    ##  6  1947 Jun        23      51     253      23      45     198 prez_dem     NA
    ##  7  1947 Jul        23      51     253      23      45     198 prez_dem     NA
    ##  8  1947 Aug        23      51     253      23      45     198 prez_dem     NA
    ##  9  1947 Sep        23      51     253      23      45     198 prez_dem     NA
    ## 10  1947 Oct        23      51     253      23      45     198 prez_dem     NA
    ## # … with 812 more rows, and 1 more variable: amount <dbl>

Description: 1.After deal with unemployment data, which total has 816
entries, 3 columns,especially( year,month,amount),the dimension is
816x3. The range of years is from 1948 to 2015. 2.After deal with
pols_month data, which total has 822 entries, 9 columns,especially(
year,month,gov_gop,sen_gop,rep_gop,gov_dem,sen_dem,rep_dem,president),the
dimension is 822x9. The range of years is from 1947 to 2015. 3.After
deal with snp data, which total has 787 entries, 3 columns,especially(
year,month,close),the dimension is 787x3. The range of years is from
1950 to 2015.
