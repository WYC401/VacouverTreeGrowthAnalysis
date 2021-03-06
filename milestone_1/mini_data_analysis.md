mini-data analysis\_1
================
Yicheng Wang
10/5/2021

# Mini Data Analysis

## 1. Choose Datasets

There are 7 datasets in the datateachr package. Let’s first import the
two packages,datateachr and tidyverse, needed for analysis.

``` r
library(datateachr)
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.5     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.0.2     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

Before finally choose 4 datasets out of 7, the first 6 rows of each
dataset is printed.

``` r
print(head(apt_buildings))
```

    ## # A tibble: 6 x 37
    ##      id air_conditioning amenities   balconies barrier_free_acc~ bike_parking   
    ##   <dbl> <chr>            <chr>       <chr>     <chr>             <chr>          
    ## 1 10359 NONE             Outdoor re~ YES       YES               0 indoor parki~
    ## 2 10360 NONE             Outdoor po~ YES       NO                0 indoor parki~
    ## 3 10361 NONE             <NA>        YES       NO                Not Available  
    ## 4 10362 NONE             <NA>        YES       YES               Not Available  
    ## 5 10363 NONE             <NA>        NO        NO                12 indoor park~
    ## 6 10364 NONE             <NA>        NO        NO                Not Available  
    ## # ... with 31 more variables: exterior_fire_escape <chr>, fire_alarm <chr>,
    ## #   garbage_chutes <chr>, heating_type <chr>, intercom <chr>,
    ## #   laundry_room <chr>, locker_or_storage_room <chr>, no_of_elevators <dbl>,
    ## #   parking_type <chr>, pets_allowed <chr>, prop_management_company_name <chr>,
    ## #   property_type <chr>, rsn <dbl>, separate_gas_meters <chr>,
    ## #   separate_hydro_meters <chr>, separate_water_meters <chr>,
    ## #   site_address <chr>, sprinkler_system <chr>, visitor_parking <chr>, ...

``` r
print(head(building_permits))
```

    ## # A tibble: 6 x 14
    ##   permit_number issue_date project_value type_of_work  address  project_descrip~
    ##   <chr>         <date>             <dbl> <chr>         <chr>    <chr>           
    ## 1 BP-2016-02248 2017-02-01             0 Salvage and ~ 4378 W ~ <NA>            
    ## 2 BU468090      2017-02-01             0 New Building  1111 RI~ <NA>            
    ## 3 DB-2016-04450 2017-02-01         35000 Addition / A~ 3732 W ~ <NA>            
    ## 4 DB-2017-00131 2017-02-01         15000 Addition / A~ 88 W PE~ <NA>            
    ## 5 DB452250      2017-02-01        181178 New Building  492 E 6~ <NA>            
    ## 6 BP-2016-01458 2017-02-02             0 Salvage and ~ 3332 W ~ <NA>            
    ## # ... with 8 more variables: building_contractor <chr>,
    ## #   building_contractor_address <chr>, applicant <chr>,
    ## #   applicant_address <chr>, property_use <chr>, specific_use_category <chr>,
    ## #   year <dbl>, bi_id <dbl>

``` r
print(head(cancer_sample))
```

    ## # A tibble: 6 x 32
    ##         ID diagnosis radius_mean texture_mean perimeter_mean area_mean
    ##      <dbl> <chr>           <dbl>        <dbl>          <dbl>     <dbl>
    ## 1   842302 M                18.0         10.4          123.      1001 
    ## 2   842517 M                20.6         17.8          133.      1326 
    ## 3 84300903 M                19.7         21.2          130       1203 
    ## 4 84348301 M                11.4         20.4           77.6      386.
    ## 5 84358402 M                20.3         14.3          135.      1297 
    ## 6   843786 M                12.4         15.7           82.6      477.
    ## # ... with 26 more variables: smoothness_mean <dbl>, compactness_mean <dbl>,
    ## #   concavity_mean <dbl>, concave_points_mean <dbl>, symmetry_mean <dbl>,
    ## #   fractal_dimension_mean <dbl>, radius_se <dbl>, texture_se <dbl>,
    ## #   perimeter_se <dbl>, area_se <dbl>, smoothness_se <dbl>,
    ## #   compactness_se <dbl>, concavity_se <dbl>, concave_points_se <dbl>,
    ## #   symmetry_se <dbl>, fractal_dimension_se <dbl>, radius_worst <dbl>,
    ## #   texture_worst <dbl>, perimeter_worst <dbl>, area_worst <dbl>, ...

``` r
print(head(flow_sample))
```

    ## # A tibble: 6 x 7
    ##   station_id  year extreme_type month   day  flow sym  
    ##   <chr>      <dbl> <chr>        <dbl> <dbl> <dbl> <chr>
    ## 1 05BB001     1909 maximum          7     7   314 <NA> 
    ## 2 05BB001     1910 maximum          6    12   230 <NA> 
    ## 3 05BB001     1911 maximum          6    14   264 <NA> 
    ## 4 05BB001     1912 maximum          8    25   174 <NA> 
    ## 5 05BB001     1913 maximum          6    11   232 <NA> 
    ## 6 05BB001     1914 maximum          6    18   214 <NA>

``` r
print(head(parking_meters))
```

    ## # A tibble: 6 x 22
    ##   meter_head  r_mf_9a_6p r_mf_6p_10 r_sa_9a_6p r_sa_6p_10 r_su_9a_6p r_su_6p_10
    ##   <chr>       <chr>      <chr>      <chr>      <chr>      <chr>      <chr>     
    ## 1 Twin        $2.00      $4.00      $2.00      $4.00      $2.00      $4.00     
    ## 2 Pay Station $1.00      $1.00      $1.00      $1.00      $1.00      $1.00     
    ## 3 Twin        $1.00      $1.00      $1.00      $1.00      $1.00      $1.00     
    ## 4 Single      $1.00      $1.00      $1.00      $1.00      $1.00      $1.00     
    ## 5 Twin        $2.00      $1.00      $2.00      $1.00      $2.00      $1.00     
    ## 6 Twin        $2.00      $1.00      $2.00      $1.00      $2.00      $1.00     
    ## # ... with 15 more variables: rate_misc <chr>, time_in_effect <chr>,
    ## #   t_mf_9a_6p <chr>, t_mf_6p_10 <chr>, t_sa_9a_6p <chr>, t_sa_6p_10 <chr>,
    ## #   t_su_9a_6p <chr>, t_su_6p_10 <chr>, time_misc <chr>, credit_card <chr>,
    ## #   pay_phone <chr>, longitude <dbl>, latitude <dbl>, geo_local_area <chr>,
    ## #   meter_id <chr>

``` r
print(head(steam_games))
```

    ## # A tibble: 6 x 21
    ##      id url    types  name  desc_snippet recent_reviews all_reviews release_date
    ##   <dbl> <chr>  <chr>  <chr> <chr>        <chr>          <chr>       <chr>       
    ## 1     1 https~ app    DOOM  Now include~ Very Positive~ Very Posit~ May 12, 2016
    ## 2     2 https~ app    PLAY~ PLAYERUNKNO~ Mixed,(6,214)~ Mixed,(836~ Dec 21, 2017
    ## 3     3 https~ app    BATT~ Take comman~ Mixed,(166),-~ Mostly Pos~ Apr 24, 2018
    ## 4     4 https~ app    DayZ  The post-so~ Mixed,(932),-~ Mixed,(167~ Dec 13, 2018
    ## 5     5 https~ app    EVE ~ EVE Online ~ Mixed,(287),-~ Mostly Pos~ May 6, 2003 
    ## 6     6 https~ bundle Gran~ Grand Theft~ NaN            NaN         NaN         
    ## # ... with 13 more variables: developer <chr>, publisher <chr>,
    ## #   popular_tags <chr>, game_details <chr>, languages <chr>,
    ## #   achievements <dbl>, genre <chr>, game_description <chr>,
    ## #   mature_content <chr>, minimum_requirements <chr>,
    ## #   recommended_requirements <chr>, original_price <dbl>, discount_price <dbl>

``` r
print(head(vancouver_trees))
```

    ## # A tibble: 6 x 20
    ##   tree_id civic_number std_street genus_name species_name cultivar_name  
    ##     <dbl>        <dbl> <chr>      <chr>      <chr>        <chr>          
    ## 1  149556          494 W 58TH AV  ULMUS      AMERICANA    BRANDON        
    ## 2  149563          450 W 58TH AV  ZELKOVA    SERRATA      <NA>           
    ## 3  149579         4994 WINDSOR ST STYRAX     JAPONICA     <NA>           
    ## 4  149590          858 E 39TH AV  FRAXINUS   AMERICANA    AUTUMN APPLAUSE
    ## 5  149604         5032 WINDSOR ST ACER       CAMPESTRE    <NA>           
    ## 6  149616          585 W 61ST AV  PYRUS      CALLERYANA   CHANTICLEER    
    ## # ... with 14 more variables: common_name <chr>, assigned <chr>,
    ## #   root_barrier <chr>, plant_area <chr>, on_street_block <dbl>,
    ## #   on_street <chr>, neighbourhood_name <chr>, street_side_name <chr>,
    ## #   height_range_id <dbl>, diameter <dbl>, curb <chr>, date_planted <date>,
    ## #   longitude <dbl>, latitude <dbl>

### 1.1 Choose 4 Out of 7

After seeing the head of dataset, my final decisions are:  
1. parking\_meters  
2. steam\_games  
3. cancer\_sample  
4. vancouver\_trees

### 1.2 Exploring Dataset

Let’s narrow down the choice. To do this, we need to do some exploration
about the four datasets. We consider to print number of rows and
columns, number of numerical columns and class of the variable.

``` r
#write a loop to show attributes of parking_meters
a=list(vancouver_trees,parking_meters,steam_games,cancer_sample)
for (val in a)
{
  #cat(as.character(val),"\n")
  cat("The number of rows ",nrow(val),"\n")
  cat("The number of columns ",ncol(val),"\n")
  cat("The class of the object",class(val),"\n")
  cat("The number of numeric columns",length(select_if(val,is.numeric)),"\n\n") #calculate the number of numeric columns 
}
```

    ## The number of rows  146611 
    ## The number of columns  20 
    ## The class of the object tbl_df tbl data.frame 
    ## The number of numeric columns 7 
    ## 
    ## The number of rows  10032 
    ## The number of columns  22 
    ## The class of the object tbl_df tbl data.frame 
    ## The number of numeric columns 2 
    ## 
    ## The number of rows  40833 
    ## The number of columns  21 
    ## The class of the object spec_tbl_df tbl_df tbl data.frame 
    ## The number of numeric columns 4 
    ## 
    ## The number of rows  569 
    ## The number of columns  32 
    ## The class of the object spec_tbl_df tbl_df tbl data.frame 
    ## The number of numeric columns 31

### 1.3 Narrowing Down To 2

I choose the dataset vancouver\_trees and steam\_games because it has a
relatively appropriate number of numeric columns. It is easier to do the
data analysis on dataframe with more numeric columns. But too many
numeric columns make me lost in numbers. So, I prefer the dataset with
medium number of numeric columns.

### 1.4 Final Decision

-   For the dataset vancouver\_trees, I am interested in the problem
    whether different kinds of trees are equally distributed among the
    Vancouver and whether specific category of trees will concentrate on
    some area.
-   For the dataset steam\_games, I am interested in the relationship
    between the number of languages the game support and its price.

After mininutes of thinking, the first research problem is more
attractive to me. So, I decided to choose the dataset vancouver\_trees.

## 2.Exploring Dataset of Interest

### 2.1 Plotting and Extracting

First, let’s draw a histogram of numeric variable diameter. As we can
see the picture, the diameter of trees concentrates on 10.

``` r
plot_1 <- ggplot(vancouver_trees, aes(x = diameter)) + 
    geom_histogram(bins=50)
print(plot_1)
```

![](webpage_preview_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Second, I wan to narrow my search down to trees of Americana species.

``` r
filter(vancouver_trees,species_name=="AMERICANA")
```

    ## # A tibble: 5,515 x 20
    ##    tree_id civic_number std_street genus_name species_name cultivar_name  
    ##      <dbl>        <dbl> <chr>      <chr>      <chr>        <chr>          
    ##  1  149556          494 W 58TH AV  ULMUS      AMERICANA    BRANDON        
    ##  2  149590          858 E 39TH AV  FRAXINUS   AMERICANA    AUTUMN APPLAUSE
    ##  3  149625          720 E 39TH AV  FRAXINUS   AMERICANA    AUTUMN APPLAUSE
    ##  4  155413         2485 W BROADWAY ULMUS      AMERICANA    BRANDON        
    ##  5  155555          779 E 39TH AV  FRAXINUS   AMERICANA    AUTUMN APPLAUSE
    ##  6  155638         4289 WINDSOR ST FRAXINUS   AMERICANA    AUTUMN APPLAUSE
    ##  7  155660         5623 ONTARIO ST FRAXINUS   AMERICANA    AUTUMN APPLAUSE
    ##  8  155661         5655 ONTARIO ST FRAXINUS   AMERICANA    AUTUMN APPLAUSE
    ##  9  155863         4805 ARGYLE ST  FRAXINUS   AMERICANA    AUTUMN APPLAUSE
    ## 10  155865         4805 ARGYLE ST  FRAXINUS   AMERICANA    AUTUMN APPLAUSE
    ## # ... with 5,505 more rows, and 14 more variables: common_name <chr>,
    ## #   assigned <chr>, root_barrier <chr>, plant_area <chr>,
    ## #   on_street_block <dbl>, on_street <chr>, neighbourhood_name <chr>,
    ## #   street_side_name <chr>, height_range_id <dbl>, diameter <dbl>, curb <chr>,
    ## #   date_planted <date>, longitude <dbl>, latitude <dbl>

Third, we want to explore the relationship between diameter and
latitude. The picture shows that the diameter of trees seem to
distribute equally under the different lantitude.

``` r
ggplot(vancouver_trees)+geom_point(aes(x=diameter,y=latitude,alpha = 0.001))
```

    ## Warning: Removed 22771 rows containing missing values (geom_point).

![](webpage_preview_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Forth. a boxplot is drawn to see if there is difference between diameter
distribution of those with tree barriers and those without. As we see,
the trees with barriers tend to have less diameter in term of the mean.

``` r
#there are too many outliers with diameter exceeding 80. To make the box part clearer, I set the limitation of y axis to 50 instead of 100 as default.
ggplot(vancouver_trees,aes(x=root_barrier,y=diameter))+geom_boxplot(width=0.3)+ coord_cartesian( ylim = c(0, 40))
```

![](webpage_preview_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

### 2.2 Explanations for Plotting and Data Transformation Above

First, drawing a histogram is to see how diameter of trees distribute.If
the histogram has more than one peak, that may be caused by some latent
categories of tree data.  
Second, searching for the trees of Americana species can help us
understand if different species trees have some specific traits like
specific mean of diameter.  
Third, the scatterplot of diameter and latitude can show us the
relationship of the two variables. For example, linear relationship
between diameter and latitude lead us to thinking about the geographical
factors may affect trees’ growth.  
Last, the boxplot of diameter is plotted to see if the trees with
barriers have bigger or smaller size. It is common sense that tree
barrier will inhibit trees’ growth.

## 3. Research Problems

1.  Are the parameters of the tree like diameter equally distributed
    among different species?
2.  Will some species of trees concentrate on some places? Or are the
    trees equally distributed among the map?
3.  What is the relationship between the age of trees and their diameter
    or other parameter?
4.  What is effect of root barrier on the growth of trees(their
    diameter)?
