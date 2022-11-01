Cyclistic: case study
================
Joris Gauliard
2022-09-01

# How Does a Bike-Share Navigate Speedy Success?

### Description of the business task

Cyclistic is a fictional bike-share company based in Chicago. The
company owns about 5800 bicycles and 600 docking stations.

In-house finance analysts have recently concluded that annual members
are much more profitable than casual riders.As a result, the strategy of
the director of marketing is clear: converting casual riders into annual
members to increase the profitability of the company.

In order to better understand why casual riders would buy a membership,
the marketing team is interested in analyzing the historical bike trip
data to identify trends.

The result of this analysis, in the form of a data-driven marketing
strategy proposal, will be submitted to the executive team for its
approval.

The scope of this study is to understand **how annual members and casual
riders use Cyclistic bikes differently.**

### Data Source

The [historical bike trip
data](https://divvy-tripdata.s3.amazonaws.com/index.html) used in this
study has been made available by Motivate International Inc. under this
[licence](https://ride.divvybikes.com/data-license-agreement).

Data starting from January 2021 has been downloaded and stored in a
folder.

Libraries are loaded for future use

Files are combined into one main dataframe (df) using reader::read_csv()
and base::rbind()

``` r
filenames <- list.files(path="data", pattern="*.csv", full.names=TRUE)
df <- do.call("rbind", lapply(filenames, read_csv, show_col_types = FALSE))
```

Summary functions help to get to know the dataset

``` r
dt.haversine <- function(lat_from, lon_from, lat_to, lon_to, r = 6378137){
  radians <- pi/180
  lat_to <- lat_to * radians
  lat_from <- lat_from * radians
  lon_to <- lon_to * radians
  lon_from <- lon_from * radians
  dLat <- (lat_to - lat_from)
  dLon <- (lon_to - lon_from)
  a <- (sin(dLat/2)^2) + (cos(lat_from) * cos(lat_to)) * (sin(dLon/2)^2)
  return(2 * atan2(sqrt(a), sqrt(1 - a)) * r)
}

df$ride_distance <- dt.haversine(df$start_lat,df$start_lng,df$end_lat,df$end_lng)
df$ride_distance <- as.integer(df$ride_distance)
```

``` r
#skim(df)
#head(df)
str(df)
```

    ## spec_tbl_df [8,697,283 × 14] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ ride_id           : chr [1:8697283] "E19E6F1B8D4C42ED" "DC88F20C2C55F27F" "EC45C94683FE3F27" "4FA453A75AE377DB" ...
    ##  $ rideable_type     : chr [1:8697283] "electric_bike" "electric_bike" "electric_bike" "electric_bike" ...
    ##  $ started_at        : POSIXct[1:8697283], format: "2021-01-23 16:14:19" "2021-01-27 18:43:08" ...
    ##  $ ended_at          : POSIXct[1:8697283], format: "2021-01-23 16:24:44" "2021-01-27 18:47:12" ...
    ##  $ start_station_name: chr [1:8697283] "California Ave & Cortez St" "California Ave & Cortez St" "California Ave & Cortez St" "California Ave & Cortez St" ...
    ##  $ start_station_id  : chr [1:8697283] "17660" "17660" "17660" "17660" ...
    ##  $ end_station_name  : chr [1:8697283] NA NA NA NA ...
    ##  $ end_station_id    : chr [1:8697283] NA NA NA NA ...
    ##  $ start_lat         : num [1:8697283] 41.9 41.9 41.9 41.9 41.9 ...
    ##  $ start_lng         : num [1:8697283] -87.7 -87.7 -87.7 -87.7 -87.7 ...
    ##  $ end_lat           : num [1:8697283] 41.9 41.9 41.9 41.9 41.9 ...
    ##  $ end_lng           : num [1:8697283] -87.7 -87.7 -87.7 -87.7 -87.7 ...
    ##  $ member_casual     : chr [1:8697283] "member" "member" "member" "member" ...
    ##  $ ride_distance     : int [1:8697283] 2244 556 280 2250 276 4541 1098 1103 267 2034 ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   ride_id = col_character(),
    ##   ..   rideable_type = col_character(),
    ##   ..   started_at = col_datetime(format = ""),
    ##   ..   ended_at = col_datetime(format = ""),
    ##   ..   start_station_name = col_character(),
    ##   ..   start_station_id = col_character(),
    ##   ..   end_station_name = col_character(),
    ##   ..   end_station_id = col_character(),
    ##   ..   start_lat = col_double(),
    ##   ..   start_lng = col_double(),
    ##   ..   end_lat = col_double(),
    ##   ..   end_lng = col_double(),
    ##   ..   member_casual = col_character()
    ##   .. )
    ##  - attr(*, "problems")=<externalptr>

``` r
#glimpse(df)
#colnames(df)
#skim_without_charts(df)
```

### Cleaning and sorting the data to prepare for analysis

We start by removing the columns we do not need for this analysis. We
then create few date-related columns to be able to identify ride length
patterns as a function of those attributes. We sort the data by
ascending ride legnth

``` r
Sys.setlocale("LC_TIME", "en_US.UTF-8")
```

    ## [1] "en_US.UTF-8"

``` r
df_v2<-
  df%>%
  select(-c(start_lat,start_lng,end_lat,end_lng,start_station_name,start_station_id,end_station_name,end_station_id))%>%
  mutate(ride_length = difftime(ended_at,started_at))%>%
  mutate(date = as.Date(started_at))%>%
  mutate(year = format(as.Date(started_at), "%Y"))%>%
  mutate(month = format(as.Date(started_at), "%m"))%>%
  mutate(day_of_week = weekdays(started_at))%>%
  mutate(hour_of_day = hour(started_at))%>%
  arrange(ride_length)

df_v2 <- df_v2[!(df_v2$ride_length <0 ),] #removing observations with a negative ride length

# Convert ride length from factor to numeric
is.factor(df_v2$ride_length)
```

    ## [1] FALSE

``` r
df_v2$ride_length <- as.numeric(as.character(df_v2$ride_length))
is.numeric(df_v2$ride_length)
```

    ## [1] TRUE

``` r
df_v2 <- df_v2%>%
  select(-c(started_at,ended_at))

colnames(df_v2)
```

    ##  [1] "ride_id"       "rideable_type" "member_casual" "ride_distance"
    ##  [5] "ride_length"   "date"          "year"          "month"        
    ##  [9] "day_of_week"   "hour_of_day"

Checking if the dataframe contains NA

``` r
colSums(is.na(df_v2))
```

    ##       ride_id rideable_type member_casual ride_distance   ride_length 
    ##             0             0             0          8240             0 
    ##          date          year         month   day_of_week   hour_of_day 
    ##             0             0             0             0             0

``` r
df_v2 <- na.omit(df_v2)
#colSums(is.na(df_v2))
```

We can now consider that the data is prepared for further analysis. The
dataset contains 8688865 observations.

``` r
head(df_v2)
```

    ## # A tibble: 6 × 10
    ##   ride_id ridea…¹ membe…² ride_…³ ride_…⁴ date       year  month day_o…⁵ hour_…⁶
    ##   <chr>   <chr>   <chr>     <int>   <dbl> <date>     <chr> <chr> <chr>     <int>
    ## 1 6B5129… classi… member      259       0 2021-01-15 2021  01    Friday       16
    ## 2 3F0277… classi… member        0       0 2021-01-29 2021  01    Friday       21
    ## 3 417EE4… electr… member      398       0 2021-01-14 2021  01    Thursd…      17
    ## 4 FBFC52… electr… member      455       0 2021-01-14 2021  01    Thursd…      17
    ## 5 578B5E… electr… casual      283       0 2021-02-24 2021  02    Wednes…      21
    ## 6 2A63B4… classi… member        0       0 2021-02-08 2021  02    Monday       11
    ## # … with abbreviated variable names ¹​rideable_type, ²​member_casual,
    ## #   ³​ride_distance, ⁴​ride_length, ⁵​day_of_week, ⁶​hour_of_day

## Descriptive analysis

Here is a summary of the ride length column (all figures in seconds)

``` r
summary(df_v2$ride_length)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       0     390     695    1184    1261 3356649

The overall ride length average is
*`as.integer(mean(df_v2$ride_length)/12))` min*

``` r
aggregate(df_v2$ride_length ~ df_v2$member_casual, FUN = mean)
```

    ##   df_v2$member_casual df_v2$ride_length
    ## 1              casual         1685.6687
    ## 2              member          787.5082

## The average ride length of casual riders is significantly greater than the average ride length of annual members

One way to explain this is that annual members most likely use bikes to
go to work, whereas casual riders may use them more for touristic
purpose.

``` r
aggregate(df_v2$ride_length ~ df_v2$member_casual, FUN = median)
```

    ##   df_v2$member_casual df_v2$ride_length
    ## 1              casual               920
    ## 2              member               564

``` r
aggregate(df_v2$ride_length ~ df_v2$member_casual, FUN = max)
```

    ##   df_v2$member_casual df_v2$ride_length
    ## 1              casual           3356649
    ## 2              member             89996

``` r
aggregate(df_v2$ride_length ~ df_v2$member_casual, FUN = min)
```

    ##   df_v2$member_casual df_v2$ride_length
    ## 1              casual                 0
    ## 2              member                 0

## The median ride length of casual riders is significantly greater than the median ride length of annual members

``` r
# Notice that the days of the week are out of order. Let's fix that.
df_v2$day_of_week <- ordered(df_v2$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(df_v2$ride_length ~ df_v2$member_casual + df_v2$day_of_week, FUN = mean)
```

    ##    df_v2$member_casual df_v2$day_of_week df_v2$ride_length
    ## 1               casual            Monday         1708.4593
    ## 2               member            Monday          764.8715
    ## 3               casual           Tuesday         1491.0370
    ## 4               member           Tuesday          742.2178
    ## 5               casual         Wednesday         1460.9048
    ## 6               member         Wednesday          744.3941
    ## 7               casual          Thursday         1461.2255
    ## 8               member          Thursday          746.8582
    ## 9               casual            Friday         1582.8132
    ## 10              member            Friday          767.7987
    ## 11              casual          Saturday         1844.1079
    ## 12              member          Saturday          880.4036
    ## 13              casual            Sunday         1953.5458
    ## 14              member            Sunday          890.1402

### Let’s visualize the average ride distance by rider type

``` r
df_v2 %>% 
  group_by(member_casual) %>% 
  summarise(average_distance = mean(ride_distance)) %>% 
  arrange(member_casual)  %>% 
  ggplot(aes(x = member_casual, y = average_distance, fill = member_casual)) +geom_col()
```

![](README_figs/README-unnamed-chunk-10-1.png)<!-- -->

### Let’s visualize the average ride length by rider type and day of week

``` r
df_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +geom_col(position = "dodge")
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

![](README_figs/README-unnamed-chunk-11-1.png)<!-- -->

### Let’s visualize the number of rides by rider type and day of week

``` r
df_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +geom_col(position = "dodge")
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

![](README_figs/README-unnamed-chunk-12-1.png)<!-- -->

### Let’s visualize the average ride length by rider type and month

``` r
df_v2 %>% 
  group_by(member_casual, month)%>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length))%>% 
  arrange(member_casual, month)%>%
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) +geom_col(position = "dodge")
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

![](README_figs/README-unnamed-chunk-13-1.png)<!-- -->

### Let’s visualize the number of rides by rider type and month

``` r
df_v2 %>% 
  group_by(member_casual, month)%>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length))%>% 
  arrange(member_casual, month)%>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +geom_col(position = "dodge")
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

![](README_figs/README-unnamed-chunk-14-1.png)<!-- -->

### Let’s visualize the number of rides by rider type and date

``` r
df_v2 %>% 
  group_by(member_casual, date)%>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length))%>% 
  arrange(member_casual, date)%>%
  ggplot(aes(x = date, y = number_of_rides, color = member_casual)) +geom_line()
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

![](README_figs/README-unnamed-chunk-15-1.png)<!-- -->

### Let’s visualize the number of rides by rider type and hour of day

``` r
df_v2 %>% 
  group_by(member_casual, hour_of_day)%>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length))%>% 
  arrange(member_casual, hour_of_day)%>%
  ggplot(aes(x = hour_of_day, y = number_of_rides, color = member_casual)) +geom_line()
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

![](README_figs/README-unnamed-chunk-16-1.png)<!-- -->

# Conclusion

### Casuals: prioritize Cyclistic bikes for leisure

### Members: prioritize Cyclistic bikes for commmuting

The bike trip number of rides has a yearly seasonality which can be
explained by the weather: people use less often bikes in the winter in
Chicago, no matter if it is an annual member or a casual rider. However,
the casual riders number of rides show a weekly seanality whereas it is
not the case for annual members ; annual members tend to use their bikes
the same way throughout the week, while casual riders more oftenly use
bikes on the weekend.

## Both User Types:

Scarcely ride Cyclistic bikes during the colder months  
Pick up and drop off their bikes at roughly the same distance

## Casuals:

Are more numerous during summer and congregate near the bay area  
Tend to be more active during the afternoon and late evenings  
Consistently have longer ride lengths year-round  
Favor docked bikes for lengthy bike trips  
Have a negligible presence during the winter months

## Members:

Have steady average ride lengths year-round  
Are active at all times of the day, more so in the morning and
afternoon  
Have trips spread out throughout Chicago’s downtown area  
Are more consistent with their bike trips year-round

## Speculations

### A. Casuals primarily use Cyclistic bikes for leisure

We make that assumption based on the fact that casuals:

Bike twice as much on Saturdays and Sundays compared to any other day of
the week  
Spend the majority of their bike trips near parks and water  
Spend significantly longer on average on every bike trip, suggesting
that they spend time in-between docking stations doing leisurely
activities  
Scarcely use Cyclistic bikes in the morning (6 am-noon)  
Do not use Cyclistic bikes often enough to warrant paying for an annual
membership

### B. Members get more out of Cyclistic bikes by using them for leisure and commuting consistently

Bbased on the fact that members:

Rely on bikes consistently each week and year-round, with no notable
preference on a single day of the week  
Use Cyclistic bikes often during the rush hours on a typical workday  
Have a large geographical spread in the downtown area, particularly in
high urban dense areas  
Are motivated by the economics of an annual membership pass

# Data that would be needed to have a better comprehension of Cyclistic riders

Age, gender. do we target the right people, potential to be more
inclusive? Pricing plan, to understand spending behavior of casual users
