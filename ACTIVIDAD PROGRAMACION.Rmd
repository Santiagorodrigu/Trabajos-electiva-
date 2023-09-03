---
title: "Activity"
author: "Santiago Rlejandro Rodriguez Ramirez"
date: "2023-08-18"
output:
  word_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction 



For the next practice, various functions of the R language were used, explaining each usage and code in detail.

## Load data set 

```{r message=FALSE}
library(nycflights13) 
library(tidyverse)
```

Here, we are loading the flight data set into the **flights_data** object, using the flights function of the **nycflights13** package.

```{r}
vuelos_totales <- nycflights13::flights
```

# 5.2.4 Exercises: Items 1 and 2

This code block solves item 1 of the exercise. We are using the **filter()** function of the tidyverse package to select only the **flights_data** rows where the value in the **arr_delay** (delay on arrival) column is greater than or equal to 2. The result is stored in the **filtered_delayed_flights** object.

```{r}
vuelos_retrasados <- filter(vuelos_totales, arr_delay >= 120)
```

In this code block:

To search for flights with a delay of two hours or more, a filter is used for the arr_delay column, specifying values in minutes.

```{r}
library(knitr)
kable(vuelos_totales[1:10,c(12,9)],caption = "ARRIVE DELAY", align = "c")
```

To search for flights from one destination to another, a filter is used with the specification dest_IAH, dest_HOU.



```{r}
vuelos_huston <-filter(vuelos_totales, dest == "IAH" | dest == "HOU")
```

```{r}
library(knitr)
kable(vuelos_huston[1:10,c(13,14)],caption = "HOUSTON DESTINY", align = "c")
```

# 5.3.1 Exercises: All Items

```{r}
vuelos_ordenados <- arrange(vuelos_totales, is.na(dep_time) ) %>%
  tail()

```

In this code block, we are performing the following actions:


Flights at this point are sorted in a way that the missing values in the database (dep_time) are displayed at the top of the table. Using %>%, we pass flight data through the operator, allowing operations to be chained together. On the other hand, the is.na command works to identify whether there are missing values in a certain column or row, where TRUE indicates a missing value and FALSE indicates a value present. 


```{r}
library(knitr)
kable(vuelos_ordenados[1:10,c(7,12)],caption = "MISSING DATA FIRST", align = "c")
```


```{r}
vuelos_retrasados <- arrange(vuelos_totales,  dep_delay)
```


```{r}
library(knitr)
kable(vuelos_retrasados[1:10,c(6,9,12)],caption = "MOST DELAYED FLIGHTS", align = "c")
```

```{r}
vuelos_rapidos <- head(arrange(flights, air_time))
```

In this code:

arrange(flights, air_time): The arrange() function is used to rearrange the rows of the dataset based on a specific column in ascending order. In this case, it's rearranging the rows in the "flights" dataset based on the "air_time" column (flight time) in ascending order. This means that flights with shorter flight times will appear first in the resulting dataset.

head(arrange(flights, air_time)): The head() function is used to select the first rows of a dataset. In this case, it's selecting the first rows of the "flights" dataset that have been rearranged by the shortest flight time. This creates a new dataset named "vuelos_rapidos" that contains the first rows of "flights" sorted by the shortest flight time.

```{r}
library(knitr)
kable(vuelos_rapidos[1:10,c(12,19)],caption = "FASTEST FLIGHTS", align = "c")
```

```{r}
vuelos_lejanos <-  arrange(flights, distance)
```

In this code:

The code uses the arrange() function from the "dplyr" package to sort the rows of a dataset named "flights" based on the "distance" column in ascending order. This means that the rows of the "flights" dataset will be rearranged so that rows with shorter distances will appear first, and rows with longer distances will appear later.

The result of this operation is assigned to the new dataset called "vuelos_lejanos." Therefore, "vuelos_lejanos" will contain flight information sorted in ascending order based on distance, allowing easy access to flights with shorter distances.


```{r}
library(knitr)
kable(vuelos_lejanos[1:10,c(12,15)],caption = "FARTHEST FLIGHTS", align = "c")
```

```{r}
closest_flights <- flights %>%
  arrange(distance)
```

In this code:

1.  We take the **`flights`** dataset and pass it through the **`%>%`** operator.

2.  We use the **`arrange()`** function to sort the rows in ascending order based on the **`distance`** column. This means that flights with the shortest distances will appear first.

```{r}
library(knitr)
kable(closest_flights[1:10,c(12,15)],caption = "CLOSEST FLIGHTS", align = "c")
```

# 5.4.1 Exercises: Items 2, 3, and 4

The values that need to be organized and presented separately from other data are year, month, and day. To express this data, it is necessary to use the "select()" function, indicating the variables you wish to analyze.

```{r message=FALSE}
vuelos_seleccionar <-  select(vuelos_totales, year, month, day)
```

The "one_of" function selects variables using a character vector instead of arguments specified with quotes, making it easier to generate vectors in programming.

```{r message=FALSE}
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars))

```

"contains" is a function that ignores the uppercase and lowercase of variables. 


```{r message=FALSE}
vuelos_pt <- select(flights, contains("TIME"))
```

This code selects columns whose names contain the string "TIME," such as "dep_time" and "arr_time."

# 5.5.2 Exercises: Items 1 and 2

```{r message=FALSE}

vuelos_modificados <- vuelos_totales %>%
  mutate(
    dep_time_mins = (dep_time %/% 100) * 60 + dep_time %% 100,
    sched_dep_time_mins = (sched_dep_time %/% 100) * 60 + sched_dep_time %% 100)


```

In this code block:

From a tibble named `vuelos_totales`, two new columns are created: `dep_time_mins` and `sched_dep_time_mins`. These columns represent the actual departure time and the scheduled departure time of the flights, converted to minutes since midnight. The conversion process involves dividing the hours by 100 to obtain the whole part (hours), multiplying by 60 to convert hours to minutes, and then adding the remainder of the division by 100 (minutes). The result is stored in the new columns of the tibble named `vuelos_modificados`.


```{r}
library(knitr)
kable(vuelos_modificados[1:10,c(12,4,5,20,21)],caption = "SCHEDULED DEPARTURE TIME", align = "c")
```

```{r message=FALSE}
comparar_resultados <- vuelos_modificados %>%
  mutate(arr_dep_time_diff = arr_time - dep_time_mins) %>%
  filter(!is.na(air_time) & !is.na(arr_dep_time_diff)) %>%
  select(air_time, arr_dep_time_diff)
```

In this second code block:

Using a tibble named `vuelos_modificados`, which contains information about flights with departure and arrival times converted to minutes:

A calculation is performed to find the difference in minutes between the arrival time and the departure time, and this difference is stored in a new column called `arr_dep_time_diff`.

Rows are filtered to keep only those with valid values in the `air_time` (flight time) and `arr_dep_time_diff` columns. Finally, only the `air_time` and `arr_dep_time_diff` columns are selected to create a new tibble named `comparar_resultados`.



```{r}
library(knitr)
kable(comparar_resultados[1:10,c(1,2)],caption = "COMPARISION OF ARRIVES AND DEPARTURES", align = "c")
```

# 5.6.7 Exercises: item 1

**Question: What's More Important - Arrival Delay or Departure Delay?**

What this question gets at is a fundamental question of data analysis: the cost function. As analysts, the reason we are interested in flight delay because it is costly to passengers. But it is worth thinking carefully about how it is costly and use that information in ranking and measuring these scenarios.

In many scenarios, arrival delay is more important. In most cases, being arriving late is more costly to the passenger since it could disrupt the next stages of their travel, such as connecting flights or scheduled meetings.
If a departure is delayed without affecting the arrival time, this delay will not have those affects plans nor does it affect the total time spent traveling. This delay could be beneficial, if less time is spent in the cramped confines of the airplane itself, or a negative, if that delayed time is still spent in the cramped confines of the airplane on the runway.

Variation in arrival time is worse than consistency. If a flight is always 30 minutes late and that delay is known, then it is as if the arrival time is that delayed time. The traveler could easily plan for this. But higher variation in flight times makes it harder to plan.

# 5.7.1 Exercises: item 2

```{r echo=FALSE}

peor_vuelo <- vuelos_totales %>% 
  filter(!is.na(tailnum), is.na(arr_time) | !is.na(arr_delay)) %>%
  mutate(on_time = !is.na(arr_time) & (arr_delay <= 0)) %>%
  group_by(tailnum) %>%
  summarise(on_time = mean(on_time), n = n()) %>%
  filter(n >= 20) %>%
  filter(min_rank(on_time) == 1)
  
peor_vuelo

```

In this code:

1. `filter(!is.na(tailnum), is.na(arr_time) | !is.na(arr_delay))` : Filters the flight data. Retains only rows where "tailnum" is not a missing (NA) value, and where "arr_time" is a missing value or "arr_delay" is not a missing value. This selects flights with tail numbers (tailnum) available and where there is information about arrival time (arr_time) or arrival delay (arr_delay).

3. `mutate(on_time = !is.na(arr_time) & (arr_delay <= 0))`: Create a new column called "on_time". This column is set to true if "arr_time" is not a missing value and if the arrival delay ("arr_delay") is less than or equal to zero, which means that the flight arrived on time or early.

4. `group_by(tailnum)`: Groups the data by the aircraft's tail number ("tailnum"). The following operations will be applied to each group separately.

5. `summarise(on_time = mean(on_time), n = n())`: Computes summaries for each group of flights with the same queue number. Calculates the average of the "on_time" column to measure the proportion of on-time flights in that group and counts the total number of flights in the group ("n").

6. `filter(n >= 20)`: Filters the flight groups to retain only those with at least 20 registered flights. This helps ensure that the results are meaningful and not based on a small amount of data.

7. `filter(min_rank(on_time) == 1)`: Filters the flight groups to retain only the group with the best performance in terms of on-time flights. It uses the `min_rank()` function to assign a rank to each group based on the proportion of on-time flights, and then selects the group with the lowest rank (ie, the best performer).


```{r}
library(knitr)
kable(peor_vuelo[1:10,c(1,2,3)],caption = "WORST PUNCTUALITY TOP", align = "c")
```


