
library(nycflights13)
library(tidyverse)

vuelos_totales <- nycflights13::flights

Ejercicio_1 <- {
# 5.2.4 Exercises: items 1, and 2
vuelos_retrasados <- filter(vuelos_totales, arr_delay >= 120)
vuelos_huston <-filter(vuelos_totales, dest == "IAH" | dest == "HOU")
}


Ejercicio_2 <- {
# 5.3.1 Exercises: all items
vuelos_rapidos <-  head(arrange(flights, air_time))
vuelos_lejanos <-  arrange(flights, distance)
}


Ejercicio_3 <- {
# 5.4.1 Exercises: items 2, 3, and 4
vuelos_seleccionar <-  select(vuelos_totales, year, month, day)
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars))
vuelos_pt <- select(flights, contains("TIME"))
}


Ejercicio_4 <- {
# 5.5.2 Exercises: items 1, and 2
vuelos_modificados <- vuelos_totales %>%
  mutate(
    dep_time_mins = (dep_time %/% 100) * 60 + dep_time %% 100,
    sched_dep_time_mins = (sched_dep_time %/% 100) * 60 + sched_dep_time %% 100)

comparar_resultados <- vuelos_modificados %>%
    mutate(arr_dep_time_diff = arr_time - dep_time_mins) %>%
      filter(!is.na(air_time) & !is.na(arr_dep_time_diff)) %>%
        select(air_time, arr_dep_time_diff)
}

Ejercicio_5 <- {
# 5.6.7 Exercises: item 1
print(
  
  "What this question gets at is a fundamental question of data analysis: the cost function. As analysts, the reason we are interested in flight delay because it is costly to passengers. But it is worth thinking carefully about how it is costly and use that information in ranking and measuring these scenarios.
  
  In many scenarios, arrival delay is more important. In most cases, being arriving late is more costly to the passenger since it could disrupt the next stages of their travel, such as connecting flights or scheduled meetings.
  If a departure is delayed without affecting the arrival time, this delay will not have those affects plans nor does it affect the total time spent traveling. This delay could be beneficial, if less time is spent in the cramped confines of the airplane itself, or a negative, if that delayed time is still spent in the cramped confines of the airplane on the runway.
  
  Variation in arrival time is worse than consistency. If a flight is always 30 minutes late and that delay is known, then it is as if the arrival time is that delayed time. The traveler could easily plan for this. But higher variation in flight times makes it harder to plan."
)
}  

Ejercicio_6 <- {
# 5.7.1 Exercises: item 2
peor_vuelo <- vuelos_totales %>% 
  filter(!is.na(tailnum), is.na(arr_time) | !is.na(arr_delay)) %>%
  mutate(on_time = !is.na(arr_time) & (arr_delay <= 0)) %>%
  group_by(tailnum) %>%
  summarise(on_time = mean(on_time), n = n()) %>%
  filter(n >= 20) %>%
  filter(min_rank(on_time) == 1)
peor_vuelo
}


retrieve_answer <- function(option = 0) {
   if (option == 1){
    result <- print(Ejercicio_1)
                    print("")
       }
   else if  (option == 2){
    result <- print(Ejercicio_2)
                    print("hola")
  }
  else if  (option == 3){
    result <- print(Ejercicio_3)
                    print("hola")
  }
  else if  (option == 4){
    result <- print(Ejercicio_4)
                    print("hola")
  }
  else if  (option == 5){
    result <- print(Ejercicio_5)
                    print("hola")
  }
  else if  (option == 2){
    result <- print(Ejercicio_2)
                    print("hola")
    
  }else{
    result <- print("LA OPCION ESCOGIDA NO ES CORRECTA, ESCOGE OTRA OPCION EN EL RANGO DE 1 A 6")
   }
   return(result) 
} 