# Cargar las bibliotecas necesarias
library(dplyr)
library(ggplot2)  # Asegúrate de que ggplot2 está instalado

# Cargar los datos
data <- read.csv("C:/Users/Santi/OneDrive/Documentos/Electiva/Data/diabetes_012_health.csv")

sum(is.na(data))

# Breve descripción de las variables
str(data)

# Realizar un Análisis Exploratorio de Datos (EDA)
# Descripción de variables numéricas
summary(data)

# Distribución de la variable de clase (Diabetes_012)
table(data$Diabetes_012)

ggplot(data = data, aes(x = BMI   , y = log(Diabetes_012))) +
  geom_point(aes(x = BMI), color = "gray30") +
  geom_smooth(aes(y = Diabetes_012), color = "firebrick") +
  theme_bw()


  cor(data)


# Visualización de la distribución de la edad (Age)
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribución de Edad", x = "Edad", y = "Frecuencia")


# Binarizar la variable de clase (Diabetes_012) en 0 (sin diabetes) y 1 (prediabetes/diabetes)
data$Diabetes_012 <- ifelse(data$Diabetes_012 > 0, 1, 0)

# Verificar la binarización
table(data$Diabetes_012)
