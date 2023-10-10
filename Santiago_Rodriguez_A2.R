library(caret)

# Cargar el conjunto de datos
data <- read.csv("C:/Users/Santi/OneDrive/Documentos/Electiva/Data/diabetes_012_health.csv")

sum(is.na(data))


# Binarizar la variable de clase (Diabetes_012)
data$Diabetes_012 <- ifelse(data$Diabetes_012 > 0, 1, 0)


# Convertir la variable de clase en un factor de dos niveles
data$Diabetes_012 <- factor(data$Diabetes_012, levels = c(0, 1))



# Crear un subconjunto de datos para BMI (1% del tamaño original)
set.seed(123)
subset_bmi <- createDataPartition(data$BMI, p = 0.01, list = FALSE)
data_bmi <- data[subset_bmi, ]


subset_mentHlth <- createDataPartition(data$MentHlth, p = 0.01, list = FALSE)
data_mentHlth <- data[subset_mentHlth, ]


subset_physHlth <- createDataPartition(data$PhysHlth, p = 0.01, list = FALSE)
data_physHlth <- data[subset_physHlth, ]



# Entrenar un modelo de regresión lineal para BMI
control <- trainControl(method = "cv", number = 10)
model_linear_bmi <- train(BMI ~ ., data = data_bmi, method = "lm", trControl = control)

# Evaluar el modelo de regresión lineal
summary(model_linear_bmi)


control <- trainControl(method = "cv", number = 10)
model_linear_mentHlth <- train(MentHlth ~ ., data = data_mentHlth, method = "lm", trControl = control)

summary(model_linear_mentHlth)


control <- trainControl(method = "cv", number = 10)
model_linear_physHlth <- train(PhysHlth ~ ., data = data_physHlth, method = "lm", trControl = control)

summary(model_linear_physHlth)


# Eliminar predictores no significativos
data_bmi_reduced <- data_bmi[, !(names(data_bmi) %in% c("Smoker", "Veggies", "Fruits", "Education", "Income"))]

# Entrenar un segundo modelo de regresión lineal con las variables reducidas
model_linear_bmi_reduced <- train(BMI ~ ., data = data_bmi_reduced, method = "lm", trControl = control)

# Evaluar el segundo modelo de regresión lineal
summary(model_linear_bmi_reduced)






# Seleccionar la mejor característica
data_bmi_best_feature <- data_bmi[, !(names(data_bmi) %in% c("Diabetes_012"))]

# Entrenar un tercer modelo de regresión lineal con la mejor variable
model_linear_bmi_best_feature <- train(BMI ~ ., data = data_bmi_best_feature, method = "lm", trControl = trainControl(method = "repeatedcv", number = 10, repeats = 10))

# Evaluar el tercer modelo de regresión lineal
summary(model_linear_bmi_best_feature)