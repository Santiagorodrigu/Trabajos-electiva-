library(caret)
# Cargar el conjunto de datos
data <- read.csv("C:/Users/Santi/OneDrive/Documentos/Electiva/Data/diabetes_012_health.csv")

sum(is.na(data))


# Binarizar la variable de clase (Diabetes_012)
data$Diabetes_012 <- ifelse(data$Diabetes_012 > 0, 1, 0)


# Convertir la variable de clase en un factor de dos niveles
data$Diabetes_012 <- factor(data$Diabetes_012, levels = c(0, 1))



# Crear subconjunto equilibrado para "Diabetes_012" (1% del conjunto de datos)
subset_diabetes <- createDataPartition(data$Diabetes_012, p = 0.01, list = FALSE)
data_diabetes <- data[subset_diabetes, ]


# Subconjunto equilibrado para "HeartDiseaseorAttack"
subset_heart <- createDataPartition(data$HeartDiseaseorAttack, p = 0.01, list = FALSE)
data_heart <- data[subset_heart, ]

# Subconjunto equilibrado para "Sex"
subset_sex <- createDataPartition(data$Sex, p = 0.01, list = FALSE)
data_sex <- data[subset_sex, ]


# Entrenar un modelo kNN con \todas las variables

control <- trainControl(method = "cv", number = 10)
model_knn <- train(Diabetes_012 ~ ., data = data_diabetes, method = "knn", trControl = control)


control <- trainControl(method = "cv", number = 10)
model_heart <- train(HeartDiseaseorAttack ~ ., data = data_heart, method = "knn", trControl = control)


control <- trainControl(method = "cv", number = 10)
model_sex <- train(Sex ~ ., data = data_sex, method = "knn", trControl = control)

summary(model_knn)
summary(model_heart)
summary(model_sex)


data_diabetes_reduced <- data_diabetes[, !(names(data_diabetes) %in% c("Smoker", "Veggies", "Fruits", "Education", "Income"))]

# Entrenar un segundo modelo kNN con las variables reducidas (encuentre un k óptimo)
control <- trainControl(method = "cv", number = 5)
model_knn_reduced <- train(Diabetes_012 ~ ., data = data_diabetes_reduced, method = "knn", trControl = control)

# Evaluar el segundo modelo
summary(model_knn_reduced)


# Eliminar 5 predictores adicionales
data_diabetes_reduced2 <- data_diabetes_reduced[, !(names(data_diabetes_reduced) %in% c("GenHlth", "NoDocbcCost", "MentHlth", "PhysHlth", "DiffWalk"))]

# Entrenar un tercer modelo kNN
model_knn_reduced2 <- train(Diabetes_012 ~ ., data = data_diabetes_reduced2, method = "knn", trControl = control)

# Evaluar el tercer modelo con validación cruzada repetida de 10 pliegues
set.seed(123)  # Para reproducibilidad
model_knn_reduced2_repeated <- train(Diabetes_012 ~ ., data = data_diabetes_reduced2, method = "knn", trControl = trainControl(method = "repeatedcv", number = 10, repeats = 10))

# Evaluar el tercer modelo
summary(model_knn_reduced2_repeated)
