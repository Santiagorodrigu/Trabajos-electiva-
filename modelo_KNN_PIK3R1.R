# Importar las librerías necesarias
library(DynamicCancerDriverKM) # Libreria del paquete solicitado por el profesor
library(class)  # Librería para KNN
library(dplyr) # Manipular y transformar datos
library(caret) # se utiliza para entrenar y evaluar modelos de aprendizaje automático
library(ROCR) # Se utiliza para evaluar el rendimiento de modelos de clasificación
library(tidyverse) # manipulación y visualización de datos 

# Cargar los datos (BRCA_normal Y BRCA_PT) y unirlos en un solo dataset
merged_data <- rbind(DynamicCancerDriverKM::BRCA_normal, DynamicCancerDriverKM::BRCA_PT)

load("C:\\Users\\miller rodriguez\\Downloads\\geneScore.rdata")

# Eliminar las columnas no relevantes para el análisis
columnas_no_relevantes <- c("barcode", "bcr_patient_barcode", "bcr_sample_barcode", "vital_status", "days_to_death", "treatments_radiation_treatment_or_therapy")
merged_data <- merged_data[, !(names(merged_data) %in% columnas_no_relevantes)]

# Verificar si hay datos nulos en el dataset
any(is.na(merged_data))


# VERIFICAR EL UMBRAL DE EXPRESION DE LAS MUESTRAS PARA CADA GEN 

# Obtener la matriz de muestras (sin la columna 'sample_type')
matriz_muestras <- as.matrix(merged_data[, -1])

# Calcular el umbral como el valor máximo en la matriz de muestras
umbral <- 0.0001 * max(matriz_muestras)

# Vector lógico que indica si cada muestra está activa para cada gen
muestras_activas <- matriz_muestras > umbral

# Contar la cantidad de TRUE para cada gen
verdaderos_por_gen <- colSums(muestras_activas)


# FILTRAR LOS GENES QUE SE EXPRESAN EN MAS DEL 20% DE LA MUESTRAS

# Calcular el umbral para conservar la columna con (20% del total de la muestra)
umbral_eliminar_columna <- nrow(matriz_muestras) * 0.2

# Encontrar las columnas a conservar (que tienen menos del 20% de TRUE)
columnas_a_conservar <- which(verdaderos_por_gen >= umbral_eliminar_columna)

# Filtrar el DataFrame original para conservar solo las columnas necesarias
filtered_data <- merged_data[, c(1, columnas_a_conservar + 1)] 


# FILTRAR LOS GENES QUE ESTAN PRESENTES EN "filtered_data" y "geneScore"


# Obtener los nombres de genes en PPI
genes_Score <- prub$features

# Obtener los nombres de genes en filtered_data
genes_merged <- colnames(filtered_data)[-1]  # Excluir la columna "sample_type"

# Encontrar los genes comunes
genes_comunes <- intersect(genes_Score, genes_merged)

# Filtrar el DataFrame PPI para incluir solo las filas con genes comunes
genes_comunes <- prub[genes_Score %in% genes_comunes, ]


# Ordenar el data frame por el campo 'score' de manera descendente
df_sorted <-  genes_comunes %>% arrange(desc(score))
# Seleccionar los primeros 100 genes con mayor score
top_genes <- df_sorted[1:100, ]


# Obtener los nombres de los 100 genes de top_genes
top_100_genes <- top_genes$features



# Obtener la variable de respuesta 'y'
y <- filtered_data$sample_type

# Filtrar las columnas de tu dataframe 'data' para mantener solo los genes seleccionados
X <- filtered_data[, top_100_genes]

# Convertir la variable de respuesta a factor
y <- as.factor(y)

# Normalizar datos
X <- scale(X)

# Dividir los datos en conjuntos de entrenamiento y prueba con validación cruzada
set.seed(123)
indices <- createDataPartition(y, p = 0.7, list = FALSE)
train_indices <- indices
X_train <- X[train_indices, ]
y_train <- y[train_indices]
X_test <- X[-train_indices, ]
y_test <- y[-train_indices]

# Ajustar el modelo KNN con validación cruzada
ctrl <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation
k_values <- c(1, 3, 5, 7, 9)
tune_grid <- expand.grid(k = k_values)
model <- train(X_train, y_train, method = "knn", tuneGrid = tune_grid, trControl = ctrl)

# Realizar predicciones en el conjunto de prueba
predictions <- predict(model, newdata = X_test)

# Evaluar el rendimiento del modelo
confusionMatrix(predictions, y_test)



