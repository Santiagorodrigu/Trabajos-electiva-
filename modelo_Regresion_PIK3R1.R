# Importar las librerías necesarias
library(DynamicCancerDriverKM) # Libreria del paquete solicitado por el profesor 
library(glmnet) # Se utiliza para ajustar modelos de regresión lineal generalizada con regularización tipo L1 (lasso) y L2 (ridge
library(caret) # Se utiliza para entrenar y evaluar modelos predictivos
library(dplyr) # Manipular y transformar datos 
library(pROC) # se utilizada para evaluar el rendimiento del modelo (curva (AUC) y curvas ROC)  
library(tidyverse) # manipulación y visualización de datos 


# Cargar los datos (BRCA_normal Y BRCA_PT) y unirlos en un solo dataset
merged_data <- rbind(DynamicCancerDriverKM::BRCA_normal, DynamicCancerDriverKM::BRCA_PT)

load("C:\\\\\\\\geneScore.rdata")

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

# Filtrar las columnas del dataframe para mantener solo los genes seleccionados
X <- filtered_data[, top_100_genes]


# Convertir la variable de respuesta a factor
y <- as.factor(y)


# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(123)
train_indices <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[train_indices, ]
y_train <- y[train_indices]
X_test <- X[-train_indices, ]
y_test <- y[-train_indices]

# Ajustar un modelo de regresión logística con regularización L1 (lasso)
model <- cv.glmnet(as.matrix(X_train), y_train, family = "binomial", alpha = 1)

# Realizar predicciones en el conjunto de prueba
predictions <- predict(model, newx = as.matrix(X_test), s = "lambda.min", type = "response")

# Convertir las probabilidades a clases
predictions <- as.factor(ifelse(predictions > 0.5, levels(y)[2], levels(y)[1]))

# Evaluar el rendimiento del modelo
confusionMatrix(predictions, y_test)

precision <- posPredValue(predictions, y_test)
roc_curve <- roc(y_test, as.numeric(predictions == "Tumor"), levels = c("Primary Tumor", "Solid Tissue Normal"))
roc_auc <- auc(roc_curve)
cat("Precision:", precision,  "AUC:", roc_auc)
