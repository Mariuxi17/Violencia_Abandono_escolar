#Cargando librerias
library(readr)
library(dplyr) 
library(tidyverse)
library(janitor)
# Cargar el archivo CSV
panel_data <- read.csv("Documentacion/Violencia_Abandono_escolar/Descriptive_stadistics/Abandono_escolar.csv")
df <- read.csv("Documentacion/Violencia_Abandono_escolar/Descriptive_stadistics/Abandono_escolar.csv")
names(df)
#Correlación variables respecto al abandono

# Eliminar espacios
df_limpio <- clean_names(df)
names(df_limpio)

# cambiar NA por 0.
df[is.na(df_limpio)] <- 0 
View(df)

# Seleccionar las variables de interés
#selected_columns <- df[, c("Abandono", "nviolencia", "victima_mujer", "Victima_Sin_Discapacidad", "Victima_Con_Discapacidad", 
               #                "NO_EXISTE_EMBARAZO", "SI_EXISTE_EMBARAZO", "Infractor.FUERA.del.sistema.educativo", 
                #               "Infractor.DENTRO.del.sistema.educativo", "Relación.Infractor.Pariente", "Relación.Infractor.Desconocido",
                 #              "Relación.Infractor.Enamorado.Novio", "Relación.Infractor.Estudiantes.del.establecimiento", 
                  #             "Relación.Infractor.Conocido.no.pariente", "Relación.Infractor.Docente", "Relación.Infractor.Compañero.de.aula",
                   #            "Relación.Infractor.Otro..Especifique.", "Relación.Infractor.Conserjes.Personal.de.limpieza", 
                   #            "Relación.Infractor.Choferes.de.transporte.escolar", "Relación.Infractor.Personal.administrativo.de.la.IE",
                   #            "Relación.Infractor.Autoridad.de.la.IE")] 

selected_columns <- df[, c(22, 23, 24, 25, 26, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52)] 

cor_matrix <- cor(selected_columns) 

# Regresión 
modelo <- lm(df[, 23] ~ df[, 22] + df[, 24] + df[, 25] + 
               df[, 26] + df[, 36] + df[, 37] + df[, 38] + df[, 39] + df[, 40] +
               df[, 41] + df[, 42] + df[, 43] + df[, 44] + df[, 45] + df[, 46] + 
               df[, 47] + df[, 48] + df[, 49] + df[, 50] + df[, 51] + df[, 52],data = df) 
summary(modelo) 
# Estadística descriptiva

# Agrupar por año lectivo y sumar las columnas de infractores
datos_resumidos <- df_limpio %>%
  group_by(ano_lectivo) %>%
  summarise(
    nviolencia = sum(infractor_fuera_del_sistema_educativo, infractor_dentro_del_sistema_educativo, na.rm = TRUE)
  ) # Se extrajo la suma de los casos de violencia dentro y fuera del sistema educativo para cada año.


#Abanddono escolar por año lectivo
datos_violencia <- df_limpio %>%
  group_by(ano_lectivo) %>%
  summarise(abandono = sum(abandono, na.rm = TRUE), .groups = 'drop') 

# Sumarizar abandonos por año
suma_abandono_por_anio <- df_limpio %>%
  group_by(ano_lectivo) %>%
  summarise(total_abandono = sum(abandono, na.rm = TRUE), .groups = 'drop')

# Sumar total de abandonos en todo el conjunto de datos
total_abandono_general <- sum(df_limpio$abandono, na.rm = TRUE)

# Calcular la suma total de abandonos por año
suma_total_abandono_por_anio <- sum(suma_abandono_por_anio$total_abandono, na.rm = TRUE)

# Comparar ambas sumas
if (suma_total_abandono_por_anio == total_abandono_general) {
  print("Las sumas coinciden.")
} else {
  print("Las sumas NO coinciden.")
}

# Tasa abandono: Calcular el total de abandonos diviico para el total de estudiantes por año lectivo
tasa_abandono <- df_limpio %>%
  group_by(ano_lectivo) %>%
  summarise(
    total_abandonos = sum(abandono, na.rm = TRUE),
    total_estudiantes = sum(total_estudiantes, na.rm = TRUE),
    tasa_abandono = (total_abandonos / total_estudiantes) * 100,
    .groups = 'drop'
  )
View(tasa_abandono)


# Cargar la librería ggplot2
library(ggplot2)
# Modificar la columna ano_lectivo para que contenga solo el primer año
datos_resumidos$ano_lectivo <- gsub("-.*", "", datos_resumidos$ano_lectivo)
tasa_abandono$ano_lectivo <- gsub("-.*", "", tasa_abandono$ano_lectivo)

# Crear el gráfico de barras violencia por año lectivo
ggplot(datos_resumidos, aes(x = factor(ano_lectivo), y = nviolencia)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Año lectivo", y = "Número de casos de violencia", 
       title = "Casos de violencia por año lectivo") +
  theme_minimal()


# Crear imagen abandono escolar por violencia sexual 

ggplot(tasa_abandono, aes(x = as.factor(ano_lectivo), y = tasa_abandono)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "Año lectivo", y = "Tasa de Abandono (%)", 
       title = "Tasa de abandono escolar por año lectivo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#Total estudiantes
total_estudiantes <- sum(df_limpio$total_estudiantes, na.rm = TRUE)

#Casos e violencia
casos_violencia <- sum(df_limpio$n_violencia, na.rm = TRUE)

