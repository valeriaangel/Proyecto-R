#Control de la contaminación del aire
#Trabajo Final
rm(list= ls())
library(pacman)
p_load(tidyverse, lubridate, skimr, stargazer, dplyr, rio, BiocManager, 
       robustbase, estimatr, rdrobust, broom, sandwich)

setwd('/Users/valeria/Desktop/202320/Control de la Contaminación/Trabajo Final')
getwd()
#Cargué la base de datos y omití missing values
data <- import("Base.xlsx")
data <- data %>% filter(!grepl("----", PM2.5, ignore.case = TRUE))
data <- data %>% filter(!grepl("----", PM10, ignore.case = TRUE))
data$DateTime <- as.POSIXct(data$DateTime, format = "%d-%m-%Y %H:%M")
data$Año <- format(data$DateTime, "%Y")
data$Hora <- format(data$DateTime, "%H")
str(data)
data$PM2.5 <- as.numeric(data$PM2.5)
data$PM10 <- as.numeric(data$PM10)
data$log_PM2.5 = log(data$PM2.5)
data$log_PM10 = log(data$PM10)

#Crear columna binaria
library(dplyr)
data <- data %>%
  mutate(año2020 = ifelse(Año == 2020, 1, 0))
#Crear base para 2019, 2020 y 2023
df_2019 <- data %>%
  filter(Año == 2019)

df_2020 <- data %>%
  filter(Año == 2020)

df_2023 <- data %>%
  filter(Año == 2023)

promedios_por_año <- aggregate(PM2.5 ~ Año, data = data, FUN = mean)

ggplot(promedios_por_año, aes(x = Año, y = PM2.5)) +
  geom_bar(stat = "identity", fill = "lightsalmon2", color = "black") +
  labs(title = "Promedio de PM2.5 por Año",
       x = "Año",
       y = "Promedio PM2.5") +
  theme_minimal()

promedios_por_año2 <- aggregate(PM10 ~ Año, data = data, FUN = mean)

ggplot(promedios_por_año2, aes(x = Año, y = PM10)) +
  geom_bar(stat = "identity", fill = "lightsalmon2", color = "black") +
  labs(title = "Promedio de PM10 por Año",
       x = "Año",
       y = "Promedio PM10") +
  theme_minimal()


promedios_por_estacion <- aggregate(PM2.5 ~ Station, data = data, FUN = mean)
promedios_por_estacion1 <- aggregate(PM10 ~ Station, data = data, FUN = mean)

ggplot(promedios_por_estacion, aes(x = Station, y = PM2.5)) +
  geom_bar(stat = "identity", fill = "lightskyblue2", color = "black") +
  labs(title = "Promedio de PM2.5 por Estación",
       x = "Estación",
       y = "Promedio PM2.5") +
  theme_minimal()

ggplot(promedios_por_estacion1, aes(x = Station, y = PM10)) +
  geom_bar(stat = "identity", fill = "lightskyblue2", color = "black") +
  labs(title = "Promedio de PM10 por Estación",
       x = "Estación",
       y = "Promedio PM10") +
  theme_minimal()

promedios_por_hora <- aggregate(PM2.5 ~ Hora, data = data, FUN = mean)
promedios_por_hora1 <- aggregate(PM10 ~ Hora, data = data, FUN = mean)

ggplot(promedios_por_hora, aes(x = Hora, y = PM2.5)) +
  geom_bar(stat = "identity", fill = "mediumpurple1", color = "black") +
  labs(title = "Promedio de PM2.5 por Hora",
       x = "Hora",
       y = "Promedio PM2.5") +
  theme_minimal()

ggplot(promedios_por_hora1, aes(x = Hora, y = PM10)) +
  geom_bar(stat = "identity", fill = "mediumpurple1", color = "black") +
  labs(title = "Promedio de PM10 por Hora",
       x = "Hora",
       y = "Promedio PM10") +
  theme_minimal()
##Promedio hora 2019, 2020, 2023
promedios_por_hora2019 <- aggregate(PM2.5 ~ Hora, data = df_2019, FUN = mean)
promedios_por_hora20191 <- aggregate(PM10 ~ Hora, data = df_2019, FUN = mean)

ggplot(promedios_por_hora2019, aes(x = Hora, y = PM2.5)) +
  geom_bar(stat = "identity", fill = "cadetblue3", color = "black") +
  labs(title = "Promedio de PM2.5 por Hora Para 2019",
       x = "Hora",
       y = "Promedio PM2.5") +
  theme_minimal()

ggplot(promedios_por_hora20191, aes(x = Hora, y = PM10)) +
  geom_bar(stat = "identity", fill = "indianred", color = "black") +
  labs(title = "Promedio de PM10 por Hora Para 2019",
       x = "Hora",
       y = "Promedio PM10") +
  theme_minimal()

promedios_por_hora2020 <- aggregate(PM2.5 ~ Hora, data = df_2020, FUN = mean)
promedios_por_hora20201 <- aggregate(PM10 ~ Hora, data = df_2020, FUN = mean)

ggplot(promedios_por_hora2020, aes(x = Hora, y = PM2.5)) +
  geom_bar(stat = "identity", fill = "cadetblue3", color = "black") +
  labs(title = "Promedio de PM2.5 por Hora Para 2020",
       x = "Hora",
       y = "Promedio PM2.5") +
  theme_minimal()

ggplot(promedios_por_hora20201, aes(x = Hora, y = PM10)) +
  geom_bar(stat = "identity", fill = "indianred", color = "black") +
  labs(title = "Promedio de PM10 por Hora Para 2020",
       x = "Hora",
       y = "Promedio PM10") +
  theme_minimal()

promedios_por_hora2023 <- aggregate(PM2.5 ~ Hora, data = df_2023, FUN = mean)
promedios_por_hora20231 <- aggregate(PM10 ~ Hora, data = df_2023, FUN = mean)

ggplot(promedios_por_hora2023, aes(x = Hora, y = PM2.5)) +
  geom_bar(stat = "identity", fill = "cadetblue3", color = "black") +
  labs(title = "Promedio de PM2.5 por Hora Para 2023",
       x = "Hora",
       y = "Promedio PM2.5") +
  theme_minimal()

ggplot(promedios_por_hora20231, aes(x = Hora, y = PM10)) +
  geom_bar(stat = "identity", fill = "indianred", color = "black") +
  labs(title = "Promedio de PM10 por Hora Para 2023",
       x = "Hora",
       y = "Promedio PM10") +
  theme_minimal()


df_totales <- bind_rows(
  mutate(promedios_por_hora2019, tabla = "2019"),
  mutate(promedios_por_hora2020, tabla = "2020"),
  mutate(promedios_por_hora2023, tabla = "2023")
)

ggplot(df_totales, aes(x = Hora, y = PM2.5, color = tabla, group = tabla)) +
  geom_line() +
  labs(title = "Evolución de concentraciones PM2.5",
       x = "Hora",
       y = "PM2.5") +
  theme_minimal()

df_totales1 <- bind_rows(
  mutate(promedios_por_hora20191, tabla = "2019"),
  mutate(promedios_por_hora20201, tabla = "2020"),
  mutate(promedios_por_hora20231, tabla = "2023")
)

ggplot(df_totales1, aes(x = Hora, y = PM10, color = tabla, group = tabla)) +
  geom_line() +
  labs(title = "Evolución de concentraciones PM10",
       x = "Hora",
       y = "PM10") +
  theme_minimal()

# Ajustar un modelo de regresión temporal para PM2.5
modelo_pm25 <- lm(PM2.5 ~ DateTime, data = data)
# Ajustar un modelo de regresión temporal para PM10
modelo_pm10 <- lm(PM10 ~ DateTime, data = data)
#stargazer(modelo_pm10, type = "html", out="modelo_pm10.html")
# Visualizar las tendencias a lo largo del tiempo
ggplot(data, aes(x = DateTime)) +
  geom_line(aes(y = PM2.5), color = "navy", size = 1, alpha = 0.7) +
  geom_line(aes(y = PM10), color = "steelblue", size = 1, alpha = 0.7) +
  labs(title = "Tendencias Temporales de PM2.5 y PM10",
       y = "Concentración",
       x = "DateTime") +
  theme_minimal()

ggplot(data = data, aes(x = VelViento, y = PM2.5)) +
  geom_point(color="darkolivegreen4") +
  geom_smooth(method = "lm",formula = y ~ poly(x, 2), se = FALSE, color="darkolivegreen") +
  labs(title = paste("Velocidad del viento vs PM 2.5 Concentrations"),
       x = "Velocidad del viento",
       y = "PM2.5 Concentration") +
  theme_minimal()

ggplot(data = data, aes(x = VelViento, y = PM10)) +
  geom_point(color="darkolivegreen4") +
  geom_smooth(method = "lm",formula = y ~ poly(x, 2), se = FALSE, color="darkolivegreen") +
  labs(title = paste("Velocidad del viento vs PM 10 Concentrations"),
       x = "Velocidad del viento",
       y = "PM10 Concentration") +
  theme_minimal()

ggplot(data = data, aes(x = PM10, y = PM2.5)) +
  geom_point(color="darkolivegreen4") +
  geom_smooth(method = "lm",formula = y ~ poly(x, 2), se = FALSE, color="darkolivegreen") +
  labs(title = paste("PM 10 vs PM 2.5 Concentrations"),
       x = "PM10 Concentration",
       y = "PM2.5 Concentration") +
  theme_minimal()

#Parte 2
# Limpiar el entorno rm(list = ls())
# Librerias del proyecto library(tidyverse) library(readxl) library(ggplot2) library(tidyr) library(writexl) library(dplyr) library(lmtest) library(MASS) library(readxl) library(survival) library(fitdistrplus) library(rriskDistributions)
# ------------------------------------------------------------------------------
# Cargar base de datos
Base <- read_excel("C:/Users/windows/Downloads/Contaminacion FINAL/Base.xlsx",
                   col_types = c("text", "date", "numeric",
                                 "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
# Ver base de datos View(Base)
# Limpiar base de datos
# Eliminar filas con celdas vacías y guardar en Base1
data <- Base[complete.cases(Base$PM10, Base$PM2.5, Base$VelViento, Base$Temp), ]
# Ver la nueva base de datos # View(Datos1)
# ------------------------------------------------------------------------------
# Estandares de la OMS
estandarOMS25 <- 15 
estandarOMS10 <-50
# Serie de tiempo
p_pm25 <- ggplot(data, aes(MyDay, PM2.5)) +
  geom_line() + labs(x = "Tiempo",
                     y = "PM.25",
                     title = "Evoluación del PM2.5 durante 2022-2023") + theme_minimal()
p_pm25
# Ahora le vamos a colocar al PM2.5 una linea para denotar el AQG
# Estandar de la OMS
p_pm25 + geom_hline(aes(yintercept = estandarOMS25), data, color = "red")
# Serie de tiempo
p_pm10 <- ggplot(data, aes(MyDay, PM10)) +
  geom_line() + labs(x = "Tiempo",
                     y = "PM10",
                     title = "Evoluación del PM10 durante 2022-2023") + theme_minimal()
p_pm10
# Ahora le vamos a colocar al PM10 una linea para denotar el AQG
# Estandar de la OMS
# Verificar el Estandar de la OMS
p_pm10 + geom_hline(aes(yintercept = estandarOMS10), data, color = "red")
# ------------------------------------------------------------------------------
## Variable de excesos y modelación binaria # Crear variables de los excedentes
# Para PM2.5
data <- data %>% mutate(ex_pm25 = case_when(PM2.5 >= estandarOMS25 ~ 1,
                                            .default = 0)) sum(is.na(data$ex_pm25))
# Para PM10
data <- data %>% mutate(ex_pm10 = case_when(PM10 >= estandarOMS10 ~ 1,
                                            .default = 0)) sum(is.na(data$ex_pm10))
# ------------------------------------------------------------------------------
# Estimacion de la probabilidad de exceder el AGQ - Modelo PROBIT
# Variable dependiente: ex_pm25
# as.factor(X): Definir variable como una categorica
probit_model <- glm(formula = ex_pm10 ~ Temp + VelViento + as.factor(dow) + as.factor(month) + as.factor(year),
                    data = data,
                    family = binomial(link = "probit"))

# Chequeo de los resultados stargazer(probit_model, type = "text")
stargazer(probit_model, type = "text")
# Valores predichos en nuestra base de datos
data <- data %>% mutate(ex_pm_hat25 = probit_model$fitted.values)
p_pm25_prob <- ggplot(data, aes(MyDay, ex_pm_hat25)) + geom_line() +
  labs(x = "Tiempo",
       y = "Probabilidad",
       title = "Evoluación de la probabilidad PM10") + theme_minimal()
p_pm25_prob
p_pm25_prob2 <- ggplot(data, aes(VelViento, ex_pm_hat25)) + geom_point() +
  labs(x = "Velocidad del Viento",
       y = "Probabilidad",
       title = "Evoluación de la probabilidad con Viento") + theme_minimal()
p_pm25_prob2
#----
p_pm25_prob <- ggplot(data, aes(MyDay, ex_pm_hat25)) + geom_line() +
  labs(x = "Tiempo",
       y = "Probabilidad",
       title = "Evoluación de la probabilidad PM2.5") + theme_minimal()
p_pm25_prob
max(data$VelViento) min(data$VelViento)
data <- data %>% mutate(VelViento_class = NA) # Crear la columna con NA inicialmente
for (i in seq(0.1, 6.3, 0.2)) {
  condition <- quo(VelViento >= i & VelViento < (i + 0.2))
  value <- i + 0.1
  data <- data %>% mutate(VelViento_class = case_when(!!condition ~ value,
                                                      TRUE ~ VelViento_class))
}
# Última condición
data <- data %>% mutate(VelViento_class = case_when(VelViento >= 6.3 & VelViento < 6.5 ~ 6.4,
                                                    TRUE ~ VelViento_class))
nombres <- names(data)
nombres[nombres == "VelViento_class"] <- "ws_class" 
names(data) <- nombres

mutate(ws_class = ifelse(VelViento == 0.7, 0.8, ws_class))
data <- data %>%
  mutate(ws_class = ifelse(VelViento == 1.3, 1.4, ws_class))
## Funcion Group_by() con summarise() 
summarise(mean_prob = mean(ex_pm_hat25)) %>% as.tibble()
p_pm25_col <- ggplot(data_ws_col, aes(ws_class, mean_prob)) + geom_point() +
  geom_line() +
  labs(x = "Velocidad del Viento",
       y = "Probabilidad",
       title = "Evoluación de la probabilidad con Viento") + theme_minimal()
p_pm25_col
data_ws_col2 <- data %>% group_by(ws_class, month) %>% summarise(mean_prob = mean(ex_pm_hat)) %>% as.tibble()
data_ws_col2 <- data_ws_col2 %>% mutate(month = as.character(month))
p_pm25_col2 <- ggplot(data_ws_col2, aes(ws_class, mean_prob, col = month)) + geom_point() +
  geom_line() +
  labs(x = "Velocidad del Viento",
       y = "Probabilidad",
       title = "Evoluación de la probabilidad con Viento") + theme_minimal()
p_pm25_col2