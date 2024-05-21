# ## Prepare packages and workspace {#prepare-packages-and-workspace}

# For Data Cleaning, Analysis and Visualization
library(tidyverse)
library(psych)
library(gridExtra)
# For Heatmap Correlation
library(reshape2)
# For Regression model and fit
library(forestmodel)
library(jtools)
library(pROC)
library(caret)
library(ResourceSelection)
library(car)
library(modelr)
library(tseries)
library(lmtest)
library(sandwich)
library(broom)

# Working Directory
setwd("C:/Users/gabri/Proyectos R/Delitos_2022_argentina")

# ## Upload crime buenos aires 2022 Dataset {#upload-crime-buenos-aires-2022-dataset}

crime_argentina_2022 <- read.csv("delitos_2022.csv")

# # Data Cleaning and Preprocessing {#data-cleaning-and-preprocessing}

# ## Quick View of Data and Structure {#quick-view-of-data-and-structure}

# First 6 view of the dataset
head(crime_argentina_2022)

# View of Structure
str(crime_argentina_2022)

# Our dataset comprises a total of `140918 observations` and `15 columns`, featuring variables of only two types :
# `Int` (Integer) : Comprising 3 variables
# `Ch` (Character) : Comprising 12 variables

# ## Handling Missing Values {#handling-missing-values}

# Check Missing Values
sapply(crime_argentina_2022, function(x) sum(is.na(x) | x == "" | tolower(x) == "null" | tolower(x) == "na"))

# Several columns contain empty cells and "null" or "na" values. The columns `barrio`, `comuna`, `latitud`, and `longitud` are interrelated, so removing null values from the `comuna` column will suffice. Additionally, we'll remove null values from the `franja` column.

# cleaning "franja" column
crime_argentina_2022 <-  crime_argentina_2022 %>%
  filter(franja != "NULL" | franja != NA)

# cleaning "barrio" column
crime_argentina_2022 <- crime_argentina_2022 %>%
  filter(comuna != "NULL" | comuna != NA)

# Checking again missing values
sapply(crime_argentina_2022, function(x) sum(is.na(x) | x == "" | tolower(x) == "null" | tolower(x) == "na"))

# Great! Now our dataset is clean and free from any empty or null values that could potentially impact our subsequent analysis.

# ## Featuring Transformation {#featuring-transformation}

# Debido al idioma del pais de origen de nuestra base de datos, las columnas estan en español, esto puede complicar la lectura para algunos analistas por lo que intentaremos traducir la base para que sea mas legible para los demas.

# Traduction of columns
crime_argentina_2022 <- rename(crime_argentina_2022, "id"= "id.mapa", "year" = "anio", "month" = "mes", "day" = "dia", "date" = "fecha", "hour" = "franja", "type" = "tipo", "subtype" = "subtipo", "weapon_use" = "uso_arma", "motorcycle_use" = "uso_moto", "neighborhood" = "barrio", "district" = "comuna", "latitude" = "latitud","longitude" = "longitud", "quantity" = "cantidad" )

# Traduction of variables
## Transformation of "month" values
months <- c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december")

crime_argentina_2022 <- crime_argentina_2022 %>%
  mutate(month = factor(case_when(
    month == "OCTUBRE" ~ "october",
    month == "NOVIEMBRE" ~ "november",
    month == "MAYO" ~ "may",
    month == "AGOSTO" ~ "august",
    month == "ENERO" ~ "january",
    month == "SEPTIEMBRE" ~ "september",
    month == "DICIEMBRE" ~ "december",
    month == "MARZO" ~ "march",
    month == "FEBRERO" ~ "february",
    month == "ABRIL" ~ "april",
    month == "JUNIO" ~ "june",
    month == "JULIO" ~ "july",
    TRUE ~ as.character(month)  # Keep other values unchanged
  ), levels = months))

# Transformation of "day" values
days <- c("sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday")
crime_argentina_2022 <- crime_argentina_2022 %>%
  mutate(day = factor(case_when(
    day == "LUNES" ~ "monday",
    day == "MARTES" ~ "tuesday",
    day == "MIERCOLES" ~ "wednesday",
    day == "JUEVES" ~ "thursday",
    day == "VIERNES" ~ "friday",
    day == "SABADO" ~ "saturday",
    day == "DOMINGO" ~ "sunday",
    TRUE ~ as.character(day)
  ), levels = days))

# Transformation of "type" values
crime_argentina_2022 <- crime_argentina_2022 %>%
  mutate(type = factor(case_when(
    type == "Robo" ~ "Robbery",
    type == "Hurto" ~ "Theft",
    type == "Vialidad" ~ "Traffic Violation",
    type == "Homicidios" ~ "Homicide",
    type == "Amenazas" ~ "Threats",
    type == "Lesiones" ~ "Injuries",
    TRUE ~ as.character(type)
  )))

# Transformation of "subtype" values
crime_argentina_2022 <- crime_argentina_2022 %>%
  mutate(subtype = factor(case_when(
    subtype == "Robo total" ~ "Total robbery",
    subtype == "Hurto total" ~ "Total theft",
    subtype == "Robo automotor" ~ "Motor vehicle theft",
    subtype == "Hurto automotor" ~ "Motor vehicle theft",
    subtype == "Lesiones por siniestros viales" ~ "Injuries from traffic accidents",
    subtype == "Homicidios dolosos" ~ "Intentional homicides",
    subtype == "Femicidios" ~ "Femicides",
    subtype == "Amenazas" ~ "Threats",
    subtype == "Lesiones Dolosas" ~ "Intentional injuries",
    subtype == "Muertes por siniestros viales" ~ "Deaths from traffic accidents",
    TRUE ~ as.character(subtype)
  )))

# Transformation of "weapon_use" values
crime_argentina_2022 <- crime_argentina_2022 %>%
  mutate(weapon_use = factor(case_when(
    weapon_use == "SI" ~ "Yes",
    weapon_use == "NO" ~ "No",
    TRUE ~ as.character(weapon_use)
  )))

# Transformation of "motorcycle_use" values
crime_argentina_2022 <- crime_argentina_2022 %>%
  mutate(motorcycle_use = factor(case_when(
    motorcycle_use == "SI" ~ "Yes",
    motorcycle_use == "NO" ~ "No",
    TRUE ~ as.character(motorcycle_use)
  )))

# Great! Now all columns and values are translated. Now we gonna create some columns for see better the hour of crimes in Argentina

# Transformation to factor variable
crime_argentina_2022$year <- factor(crime_argentina_2022$year)

ordered_hours <- 1:24
crime_argentina_2022$hour <- as.numeric(crime_argentina_2022$hour) + 1
crime_argentina_2022$hour <- factor(crime_argentina_2022$hour, levels = ordered_hours)
crime_argentina_2022$neighborhood <- factor(crime_argentina_2022$neighborhood)
crime_argentina_2022$district <- factor(crime_argentina_2022$district)

# Create a new column called "hour interval"
ordered_hours_interval <- c("Morning", "Afternoon", "Evening", "Night")
crime_argentina_2022 <- crime_argentina_2022 %>%
  mutate(hour_interval = factor(case_when(
    hour %in% 1:6 ~ "Night",
    hour %in% 7:12 ~ "Morning",
    hour %in% 13:18 ~ "Afternoon",
    hour %in% 19:24 ~ "Evening"
  ), levels = ordered_hours_interval))

# Create a new column called "Season"
ordered_seasons <- c("Spring", "Summer", "Autumn", "Winter")
crime_argentina_2022 <- crime_argentina_2022 %>%
  mutate(season = factor(case_when(
    month %in% c("december", "january", "february") ~ "Winter",
    month %in% c("march", "april", "may") ~ "Spring",
    month %in% c("june", "july", "august") ~ "Summer",
    month %in% c("september", "october", "november") ~ "Autumn"
  ), levels = ordered_seasons))

# Relocate columns for better analysis
crime_argentina_2022 <- crime_argentina_2022 %>%
  relocate(hour_interval, .after = hour) %>%
  relocate(season, .before = month)

# Transform date column
crime_argentina_2022$date <- as.Date(crime_argentina_2022$date, format = "%Y-%m-%d")

# See results
head(crime_argentina_2022)


# #Exploratory Data Analysis (EDA)

# Calculate incident counts by date
incident_date <- crime_argentina_2022 %>%
  group_by(date) %>%
  summarize(count = sum(quantity))

# Calculate incident counts by month and season, and average count per day
incident_month <- crime_argentina_2022 %>%
  group_by(season, month) %>%
  summarize(count = sum(quantity)) %>%
  mutate(avg_count_crime = round(count / 30, 2))

# Plot incident counts by date
ggplot(incident_date, aes(date, count)) +
  geom_line(stat = "identity") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Crime Counts Over Time in Buenos Aires", x = "Date", y = "Number of Incidents")

# Plot incident counts by month and season
ggplot(incident_month, aes(month, avg_count_crime, fill = season)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Spring" = "#8FBC8F", "Summer" = "#FFD700", "Autumn" = "#FFA07A", "Winter" = "#ADD8E6")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(title = "Monthly Average Daily Crime Rates by Seasons", x = "", y = "Average Number of Incidents per day")

# Summarize total incidents by day and hour interval
incident_day <- crime_argentina_2022 %>%
  group_by(day, hour_interval) %>%
  summarize(count = sum(quantity))

# Create a stacked bar chart of total crimes by day and hour interval
ggplot(incident_day, aes(day, count, fill = hour_interval)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("Morning" = "#FFD700", "Afternoon" = "#90EE90", "Evening" = "#87CEFA", "Night" = "#9932CC")) +
  labs(title = "Stacked Bar Chart of Total Crimes by Day and Hour Interval", 
       x = "Day", y = "Number of Crimes", fill = "Hour Interval")

# Summarize total incidents by hour interval and weapon use
incident_weapon_use <- crime_argentina_2022 %>%
  group_by(hour_interval, weapon_use) %>%
  summarize(count = sum(quantity))

# Create a grouped bar chart of total crimes by hour interval and weapon use
ggplot(incident_weapon_use, aes(hour_interval, count, fill = hour_interval)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("Morning" = "#FFD700", "Afternoon" = "#90EE90", "Evening" = "#87CEFA", "Night" = "#9932CC")) +
  facet_grid(~weapon_use) +
  labs(title = "Total Crimes by Hour Interval and Weapon Use", x = "Hour Interval", y = "Number of Crimes")

# Summarize total incidents by hour interval and motorcycle use
incident_motorcycle_use <- crime_argentina_2022 %>%
  group_by(hour_interval, motorcycle_use) %>%
  summarize(count = sum(quantity))

# Create a grouped bar chart of total crimes by hour interval and motorcycle use
ggplot(incident_motorcycle_use, aes(hour_interval, count, fill = hour_interval)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("Morning" = "#FFD700", "Afternoon" = "#90EE90", "Evening" = "#87CEFA", "Night" = "#9932CC")) +
  facet_grid(~motorcycle_use) +
  labs(title = "Total Crimes by Hour Interval and Motorcycle Use", x = "Hour Interval", y = "Number of Crimes")

# Group the dataset by crime subtype
ordered_subtype_crime <- crime_argentina_2022 %>%
  group_by(subtype) %>%
  summarize(Number_of_crime = sum(quantity)) %>%
  arrange(desc(Number_of_crime))

# Create a bar plot using ggplot
ggplot(ordered_subtype_crime, aes(reorder(subtype, Number_of_crime), Number_of_crime)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Number_of_crime), hjust = -0.25, color = "black") +
  ylim(0, 60000) +
  coord_flip() +
  labs(title = "Total Number of Crimes by Crime Types", x = "", y = "Numbers of Crimes")

# Assigning colors to neighborhoods
colores_barrios <- c(
  PALERMO = "#C51605", BALVANERA = "#C51605", FLORES = "#C51605", RECOLETA = "#C51605", CABALLITO = "#C51605",
  `VILLA LUGANO` = "#C51605", ALMAGRO = "#C51605", `SAN NICOLAS` = "#C51605", BARRACAS = "#C51605",
  CONSTITUCION = "#C51605", BELGRANO = "#C51605", `VILLA CRESPO` = "#C51605", RETIRO = "#C51605",
  MATADEROS = "#C51605", MONSERRAT = "#C51605", `NUEVA POMPEYA` = "#C51605", `PARQUE CHACABUCO` = "#C51605"
)

# Bar chart of Number of crimes by Neighborhood in Buenos Aires
crime_argentina_2022 %>%
  group_by(neighborhood) %>%
  summarize(Number_crime = sum(quantity)) %>%
  arrange(desc(Number_crime)) %>%
  ggplot(aes(reorder(neighborhood, Number_crime), Number_crime, fill = neighborhood)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  geom_hline(yintercept = 2885.917, size = 1.05, color = "#272829") +
  annotate("text", y = 5887, x = 10, label = "Avg. crimes/neighborhood: 2885.917") +
  scale_fill_manual(values = colores_barrios) +
  theme(axis.text.y = element_text(size = 6)) +
  labs(title = "Crime Counts by Neighborhood in Buenos Aires", x = "Neighborhood", y = "Number of Crimes")

# Scatterplot of crime count by hour, neighborhood, and type of crime
crime_argentina_2022 %>%
  group_by(hour, neighborhood, type) %>%
  summarize(crime_count = sum(quantity)) %>%
  ggplot(aes(as.numeric(hour), crime_count, color = type)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, size = 6), axis.text.y = element_text(size = 6), strip.text = element_text(size = 5)) +
  facet_wrap(~neighborhood) +
  labs(title = "Understanding Crime Patterns: Hourly Insights by Neighborhood and Type", y = "Frequency",
       x = "Hour")

# #Model Selection

# ## Linear Model


# Create the explanatory data
regression_data <- crime_argentina_2022 %>%
  group_by(hour, neighborhood) %>%
  summarize(crime_count = sum(quantity))

# Visualize the connection between response variable and other variables
ggplot(regression_data, aes(as.numeric(hour), crime_count)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, size = 6),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 6)) +
  facet_wrap(~neighborhood) +
  labs(title = "Linear Regression Analysis: Understanding Variable Interactions",
       y = "Frequency",
       x = "Hour")

# Create linear regression model
linear_model <- lm(crime_count ~ hour + neighborhood + 0, data = regression_data)

# Display model summary
summary(linear_model)


# ## Model Fit


summ(linear_model, model.info = TRUE, model.fit = TRUE, model.coefs = FALSE)

forest_model(linear_model)

# Add predictions and residuals to the data

explanatory_data <- expand_grid(
  hour = factor(1:24),
  neighborhood = unique(crime_argentina_2022$neighborhood)
)

prediction_data <- explanatory_data %>%
  mutate(
    crime_count = predict(linear_model, explanatory_data)
  )

# Index prediction_data
index <- 1:nrow(prediction_data)

# Add new column
prediction_data$index <- index

prediction_data <- relocate(prediction_data, index, .before = hour)

# Visualize the new prediction_data
ggplot(regression_data, aes(as.numeric(hour), crime_count)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(data = prediction_data, color = "lightblue", size = 0.95) +
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 6)) +
  facet_wrap(~neighborhood) +
  labs(title = "Assessing Model Accuracy: Predictions vs. Actual Values",
       y = "Frequency",
       x = "Hour")


# Plot diagnostic plots
par(mfrow = c(2, 2))
plot(linear_model)

augment(linear_model) %>%
  arrange(desc(.cooksd))


# Jarque-Bera test for normality
jarque.bera.test(linear_model$residuals)

# Summary of residuals
summary(linear_model$resid)


# Breusch-Pagan test for heteroscedasticity
bptest(linear_model)

# Heteroscedasticity-robust coefficient test
coeftest(linear_model, vcov = vcovHC(linear_model, type = "HC2"))
