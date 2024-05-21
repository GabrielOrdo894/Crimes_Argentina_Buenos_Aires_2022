# Crimes_Argentina_Buenos_Aires_2022
This repository contains a dataset and analysis of crimes in Buenos Aires, Argentina for the year 2022. The dataset includes detailed information on various types of crimes, dates, and locations of incidents, among other relevant data points. This analysis aims to provide insights into crime patterns and trends within Buenos Aires.

# Introduction {#introduction}

Crime is a significant concern for cities worldwide, impacting public safety, quality of life, and socioeconomic development. In Buenos Aires, Argentina, addressing crime and enhancing public safety are critical priorities for local authorities and communities. To gain insights into crime dynamics and develop effective strategies for crime prevention and intervention, we embark on an extensive exploration and analysis of crime data reported in Buenos Aires for the year 2022.

This project combines traditional exploratory data analysis (EDA) techniques with predictive modeling to delve deep into the patterns, trends, and underlying factors associated with reported crime incidents. By leveraging data-driven approaches, we aim to uncover actionable insights that can inform decision-making processes and contribute to the development of evidence-based crime reduction strategies.

## Dataset Overview {#dataset-overview}

The dataset "delitos_2022.csv" provides a comprehensive record of crime incidents reported in Buenos Aires throughout the year 2022. It encompasses various attributes associated with each reported crime, including the type of crime, location, date, demographic information, and additional contextual details.

Our analysis will focus on exploring the temporal, spatial, and demographic dimensions of crime in Buenos Aires. Additionally, we will investigate the relationship between different crime types, examine crime hotspots across neighborhoods and districts, and analyze the demographic characteristics of both perpetrators and victims.

Furthermore, we will employ predictive modeling techniques to develop machine learning models capable of forecasting crime occurrences and identifying factors that contribute to the likelihood of specific types of crimes. By building predictive models, we aim to enhance our understanding of the complex dynamics underlying criminal activities and improve the accuracy of future crime predictions.

Throughout this analysis, we emphasize the importance of data-driven decision-making and the potential of data analytics to support policymakers, law enforcement agencies, and community stakeholders in their efforts to combat crime and promote public safety in Buenos Aires.

### Data Source {#exploracion-de-datos}

The dataset "delitos_2022.csv" is sourced from the Buenos Aires Open Data Portal and represents a valuable resource for studying crime trends and patterns in the city. The dataset contains anonymized information about reported crime incidents and is publicly available for research and analysis.

| Variable | Description                                             |
|----------|---------------------------------------------------------|
| id.mapa  | Identifier map                                          |
| anio     | Year of the crime occurrence                            |
| mes      | Month of the crime occurrence                           |
| dia      | Day of the crime occurrence                             |
| fecha    | Date of the crime occurrence                            |
| franja   | Hour of crime                                           |
| tipo     | Type of crime                                           |
| subtipo  | Subtype of crime                                        |
| uso_arma | Whether weapons were used in the crime (Yes/No)         |
| uso_moto | Whether motorcycles were involved in the crime (Yes/No) |
| barrio   | Neighborhood where the crime occurred                   |
| comuna   | District where the crime occurred                       |
| latitud  | Latitude coordinate of the crime location               |
| longitud | Longitude coordinate of the crime location              |
| cantidad | Quantity or count of crimes reported                    |

[Buenos Aires Open Data Portal](https://data.buenosaires.gob.ar/dataset/delitos/resource/190ab807-5033-427b-8e27-b5659038f465)
