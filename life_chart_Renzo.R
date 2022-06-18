
####Life Chart Renzo Falciglia####

##Paquetes

#install.packages("waffle")
#install.packages("hrbrthemes")
#install.packages("extrafont")
#install.packages("waffle", repos = "https://cinc.rud.is")
#install.packages("plyr")

library(waffle)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(waffle)
library(hrbrthemes)
library(extrafont)
library(plyr)

## Creando los datos

life_data <- tibble(months = factor(rep(month.abb[1:12], 41), levels=month.abb[1:12])) %>%  #Se crean los meses
  tibble(age = rep(0:40, each = 12)) ## Rango de edad 0-40

life_data <- life_data %>% rowid_to_column("row_name") # Columna con el numero de fila
    

## Agregar las distintas "eras" para pintar en el waffle chart

life_data <- life_data %>%
  mutate(era = fct_inorder(case_when(row_name < 181 ~ "Infancia en C. Casares",
                                     row_name < 217 ~ "Secundaria C. Casares",
                                     row_name < 289 ~ "Economía, UNLP",
                                     row_name < 307 ~ "Sector privado, CABA",
                                     row_name < 331 ~ "Sector público, CABA",
                                     row_name < 355 ~ "Pol. Púbicas, San Andrés, Sector público",
                                     row_name < 391 ~ "Sector público, CABA ",
                                     row_name < 493 ~ "Tiempo restante hasta 40 años")))
                                     
## Waffle chart basico

life_in_months <- life_data %>%
  count("era") #Se cuentan los meses de cada "era" para representarlos en el gráfico

ggplot(life_in_months, aes(fill = era, values = freq))+
  geom_waffle(color = "#F7F7F7", n_rows = 12, size = 1, flip = TRUE)

## Waffle chart completo
  
ggplot(life_in_months, aes(fill = era, values = freq))+
  geom_waffle(color = "#FFFFFF", n_rows = 12, size = 1, flip = F) + #Cada columna tiene 12 cuadrados que representan los meses
  scale_fill_manual(name = "", values = c("#35DB75","#2C5C00","#E0E80C","#EB7F0A","#498BF5","#00235C","#498BF5", "#EAEAE4")) +  ## assign colors to the eras
  coord_equal() +
  theme_ipsum(grid = "#FFFFFF") +
  theme(legend.text = element_text(family = "Cooper Lt BT", size = 8),
        plot.background = element_rect(fill = "#F7F7F7", color = "#F7F7F7")) +
  theme(axis.text.y = element_blank()) +
  theme(plot.background = element_rect(fill = "#FFFFFF")) +
  ggtitle("Life Chart Renzo Falciglia",
          subtitle = "Cada cuadrado representa un mes de mi vida y cada columna un año")

# Save the chart
ggsave("life_chart_renzo.png", device = "png", width = 12, height = 4, dpi = 150)

