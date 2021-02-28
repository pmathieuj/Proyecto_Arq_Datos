library(dplyr)
library(ggplot2)


#Extraccion Data Autos
library(readxl)
Data_Autos_2 <- read_excel("TAD/TAD 5/Proyectos/Data Autos 2.xls", 
                           col_types = c("numeric", "text", "text", 
                                         "numeric", "text", "text", "numeric", 
                                         "numeric", "numeric", "numeric"))
View(Data_Autos_2)

#Extraccion Ventas
library(readxl)
Sales_2 <- read_excel("C:/Users/pmath/OneDrive/Universidad/Arquitectura y Programación de Datos/Proyecto/Updated Data/Sales 2.xlsx", 
                      col_types = c("numeric", "numeric", "numeric", 
                                    "text", "numeric", "numeric", "text"))
View(Sales_2)


Autos <- as.data.frame(Data_Autos_2)
Ventas <- as.data.frame(Sales_2)

#Algoritmo 1

ggplot(data = Ventas) +
  geom_bar(mapping = aes(x = Store_Name  , y=Sales), stat = "identity")

#Algoritmo 2
#Algoritmo 3
#Algoritmo 4
#Algoritmo 5

#Algoritmo 6

