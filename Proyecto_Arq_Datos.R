library(dplyr)
library(ggplot2)
library(tidyverse)


#Extraccion Data Autos
library(readxl)
Data_Autos_2 <- read_excel("C:/Users/pmath/OneDrive/Universidad/Arquitectura y Programación de Datos/Proyecto/Updated Data/Data Autos 2.xls", 
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

Join <- merge(x = Autos, y = Ventas, by = "ID_Car")



#Algoritmo 1 Cantidad de Carros Vendidos por Tienda

Filtro1 <- as.data.frame( Join %>% 
                            group_by(ID_Store) %>%        # Agrupamos por folio
                            summarise(Cantidad = sum(Amount)             # Sumamos venta
                            ))
arrange(Filtro1, Cantidad)

barplot(Filtro1$ID_Store,
        Filtro1$Cantidad,
        main = "Venta de Carros por Tienda",
        xlab = "Tienda", ylab = "Cantidad Carros",
        names = Filtro1$ID_Store,
        col ="blue")



#Algoritmo 2 Ganancias por tienda

Filtro2 <- as.data.frame( Ventas %>% 
                            group_by(Store_Name) %>%                 # Agrupamos por folio
                            summarise(venta = sum(Sales)             # Sumamos venta
                            ))

ggplot(data = Filtro2) +
  geom_bar(mapping = aes(x = Store_Name  , y=venta), stat = "identity")


distinct(Ventas,Store_Name, ID_Store) # Tienda y ID



#Algoritmo 3 Cantidad de Ventas por Rango de Precio

hist(Join$Sales,
     main = "Cantidad de Carros Vendidos por Rango de Precio",
     xlab = "Rango de Ventas",
     ylab = "Cantidad Ventas",
     col= rainbow(7))




#Algoritmo 4 Ganancias de tienda por Fabricante

Filtro4 <- as.data.frame( Join %>% 
                            group_by(`Country of Origin`) %>%        # Agrupamos por folio
                            summarise(venta = sum(Sales)             # Sumamos venta
                            ))


etiquetas <- paste0(Filtro4$`Country of Origin`, " - $" ,Filtro4$venta, " = ", round(100 * Filtro4$venta/sum(Filtro4$venta), 2), "%")

pie(Filtro4$venta, 
    labels = etiquetas,
    col = rainbow(8),
    main = "Ventas por Fabricante")



#Algoritmo 5 Cantidad de Carros Vendidos por Fabricante

Filtro5.1 <- as.data.frame( Join %>% 
                              group_by(`Country of Origin`) %>%        # Agrupamos por folio
                              summarise(CantidadGanancia = sum(Sales)             # Sumamos venta
                              ))

Filtro5.2 <- as.data.frame( Join %>% 
                              group_by(`Country of Origin`) %>%        # Agrupamos por folio
                              summarise(CantidadVentas = sum(Amount)             # Sumamos venta
                              ))

Filtro5.3 <-  merge(x = Filtro5.1, y = Filtro5.2, by = "Country of Origin" )

arrange(Filtro5.3, CantidadVentas)

barplot(Filtro5.3$CantidadVentas,
        main = "Cantidad de Carros Vendidos por Fabricante",
        xlab = "Fabricante", ylab = "Unidades Vendidas",
        col = c("red"),
        names = Filtro5.3$`Country of Origin`)

Plot5 = function(Pais){
  barplot(Filtro5.3[Filtro5.3$`Country of Origin` == Pais,]$CantidadVentas,
          Filtro5.3[Filtro5.3$`Country of Origin` == Pais,]$CantidadGanancia,
          main = "Cantidad de Carros Vendidos por Fabricante",
          xlab = "Fabricante", ylab = "Unidades Vendidas",
          col = c("red"),
          names = Pais)
}

Plot5("Japan")

distinct(Join, `Country of Origin`) # Lista Paises




#Algoritmo 6 - Ingresos por venta de modelos para una marca especifica

Toyota <- as.data.frame (Autos[Autos$Brand == "Toyota" & Autos$Price >= "4000",])

ggplot(data = Toyota) +
  geom_bar(mapping = aes(x=Model, y=Price), stat = "identity")



#Algoritmo 7 - Distribucion de modelos para una marca especifica por año

Audi <- as.data.frame (Autos[Autos$Brand == "Audi",]) 

View(Audi)

ggplot(data=Audi) +
  geom_point(aes(x=Model, y=Year, color=Model)) +  
  stat_smooth(aes(x=Model, y=Year))



#Algoritmo 8 - Distribucion de modelos para una marca especifica por año

Ford <- as.data.frame (Autos[Autos$Brand == "Ford",]) #agregar modelo?

plot(Ford[Ford$Brand == "Ford",]$Year, 
     pch=16, col="red", xlab = "Quantity", ylab = "Year", main = 'Models per Year')   



#Algoritmo 9 - Modelos vendidos por marca perteneciente un pais especifico


Italy <- as.data.frame (Autos[Autos$Country == "Italy",])
View(Italy)

ggplot(data=Italy, aes(x=Brand, y=Model, fill=Model)) + 
  geom_bar(stat="identity", position="stack") 



#Algoritmo 10 - Distribución de modelos en pie para una marca específica

MBZ <- as.data.frame (Autos[Autos$Brand == "Mercedes-Benz",]) 


ggplot(MBZ,aes(x="",y="", fill=Model))+
  geom_bar(stat = "Identity",color="white")+
  coord_polar(theta="y")



