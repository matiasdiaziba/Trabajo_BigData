################################################################################
################################################################################
################################################################################
##########                         #############################################                      
##########Trabajo final de Big Data#############################################
##########                         #############################################
################################################################################
#Integrantes:
# Matías DÍAZ
#Paula Muñoz 
#Patricia Vallejos 
#INGENIERIA EN COMERCIO INTERNACIONAL 

#Nuestro trabajo consiste en extraer datos de tres páginas web dedicadas a la venta de libros 
#Estas páginas son https://www.buscalibre.com --- www.puntoycomalibros.com --- www.top10books.cl
#extraeremos datos de la sección de libros de ficción y los analizaremos 
#Decidimos analizar las primeras páginas de las secciones de ficción para poder compararlas entre ellas 
#esto último debido a que todas poseen una cantidad de libros distintos y hace imposible realizar una comparación justa 

####Desarrollo de la extracción de datos

setwd("C:/Users/matie/Desktop/Big Data/Trabajo_BigData")
rm(list = ls())
#partiremos activando los paquetes que usaremos durante la extracción de datos 
library(rvest)
library(xml2)
library(dplyr)
library(ggplot2)



#aclaración durante el tiempo que estuvimos analizando estas páginas la cantidad de libros mostrados por página
#fueron  variando , por eso decidimos usar lenght() para contar la cantidad de libros antes de crear
#un data frame, si toca ocurrir que las paginas modifican la cantidad de libros nosotros 
#pusimos un #IMPORTANTE !!!  junto al número que se tendrá que modificar dependiendo lo contado por
#length() con anterioridad. 



#######################################################################################
#######################################################################################
#######################################################################################
########################
#### BUSCA LIBRE #######
########################
#######################################################################################
#######################################################################################
#######################################################################################

#### Partiremos extrayendo datos de La página web de busca libre específicamente 
#la sección de best sellers de ficción
#sacaremos los datos suficientes para ver el precio de estos libros, y el descuento de estos mismos. 
#Terminado por analizar estos datos. 


# Partimos identificando el link de la sección 
link_busca <- "https://www.buscalibre.cl/libros-envio-express-chile-best-sellers-ficcion_t.html"
Pag_busca <- read_html(link_busca)

#identificamos el lugar donde se encuentran los nombres de los libros y creamos la función busca_nombre 
#buscanombre_nombre: Entrada : string  salida: stinrg 

nombre_busca <- "div.nombre.margin-top-10.text-align-left"
busca_html <- html_nodes(Pag_busca,nombre_busca)#para identificar todos los sectores donde se Encuentra el nombre de los libros en la pagina 
busca_nombre <- html_text(busca_html)#para traducir lo anterior hecho  e identificar los nombres 
busca_nombre


#procedemos a identificar el sector donde estan los precios de los libros y creamos la función PreciosB
#PreciosB : Entrada: string salida: Enteros 
lugar_preciosB <- "h3.precio-ahora.hide-on-hover.margin-0.font-size-medium"
preB_html <- html_nodes(Pag_busca,lugar_preciosB)#para identificar todos los sectores donde se encuentran el precio de los libros en la pagina 
preB_text <-html_text(preB_html)#para traducir lo anterior hecho  e identificar los precios 
preB_text
limpiador<- gsub("\\$","",preB_text) # Para limpiar datos que no necesitamos 
limpiador<- gsub("\\.","",limpiador)# Para limpiar datos que no necesitamos 
limpiador
PreciosB <- as.numeric(limpiador) #para forzar que los datos pasen de character a numérico 
data.class(PreciosB)#para asegurar que los datos son numéricos 
PreciosB

#procedemos a identificar el sector donde están descuentos de los libros y creamos la función descuento
#decuento: Entrada: string  Salida: Double  
Lugar_descuento <- "div.descuento-v2.color-white.position-relative"
des_html <- html_nodes(Pag_busca,Lugar_descuento)#para identificar todos los sectores donde se encuentran el descuento de los libros en la pagina 
des_text <- html_text(des_html)#para traducir lo anterior hecho  e identificar los descuentos 
des_lim <- gsub("\\%dcto","",des_text)# Para limpiar datos que no necesitamos 
des_lim
descuento <- as.numeric(des_lim) # Para limpiar datos que no necesitamos
descuento


length(busca_nombre) #lo siguiente es para contar la cantidad de libros que se encuentran en la pagina 
#IMPORTANTE !!!! esto se MODIFICA dependiendo de lo anterio contado con el lenght
Lugar <- seq(1,75)#IMPORTANTE !!!!! para asignar un numero a cada libro (se cambia el ultimo número dependiendo del total de libros)
Lugar
#unir datos y creamos un data frame donde las columnas tienen los siguientes nombres 
#Lugar: aquí se encuentra la posición de los libros dentro de la pagina 
#Libro: aquí se encuentran el nombre de los libros dentro de la pagina 
#Precio: aquí se encuentra el valor de todos los libros con el descuento ya aplicado 
#Porcentaje_DCTO: aquí encontramos el porcentaje de descuento de los libros 
library(tidyverse) #activamos tidyverse para poder ver las tablas con view y analizar datos posteriormente 

buscarlibre_fic <- data_frame(Lugar = Lugar,  Libro = busca_nombre,Precio = PreciosB ,Porcentaje_DCTO = descuento)
view(buscarlibre_fic)
library(ggplot2) # activamos ggplot para hacer graficos 

#Creamos un gráfico donde analizamos los precios de los libros el nombre del grafico  será Busca Libre Precios
GRAFIC_B <- ggplot(data = buscarlibre_fic,
                   mapping = aes(x = Lugar , y = PreciosB)) +
  geom_point(size=3,pch=15,colour = "cyan") +
  geom_line(size = 0,colour = "blue")+
  labs(title = "Busca Libre precios ")

GRAFIC_B

#Creamos un gráfico donde analizamos los precios de los libros el nombre del grafico será Busca Libre Descuento
GRAFIC_c <- ggplot(data = buscarlibre_fic,
                   mapping = aes(x = Lugar , y = descuento)) +
  geom_col(width = .9,colour = "cyan",fill= "red") +
  geom_line(size = 0,colour = "blue")+
  labs(title = "Busca Libre Descuento")
GRAFIC_c


#Anlizaremos los DCTOS DE BUSCALIBRE 
#Decidimos ordenar los libros haciendo dos tablas la primera con los descuentos mayores o iguales a 50 % y la segunda con los descuentos menores o iguales a 50%
#Lugar: aquí se encuentra la posición de los libros dentro de la pagina 
#Libro: aquí se encuentran el nombre de los libros dentro de la pagina 
#Precio: aquí se encuentra el valor de todos los libros con el descuento ya aplicado 
#Porcentaje_DCTO: aquí encontramos el porcentaje de descuento de los libros 

#### >= 50% 
dctomas50 <-  buscarlibre_fic %>% 
  filter(Porcentaje_DCTO >= 50) #vemos los libros que tienen un descuento mayor o igual al 50%
view(dctomas50)
vermaxdcto <- buscarlibre_fic %>% 
  summarise(Max_dcto =max(Porcentaje_DCTO))#vemos el descuento más alto de la página (en el momento de realizar este código el descuento es del 80%)
view(vermaxdcto)
maxdctolibro <- buscarlibre_fic %>% 
  filter(Porcentaje_DCTO == 80)#vemos los libros que tienen el porcentaje de descuento mas alto 
view(maxdctolibro)

###<= 50%

dctomen50 <-  buscarlibre_fic %>% 
  filter(Porcentaje_DCTO <= 50) #vemos los libros que tienen un descuento menor o igual al 50%
view(dctomen50)
vermindcto <- buscarlibre_fic %>% 
  summarise(min_dcto =min(Porcentaje_DCTO))#Vemos el descuento más bajo de la página(en el momento de realizar este código es del 15 %) 
view(vermindcto)
mindctolibro <- buscarlibre_fic %>% 
  filter(Porcentaje_DCTO == 15)#vemos los libros que tienen el porcentaje de descuento mas bajo 
view(mindctolibro)
###
#Quisimos sacar el promedio de precios de esta pagina  para realizar una posterior comparación 
PromedioB <- buscarlibre_fic %>%
  summarise(Promediobus = mean(Precio))
view( PromedioB)
#######################################################################################
#######################################################################################
#######################################################################################
########################
####PUNTO Y COMA #######
########################
#######################################################################################
#######################################################################################
#######################################################################################
#### Partiremos extrayendo datos de La página web de Punto y como específicamente 
#la sección de libros ficción pag 1 
#sacaremos los datos suficientes para ver el precio de estos libros.

# Partimos identificando el link de la sección 
link_punto <- "https://www.puntoycomalibros.com/libros-de/ficcion-01/"
Pag_punto <- read_html(link_punto)
#identificamos el lugar donde se encuentran los nombres de los libros y creamos la función punto_nombre 
#punto_nombre: Entrada: string salida: stinrg 
nombre_punto <- "dd.title a"  #para identificar donde está el nombre dentro de la pagina 
punto_html <- html_nodes(Pag_punto,nombre_punto) #para identificar todos los sectores donde se encuentran el nombre de los libros en la pagina 
punto_nombre <- html_text(punto_html)#para traducir lo anterior hecho  e identificar los nombres 
punto_nombre

#procedemos a identificar el sector donde están los precios de los libros y creamos la función PreciosP
#PreciosP : Entrada: string salida: Enteros 
lugar_preciosP <-"p.precio strong" #para identificar donde está el precio dentro de la pagina 
preP_html <- html_nodes(Pag_punto,lugar_preciosP)#para identificar todos los sectores donde se encutran los precios de los libros en la pagina
preP_text <-html_text(preP_html)#para traducir lo anterior hecho  e identificar los nombres 
preP_text
limpiadorP <- gsub("\\$","",preP_text)  # Para limpiar datos que no necesitamos 
limpiadorP <- gsub("\\.","",limpiadorP) # Para limpiar datos que no necesitamos 
limpiadorP
PreciosP <- as.numeric(limpiadorP)
data.class(PreciosP)
PreciosP

length(punto_nombre)#lo siguiente es para contar la cantidad de libros que se encuentran en la pagina 
#IMPORTANTE !!!! esto se MODIFICA  dependiendo de lo anterio contado con el lenght
Orden <- seq(1,30)#IMPORTANTE!!! para asignar un numero a cada libro (se cambia el ultimo número dependiendo del total de libros)

#PAGINA 2 
link_punto2 <- "https://www.puntoycomalibros.com/libros-de/ficcion-01/?pagSel=2&cuantos=30&orden=prioridad%2C+fecha_edicion+desc&codMateria=01&tipoArticulo=L"
Pag_punto2 <- read_html(link_punto2)
#identificamos el lugar donde se encuentran los nombres de los libros y creamos la función punto_nombre 
#punto_nombre: Entrada : string  salida: stinrg 
nombre_punto2 <- "dd.title a"  #para identificar donde está el nombre dentro de la pagina 
punto_html2 <- html_nodes(Pag_punto2,nombre_punto2) #para identificar todos los sectores donde se encutra el nombre de los libros en la pagina 
punto_nombre2 <- html_text(punto_html2)#para traducir lo anterior hecho  e identificar los nombres 
punto_nombre2

#procedemos a identificar el sector donde están los precios de los libros y creamos la función PreciosP
#PreciosP : Entrada: string salida: Enteros 
lugar_preciosP2 <-"p.precio strong" #para identificar donde está el precio dentro de la pagina 
preP_html2 <- html_nodes(Pag_punto2,lugar_preciosP2)#para identificar todos los sectores donde se encuentran los precios de los libros en la pagina
preP_text2 <-html_text(preP_html2)#para traducir lo anterior hecho  e identificar los nombres 
preP_text2
limpiadorP2 <- gsub("\\$","",preP_text2)  # Para limpiar datos que no necesitamos 
limpiadorP2 <- gsub("\\.","",limpiadorP2) # Para limpiar datos que no necesitamos 
limpiadorP2
PreciosP2 <- as.numeric(limpiadorP2)
data.class(PreciosP2)
PreciosP2

length(punto_nombre2)#lo siguiente es para contar la cantidad de libros que se encuentran en la pagina
#IMPORTANTE !!!! esto se MODIFICA dependiendo de lo anterio contado con el lenght
Orden2 <- seq(31,60)#IMPORTANTE!!!! para asiganar un numero a cada libro (se cambia el ultimo número dependiendo del total de libros)

#IMPORTANTE !!!! esto se repite dependiendo de lo anterio contado con el lenght(busca_nombre) se modifica el numero

punto_pagina <- rep(c("Punto y coma"),time=c(30)) #IMPORTANTE !!!!

#Creamos los data frames por separado de las dos pagina 
#Lugar: aquí se encuentra la posición de los libros dentro de la pagina 
#Libro: aquí se encuentran el nombre de los libros dentro de la pagina 
#Precio: aquí se encuentra el valor de todos los libros con el descuento ya aplicado 
#Pagina : pagina web que vende el libro 
Punto_y_coma1 <- data_frame(Lugar = Orden ,  Libro = punto_nombre,Precio = PreciosP,Pagina = punto_pagina) 
view(Punto_y_coma1)
Punto_y_coma2 <- data_frame(Lugar = Orden2 ,  Libro = punto_nombre2,Precio = PreciosP2,Pagina = punto_pagina)
view(Punto_y_coma2)

#Por último unimos los datos de las dos páginas en un solo data frame que usaremos más tarde para comparar  
#el nombre de este data frame es todo_punto y coma y tiene los unidos de punto_y_coma1 y punto_y_coma2
#Lugar: aquí se encuentra la posición de los libros dentro de la pagina 
#Libro: aquí se encuentran el nombre de los libros dentro de la pagina 
#Precio: aquí se encuentra el valor de todos los libros con el descuento ya aplicado 
#Pagina : pagina web que vende el libro 
compara_punto_y_coma <- rbind(Punto_y_coma1,Punto_y_coma2)
view(compara_punto_y_coma)


#unir datos y creamos un data frame donde las columnas tienen los siguientes nombres 
#Lugar: aquí se encuentra la posición de los libros dentro de la pagina 
#Libro: aquí se encuentran el nombre de los libros dentro de la pagina 
#Precio: aquí se encuentra el valor de todos los libros con el descuento ya aplicado 


#Creamos los gráficos de precios.
#donde el eje X representa el puesto del libro dentro de la pagina 
#y el eje Y representa los precios. 
## el primer grafico tendrá el nombre de "Punto y Coma Precios pag 1 " y en él se encuentran los libros 
#de la primera página de ficción 
GRAFIC_Punto1   <- ggplot(data = Punto_y_coma1,
                          mapping = aes_(x = Orden, y = PreciosP)) +
  geom_col(width = .8,colour = "cyan",fill= "pink") +
  geom_line(size = 1,colour = "green")+
  
  labs(title = "Punto y Coma Precios pag 1 ")


GRAFIC_Punto1

## el segundo grafico tendrá el nombre de "Punto y Coma Precios pag 2 " y en el se encuentran los libros 
#de la segunda página de ficción.
GRAFIC_Punto2   <- ggplot(data = Punto_y_coma2,
                          mapping = aes_(x = Orden2, y = PreciosP2)) +
  geom_col(width = .8,colour = "pink",fill= "cyan") +
  geom_line(size = 1,colour = "green")+
  
  labs(title = "Punto y Coma Precios pag 2 ")
GRAFIC_Punto2



#Quisimos ver todos los libros menores a 15000 pesos en punto y coma
#Lugar: aquí se encuentra la posición de los libros dentro de la pagina 
#Libro: aquí se encuentran el nombre de los libros dentro de la pagina 
#Precio: aquí se encuentra el valor de todos los libros con el descuento ya aplicado 

menor_a <- compara_punto_y_coma%>% 
  filter(Precio <= 15000)
view(menor_a)
#Quisimos sacar el promedio de precios de esta pagina  para realizar una posterior comparación 
Promedio <- compara_punto_y_coma %>% 
 summarise(PromedioP = mean(PreciosP))
view(Promedio) 
#######################################################################################
#######################################################################################
#######################################################################################
########################
#### TOP 10 BOOKS #####
########################
#######################################################################################
#######################################################################################
#######################################################################################
#### Partiremos extrayendo datos de La página web de TOP 10 BOOKS específicamente 
#la sección de top 10 libros de ficción 
#sacaremos los datos suficientes para ver el precio de estos libros.

# Partimos identificando el link de la sección 
link_top10 <- "https://www.top10books.cl/ficcion-101605.html"
Pag_top10 <- read_html(link_top10)
#identificamos el lugar donde se encuentran los nombres de los libros y creamos la función NombresTOP
#NombresTOP: Entrada : string  salida: stinrg 
lugar_no10 <- "a.product-item-link"
no10_html <- html_nodes(Pag_top10,lugar_no10) 
no10_texto <- html_text(no10_html)
no10_texto
limpioador_1t <- gsub("\n","",no10_texto) # Para limpiar datos que no necesitamos 
NombresTOP <- gsub("   ","",limpioador_1t) # Para limpiar datos que no necesitamos 
NombresTOP

#procedemos a identificar el sector donde están los precios de los libros y creamos la función PrecioSTOP
#PreciosTOP : Entrada: string salida: Enteros 
lugar_preciosTOP <- "span.special-price"#para identificar todos los sectores donde se encuentran los precios de los libros en la pagina
pre_htmlTOP <- html_nodes(Pag_top10,lugar_preciosTOP)#para traducir todo lo anterior  
pre_textTOP <-html_text(pre_htmlTOP)
pre_textTOP
Precio_chTOP <- gsub("\\$","",pre_textTOP) # Para limpiar datos que no necesitamos 
Precio_chTOP
Precio_chTOP <- gsub("Precio especial","",Precio_chTOP) # Para limpiar datos que no necesitamos 
Precio_chTOP <- gsub("\\.","",Precio_chTOP) # Para limpiar datos que no necesitamos 
Precio_chTOP <- gsub ("\n","",Precio_chTOP) # Para limpiar datos que no necesitamos 
PrecioTOP <- gsub("\\  ","",Precio_chTOP) # Para limpiar datos que no necesitamos 
PrecioTOP

#COMO SABEMOS QUE EN ESTA PAGINA SE HACE UN TOP 10 DE LOS LIBROS MAS VENDIDOS CREAMOS ESTE VECTOR PARA ASIGNAR LOS NUMEROS CORRESPONDIENTES A LOS LIBROS 
length(NombresTOP)#lo siguiente es para contar la cantidad de libros que se encuentran en la pagina
#IMPORTANTE !!!! esto se MODIFICA dependiendo de lo anterio contado con el lenght
TOP10 <- seq(1,15)#IMPORTANTE !!!!!!!
TOP10

#Donde compararemos los libros de ficción y no ficción de esta página crearemos el vector ficción donde se repite ficción 10 veces para poder asignarles este término a 
#los libros de ficción en el data frame
#IMPORTANTE !!!! esto se repite dependiendo de lo anterio contado con el lenght
Ficción <- rep(c("Ficcion"),time=c(15))#IMPORTANTE!!! 

#unimos los datos y creamos un data frame donde las columnas tienen los siguientes nombres 
#TOP_10: aquí se encuentra la posición de los libros dentro de la pagina 
#Libro: aquí se encuentran el nombre de los libros dentro de la pagina 
#Precio: aquí se encuentra el valor de todos los libros 
#Categoria: aquí se identifica de que categoría es el libro 
TOP_10_LIBROS_FIC <- data_frame(TOP_10 = TOP10,Libro = NombresTOP,Precios = PrecioTOP,categoria = Ficción)
view(TOP_10_LIBROS_FIC)

# SE crea grafico   TOP 10 BOOKS DE FICCION donde el eje x representan el nombre de los libros 
#el eje   Y representa el precio de los libros. 

GRAFIC_TOP10 <- ggplot(data = TOP_10_LIBROS_FIC,
                       mapping = aes(x = NombresTOP, y = PrecioTOP)) +
  geom_col(width = .5,colour = "PINK",fill= "CYAN") +
  
  labs(title = "TOP 10 BOOKS de Ficcion  ")

GRAFIC_TOP10



#### Continuamos extrayendo datos de La página web de TOP 10 BOOKS  específicamente 
#la sección de top 10 libros de no ficción 
#sacaremos los datos suficientes para ver el precio de estos libros.

# Partimos identificando el link de la sección

link_top10f <- "https://www.top10books.cl/no-ficcion-102980.html"
Pag_top10f <- read_html(link_top10f)
#identificamos el lugar donde se encuentran los nombres de los libros y creamos la función Nombresf
#Nombresf: Entrada : string  salida: stinrg 
lugar_nof10 <- "a.product-item-link" #en este lugar se encuentran los nombres de los libros 
nof10_html <- html_nodes(Pag_top10f,lugar_nof10) #para identificar todos los sectores donde se encuentran los nombres de los libros en la pagina
nof10_texto <- html_text(nof10_html)#para traducir todo lo anterior 
nof10_texto
limpioadorf_1 <- gsub("\n","",nof10_texto)# Para limpiar datos que no necesitamos 
Nombresf <- gsub("   ","",limpioadorf_1)
Nombresf# Para limpiar datos que no necesitamos 

#identificamos el lugar donde se encuentran los precios de los libros y creamos la función Preciof
#Preciof: Entrada : string  salida: double
lugarf_precios <- "span.special-price"  #en este lugar se encuentran los precios de los libros 
pref_html <- html_nodes(Pag_top10f,lugarf_precios)#para identificar todos los sectores donde se encutran los precios de los libros en la pagina
pref_text <-html_text(pref_html)#para traducir todo lo anterior 
pref_text
data.class(pref_text)
Preciof_ch <- gsub("\\$","",pref_text)# Para limpiar datos que no necesitamos 
Preciof_ch
Preciof_ch <- gsub("Precio especial","",Preciof_ch)# Para limpiar datos que no necesitamos 
Preciof_ch <- gsub("\\.","",Preciof_ch)# Para limpiar datos que no necesitamos 
Preciof_ch <- gsub ("\n","",Preciof_ch)# Para limpiar datos que no necesitamos 
Preciof <- gsub("\\  ","",Preciof_ch)# Para limpiar datos que no necesitamos 
#Se utiliza length para contar la cantidad de libros que tiene esta pagiana 
length(Preciof)
#IMPORTANTE !!!! esto se MODIFICA dependiendo de lo anterio contado con el lenght
TOP10f <- seq(1,10)#IMPORTANTE!!!! 
TOP10f
#Donde compararemos los libros de ficción y no ficción de esta página crearemos el vector NO_Ficcion donde se repite ficción 10 veces para poder asignarles este término a 
#los libros de ficción en el data frame 
NO_Ficción <- rep(c("No Ficcion"),time=c(10)) 

#Creamos el data frame de los libros de no ficción 
#unimos datos y creamos un data frame donde las columnas tienen los siguientes nombres 
#TOP_10: aquí se encuentra la posición de los libros dentro de la pagina 
#Libro: aquí se encuentran el nombre de los libros dentro de la pagina 
#Precio: aquí se encuentra el valor de todos los libros 
#Categoria: aquí se identifica de que categoría es el libro 
TOP_10_LIBROS_no_FIC <- data_frame(TOP_10 = TOP10f,Libro = Nombresf,Precios = Preciof,categoria = NO_Ficción)
view(TOP_10_LIBROS_no_FIC)
# SE crea grafico   TOP 10 BOOKS NO FICCION  donde el eje x representa el nombre de los libros 
#el eje   Y representa el precio de los libros. 
GRAFIC_TOP10_NO <- ggplot(data = TOP_10_LIBROS_no_FIC,
                          mapping = aes(x = Nombresf, y = Preciof)) +
  geom_col(width = .5,colour = "PINK",fill= "CYAN") +
  
  labs(title = "TOP 10 BOOKS NO FICCIÓN  ")

GRAFIC_TOP10_NO


#Por último unimos los data frame y creamos el data frame llamado todo_topboks 
#este data frame tiene unido los dos anteriores hechos junto a sus respectivos detalles 
#TOP_10: aquí se encuentra la posición de los libros dentro de la pagina 
#Libro: aquí se encuentran el nombre de los libros dentro de la pagina 
#Precio: aquí se encuentra el valor de todos los libros 
#Categoria: aquí se identifica de que categoría es el libro
names(TOP_10_LIBROS_no_FIC)
names(TOP_10_LIBROS_FIC)
todo_topbooks <- rbind(TOP_10_LIBROS_FIC,TOP_10_LIBROS_no_FIC)
view (todo_topbooks)




#por último queremos ver el top 5 de los libros de ficción y no ficción 
#para eso creamos un filtro que nos muestre los 5 primeros libros más vendidos de ficción y no ficcion 
#TOP_10: aquí se encuentra la posición de los libros dentro de la pagina 
#Libro: aquí se encuentran el nombre de los libros dentro de la pagina 
#Precio: aquí se encuentra el valor de todos los libros 
#Categoria: aquí se identifica de que categoría es el libro
TOP5 <-todo_topbooks %>% 
  filter(TOP_10 <= 5)
view(TOP5)
#######################################################################################
#######################################################################################
#######################################################################################
#################################################
#### comparación BUSCA LIBRE  PUNTO Y COMA  #####
#################################################
#######################################################################################
#######################################################################################
#######################################################################################

##Para terminar queremos comparar los precios de "Busca Libre y "Punto y Coma" 

#Para eso creamos dos data frame que tienen que tener el mismo contenido 
length(busca_nombre) #para buscar cuantos libros tenmos y usar en lo siguiente 
#IMPORTANTE !!!! esto se repite dependiendo de lo anterio contado con el lenght(busca_nombre) se modifica el numero
Busca_pagina <-  rep(c("Busca libre"),time=c(75))#IMPORTANTE !!!! 
compara_busca <- data_frame(Lugar = Lugar,  Libro = busca_nombre,Precio = PreciosB, Pagina = Busca_pagina)
view(compara_busca)

#Ahora unimos los data frames compara_busca y compara_punto_y_coma anteriormente echo 
Busca_y_punto <- rbind(compara_busca,compara_punto_y_coma)
view(Busca_y_punto)


#queremos ver lo libros que cuestan menos de 10.000 pesos entre las dos paginas 
#para eso creamos la función menor_o_mayor la cual nos ayudara a ver los libros con un precio menor a 10.000 pesos 
menor_o_mayor <- Busca_y_punto %>% 
  filter(Precio <= 10000)

view(menor_o_mayor) # de esta forma vemos los libros menores a 10.000 dentro de Busca Libre y Punto y Coma 


#la funcion cuantos_busca nos dará el número de libros de Busca Libre con un valor menor a 10.000 pesos 
cuantos_busca <- menor_o_mayor %>% 
  filter(Pagina == "Busca libre") %>% 
  summarise(Contar = n())
view(cuantos_busca)

#la funcion cuantos_punto nos dará el número de libros de Punto y Coma con un valor menor a 10.000 pesos
cuantos_punto <- menor_o_mayor %>% 
  filter(Pagina == "Punto y coma") %>% 
  summarise(Contar = n())
view(cuantos_punto)

#decidimos comparar solo estas dos páginas ya que top 10 books no posee la misma cantidad de libros 
#comparados con Busca Libre y Punto y coma Libros




###########################################################
##########################     ############################
########################## Fin ############################
##########################     ############################
###########################################################