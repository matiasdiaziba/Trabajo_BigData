
####Desarrollo de la extracción de datos



library(rvest)
library(xml2)
library(dplyr)
library(ggplot2)





rm(list = ls())


###############
####################
##################
################


rm(list = ls())
######busca libre #######

#### Partiremos extrellendo datos de La pagina web de buscalibre espesificamente 
#la seccion de best sellers de ficcion
#sacaremos los datos suficientes para ver el 


link_busca <- "https://www.buscalibre.cl/libros-envio-express-chile-best-sellers-ficcion_t.html"
Pag_busca <- read_html(link_busca)
nombre_busca <- "div.nombre.margin-top-10.text-align-left"
busca_html <- html_nodes(Pag_busca,nombre_busca) 
busca_nombre <- html_text(busca_html)
busca_nombre



lugar_preciosB <- "h3.precio-ahora.hide-on-hover.margin-0.font-size-medium"
preB_html <- html_nodes(Pag_busca,lugar_preciosB)
preB_text <-html_text(preB_html)
preB_text
limpiador<- gsub("\\$","",preB_text) 
limpiador<- gsub("\\.","",limpiador)
limpiador
PreciosB <- as.numeric(limpiador)
data.class(PreciosB)
PreciosB

Lugar_descuento <- "div.descuento-v2.color-white.position-relative"
des_html <- html_nodes(Pag_busca,Lugar_descuento)
des_text <- html_text(des_html)
des_lim <- gsub("\\%dcto","",des_text)
des_lim
descuento <- as.numeric(des_lim)

length(busca_nombre)
Lugar <- seq(1,78)
Lugar
#unir datos 

buscarlibre_fic <- data_frame(Lugar = Lugar,  Libro = busca_nombre,Precio = PreciosB ,Porcentaje_DCTO = descuento)
buscarlibre_fic

GRAFIC_B <- ggplot(data = buscarlibre_fic,
       mapping = aes(x = Lugar , y = PreciosB)) +
  geom_point(size=3,pch=15,colour = "cyan") +
  geom_line(size = 0,colour = "blue")+
  labs(title = "Busca libre ")

GRAFIC_B
GRAFIC_c <- ggplot(data = buscarlibre_fic,
                   mapping = aes(x = Lugar , y = descuento)) +
  geom_col(width = .9,colour = "cyan",fill= "red") +
  geom_line(size = 0,colour = "blue")+
  labs(title = "Descuento")
GRAFIC_c

library(tidyverse)
 

dctomas50 <-  buscarlibre_fic %>% 
                filter(Porcentaje_DCTO >= 50)
view(dctomas50)

vermaxdcto <- buscarlibre_fic %>% 
  summarise(Max_dcto =max(Porcentaje_DCTO))
view(vermaxdcto)
maxdctolibro <- buscarlibre_fic %>% 
  filter(Porcentaje_DCTO == 80)
view(maxdctolibro)

###

dctomen50 <-  buscarlibre_fic %>% 
  filter(Porcentaje_DCTO <= 50)
view(dctomen50)
vermindcto <- buscarlibre_fic %>% 
  summarise(min_dcto =min(Porcentaje_DCTO))
view(vermindcto)
mindctolibro <- buscarlibre_fic %>% 
  filter(Porcentaje_DCTO == 15)
view(mindctolibro)
#
#
#
#####
####
###
####
###
###
###
#
rm(list = ls())


####PUNTO Y COMA #######

link_punto <- "https://www.puntoycomalibros.com/libros-de/ficcion-01/"
paths_allowed(paths = c(link_punto))

Pag_punto <- read_html(link_punto)
nombre_punto <- "dd.title a"
punto_html <- html_nodes(Pag_punto,nombre_punto) 
punto_nombre <- html_text(punto_html)



lugar_precios <-"p.precio strong"
pre_html <- html_nodes(Pag_punto,lugar_precios)
pre_text <-html_text(pre_html)
pre_text
limpiador <- gsub("\\$","",pre_text) 
limpiador <- gsub("\\.","",limpiador)
limpiador
Precios <- as.numeric(limpiador)
data.class(Precios)
Precios
Orden <- seq(1,30)

#unir datos 

Punto_y_coma <- data_frame(Orden = Orden ,  Libro = punto_nombre,Precio = Precios)
Punto_y_coma

GRAFIC_P   <- ggplot(data = Punto_y_coma,
                       mapping = aes_(x = Orden, y = Precios)) +
  geom_col(width = .8,colour = "cyan",fill= "pink") +
  geom_line(size = 1,colour = "green")+
  
  labs(title = "Punto y Coma")


GRAFIC_P

##
###
###
###
##
##
########top 10  ######
###ficcio###
link_top10 <- "https://www.top10books.cl/ficcion-101605.html"
Pag_top10 <- read_html(link_top10)

lugar_no10 <- "a.product-item-link"
no10_html <- html_nodes(Pag_top10,lugar_no10) 
no10_texto <- html_text(no10_html)
no10_texto
limpioador_1 <- gsub("\n","",no10_texto)
Nombres <- gsub("   ","",limpioador_1)
Nombres


lugar_precios <- "span.special-price"
pre_html <- html_nodes(Pag_top10,lugar_precios)
pre_text <-html_text(pre_html)
pre_text
data.class(pre_text)
Precio_ch <- gsub("\\$","",pre_text)
Precio_ch
Precio_ch <- gsub("Precio especial","",Precio_ch)
Precio_ch <- gsub("\\.","",Precio_ch)
Precio_ch <- gsub ("\n","",Precio_ch)
Precio <- gsub("\\  ","",Precio_ch)
Precio
TOP10 <- seq(1,10)
TOP10

Ficción <- rep(c("Ficcion"),time=c(10)) 


TOP_10_LIBROS_FIC <- data_frame(TOP_10 = TOP10,Libro = Nombres,Precios = Precio,categoria = Ficción)

library(ggplot2)

GRAFIC_TOP10 <- ggplot(data = TOP_10_LIBROS_FIC,
                       mapping = aes(x = Nombres, y = Precio)) +
  geom_col(width = .5,colour = "PINK",fill= "CYAN") +
  
  labs(title = "TOP 10 BOOKS  ")

GRAFIC_TOP10




####Mo ficcion###### 
link_top10f <- "https://www.top10books.cl/no-ficcion-102980.html"
Pag_top10f <- read_html(link_top10f)
lugar_nof10 <- "a.product-item-link"

nof10_html <- html_nodes(Pag_top10f,lugar_nof10) 
nof10_texto <- html_text(nof10_html)
nof10_texto
limpioadorf_1 <- gsub("\n","",nof10_texto)
Nombresf <- gsub("   ","",limpioadorf_1)
Nombresf


lugarf_precios <- "span.special-price"
pref_html <- html_nodes(Pag_top10f,lugarf_precios)
pref_text <-html_text(pref_html)
pref_text
data.class(pref_text)
Preciof_ch <- gsub("\\$","",pref_text)
Preciof_ch
Preciof_ch <- gsub("Precio especial","",Preciof_ch)
Preciof_ch <- gsub("\\.","",Preciof_ch)
Preciof_ch <- gsub ("\n","",Preciof_ch)
Preciof <- gsub("\\  ","",Preciof_ch)

TOP10 <- seq(1,10)
TOP10
NO_Ficción <- rep(c("No Ficcion"),time=c(10)) 

TOP_10_LIBROS_no_FIC <- data_frame(TOP_10 = TOP10,Libro = Nombresf,Precios = Preciof,categoria = NO_Ficción)

GRAFIC_TOP10_NO <- ggplot(data = TOP_10_LIBROS_no_FIC,
                          mapping = aes(x = Nombresf, y = Preciof)) +
  geom_col(width = .5,colour = "PINK",fill= "CYAN") +
  
  labs(title = "TOP 10 BOOKS NO FICCIÓN  ")

GRAFIC_TOP10_NO


names(TOP_10_LIBROS_no_FIC)
names(TOP_10_LIBROS_FIC)
todo_topbooks <- rbind(TOP_10_LIBROS_FIC,TOP_10_LIBROS_no_FIC)
todo_topbooks


library(tidyverse)



todo_topbooks %>% 
  filter(TOP_10 <= 5)



