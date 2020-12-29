
####Desarrollo de la extracción de datos



library(rvest)


rm(list = ls())



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





TOP10 <- seq(1,10)
TOP10
TOP_10_LIBROS_FIC <- data_frame(TOP_10 = TOP10,Libro = Nombres,Precio = Precio)

GRAFIC_TOP10 <- ggplot(data = TOP_10_LIBROS_FIC,
                   mapping = aes(x = Nombres, y = Precio)) +
  geom_col(width = .5,colour = "PINK",fill= "CYAN") +
  
  labs(title = "TOP 10 BOOKS  ")

GRAFIC_TOP10




####Mo ficcion###### 


link_top10 <- "https://www.top10books.cl/no-ficcion-102980.html"
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




TOP10 <- seq(1,9)
TOP10
TOP_10_LIBROS_FIC <- data_frame(TOP_10 = TOP10,Libro = Nombres,Precio = Precio)

GRAFIC_TOP10 <- ggplot(data = TOP_10_LIBROS_FIC,
                       mapping = aes(x = Nombres, y = Precio)) +
  geom_col(width = .5,colour = "PINK",fill= "CYAN") +
  
  labs(title = "TOP 10 BOOKS  ")

GRAFIC_TOP10




############################
#########################
.#####################
#######################
###################
############
#################
####################
##################
################


rm(list = ls())
######busca libre #######

link_busca <- "https://www.buscalibre.cl/libros-envio-express-chile-best-sellers-ficcion_t.html"
paths_allowed(paths = c(link_busca))

Pag_busca <- read_html(link_busca)
nombre_busca <- "div.nombre.margin-top-10.text-align-left"
busca_html <- html_nodes(Pag_busca,nombre_busca) 
busca_nombre <- html_text(busca_html)
busca_nombre



lugar_precios <- "h3.precio-ahora.hide-on-hover.margin-0.font-size-medium"
pre_html <- html_nodes(Pag_busca,lugar_precios)
pre_text <-html_text(pre_html)
pre_text
limpiador <- gsub("\\$","",pre_text) 
limpiador_2 <- gsub("\\.","",limpiador)
limpiador
Precios <- as.numeric(limpiador_2)
data.class(Precios)
Precios

Lugar_descuento <- "div.descuento-v2.color-white.position-relative"
des_html <- html_nodes(Pag_busca,Lugar_descuento)
des_text <- html_text(des_html)
des_lim <- gsub("\\%dcto","",des_text)
des_lim
descuento <- as.numeric(des_lim)
 
Lugar <- seq(1,76)
Lugar
#unir datos 

buscarlibre_fic <- data_frame(Lugar = Lugar,  Libro = busca_nombre,Precio = Precios ,Porcentaje_DCTO = descuento)
buscarlibre_fic

GRAFIC_B <- ggplot(data = buscarlibre_fic,
       mapping = aes(x = Lugar , y = Precios)) +
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
#
#
#
#
#
#####
####
###
####
###
####
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




