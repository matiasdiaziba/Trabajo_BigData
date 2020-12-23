
####Desarrollo de la extracción de datos
#probando git 
library("rvest")




rm(list = ls())



link_top10 <- "https://www.top10books.cl/ficcion-101605.html"
Pag_top10 <- read_html(link_top10)
lugar_no10 <- "strong.product.name.product-item-name"
no10_html <- html_nodes(Pag_top10,lugar_no10) 
no10_texto <- html_text(no10_html)
limpioador_1 <- gsub("\n","",no10_texto)
Nombres <- gsub("   ","",limpioador_1)
Nombres


lugar_precios <- "span.price"
pre_html <- html_nodes(Pag_top10,lugar_precios)
pre_text <-html_text(pre_html)
pre_text
