# PREGUNTA 1.1
#Librerias
install.packages("httr")
install.packages("XML")
install.packages("ggpubr")
install.packages("ggplot2")


#Cargar librerías
library(httr)
library(XML)

#URL
url <- "https://www.mediawiki.org/wiki/MediaWiki"

#Download page
page <- GET(url)

#Status
status_code(page)

#De HTML a formato XML
parsed_page <- htmlParse(content(page, as = "text"))

# PREGUNTA 1.2
#Título pagina
title <- xpathSApply(parsed_page, "//title", xmlValue)
title

#Styless
stylesheets <- xpathSApply(parsed_page, "//link[@rel='stylesheet']/@href")
stylesheets

#Autor
author <- xpathSApply(parsed_page, "//meta[@name='author']/@content")
author

#Descripcion
description <- xpathSApply(parsed_page, "//meta[@name='description']/@content")
description

#Codificación Type
encoding <- xpathSApply(parsed_page, "//meta[@charset]/@charset")
encoding

#keywords
keywords <- xpathSApply(parsed_page, "//meta[@name='keywords']/@content")
keywords

# PREGUNTA 1.3
#Texto del enlace
links_text <- xpathSApply(parsed_page, "//a", xmlValue)
print(links_text)

#URL
links_url <- xpathSApply(parsed_page, "//a", xmlGetAttr, 'href')
print(links_url)

# PREGUNTA 1.4 & 1.5
tabla <- data.frame(links_text = character(),
                    links_original_url = character(),
                    links_url = character(),
                    links_relative = character(),
                    links_internal = character(),
                    repeticiones = numeric(),
                    scraps = character(),
                    stringsAsFactors = FALSE)

frecuencia <- table(links_url)

for (i in 1:length(links_text)) {
  Sys.sleep(2)
  print("round done")
  tabla[i, "links_text"] <- links_text[i]
  tabla[i, "links_original_url"] <- links_url[i]
  tabla[i, "links_url"] <- links_url[i]
  tabla[i, "links_relative"] <- "N"
  tabla[i, "links_internal"] <- "S"
  tabla[i, "repeticiones"] <- frecuencia[links_url[i]]
  
  #si inicia con /wiki/
  validation_wiki <- startsWith(links_url[i], "/wiki/")
  if(validation_wiki) {
    tabla[i, "links_url"] <- paste0("https://www.mediawiki.org", links_url[i])
    tabla[i, "links_relative"] <- "S"
  }
  
  #si inicia con /https/
  validation_https <- startsWith(links_url[i], "https:")
  if(validation_https) {
    tabla[i, "links_url"] <- links_url[i]
    validation_internal <- startsWith(links_url[i], "https://www.mediawiki.org")
    if(!validation_internal) {
      tabla[i, "links_internal"] <- "N"
    }
  }
  
  #si inicia con //
  validation_slash <- startsWith(links_url[i], "//")
  if(validation_slash) {
    tabla[i, "links_url"] <- paste0("https:", links_url[i])
    tabla[i, "links_relative"] <- "S"
  }
  
  #si inicia con /w/
  validation_w <- startsWith(links_url[i], "/w/")
  if(validation_w) {
    tabla[i, "links_url"] <- paste0("https://www.mediawiki.org", links_url[i])
    tabla[i, "links_relative"] <- "S"
  }
  
  #si inicia con /#/
  validation_hash <- startsWith(links_url[i], "#")
  if(validation_hash) {
    tabla[i, "links_url"] <- paste0("https://www.mediawiki.org/wiki/MediaWiki", links_url[i])
    tabla[i, "links_relative"] <- "S"
  }
  
  print(paste0("TEST> " , links_url[i]))
  
  #obtencion de STATUS CODE
  code <- status_code(HEAD(tabla[i, "links_url"]))
  tabla[i, "scraps"] <- code
  print(code)
}

#tabla

#PREGUNTA 2.1 y 2.2
library(ggplot2)
library(ggpubr)

relative <- c(1, 2, 3, 4, 5, 4, 3, 2, 1)
no_relative <- c(2, 4, 6, 8, 10, 8, 6, 4, 2)

tabla2 <- tabla[tabla$links_relative =="S", c("repeticiones")]
tabla3 <- tabla[tabla$links_relative =="N", c("repeticiones")]
tabla6 <- tabla[TRUE, c("links_internal")]
tabla7 <- as.numeric(tabla[TRUE, c("scraps")])
tabla7 <- (tabla[TRUE, c("scraps")])

# Crear datos
relative <- tabla2
no_relative <- tabla3

# Genera 1er histograma
p1 <- ggplot(data.frame(x=relative), aes(x=x)) + 
  geom_histogram(aes(y=..count..), fill="blue", alpha=0.5) +
  labs(title="Histograma de relative", x="Valores", y="Frecuencia") 

# Genera 2do histograma
p2 <- ggplot(data.frame(x=no_relative), aes(x=x)) + 
  geom_histogram(aes(y=..count..), fill="red", alpha=0.5) +
  labs(title="Histograma de no-relative", x="Valores", y="Frecuencia")

# Genera 3er histograma
factor_tabla6 <- factor(tabla6, levels = c("S", "N"))

# Data frame columna "x"
data <- data.frame(x = factor_tabla6)

# Crear el histograma
p3 <- ggplot(data, aes(x = x)) + 
  geom_bar(aes(y=..count../sum(..count..)), fill="blue", alpha=0.5, stat = "count") +
  labs(title="Histograma de tabla6", x="Valores", y="Frecuencia")

# Ordena los gráficos 
theme_set(theme_classic())

# Ordena filas y columnas
ggarrange(p1, p2, p3, ncol=1, heights=c(1,1,1.2))

#PREGUNTA 2.3
# Librerias ggplot2 y ggpubr

library(ggplot2)
library(ggpubr)

# Crea datos
relative <- tabla2
no_relative <- tabla3

# Genera 1er histograma
p1 <- ggplot(data.frame(x=relative), aes(x=x)) + 
  geom_histogram(aes(y=..count..), fill="blue", alpha=0.5) +
  labs(title="Histograma de relative", x="Valores", y="Frecuencia") 

# Genera 2do histograma
p2 <- ggplot(data.frame(x=no_relative), aes(x=x)) + 
  geom_histogram(aes(y=..count..), fill="red", alpha=0.5) +
  labs(title="Histograma de no-relative", x="Valores", y="Frecuencia")

# Genera 2do histograma
factor_tabla6 <- factor(tabla6, levels = c("S", "N"))

# Data frame columna "x"
data <- data.frame(x = factor_tabla6)

# Genera el histograma
p3 <- ggplot(data, aes(x = x)) + 
  geom_bar(aes(y=..count../sum(..count..)), fill="blue", alpha=0.5, stat = "count") +
  labs(title="Histograma de tabla6", x="Valores", y="Frecuencia")

p4 <- ggplot(data.frame(x=tabla7), aes(x= "", fill = x)) + 
  geom_bar(width = 1) + coord_polar(theta = "y") +
  labs(title="status code")

# Ordena gráficos
theme_set(theme_classic())

# Reordena 2 filas, 1 columna 
ggarrange(p1, p2, p3, p4, ncol=1, heights=c(1,1,1,1.2))



