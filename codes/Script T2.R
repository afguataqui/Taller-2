#####TALLER 2 #####
#Andres Felipe Guatqui Delgado Codigo: 201631256
#1.1 
#Eliminar Enviroment
rm(list=ls())

#Limpiar la consola
cat("\f")
#Cual es mi directorio de trabajo
getwd()
#Fijar directorio de trabajo
setwd("~/Desktop/R/Tasks")

#Clonar repositorio Github
download.file(url = "https://github.com/taller-R/task-2/archive/main.zip", 
              destfile = "Taller_2.zip") ##nombre de como va a quedar guardado##

#Descomprimir
unzip(zipfile = "Taller_2.zip")

setwd("~/Desktop/R/Tasks/taller-2/")
list.files()
options("scipen"=100, "digits"=4) # Forzar a R a no usar e+

#### Librerias
paquetes = c('tidyverse','haven')
sapply(paquetes,require,character.only=T) 
#1.2
#### Crear un vector con los nombres de los archivos
meses = list.files('data/orignal/2019') %>% paste0('data/orignal/2019/',.,'/')
files = lapply(1:length(meses),function(x) list.files(meses[x]) %>% paste0(meses[x],.))  %>% unlist()
files

#### Funcion que lee los archivos
f_read = function(path){
         data = read.csv(file = path , sep = ';')
         colnames(data) = tolower(colnames(data))
         return(data)
}
#### Aplicar la funcion
lista_archivos = files[c(grep('Cabecera',files),grep('Resto',files))] %>% 
                  .[c(grep('Caracter',.),grep('Desocupados',.),grep('Inactivos',.),grep('Ocupados',.),grep('Fuerza',.))]

lista_data = lapply(lista_archivos, function(x) f_read(path = x))
###grep a utilizar para crear los objetos
lista_archivos
grep('Cabecera - Cara',lista_archivos) ##1-12
grep('Resto - Cara',lista_archivos)  ##13-24
grep('Cabecera - Desocu',lista_archivos) ##25-36
grep('Resto - Desocu',lista_archivos) ##37-48
grep('Cabecera - Inact',lista_archivos) ##49-60
grep('Resto - Inact',lista_archivos) ##61-72
grep('Cabecera - Ocupado',lista_archivos) ##73-84
grep('Resto - Ocupado',lista_archivos) ##85-96
grep('Cabecera - Fuerza',lista_archivos) ##97-108
grep('Resto - Fuerza',lista_archivos) ##109-120
#### Crear objetos por modulo
#Crear individualmente y luego unirlos
caracte_u = lista_data[grep('Cabecera - Cara',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T) %>% mutate(urbano = 1)
caracte_r = lista_data[grep('Resto - Cara',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T) %>% mutate(urbano = 0)
caracte = plyr::rbind.fill(caracte_u,caracte_r)
#Crear urbano y resto con el mismo codigo
desocupado = plyr::rbind.fill(lista_data[grep('Cabecera - Desocu',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T) %>% mutate(urbano = 1),
                           lista_data[grep('Resto - Desocu',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T) %>% mutate(urbano = 0))

inactivo = plyr::rbind.fill(lista_data[grep('Cabecera - Inact',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T) %>% mutate(urbano = 1),
                           lista_data[grep('Resto - Inact',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T) %>% mutate(urbano = 0))

ocupado = plyr::rbind.fill(lista_data[grep('Cabecera - Ocupado',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T) %>% mutate(urbano = 1),
                           lista_data[grep('Resto - Ocupado',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T) %>% mutate(urbano = 0))

fuerza = plyr::rbind.fill(lista_data[grep('Cabecera - Fuerza',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T) %>% mutate(urbano = 1),
                          lista_data[grep('Resto - Fuerza',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T) %>% mutate(urbano = 0))
#Borrar los que no se van a utilizar 
rm(caracte_r, caracte_u, files, lista_archivos, lista_data, meses, paquetes, f_read)
rm(GEIH_2019)
#1.3 
GEIH_2019 = merge(caracte, desocupado ,by = c('secuencia_p', 'orden', 'directorio'), all.x = T) %>%
  merge(., fuerza ,by= c('secuencia_p', 'orden', 'directorio'), all.x =T) %>%
  merge(., ocupado,by= c('secuencia_p', 'orden', 'directorio'), all =T) %>% #problemas con Ocupado e Inactivo
  merge(., inactivo,by= c('secuencia_p', 'orden', 'directorio'), all =T) 

#1.4



#2



