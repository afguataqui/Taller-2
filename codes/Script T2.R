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
paquetes = c('tidyverse','haven', 'sf', 'grid')
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
desocupado = plyr::rbind.fill(lista_data[grep('Cabecera - Desocu',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T),
                           lista_data[grep('Resto - Desocu',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T)) 

inactivo = plyr::rbind.fill(lista_data[grep('Cabecera - Inact',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T),
                           lista_data[grep('Resto - Inact',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T)) 

ocupado = plyr::rbind.fill(lista_data[grep('Cabecera - Ocupado',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T),
                           lista_data[grep('Resto - Ocupado',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T)) 

fuerza = plyr::rbind.fill(lista_data[grep('Cabecera - Fuerza',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T), 
                          lista_data[grep('Resto - Fuerza',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T))
#Borrar los que no se va a utilizar 
rm(caracte_r, caracte_u, files, lista_archivos, lista_data, meses, paquetes, f_read)

#1.3 
#Borrar variables que se repiten en data frames, dejarlas solo en 'caracte'
desocupado=select(desocupado, -hogar, -regis, -area, -clase, -mes, -dpto, -fex_c_2011)
fuerza=select(fuerza, -hogar, -regis, -area, -clase, -mes, -dpto, -fex_c_2011)
ocupado=select(ocupado, -hogar, -regis, -area, -clase, -mes, -dpto, -fex_c_2011)
inactivo=select(inactivo, -hogar, -regis, -area, -clase, -mes, -dpto, -fex_c_2011)

GEIH_2019 = merge(caracte, desocupado ,by = c('secuencia_p', 'orden', 'directorio'), all.x = T) %>%
  merge(., fuerza ,by= c('secuencia_p', 'orden', 'directorio'), all.x =T) %>%
  merge(., ocupado,by= c('secuencia_p', 'orden', 'directorio'), all.x =T) %>% #problemas con Ocupado e Inactivo
  merge(., inactivo,by= c('secuencia_p', 'orden', 'directorio'), all.x =T) 

View(GEIH_2019)
colnames(GEIH_2019) %>% sort(decreasing = F)

#1.4
##Liberar memoria, borrar lo que no se s¡necesita mas
saveRDS(file='data/procesada/GEIH 2019.rds', object= GEIH_2019)
rm(list=ls())
GEIH_2019= readRDS(file='data/procesada/GEIH 2019.rds') %>% subset(p6040>=18&p6040<=28)

#Como se tienen los datos de todo el año, se debe dividir el peso entre 12; cambiar el ',' por '.'; volverlo numerico
GEIH_2019= mutate(GEIH_2019, fex_c_2011 = as.character(fex_c_2011) %>% gsub(',','.',.) %>% as.numeric()) %>%
  mutate(peso = fex_c_2011/12)
GEIH_2019= mutate(GEIH_2019, p6160 = as.character(p6160)  %>% as.numeric())

class(GEIH_2019$fex_c_2011)
class(GEIH_2019$peso)
class(GEIH_2019$p6160)
#Tasa analfabetismo
##P6160 variable que pregunta: ¿Sabe leer y escribir?
t_alfabeta= GEIH_2019 %>% group_by(dpto, p6160) %>% summarise(n_p=sum(peso)) #PROBLEMA: Crea variable de 1obs 
t_alfabeta= GEIH_2019 %>% group_by(dpto, p6160) %>% summarise(n_p= sum(peso)) %>%
  reshape2::dcast(.,dpto~p6160)
colnames(t_falbeta) = c('dpto','sabe_le','no_sabe_le') 
t_falbeta = mutate(t_falbeta , tasa = no_sabe_le/(no_sabe_le+sabe_le)*100)
#Tasa graduados educacion superior
##P6269 variable que pregunta: ¿Se graduo usted de una escuela normal superior?

#Ingresos laborales
##inglabo variable que pregunta por los ingresos laborales


#2
##Importar shapefile
Dptos = st_read(dsn = 'data/orignal/MGN_DPTO_POLITICO.shp')
str(Dptos)





