
#### Limpiar la consola y el entorno
cat("\f")
rm(list=ls())
options("scipen"=100, "digits"=4) # Forzar a R a no usar e+

#### Establecer directorio de trabajo
setwd("~/Dropbox/teaching/Taller de R/uniandes_2020_2/Task/task-2 answer oficial") # Eduard
getwd()

#### Librerias
paquetes = c('tidyverse','sf')
sapply(paquetes,require,character.only=T) 

#### Crear un vector con los nombres de los archivos
meses = list.files('data/orignal/2019/') %>% paste0('data/orignal/2019/',.,'/')
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

#### Crear objetos por modulo
caracte_u = lista_data[grep('Cabecera - Cara',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T) %>% mutate(urbano = 1)
caracte_r = lista_data[grep('Resto - Cara',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T) %>% mutate(urbano = 0)
caracte = plyr::rbind.fill(caracte_u,caracte_r)

desocupado = plyr::rbind.fill(lista_data[grep('Cabecera - Desocu',lista_archivos) ] %>% data.table::rbindlist(use.names = T,fill = T),
                              lista_data[grep('Resto - Desocu',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T)) %>% 
             dplyr::select(-hogar,-regis, -area, -clase, -mes, -dpto, -fex_c_2011)

inactivo = plyr::rbind.fill(lista_data[grep('Cabecera - Inact',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T),
                            lista_data[grep('Resto - Inact',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T) ) %>% 
           dplyr::select(-hogar,-regis, -area, -clase, -mes, -dpto, -fex_c_2011)

ocupado = plyr::rbind.fill(lista_data[grep('Cabecera - Ocupado',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T) ,
                           lista_data[grep('Resto - Ocupado',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T) )%>% 
          dplyr::select(-hogar,-regis, -area, -clase, -mes, -dpto, -fex_c_2011)

fuerza = plyr::rbind.fill(lista_data[grep('Cabecera - Fuerza',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T) ,
                          lista_data[grep('Resto - Fuerza',lista_archivos)] %>% data.table::rbindlist(use.names = T,fill = T) )%>% 
         dplyr::select(-hogar,-regis, -area, -clase, -mes, -dpto, -fex_c_2011)

#### Crear una base de datos
geih = merge(caracte,desocupado,c('secuencia_p','orden','directorio'),all.x = T) %>% 
       merge(.,ocupado,c('secuencia_p','orden','directorio'),all.x = T) %>% 
       merge(.,fuerza,c('secuencia_p','orden','directorio'),all.x = T) %>% 
       merge(.,inactivo,c('secuencia_p','orden','directorio'),all.x = T)
saveRDS(file = 'data/procesada/GEIH 2019.rds',object = geih)         

#### 1.4.
rm(list = ls())
geih = readRDS(file = 'data/procesada/GEIH 2019.rds') %>% 
       subset(p6040>=18&p6040<=28)
geih = mutate(geih , fex_c_2011 = as.character(fex_c_2011) %>% gsub(',','.',.) %>% as.numeric()) %>% 
       mutate(peso = fex_c_2011/12)

# Calcular tasa
t_falbeta = geih %>% group_by(dpto,p6160) %>% summarise(n_p = sum(peso)) %>%
            reshape2::dcast(.,dpto~p6160)
colnames(t_falbeta) = c('dpto','sabe_le','no_sabe_le') 
t_falbeta = mutate(t_falbeta , tasa = no_sabe_le/(no_sabe_le+sabe_le)*100)

# Pintar histograma
t_falbeta %>%
  mutate(name = fct_reorder(as.character(dpto), tasa)) %>%
  ggplot( aes(x=as.character(dpto), y=tasa)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

# Pintar mapa
mapa = st_read(dsn = 'data/orignal/MGN_DPTO_POLITICO.shp')
str(mapa)
str(t_falbeta)
mapa = mutate(mapa, dpto = as.numeric(as.character(DPTO_CCDGO)))# Crear variable dpto 
mapa = merge(mapa,t_falbeta,'dpto',all.x=T)# pegar variable a pintar

ggplot() + geom_sf(data = mapa,aes(fill=tasa),col='red')
