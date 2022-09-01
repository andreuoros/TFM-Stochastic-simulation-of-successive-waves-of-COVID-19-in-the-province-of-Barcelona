# Packages ----------------------------------------------------------------

library(jsonlite)
library(tidyverse)
library(lubridate)
library(plotly)
library(knitr)
library(tidytext)

'%!in%' = Negate('%in%')
# ABS names file ----------------------------------------------------------

# abs_ss_rs <- 
#   read.csv2('BSC/abs_2022-06.csv',
#             colClasses = c('character','character','character','character','character','character')) %>% 
#   tibble()
# 
# abs_ss_rs <- read.csv2('BSC/ABS_metadata.csv',sep = ',') %>% 
#   rename(ABS = CODIABS, ABS_nom = NOMABS, RS_nom = NOMRS) %>% 
#   tibble() %>% 
#   filter(ABS != 'SES')
# 
# abs_bsc <- read.csv(file = 'BSC/ABS_metadata-BSC_modified.csv',encoding = "UTF-32")
# 
# bsc_abs_absname <- 
#   read_csv('BSC/bsc_abs_absname.csv') %>% tibble() %>% select(id, absname)

#con la matriz de abs de TONI

abs_ss_rs <- read_csv('BSC/Matriz_propuesta_agrupación_ABS_formatted_forPython.csv') %>% 
  tibble() %>% 
  select(`Código ABS Original`, Nombre) %>% 
  rename(ABS = `Código ABS Original`, ABS_nom = Nombre) %>% 
  mutate(ABS = as.character(ABS),
         ABS = case_when(nchar(ABS) == 1 ~ paste0('00',ABS),
                         nchar(ABS) == 2 ~ paste0('0',ABS),
                         nchar(ABS) == 3 ~ ABS))


# MITMA-ABS Tabla Conversion  ------------------------------------------------------------

# Cuando hacemos una consulta a bsc, el formato es mitma, es una formato a abs
# hacer la conversion de mitma a abs

# Conservamos solo el ratio1, por que es el ratio mitma -> abs, el otro rario es para hacer la ocnversion contraria
# no interesa

raw_overlaps <- as.list(1:2) %>%
  map(~ paste0(
    # Esta linea te conecta con api de bsc a la seccion de overlaps, overlaps es solapamiento de capas
    # hace la proyeccion de una capa sobre otra
    'https://flowmaps.life.bsc.es/api/layers.overlaps_population',
    # la cluasa where es para indicarle los parametros de la query
    '?where={',
    '"l.layer":"abs_09",',
    '"m.layer":"mitma_mov"}',
    '&page=',.,
    '&max_results=1000')) %>%
  map(fromJSON) %>%
  map('_items') %>%
  bind_rows() %>%
  as_tibble() %>%
  select(-('_id')) %>% 
  unnest() %>% 
  #filter(ratio >= 0.1 | ratio1 >= 0.1) %>%
  rename(codigo_abs = id, 
         codigo_mitma = id1, 
         ratio_mitma_abs = ratio1,
         ratio_abs_mitma = ratio) %>%
  select(codigo_mitma, codigo_abs, ratio_mitma_abs, ratio_abs_mitma) %>% 
  relocate(codigo_mitma, ratio_mitma_abs, codigo_abs, ratio_abs_mitma)


#Clean Codigos_mitma whose ratio sum equals 0 - we assume they are outside of Catalonia
##Get mitmas that are null after grouping
null_mitmas <-
  raw_overlaps %>%
  group_by(codigo_mitma) %>%
  summarise(ratio_mitma_abs = sum(ratio_mitma_abs)) %>%
  #0.05 <- valor encontrado como threshold para ciertas mitmas con esta reperesentacion o inferior
  filter(ratio_mitma_abs <= 0.05) %>%
  pull(codigo_mitma)

##Erase them from main dataset
raw_overlaps <- raw_overlaps %>%
  filter(!codigo_mitma %in% null_mitmas)

# Split raw overlaps between mitma_abs and abs_mitma conversion
mitma_abs <- raw_overlaps %>% 
  select(codigo_mitma, codigo_abs, ratio_mitma_abs) %>% 
  arrange(codigo_mitma)
#Todas las mitma contenidas en cataluña
codigos_mitma <- unique(raw_overlaps$codigo_mitma)


# Read raw data -----------------------------------------------------------

#ABS de referencia

# month_data <- c('february','march')
# 
# files_names <- 
#   month_data %>% 
#   map(~dir(paste0('BSC/Mobilidad_horas/txt_files/',.))) %>% 
#   transpose()
# 
# files_paths <- tibble(month_data)
abs_referencia <- c('025','078','351','247','152','290','136','259',
                    '240','339','255','124','208','103')

rango_fechas <- seq(as.Date('2020-02-24'),as.Date('2020-03-01'), by = '1 days')

files_names <- 
  gsub('-','', rango_fechas) %>% 
  map(~ list.files(path = 'BSC/Mobilidad_horas/raw_data_MITMA/',pattern = .)) %>% 
  compact()

#Filters for data
filter_actividad_origen <- c('casa')
filter_actividad_destino <- c('trabajo')

#Read data
raw_df_ABS <- 
  #files_february %>% 
  #files_march %>% 
  files_names %>% 
  map(~ read.table(file = paste0('BSC/Mobilidad_horas/raw_data_MITMA/',.x),
                   header = T,sep = '|') %>% 
        tibble() %>% 
        filter(origen %in% codigos_mitma & 
                 destino %in% codigos_mitma,
               #actividad_origen %in% filter_actividad_origen & actividad_destino %in% filter_actividad_destino
               (actividad_origen == 'casa' & actividad_destino == 'trabajo') |
                 (actividad_origen == 'otros' & actividad_destino == 'otros')
               ) %>% 
        select(-c(edad))) %>% 
  reduce(bind_rows) %>% 
  mutate(fecha = as.Date(as.character(fecha), "%Y%m%d")) %>% 
  left_join(mitma_abs, by = c('origen' = 'codigo_mitma')) %>%
  left_join(mitma_abs, by = c('destino' = 'codigo_mitma')) %>%
  rename(
    abs_origen = codigo_abs.x,
    abs_destino = codigo_abs.y,
    ratio_mitma_abs_origen = ratio_mitma_abs.x,
    ratio_mitma_abs_destino = ratio_mitma_abs.y) %>%
  mutate(
    viajes_conversion = viajes * ratio_mitma_abs_origen * ratio_mitma_abs_destino,
    viajes_km_conversion = viajes_km * ratio_mitma_abs_origen * ratio_mitma_abs_destino) %>%
  select(abs_origen, abs_destino, fecha, periodo, viajes_conversion, viajes_km_conversion,
         distancia,actividad_origen, actividad_destino) %>%
  mutate(day_of_week = factor(weekdays(fecha), 
                              levels = c('Monday','Tuesday','Wednesday','Thursday',
                                         'Friday','Saturday','Sunday')),
         weekend = case_when(day_of_week == 'Saturday' | day_of_week == 'Sunday' ~ 'Weekend',
                             day_of_week != 'Saturday' & day_of_week != 'Sunday' ~ 'Weekdays')) %>% 
  filter(abs_origen %in% abs_referencia)

raw_df_ABS <- 
  raw_df_ABS %>% 
  full_join(abs_ss_rs, by=c('abs_origen'= 'ABS')) %>%
  rename(c('NOM_ABS_origen' = 'ABS_nom')) %>% 
  full_join(abs_ss_rs, by=c('abs_destino'= 'ABS')) %>%
  rename(c('NOM_ABS_destino' = 'ABS_nom'))


# Distances distribution Whole day vs Morning --------------------------------------------------


## Numeric distribution-------------------------------------------------------------------------

#with numeric distances
raw_df_ABS %>% 
  filter(
    abs_origen %in% c(abs_referencia),
    #periodo %in% c(4,5,6,7,8,9,10),
    weekend == 'Weekdays',
    #abs_origen != abs_destino
    ) %>% 
  
  mutate(distancia_media = round(viajes_km_conversion/viajes_conversion,2)) %>% 
  filter(distancia_media <= 90) %>% 
  group_split(abs_origen) %>% 
  
  map(~ .x %>% 
        group_by(abs_origen, NOM_ABS_origen, abs_destino, distancia) %>% 
        summarise(viajes_total = sum(viajes_conversion), 
                  distancia_media = mean(distancia_media)) %>% 
        filter(viajes_total > 1) %>% 
        mutate(viajes_total = round(viajes_total,0)) %>% 
        uncount(viajes_total) %>% 
        
        ggplot(aes(distancia_media)) +
        geom_density() + 
        scale_x_continuous(breaks = seq(0,150,5)) +
        #facet_grid(~ month) +
        geom_vline(xintercept = 0.5, col= 'red', alpha = 0.5)+
        geom_vline(xintercept = 2, col= 'red', alpha = 0.5)+
        geom_vline(xintercept = 5, col= 'red', alpha = 0.5)+
        geom_vline(xintercept = 10, col= 'red', alpha = 0.5)+
        geom_vline(xintercept = 20, col= 'red', alpha = 0.5)+
        geom_vline(xintercept = 50, col= 'red', alpha = 0.5)+
        labs(title = paste0('Distances distribution for ',unique(.x$NOM_ABS_origen)),
             subtitle = 'Weekdays; Whole day'))

p <- raw_df_ABS_whole %>% 
  filter(
    abs_origen %in% c(abs_referencia),
    abs_origen != '136',
    periodo %in% c(5,6,7,8,9,10,11),
    weekend == 'Weekdays',
    #abs_origen != abs_destino,
    month == 'February') %>% 
  mutate(distancia_media = round(viajes_km_conversion/viajes_conversion,2)) %>% 
  filter(distancia_media <= 90) %>% 
  group_by(abs_origen, NOM_ABS_origen, abs_destino, distancia, month) %>% 
  summarise(viajes_total = sum(viajes_conversion), 
            distancia_media = mean(distancia_media)) %>% 
  filter(viajes_total > 1) %>% mutate(viajes_total = round(viajes_total,0)) %>% 
  uncount(viajes_total) %>% 
  ggplot(aes(distancia_media, group = NOM_ABS_origen, col = NOM_ABS_origen)) +
  geom_density() + 
  scale_x_continuous(breaks = seq(0,150,10)) +
  facet_grid(~ month) +
  labs(title = paste0('Distances distribution'))

p <- ggplotly(p)

htmlwidgets::saveWidget(p, "distances_distribution_per_abs.html")


## Categoric Distribution --------------------------------------------------


raw_df_ABS %>% 
  filter(
    abs_origen %in% c(abs_referencia),
    #periodo %in% c(4,5,6,7,8,9,10),
    weekend == 'Weekdays') %>% 
  select(-c(periodo, actividad_origen,actividad_destino, weekend, NOM_RS_destino,NOM_RS_origen)) %>% 
  
  mutate(distancia_media = round(viajes_km_conversion/viajes_conversion,2)) %>% 
  mutate(distancia = 
           case_when(distancia_media <= 2 ~ '[0.5,2]',
                     distancia_media > 2 & distancia_media <= 5 ~ '(2,5]',
                     distancia_media > 5 & distancia_media <= 10 ~ '(5,10]',
                     distancia_media > 10 & distancia_media <= 20 ~ '(10,20]',
                     distancia_media > 20 & distancia_media <= 50 ~ '(20,50]',
                     distancia_media > 50 ~ '>50'),
         distancia = factor(distancia,
                            levels = c('[0.5,2]','(2,5]','(5,10]',
                                       '(10,20]','(20,50]','>50'))) %>% 
  #filter(distancia_media <= 30) %>% 
  group_split(abs_origen) %>% 
  
  map(~ .x %>% 
        group_by(abs_origen, NOM_ABS_origen, abs_destino, distancia) %>% 
        summarise(viajes_total = sum(viajes_conversion), 
                  distancia_media = mean(distancia_media)) %>% 
        filter(viajes_total > 1) %>% 
        mutate(viajes_total = round(viajes_total,0)) %>% 
        uncount(viajes_total) %>% 
        group_by(abs_origen, NOM_ABS_origen, distancia) %>% 
        summarise(distancia_freq = n()) %>% 
        group_by(abs_origen,NOM_ABS_origen,.add = T) %>% 
        mutate(distancia_perc = (distancia_freq/sum(distancia_freq))*100) %>% 
        ggplot(aes(distancia,distancia_perc)) +
        geom_col() + 
        labs(title = paste0('Distances distribution for ',unique(.x$NOM_ABS_origen)),
             subtitle = 'Without ABS cut; Weekdays; 4am to 10am'))

# % distribution among distances bins and ABS

distances_distribution <- 
  raw_df_ABS %>% 
  filter(
    abs_origen %in% c(abs_referencia),
    #periodo %in% c(4,6,7,8,9,10,10),
    weekend == 'Weekdays') %>% 
    #abs_origen != abs_destino) %>% 
  
  mutate(distancia_media = round(viajes_km_conversion/viajes_conversion,2)) %>% 
  mutate(distancia = 
           case_when(distancia_media <= 2 ~ '[0.5,2]',
                     distancia_media > 2 & distancia_media <= 5 ~ '(2,5]',
                     distancia_media > 5 & distancia_media <= 10 ~ '(5,10]',
                     distancia_media > 10 & distancia_media <= 20 ~ '(10,20]',
                     distancia_media > 20 & distancia_media <= 50 ~ '(20,50]',
                     distancia_media > 50 ~ '>50'),
         distancia = factor(distancia,
                            levels = c('[0.5,2]','(2,5]','(5,10]',
                                       '(10,20]','(20,50]','>50'))) %>% 
  group_by(abs_origen, NOM_ABS_origen, abs_destino, distancia) %>% 
  summarise(viajes_total = sum(viajes_conversion), 
            distancia_media = mean(distancia_media)) %>% 
  filter(viajes_total >= 1) %>% 
  mutate(viajes_total = round(viajes_total,0)) %>% 
  uncount(viajes_total) %>%  
  group_by(abs_origen, NOM_ABS_origen, distancia) %>% 
  summarise(distancia_freq = n()) %>% 
  group_by(abs_origen,NOM_ABS_origen,.add = T) %>% 
  mutate(distancia_perc = (distancia_freq/sum(distancia_freq))*100) %>% 
  mutate(abs_origen = as.integer(abs_origen)) %>%
  ungroup() %>% 
  select(abs_origen, distancia, distancia_perc)


write_csv(distances_distribution, file = 'BSC/Mobilidad_horas/distances_distribution_whole_day.csv')


# Otros daily time series -------------------------------------------------

# horas_dia <- 
#   (raw_df_ABS %>% 
#      filter(abs_origen == '025' & weekend == 'Weekdays') %>% 
#      unite(rol, c(actividad_origen, actividad_destino), sep='-' ) %>%
#   
#      group_by(NOMABS_origen, evstart, rol, weekend) %>%
#      summarize(viajes= sum(viajes_conversion)) %>% 
#      mutate(hora_evstart = format(evstart, "%H:%M:%S")) %>% 
#      pull(hora_evstart))[1:24]


raw_df_ABS %>% 
  filter(abs_origen %in% abs_referencia) %>% 
  group_split(abs_origen) %>% 
  map(~ .x %>% 
        #filter(weekend == 'Weekdays') %>% 
        unite(rol, c(actividad_origen, actividad_destino), sep='-' ) %>%
        # filter(rol == 'casa-trabajo' |
        #          rol == 'casa-otros' |
        #          rol == 'trabajo-otros' |
        #          rol == 'otros-otros') %>%
        # mutate(hora_evstart = substr(factor(format(evstart, "%H:%M:%S"),
        #                                     levels = horas_dia),1,2)) %>% 
        group_by(NOM_ABS_origen, periodo, rol, weekend) %>%
        summarize(viajes = sum(viajes_conversion)) %>%
        ungroup() %>% 
        ggplot(aes(periodo, viajes, col = rol, group = rol)) +
        geom_point(alpha=0.3) +
        geom_line(size=1.01) +
        #scale_x_datetime(breaks = '3 hours') + 
        theme(axis.text.x = element_text(angle=45,vjust = 0.9, hjust=1)) +
        labs(title =  paste0('Mobilidad agregada por horas en ',unique(.x$NOM_ABS_origen)),
             #subtitle= paste('Rol: ',.y),
             x='Hora de inicio de viaje',
             y= 'Nº de viajes',
             color= 'Motivo de origen \n y destino del viaje') +
        facet_grid(~weekend))

raw_df_ABS %>% 
  filter(abs_origen %in% abs_referencia) %>% 
  group_split(abs_origen) %>% 
  map(~ .x %>% 
        #filter(weekend == 'Weekdays') %>% 
        unite(rol, c(actividad_origen, actividad_destino), sep='-' ) %>%
        # filter(rol == 'casa-trabajo' |
        #          rol == 'casa-otros' |
        #          rol == 'trabajo-otros' |
        #          rol == 'otros-otros') %>%
        # mutate(hora_evstart = substr(factor(format(evstart, "%H:%M:%S"),
        #                                     levels = horas_dia),1,2)) %>% 
        group_by(NOM_ABS_origen, day_of_week, rol) %>%
        summarize(viajes = sum(viajes_conversion)) %>%
        ungroup() %>% 
        ggplot(aes(day_of_week, viajes, col = rol, group = rol)) +
        geom_point(alpha=0.3) +
        geom_line(size=1.01) +
        #scale_x_datetime(breaks = '3 hours') + 
        theme(axis.text.x = element_text(angle=45,vjust = 0.9, hjust=1)) +
        labs(title =  paste0('Mobilidad agregada por horas en ',unique(.x$NOM_ABS_origen)),
             #subtitle= paste('Rol: ',.y),
             x='Hora de inicio de viaje',
             y= 'Nº de viajes',
             color= 'Motivo de origen \n y destino del viaje')) 
      #+ facet_grid(~weekend))


# ABS-list distances quality check -----------------------------------------------------------
#Matriz distancias entre ABS

distance_matrix <- readxl::read_xlsx('BSC/Distancias_ABS.xlsx') %>% tibble()
distance_matrix$DURATION_MIN <- distance_matrix$DURATION_H*60
distance_matrix <- distance_matrix %>% 
  mutate(FROM_ID = as.character(FROM_ID),
         TO_ID = as.character(TO_ID),
         FROM_ID = 
           case_when(nchar(FROM_ID) == 1 ~ paste0('00',FROM_ID),
                     nchar(FROM_ID) == 2 ~ paste0('0',FROM_ID),
                     nchar(FROM_ID) == 3 ~ FROM_ID),
         TO_ID = 
           case_when(nchar(TO_ID) == 1 ~ paste0('00',TO_ID),
                     nchar(TO_ID) == 2 ~ paste0('0',TO_ID),
                     nchar(TO_ID) == 3 ~ TO_ID))
distance_matrix <- distance_matrix %>% 
  #unite(col = fromABS_toABS, c(FROM_ID,TO_ID), sep = '-') %>% 
  mutate(fromABS_toABS = paste0(FROM_ID,'-',TO_ID)) %>% 
  select(-c(DURATION_H)) %>% 
  rename(time_min_matrix = DURATION_MIN, distancia_km_matrix  = DIST_KM)

df_ABS <- 
  raw_df_ABS %>% 
  filter(weekend == 'Weekdays',
         abs_origen != abs_destino) %>% 
  mutate(distancia_media = round(viajes_km_conversion/viajes_conversion,2)) %>% 
  mutate(distancia = 
           case_when(distancia_media <= 2 ~ '[0.5,2]',
                     distancia_media > 2 & distancia_media <= 5 ~ '(2,5]',
                     distancia_media > 5 & distancia_media <= 10 ~ '(5,10]',
                     distancia_media > 10 & distancia_media <= 20 ~ '(10,20]',
                     distancia_media > 20 & distancia_media <= 50 ~ '(20,50]',
                     distancia_media > 50 ~ '>50'),
         distancia = factor(distancia,
                            levels = c('[0.5,2]','(2,5]','(5,10]',
                                       '(10,20]','(20,50]','>50'))) %>% 
  group_by(abs_origen, abs_destino,distancia, NOM_ABS_origen, NOM_ABS_destino) %>% 
  summarise(viajes_total = sum(viajes_conversion), 
            distancia_media = mean(distancia_media)) %>% 
  mutate(viajes_perc = round((viajes_total/sum(viajes_total))*100,2))

df_ABS <- df_ABS %>% 
  mutate(fromABS_toABS = paste0(abs_origen,'-',abs_destino))

df_ABS <- df_ABS %>% 
  left_join(distance_matrix, by = c('fromABS_toABS'='fromABS_toABS'))
  


df_ABS %>% 
  ggplot(aes(x = distancia_media, y = distancia_km_matrix)) + 
  geom_jitter(width = 0.1) + 
  scale_y_continuous(limits = c(0,300),breaks = seq(0,300,50)) + 
  scale_x_continuous(
    limits = c(0,300),
    breaks = seq(0,300,50)) + 
  xlab('Distancias BSC') + 
  ylab('Distancias Matriz Toni')


#only Barcelona
df_ABS %>% 
  filter(str_detect(NOM_ABS_origen, 'BARCELONA') & 
           str_detect(NOM_ABS_destino,'BARCELONA')) %>% 
  ggplot(aes(distancia_media, distancia_km_matrix)) + 
  geom_jitter(width = 0.1) + 
  xlab('Distancia BSC (BARCELONA)') + 
  ylab('Distancia Matriz Toni (BARCELONA)')
df_ABS %>% 
  filter(str_detect(NOM_ABS_origen, 'HOSPITALET') & 
           str_detect(NOM_ABS_destino,'HOSPITALET')) %>% 
  ggplot(aes(distancia_media, distancia_km_matrix)) + 
  geom_jitter(width = 0.1) + 
  xlab('Distancia BSC (HOSPITALET)') + 
  ylab('Distancia Matriz Toni (HOSPITALET)')

# WORK ABS-list -----------------------------------------------------------

# First extract raw_df_ABS with CASA-TRABAJO
abs_list_work <- 
  raw_df_ABS %>% 
  filter(weekend == 'Weekdays') %>% 
  mutate(distancia_media = round(viajes_km_conversion/viajes_conversion,2)) %>% 
  mutate(distancia = 
           case_when(distancia_media <= 2 ~ '[0.5,2]',
                     distancia_media > 2 & distancia_media <= 5 ~ '(2,5]',
                     distancia_media > 5 & distancia_media <= 10 ~ '(5,10]',
                     distancia_media > 10 & distancia_media <= 20 ~ '(10,20]',
                     distancia_media > 20 & distancia_media <= 50 ~ '(20,50]',
                     distancia_media > 50 ~ '>50'),
         distancia = factor(distancia,
                            levels = c('[0.5,2]','(2,5]','(5,10]',
                                       '(10,20]','(20,50]','>50'))) %>% 
  split(f = as.factor(.$abs_origen)) %>% 
  map(~ .x %>% split(f = as.factor(.$distancia)) %>% 
        map(~ .x %>% 
              group_by(abs_origen, abs_destino) %>% 
              summarise(viajes_total = sum(viajes_conversion)) %>% 
              mutate(viajes_perc = round((viajes_total/sum(viajes_total))*100,5)) %>% 
              arrange(desc(viajes_perc)) %>% 
              filter(viajes_perc > 0.05) %>%
              #Recalcular los porcentajes despues de filtrar
              mutate(viajes_perc = round((viajes_total/sum(viajes_total))*100,5)) %>% 
              arrange(desc(viajes_perc)) %>%
              mutate(viajes_perc_cumsum = cumsum(viajes_perc)) %>% 
              # filter(viajes_perc_cumsum <= 99) %>% 
              # mutate(viajes_perc = (viajes_total/sum(viajes_total)),
              #        viajes_perc_cumsum = cumsum(viajes_perc)) %>% 
              ungroup() %>% 
              select(abs_destino,viajes_perc_cumsum) %>% 
              #to keep order by viajes_perc_cumsum, we transform abs_destino to factor to avoid alphabetical ordering of string
              mutate(abs_destino = factor(abs_destino, levels = abs_destino)) %>% 
              spread(abs_destino,viajes_perc_cumsum) 
        ))


write_json(abs_list_work, 'ABSlist_work.json')

## WORK ABS-list comparisons -----------------------------------------------

#Alternative without raw_overlaps
all_mobility_abs_work <- read_csv('BSC/work_abs_mobility_evolution_20200214_20210430.csv')

mobility_abs_work <- all_mobility_abs_work %>% filter(fecha >= '2020-02-24' & fecha <= '2020-03-01')
mobility_abs_work <- 
  mobility_abs_work %>% 
  mutate(day_of_week = factor(weekdays(fecha), 
                              levels = c('Monday','Tuesday','Wednesday','Thursday',
                                         'Friday','Saturday','Sunday')),
         weekend = case_when(day_of_week == 'Saturday' | day_of_week == 'Sunday' ~ 'Weekend',
                             day_of_week != 'Saturday' & day_of_week != 'Sunday' ~ 'Weekdays')) %>% 
  filter(abs_origen %in% abs_referencia) %>% 
  full_join(abs_ss_rs, by=c('abs_origen'= 'ABS')) %>%
  rename(c('NOM_ABS_origen' = 'ABS_nom')) %>% 
  full_join(abs_ss_rs, by=c('abs_destino'= 'ABS')) %>%
  rename(c('NOM_ABS_destino' = 'ABS_nom'))

raw_df_ABS <- mobility_abs_work

# First extract raw_df_ABS with CASA-TRABAJO
raw_df_ABS %>% 
  #filter(weekend == 'Weekdays') %>% 
  filter(abs_origen %in% abs_referencia) %>% 
  #mutate(distancia_media = round(viajes_km_conversion/viajes_conversion,2)) %>% 
  mutate(distancia = 
           case_when(distancia_media <= 2 ~ '[0.5,2]',
                     distancia_media > 2 & distancia_media <= 5 ~ '(2,5]',
                     distancia_media > 5 & distancia_media <= 10 ~ '(5,10]',
                     distancia_media > 10 & distancia_media <= 20 ~ '(10,20]',
                     distancia_media > 20 & distancia_media <= 50 ~ '(20,50]',
                     distancia_media > 50 ~ '>50'),
         distancia = factor(distancia,
                            levels = c('[0.5,2]','(2,5]','(5,10]',
                                       '(10,20]','(20,50]','>50'))) %>% 
  split(f = as.factor(.$NOM_ABS_origen)) %>% 
  # map(~ .x %>% 
  # split(f = as.factor(.$distancia)) %>% 
  map(~ .x %>% 
        group_by(NOM_ABS_origen, NOM_ABS_destino, distancia, weekend) %>% 
        summarise(viajes_total = sum(total_viajes)) %>% 
        ungroup() %>% 
        
        group_by(NOM_ABS_origen, distancia, weekend) %>% 
        mutate(viajes_perc = round((viajes_total/sum(viajes_total))*100,5)) %>% 
        #arrange(desc(viajes_perc)) %>% 
        filter(viajes_perc >= 1) %>%
        group_by(NOM_ABS_origen, distancia, weekend) %>% 
        mutate(viajes_perc = round((viajes_total/sum(viajes_total))*100,5)) %>% 
        #Recalcular los porcentajes despues de filtrar
        #ungroup() %>% 
        arrange(distancia,weekend, desc(viajes_perc)) %>% 
        ggplot() + 
        geom_col(aes(x = reorder_within(NOM_ABS_destino, viajes_perc, list(distancia,weekend)),
                     y = viajes_perc),
                 position = 'dodge') +
        scale_x_reordered() +
        labs(title = paste0('ABS Origin: ',unique(.x$NOM_ABS_origen)),
             subtitle = '% of trips recomputed after filtering <1% trips destinations') +
        theme(axis.text.x = element_text(size = 9),
              axis.text.y = element_text(size = 6),
              title = element_text(size=12)) +
        facet_wrap(~distancia+weekend,scales = 'free') +
        xlab('ABS Destination') +
        ylab('% trips') +
        coord_flip()
  )


# OTROS ABS-list -----------------------------------------------------------

# First extract raw_df_ABS with OTROS-OTROS
abs_list_otros <- 
  raw_df_ABS %>% 
  filter(weekend == 'Weekdays') %>% 
  mutate(distancia_media = round(viajes_km_conversion/viajes_conversion,2)) %>% 
  mutate(distancia = 
           case_when(distancia_media <= 2 ~ '[0.5,2]',
                     distancia_media > 2 & distancia_media <= 5 ~ '(2,5]',
                     distancia_media > 5 & distancia_media <= 10 ~ '(5,10]',
                     distancia_media > 10 & distancia_media <= 20 ~ '(10,20]',
                     distancia_media > 20 & distancia_media <= 50 ~ '(20,50]',
                     distancia_media > 50 ~ '>50'),
         distancia = factor(distancia,
                            levels = c('[0.5,2]','(2,5]','(5,10]',
                                       '(10,20]','(20,50]','>50'))) %>% 
  split(f = as.factor(.$abs_origen)) %>% 
  map(~ .x %>% split(f = as.factor(.$distancia)) %>% 
        map(~ .x %>% 
              group_by(abs_origen, abs_destino) %>% 
              summarise(viajes_total = sum(viajes_conversion)) %>% 
              mutate(viajes_perc = round((viajes_total/sum(viajes_total))*100,5)) %>% 
              arrange(desc(viajes_perc)) %>% 
              filter(viajes_perc > 0.05) %>%
              #Recalcular los porcentajes despues de filtrar
              mutate(viajes_perc = round((viajes_total/sum(viajes_total))*100,5)) %>% 
              arrange(desc(viajes_perc)) %>%
              mutate(viajes_perc_cumsum = cumsum(viajes_perc)) %>% 
              # filter(viajes_perc_cumsum <= 99) %>% 
              # mutate(viajes_perc = (viajes_total/sum(viajes_total)),
              #        viajes_perc_cumsum = cumsum(viajes_perc)) %>% 
              ungroup() %>% 
              select(abs_destino,viajes_perc_cumsum) %>% 
              #to keep order by viajes_perc_cumsum, we transform abs_destino to factor to avoid alphabetical ordering of string
              mutate(abs_destino = factor(abs_destino, levels = abs_destino)) %>% 
              spread(abs_destino,viajes_perc_cumsum) 
        ))

write_json(abs_list_work, 'ABSlist_work.json')


## OTROS ABS-list comparisons -----------------------------------------------

# First extract raw_df_ABS with OTROS-OTROS
raw_df_ABS %>% 
  #filter(weekend == 'Weekdays') %>% 
  filter(abs_origen %in% abs_referencia) %>% 
  mutate(distancia_media = round(viajes_km_conversion/viajes_conversion,2)) %>% 
  mutate(distancia = 
           case_when(distancia_media <= 2 ~ '[0.5,2]',
                     distancia_media > 2 & distancia_media <= 5 ~ '(2,5]',
                     distancia_media > 5 & distancia_media <= 10 ~ '(5,10]',
                     distancia_media > 10 & distancia_media <= 20 ~ '(10,20]',
                     distancia_media > 20 & distancia_media <= 50 ~ '(20,50]',
                     distancia_media > 50 ~ '>50'),
         distancia = factor(distancia,
                            levels = c('[0.5,2]','(2,5]','(5,10]',
                                       '(10,20]','(20,50]','>50'))) %>% 
  split(f = as.factor(.$NOM_ABS_origen)) %>% 
  # map(~ .x %>% 
        # split(f = as.factor(.$distancia)) %>% 
        map(~ .x %>% 
              group_by(NOM_ABS_origen, NOM_ABS_destino, distancia, weekend) %>% 
              summarise(viajes_total = sum(viajes_conversion)) %>% 
              ungroup() %>% 
              
              group_by(NOM_ABS_origen, distancia, weekend) %>% 
              mutate(viajes_perc = round((viajes_total/sum(viajes_total))*100,5)) %>% 
              #arrange(desc(viajes_perc)) %>% 
              filter(viajes_perc >= 1) %>%
              group_by(NOM_ABS_origen, distancia, weekend) %>% 
              mutate(viajes_perc = round((viajes_total/sum(viajes_total))*100,5)) %>% 
              #Recalcular los porcentajes despues de filtrar
              #ungroup() %>% 
              arrange(distancia,weekend, desc(viajes_perc)) %>% 
              ggplot() + 
            geom_col(aes(x = reorder_within(NOM_ABS_destino, viajes_perc, list(distancia,weekend)),
                         y = viajes_perc),
                     position = 'dodge') +
              scale_x_reordered() +
              labs(title = paste0('ABS Origin: ',unique(.x$NOM_ABS_origen)),
                   subtitle = '% of trips recomputed after filtering <1% trips destinations') +
              theme(axis.text.x = element_text(size = 9),
                    axis.text.y = element_text(size = 6),
                    title = element_text(size=12)) +
              facet_wrap(~distancia+weekend,scales = 'free') +
              xlab('ABS Destination') +
              ylab('% trips') +
              coord_flip()
        )

# WORK vs OTROS ABS-list comparisons -----------------------------------------------

#Alternative without raw_overlaps

all_mobility_abs_work <- read_csv('BSC/work_abs_mobility_evolution_20200214_20210430.csv')
all_mobility_abs_work$rol <- 'trabajo'

all_mobility_abs_otros <- read_csv('BSC/otros_abs_mobility_evolution_20200214_20210430.csv')
all_mobility_abs_otros$rol <- 'otros'

all_mobility <- rbind(all_mobility_abs_otros,all_mobility_abs_work)

all_mobility <- all_mobility %>% filter(fecha >= '2020-02-24' & fecha <= '2020-03-01')

all_mobility <- 
  all_mobility %>% 
  mutate(day_of_week = factor(weekdays(fecha), 
                              levels = c('Monday','Tuesday','Wednesday','Thursday',
                                         'Friday','Saturday','Sunday')),
         weekend = case_when(day_of_week == 'Saturday' | day_of_week == 'Sunday' ~ 'Weekend',
                             day_of_week != 'Saturday' & day_of_week != 'Sunday' ~ 'Weekdays')) %>% 
  filter(abs_origen %in% abs_referencia) %>% 
  full_join(abs_ss_rs, by=c('abs_origen'= 'ABS')) %>%
  rename(c('NOM_ABS_origen' = 'ABS_nom')) %>% 
  full_join(abs_ss_rs, by=c('abs_destino'= 'ABS')) %>%
  rename(c('NOM_ABS_destino' = 'ABS_nom'))

raw_df_ABS <- all_mobility

# First extract raw_df_ABS with OTROS-OTROS
raw_df_ABS %>% 
  filter(weekend == 'Weekdays') %>% 
  filter(abs_origen %in% abs_referencia) %>% 
  #mutate(distancia_media = round(viajes_km_conversion/viajes_conversion,2)) %>% 
  mutate(distancia = 
           case_when(distancia_media <= 2 ~ '[0.5,2]',
                     distancia_media > 2 & distancia_media <= 5 ~ '(2,5]',
                     distancia_media > 5 & distancia_media <= 10 ~ '(5,10]',
                     distancia_media > 10 & distancia_media <= 20 ~ '(10,20]',
                     distancia_media > 20 & distancia_media <= 50 ~ '(20,50]',
                     distancia_media > 50 ~ '>50'),
         distancia = factor(distancia,
                            levels = c('[0.5,2]','(2,5]','(5,10]',
                                       '(10,20]','(20,50]','>50'))) %>% 
  #unite(rol, c(actividad_origen,actividad_destino), sep = '-') %>% 
  split(f = as.factor(.$NOM_ABS_origen)) %>% 
  # map(~ .x %>% 
  # split(f = as.factor(.$distancia)) %>% 
  map(~ .x %>% 
        group_by(NOM_ABS_origen, NOM_ABS_destino, distancia, rol) %>% 
        summarise(viajes_total = sum(total_viajes)) %>% 
        ungroup() %>% 
        
        group_by(NOM_ABS_origen, distancia, rol) %>% 
        mutate(viajes_perc = round((viajes_total/sum(viajes_total))*100,5)) %>% 
        #arrange(desc(viajes_perc)) %>% 
        filter(viajes_perc >= 1) %>%
        group_by(NOM_ABS_origen, distancia, rol) %>% 
        mutate(viajes_perc = round((viajes_total/sum(viajes_total))*100,5)) %>% 
        #Recalcular los porcentajes despues de filtrar
        #ungroup() %>% 
        arrange(distancia,rol, desc(viajes_perc)) %>% 
        ggplot() + 
        geom_col(aes(x = reorder_within(NOM_ABS_destino, viajes_perc, list(distancia,rol)),
                     y = viajes_perc),
                 position = 'dodge') +
        scale_x_reordered() +
        labs(title = paste0('ABS Origin: ',unique(.x$NOM_ABS_origen)),
             subtitle = '% of trips recomputed after filtering <1% trips destinations') +
        theme(axis.text.x = element_text(size = 9),
              axis.text.y = element_text(size = 6),
              title = element_text(size=12)) +
        facet_wrap(~distancia+rol,scales = 'free') +
        xlab('ABS Destination') +
        ylab('% trips') +
        coord_flip()
  )


# Correlation plot CENSO-BSC ----------------------------------------------

distance_files <- list.files(path = 'BSC/censo_distancias/')

raw_df_ABS %>% 
  filter(weekend == 'Weekdays', abs_origen %in% abs_referencia) %>% 
  mutate(distancia_media = round(viajes_km_conversion/viajes_conversion,2))

df_censo_distances <- 
  distance_files %>% 
  map(~ read.csv(paste0('BSC/censo_distancias/', .x)) %>% 
        select(individualABS,distanceGAMMA_v2)) %>% 
  reduce(bind_rows)
  

# Comparing old and new data -----------------------------------------------------------

#Read df_ABS from categorizations_insights.R
df_ABS %>% 
  filter(weekend == 'Weekdays') %>% 
  mutate(distancia = as.factor(distancia)) %>% 
  mutate(distancia = forcats::fct_relevel(distancia, c("0.5 - 2","2 - 10","10 - 50",">50"))) %>% 
  group_split(abs_origen, distancia) %>% 
  map(~ .x %>% 
        unite(rol_unido, c(role_origen,role_destino), sep = '-') %>% 
        #mutate(rol_unido = case_when(rol == 'otros'))
        filter(rol_unido == 'casa-trabajo') %>%
        group_by(abs_origen, NOMABS_origen, NOMABS_destino,distancia) %>% 
        summarise(viajes_total = sum(viajes_conversion)) %>% 
        ungroup() %>% 
        mutate(viajes_perc = round((viajes_total/sum(viajes_total))*100,2)) %>% 
        arrange(desc(viajes_perc)) %>% 
        mutate(viajes_perc_cumsum = cumsum(viajes_perc)) %>% 
        filter(viajes_perc_cumsum <= 99)) %>% 
  reduce(bind_rows) %>% 
  group_split(abs_origen) %>% 
  map(~ .x %>%
        ungroup() %>% 
        group_by(distancia) %>% 
        ggplot() + 
        geom_col(aes(x = reorder_within(NOMABS_destino, viajes_total, 
                                        list(distancia)),
                     y = viajes_total)) + 
        scale_x_reordered() + 
        labs(title = paste0(unique(.x$NOMABS_origen)),
             subtitle = 'Casa-Trabajo') + 
        theme(axis.text.x = element_text(size = 9),
              axis.text.y = element_text(size = 6),
              title = element_text(size=12)) +
        facet_wrap(~distancia,scales = 'free') + 
        coord_flip())

new_df_ABS <- raw_df_ABS %>% filter(abs_origen %in% c('365'))
new_df_ABS %>% 
  filter(weekend == 'Weekdays') %>% 
  mutate(distancia = as.factor(distancia)) %>% 
  mutate(distancia = forcats::fct_relevel(distancia, c("0005-002","002-005","005-010",
                                                       "010-050","050-100","100+"))) %>% 
  group_split(abs_origen, distancia) %>% 
  map(~ .x %>% 
        # unite(rol_unido, c(role_origen,role_destino), sep = '-') %>% 
        # #mutate(rol_unido = case_when(rol == 'otros'))
        # filter(rol_unido == 'casa-trabajo') %>%
        group_by(abs_origen, NOM_ABS_origen, NOM_ABS_destino, distancia) %>% 
        summarise(viajes_total = sum(viajes_conversion)) %>% 
        ungroup() %>% 
        mutate(viajes_perc = round((viajes_total/sum(viajes_total))*100,2)) %>% 
        arrange(desc(viajes_perc)) %>% 
        mutate(viajes_perc_cumsum = cumsum(viajes_perc)) %>% 
        filter(viajes_perc_cumsum <= 99)) %>% 
  reduce(bind_rows) %>% 
  group_split(abs_origen) %>% 
  map(~ .x %>%
        ungroup() %>% 
        group_by(distancia) %>% 
        ggplot() + 
        geom_col(aes(x = reorder_within(NOM_ABS_destino, viajes_total, 
                                        list(distancia)),
                     y = viajes_total)) + 
        scale_x_reordered() + 
        labs(title = paste0(unique(.x$NOM_ABS_origen)),
             subtitle = 'Casa-Trabajo') + 
        theme(axis.text.x = element_text(size = 9),
              axis.text.y = element_text(size = 6),
              title = element_text(size=12)) +
        facet_wrap(~distancia,scales = 'free') + 
        coord_flip())

