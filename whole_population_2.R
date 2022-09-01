library(jsonlite)
library(tidyverse)
library(ggpubr)
library(plotly)
library(htmlwidgets)

'%!in%' <- function(x,y)!('%in%'(x,y))
# /////////////////////////////////////////////////////////////////////////////////////////////////
# Setting for BSC API
# /////////////////////////////////////////////////////////////////////////////////////////////////

dates <- c(seq(as.Date("2020/01/01"), as.Date("2021/08/01"),by='days'))
# pages <- 1:2
pages <- 1:2


# ////////////////////////////////////////////////////////////
# Population data download ----
# ////////////////////////////////////////////////////////////

BSC_ABS_population <- dates %>% 
  as.list() %>%
  map(~ paste0(
    'https://flowmaps.life.bsc.es/api/layers.data.consolidated',
    '?where={',
    '%22type%22:%22population%22,',
    '%22layer%22:%22abs_09%22,',
    '%22date%22:%22', . ,'%22}',
    '&max_results=1000') ) %>%
  map(fromJSON)  %>%
  map('_items') %>%
  bind_rows() %>%
  select(date , id, population) %>%
  tibble() %>% 
  rename(abs = id) %>% 
  mutate(date= as_datetime(date))


# ABS-SS-RS table ---------------------------------------------------------

# abs_ss_rs2 <- read.csv('BSC/abs_ss_rs.csv') %>% select(-c('X')) %>% tibble()
# abs_ss_rs2 <- abs_ss_rs2 %>% 
#   mutate(ABS1 = case_when(nchar(ABS1) == 1 ~ paste0('00',ABS1),
#                           nchar(ABS1) == 2 ~ paste0('0',ABS1),
#                           nchar(ABS1) == 3 ~ ABS1)) %>% 
#   distinct(ABS1,ABS2,SS1,SS2,RS1,RS2) %>% 
#   filter(ABS1 != 'SES')

abs_ss_rs <- read_csv('BSC/Matriz_propuesta_agrupación_ABS_formatted_forPython.csv') %>% 
  tibble() %>% 
  select(`Código ABS Original`, Nombre) %>% 
  rename(ABS = `Código ABS Original`, ABS_nom = Nombre) %>% 
  mutate(ABS = as.character(ABS),
         ABS = case_when(nchar(ABS) == 1 ~ paste0('00',ABS),
                         nchar(ABS) == 2 ~ paste0('0',ABS),
                         nchar(ABS) == 3 ~ ABS))

BSC_ABS_population <- read_csv('BSC/Population_time_series/abs_population_timeseries.csv')
BSC_ABS_population <- 
  BSC_ABS_population %>% 
  left_join(abs_ss_rs, by = c('id'='ABS')) %>% 
  drop_na() %>% 
  select(-`...1`) %>% 
  rename(ABS = id)

BSC_ABS_population <- 
  BSC_ABS_population %>% 
  left_join(comarques_abs, by = c('ABS' = 'Codi ABS'))

#División de Barcelona entre Barcelona Ciutat y Resto
BSC_ABS_RS_BCN_pop <- BSC_ABS_RS_pop %>% 
  mutate(RS2 = case_when(grepl('BARCELONA', SS2) ~ 'BARCELONA-Ciutat',
                         !grepl('BARCELONA',SS2) ~ RS2)) %>% 
  mutate(RS2 = case_when(RS2 == 'BARCELONA' ~ 'BARCELONA-Resto',
                         RS2 != 'BARCELONA' ~ RS2)) 


# Whole population evolution ----------------------------------------------
BSC_ABS_RS_pop %>%
  group_by(date) %>%
  summarise(population = sum(population)) %>% 
  ggplot(aes(date, population)) +
  geom_line() +
  scale_x_datetime(date_breaks = '2 month',date_labels = '%b')

# Population by RS TS -----------------------------------------------------
#With no division in Barcelona
pop_ts <- BSC_ABS_RS_pop %>%
  group_by(date,RS2) %>%
  summarise(population = sum(population)) %>%
  ggplot(aes(date,population, col=RS2)) +
  geom_line() +
  scale_x_datetime(date_breaks = '1 month',date_labels = '%m-%d') + 
  ggtitle('Population by RS Time Series')
ggplotly(pop_ts)
#Barcelona divided into City and Rest
BSC_ABS_RS_BCN_pop %>%
  group_by(date,RS2) %>%
  summarise(population = sum(population)) %>%
  ggplot(aes(date,population, col=RS2)) +
  geom_line() +
  scale_x_datetime(date_breaks = '1 month',date_labels = '%m-%d') + 
  ggtitle('Population by RS Time Series')





# Population absolute variation by RS  ------------------------------------------
min_date_ref <- as_datetime('2020-02-17')
max_date_ref <- as_datetime('2020-02-23')

mean_pop_rs <- BSC_ABS_RS_pop %>% 
  filter(date >= min_date_ref & 
           date <= max_date_ref) %>% 
  group_by(date, RS2) %>% 
  summarise(population = sum(population)) %>% 
  group_by(RS2) %>% 
  summarise(population = mean(population))

mean_pop_total <- BSC_ABS_RS_pop %>% 
  filter(date >= min_date_ref & date <= max_date_ref) %>% 
  mutate(RS2 = 'Total Catalunya') %>% 
  group_by(RS2, date) %>% 
  summarise(population = sum(population)) %>% 
  group_by(RS2) %>% 
  summarise(population  = mean(population))

total_pop_change <- BSC_ABS_RS_pop %>% 
  group_by(date) %>% 
  summarise(population = sum(population)) %>%
  mutate(RS2 = 'Total Catalunya') %>% 
  left_join(mean_pop_total, by = c('RS2'='RS2')) %>% 
  mutate(pop_abs_change = round(population.x - population.y,2))

pop_abs_change <- BSC_ABS_RS_pop %>% 
  group_by(date,RS2) %>% 
  summarise(population = sum(population)) %>% 
  left_join(mean_pop_rs, by = c('RS2'='RS2')) %>% 
  mutate(pop_abs_change = round(population.x - population.y,2)) %>% 
  bind_rows(total_pop_change) %>% 
  ggplot(aes(date,pop_abs_change, col=RS2)) +
  geom_point(size=0.3) + 
  geom_line() + 
  scale_x_datetime(date_breaks = '1 month',date_labels = '%b-%y') +
  scale_y_continuous(breaks = seq(-800000,400000,50000))+
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(title = 'Population absolute variation time series',
       subtitle = 'Baseline 24/02/20-28/02/20; Barcelona RS divided into Ciutat and Rest') + 
  geom_vline(xintercept = as.numeric(min_date_ref),col='black',linetype = 'dashed',size=0.1) +
  geom_vline(xintercept = as.numeric(max_date_ref),col='black',linetype = 'dashed',size=0.1) 
  
ggplotly(pop_abs_change)

# Population absolute variation by ABS  ------------------------------------------
min_date_ref <- as_datetime('2020-02-17')
max_date_ref <- as_datetime('2020-02-23')

mean_pop_abs <- BSC_ABS_population %>% 
  filter(date >= min_date_ref & 
           date <= max_date_ref) %>% 
  group_by(date, ABS_nom) %>% 
  summarise(population = sum(population)) %>% 
  group_by(ABS_nom) %>% 
  summarise(population = mean(population))

# mean_pop_total <- BSC_ABS_population %>% 
#   filter(date >= min_date_ref & date <= max_date_ref) %>% 
#   mutate(RS2 = 'Total Catalunya') %>% 
#   group_by(RS2, date) %>% 
#   summarise(population = sum(population)) %>% 
#   group_by(RS2) %>% 
#   summarise(population  = mean(population))

# total_pop_change <- BSC_ABS_population %>% 
#   group_by(date) %>% 
#   summarise(population = sum(population)) %>%
#   mutate(RS2 = 'Total Catalunya') %>% 
#   left_join(mean_pop_total, by = c('RS2'='RS2')) %>% 
#   mutate(pop_abs_change = round(population.x - population.y,2))

pop_abs_change <- 
  BSC_ABS_population %>% 
  filter(date >= min_date_ref) %>% 
  group_by(date, ABS_nom) %>% 
  summarise(population = sum(population)) %>% 
  left_join(mean_pop_abs, by = c('ABS_nom'='ABS_nom')) %>% 
  mutate(pop_abs_change = round(population.x - population.y,2)) %>% 
  #bind_rows(total_pop_change) %>% 
  ggplot(aes(date,pop_abs_change, col=ABS_nom)) +
  geom_point(size=0.3) + 
  geom_line() + 
  scale_x_date(labels = scales::date_format('%b-%y'),
               date_breaks = '2 months') +
  #scale_y_continuous(breaks = seq(-800000,400000,50000))+
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(title = 'Population absolute variation time series',
       subtitle = 'Baseline 24/02/20-28/02/20; Barcelona RS divided into Ciutat and Rest') + 
  geom_vline(xintercept = as.numeric(min_date_ref),col='black',linetype = 'dashed',size=0.1) +
  geom_vline(xintercept = as.numeric(max_date_ref),col='black',linetype = 'dashed',size=0.1) 

ggplotly(pop_abs_change)


# Pop abs var by Comarques -----------------------------------------------
comarques_poblacio <- read_csv2('BSC/poblacio_comarques.csv') %>% select(comarca, `2020`)

min_date_ref <- as_datetime('2020-02-17')
max_date_ref <- as_datetime('2020-02-23')

mean_pop_abs <- BSC_ABS_population %>% 
  filter(date >= min_date_ref & 
           date <= max_date_ref) %>% 
  group_by(date, `Nom comarca`) %>% 
  summarise(population = sum(population)) %>% 
  group_by(`Nom comarca`) %>% 
  summarise(population = mean(population))

# mean_pop_total <- BSC_ABS_population %>% 
#   filter(date >= min_date_ref & date <= max_date_ref) %>% 
#   mutate(RS2 = 'Total Catalunya') %>% 
#   group_by(RS2, date) %>% 
#   summarise(population = sum(population)) %>% 
#   group_by(RS2) %>% 
#   summarise(population  = mean(population))

# total_pop_change <- BSC_ABS_population %>% 
#   group_by(date) %>% 
#   summarise(population = sum(population)) %>%
#   mutate(RS2 = 'Total Catalunya') %>% 
#   left_join(mean_pop_total, by = c('RS2'='RS2')) %>% 
#   mutate(pop_abs_change = round(population.x - population.y,2))

pop_abs_change <- 
  BSC_ABS_population %>% 
  filter(date >= min_date_ref) %>% 
  group_by(date, `Nom comarca`) %>% 
  summarise(population = sum(population)) %>% 
  #left_join(mean_pop_abs, by = c('Nom comarca'='Nom comarca')) %>% 
  left_join(comarques_poblacio, by = c('Nom comarca'='comarca')) %>% 
  mutate(pop_abs_change = round(population - `2020`,2)) %>% 
  #bind_rows(total_pop_change) %>% 
  ggplot(aes(date,pop_abs_change, col=`Nom comarca`)) +
  geom_point(size=0.3) + 
  geom_line() + 
  scale_x_date(labels = scales::date_format('%b-%y'),
               date_breaks = '2 months') +
  #scale_y_continuous(breaks = seq(-800000,400000,50000))+
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(title = 'Population absolute variation time series',
       subtitle = 'Baseline 24/02/20-28/02/20; Barcelona RS divided into Ciutat and Rest') + 
  geom_vline(xintercept = as.numeric(min_date_ref),col='black',linetype = 'dashed',size=0.1) +
  geom_vline(xintercept = as.numeric(max_date_ref),col='black',linetype = 'dashed',size=0.1) 

ggplotly(pop_abs_change)

# Population absolute variation by SS ------------------------------------------
min_date_ref <- as_datetime('2020-02-24')
max_date_ref <- as_datetime('2020-02-28')

mean_pop_rs <- BSC_ABS_RS_pop %>% 
  filter(date >= min_date_ref & 
           date <= max_date_ref) %>% 
  group_by(date, SS2) %>% 
  summarise(population = sum(population)) %>% 
  group_by(SS2) %>% 
  summarise(population = mean(population))

mean_pop_total <- BSC_ABS_RS_pop %>% 
  filter(date >= min_date_ref & date <= max_date_ref) %>% 
  mutate(SS2 = 'Total Catalunya') %>% 
  group_by(SS2, date) %>% 
  summarise(population = sum(population)) %>% 
  group_by(SS2) %>% 
  summarise(population  = mean(population))

total_pop_change <- BSC_ABS_RS_pop %>% 
  group_by(date) %>% 
  summarise(population = sum(population)) %>%
  mutate(SS2 = 'Total Catalunya') %>% 
  left_join(mean_pop_total, by = c('SS2'='SS2')) %>% 
  mutate(pop_abs_change = round(population.x - population.y,2))

pop_abs_change <- BSC_ABS_RS_pop %>% 
  group_by(date,SS2) %>% 
  summarise(population = sum(population)) %>% 
  left_join(mean_pop_rs, by = c('SS2'='SS2')) %>% 
  mutate(pop_abs_change = round(population.x - population.y,2)) %>% 
  bind_rows(total_pop_change) %>% 
  ggplot(aes(date,pop_abs_change, col=SS2)) +
  geom_point(size=0.3) + 
  geom_line() + 
  scale_x_datetime(date_breaks = '1 month',date_labels = '%b-%y') +
  scale_y_continuous(breaks = seq(-800000,400000,50000))+
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(title = 'Population absolute variation by SS time series') + 
  geom_vline(xintercept = as.numeric(min_date_ref),col='black',linetype = 'dashed',size=0.1) +
  geom_vline(xintercept = as.numeric(max_date_ref),col='black',linetype = 'dashed',size=0.1) 

ggplotly(pop_abs_change)

# Population abs var with BCN by RS  ------------------------------------------
min_date_ref <- as_datetime('2020-02-24')
max_date_ref <- as_datetime('2020-02-28')

mean_pop_rs <- BSC_ABS_RS_BCN_pop %>% 
  filter(date >= min_date_ref & date <= max_date_ref) %>% 
  group_by(date, RS2) %>% 
  summarise(population = sum(population)) %>% 
  group_by(RS2) %>% 
  summarise(population = mean(population))

mean_pop_total <- BSC_ABS_RS_BCN_pop %>% 
  filter(date >= min_date_ref & date <= max_date_ref) %>% 
  mutate(RS2 = 'Total Catalunya') %>% 
  group_by(RS2, date) %>% 
  summarise(population = sum(population)) %>% 
  group_by(RS2) %>% 
  summarise(population  = mean(population))

total_pop_change <- BSC_ABS_RS_BCN_pop %>% 
  group_by(date) %>% 
  summarise(population = sum(population)) %>%
  mutate(RS2 = 'Total Catalunya') %>% 
  left_join(mean_pop_total, by = c('RS2'='RS2')) %>% 
  mutate(pop_abs_change = round(population.x - population.y,2))

pop_abs_change <- BSC_ABS_RS_BCN_pop %>% 
  group_by(date,RS2) %>% 
  summarise(population = sum(population)) %>% 
  left_join(mean_pop_rs, by = c('RS2'='RS2')) %>% 
  mutate(pop_abs_change = round(population.x - population.y,2)) %>% 
  bind_rows(total_pop_change) %>% 
  ggplot(aes(date,pop_abs_change, col=RS2)) +
  #geom_point() + 
  geom_line() + 
  scale_x_datetime(date_breaks = '1 month',date_labels = '%b-%y') +
  scale_y_continuous(breaks = seq(-800000,400000,100000))+
  #geom_hline(yintercept=0, size=0.3, linetype='dashed') +
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(title = 'Population absolute variation time series',
       caption = 'Baseline in period: Monday 24/02 to Firday 28/02; Barcelona RS divided into Ciutat and Rest')

l <- ggplotly(pop_abs_change)
htmlwidgets::saveWidget(l, "summer_population_variation.html")
# pop_perc_change <- BSC_ABS_population %>% 
#   group_by(date,RS2) %>% 
#   summarise(population = sum(population)) %>% 
#   left_join(mean_pop_rs, by = c('RS2'='RS2')) %>% 
#   mutate(pop_perc_change = round(((population.x - population.y)/population.y)*100,2)) %>% 
#   ggplot(aes(date,pop_perc_change, col=RS2)) +
#   #geom_point() + 
#   geom_line() + 
#   scale_x_datetime(date_breaks = '1 month',date_labels = '%m-%d') + 
#   geom_hline(yintercept=0)
# 
# ggplotly(pop_perc_change)



# Pop var with PADRIS pop data --------------------------------------------
padris_pop <- read_csv('BSC/Population_validation/population_ABS_Padris.csv') %>% 
  tibble() %>% 
  select(ABS1, NUM_INDIV) %>% 
  inner_join(abs_ss_rs, by = c('ABS1'='ABS1')) %>% 
  group_by(RS2) %>% 
  summarise(population = sum(NUM_INDIV))

pop_abs_change <- BSC_ABS_RS_pop %>% 
  group_by(date,RS2) %>% 
  summarise(population = sum(population)) %>% 
  left_join(padris_pop, by = c('RS2'='RS2')) %>% 
  mutate(pop_abs_change = round(population.x - population.y,2)) %>% 
  ggplot(aes(date,pop_abs_change, col=RS2)) +
  #geom_point() + 
  geom_line() + 
  scale_x_datetime(date_breaks = '1 month',date_labels = '%b-%y') + 
  #geom_hline(yintercept=0, size=0.3, linetype='dashed') +
  theme(axis.text.x = element_text(angle = 45))

ggplotly(pop_abs_change)



BSC_ABS_population %>%
  group_by(date) %>%
  mutate(date=as.Date(date)) %>%
  summarize(total_population = sum(population)) %>%
  ungroup() %>%
  fwrite(.,'BSC/Population_time_series/Population_Whole_Catalonia_time_series.csv')


population_timeseries <- read.csv('BSC/Population_time_series/whole_population_time_series.csv') %>% 
  as_tibble() %>%
  mutate(date=as.Date(date))



