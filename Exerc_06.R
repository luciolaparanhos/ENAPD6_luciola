# Exercícios aula 05
lista.de.pacotes = c("tidyverse","lubridate","janitor","readxl","stringr","repmis","janitor") # escreva a lista de pacotes
novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in%
                                      installed.packages()[,"Package"])]
if(length(novos.pacotes) > 0) {install.packages(novos.pacotes)}
lapply(lista.de.pacotes, require, character.only=T)
rm(lista.de.pacotes,novos.pacotes)
gc()



# inner join
inner_decisoes <- decisoes %>% 
  filter(data_registro == "18/01/2018", !is.na(id_decisao)) %>% 
  select(id_decisao, n_processo)  %>%
  inner_join(
    processos %>% 
      dplyr::select(n_processo,partes))


# Right join
right_decisoes <- decisoes %>% 
  filter(data_registro == "18/01/2018", !is.na(id_decisao)) %>% 
  select(id_decisao, n_processo) %>% 
  right_join(processos %>% dplyr::select(n_processo,partes))


decisoes_selecao <- 

processos_selecao <- 

right_decisoes <- decisoes %>% 
  filter(data_registro == "18/01/2018", !is.na(id_decisao)) %>% 
  select(id_decisao, n_processo) %>%
  right_join(processos %>%  
               dplyr::select(n_processo,partes))

# Crie um objeto contendo informações sobre os tamanhos das bancadas dos ----
# partidos (arquivo `bancadas.rds`), suas respectivas coligações 
# eleitorais para 2018 (arquivo `coligacoes.xlsx`) e o 
# grau de concordância com a agenda do Gov 
# Temer (arquivo `governismo_temer.xlsx`). 
setwd("C:/Users/Aluno/Desktop/git/aulas_ENAP/CADS2018/Exercícios/dados")

#lendo arquivo bancadas
bancadas <- read_rds("bancadas.rds") 

coligacoes <- read_xlsx("coligacoes.xlsx")

governismo <- read_xlsx("governismo_temer.xlsx")


bancadas_coligacoes_governismo <- bancadas %>%
  left_join(coligacoes) %>%
  left_join(governismo)



# Bônus: use `group_by` e `summarise` para identificar qual candidato tem a ----
# coligação com menor média de concordância e qual candidato 
# tem a maior proporção total de assentos.

bancadas2 <- bancadas_coligacoes_governismo %>%
  unite(pres_ppres, #nome da coluna criada
        president,partypresid, #colunas que estou unindo
        sep=" - ",
        remove=F)

governismo_coligacao <- bancadas2 %>%
  mutate(prop=size/sum(size)) %>%
  group_by(president,partypresid) %>%
  summarise(prop_total=sum(prop,na.rm = T),
            apoio = mean(governismo,na.rm=T) )
    
governismo_coligacao2 <- bancadas2 %>%
  group_by(president,partypresid) %>%
  summarise(size_total=sum(size,na.rm = T),
            apoio = governismo*size/sum(size,na.rm=T)) %>%
  mutate(prop=prop.table(size_total)) 


# Create a data.frame with dirty names
test_df <- as.data.frame(matrix(ncol = 6))
names(test_df) <- c("firstName", "ábc@!*", "% successful (2009)",
                    "REPEAT VALUE", "REPEAT VALUE", "")

test_df %>%
  clean_names()

mtcars %>%
  tabyl(gear, cyl) %>%
  adorn_totals("col") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() %>%
  adorn_title()

