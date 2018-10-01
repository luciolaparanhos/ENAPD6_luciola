library(tidyverse)
library(magrittr)
library(lubridate)
lista.de.pacotes = c("tidyverse","lubridate") # escreva a lista de pacotes
novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in%
                                      installed.packages()[,"Package"])]
if(length(novos.pacotes) > 0) {install.packages(novos.pacotes)}
lapply(lista.de.pacotes, require, character.only=T)
rm(lista.de.pacotes,novos.pacotes)
gc()


read_rds("C:/Users/aluno/Documents/ENAPD6_luciola/decisoes.rds")
decisoes <- read_rds("C:/Users/aluno/Documents/ENAPD6_luciola/decisoes.rds")

juizes_drogas_CL <- decisoes %>%
  select(juiz,municipio,txt_decisao,data_registro, data_decisao) %>%
  mutate(txt_decisao = tolower(txt_decisao),
         droga = str_detect(txt_decisao,"droga|entorpecente"),
         tempo = dmy(data_registro) - dmy(data_decisao)) %>%
           filter(droga==T, municipio %in% c("Campinas","Limeira")) %>%
           group_by(juiz)%>%
           summarise(tempo_medio = mean(tempo, na.rm=T))
juizes_drogas_CL         
write_rds(juizes_drogas_CL,"C:/Users/aluno/Documents/ENAPD6_luciola/juizes_drogas_CL.rds")

decisoes_juiz <- decisoes %>%
  #select(juiz, n_processo_drogas,n_processosn_drogas,total_processos)
  filter(is.na(txt_deecisao))%>%
  mutate(txt_decisao = tolower(txt_decisao),
         droga = str_detect(txt_decisao,"droga|entorpecente"),
         droga= case_when(
           droga==TRUE~"droga"
           droga==FALSE~"N_droga"
         ))%>%
group_by(juiz, droga)%>%
  summarise(qtd_decisoes=n(),)
muate(total=droga+n_droga,
      proporcao=droga/total)%>%
  arrange(-proporcao)

decisoes_juiz
