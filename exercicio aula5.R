#carregando pacotes

lista.de.pacotes = c("tidyverse","lubridate","janitor","readxl","stringr","repmis") # escreva a lista de pacotes
novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in%
                                      installed.packages()[,"Package"])]
if(length(novos.pacotes) > 0) {install.packages(novos.pacotes)}
lapply(lista.de.pacotes, require, character.only=T)
rm(lista.de.pacotes,novos.pacotes)
gc()

# carregando arquivo decisoes.rds
decisoes <- read_rds("C:/Users/Luciola/Documents/Pos ENAP ADPP/D6 Análise dados secundários/ENAPD6_luciola/decisoes.rds")

#cria objeto com total mensal de decisoes por juiz
juiz_mes <- decisoes %>%
  filter(!is.na(id_decisao))%>%
  mutate(mes=month(dmy(data_decisao)))%>%
  group_by(juiz,mes)%>%
  summarise(n_decisoes = n())%>%
  spread(mes,n_decisoes,fill=0) 


# Crie um objeto contendo informações sobre os tamanhos das bancadas dos ----
# partidos (arquivo `bancadas.rds`), suas respectivas coligações 
# eleitorais para 2018 (arquivo `coligacoes.xlsx`) e o 
# grau de concordância com a agenda do Gov 
# Temer (arquivo `governismo_temer.xlsx`). 

# carregando arquivos bancadas.rds, coligacoes.xlsx e goverismo_temer.xlsx
bancadas <- read_rds("C:/Users/Luciola/Documents/Pos ENAP ADPP/D6 Análise dados secundários/ENAPD6_luciola/bancadas.rds")
coligacoes <- read_xlsx("C:/Users/Luciola/Documents/Pos ENAP ADPP/D6 Análise dados secundários/ENAPD6_luciola/coligacoes.xlsx")
governismo <- read_xlsx("C:/Users/Luciola/Documents/Pos ENAP ADPP/D6 Análise dados secundários/ENAPD6_luciola/governismo_temer.xlsx")

bancadas_2 <- bancadas%>%
  left_join(coligacoes,"party")%>%
  left_join(governismo,"party")

# Bônus: use `group_by` e `summarise` para identificar  ----
# qual candidato tem a coligação com menor média de concordância e  
# qual candidato tem a maior proporção total de assentos.

bancadas_2 %>%
  unite(candidato_partido,president,party,sep = "_", remove = F)
  #summarise(prop.table(size))

