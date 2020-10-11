# Primeiro, devem ser realizados os procedimentos básicos de carregamento 
# do que será utilizado:

setwd("C:\\Users\\Renato\\Desktop\\Mestrado - 2020.1\\SEMESTRE I\\Análise de Dados\\ARTIGO FINAL\\DADOS\\PRESTAÇÃO DE CONTAS - CANDIDATOS\\")

library(tidyverse)
library(readxl)
library(dplyr)
library(poliscidata)
library(vcd)
library(graphics)
library(scales)
library(dotwhisker)
library(pscl)
library(sjPlot)
library(margins)
library(InformationValue)

# Como o TSE não disponibiliza links baixáveis para a importação direta
# pelo RStudio, disponibilizaremo os links de acesso ao arquivo para 
# a realização do download dos bancos necessários:

link <- "https://raw.githubusercontent.com/lirarenato22/Analise_de_dados_Trabalho_Final/main/consulta_cand_2018_BRASIL.csv"

link_2 <- "https://www.dropbox.com/s/u2ujij8bd3saur3/despesas_contratadas_candidatos_2018_BRASIL.csv?raw=1"

download.file(link, "consulta_cand_2018_BRASIL.csv", mode = "wb")

# Após isso, será necessário fazer o download do banco de dados 
# "consulta_cand_2018_BRASIL", do qual utilizaremos variáveis de 
# características dos candidatos.

BANCO_CONSULTA_CANDIDATOS <- read.csv2("consulta_cand_2018_BRASIL.csv")

# A partir do banco inicial "consulta_cand_2018_BRASIL.csv", selecionamos
# as variáveis CD_CARGO, NR_TURNO, SG_UF, SQ_CANDIDATO, CD_DETALHE_SITUACAO_CAND, 
# DS_COR_RACA, ST_REELEICAO, DS_GENERO, TP_AGREMIACAO, SG_PARTIDO,
# NM_PARTIDO, NM_CANDIDATO e NR_IDADE_DATA_POSSE:

BANCO_CONSULTA_CANDIDATOS_FILTRADO <- BANCO_CONSULTA_CANDIDATOS %>%
  select(CD_CARGO, NR_TURNO, SG_UF, SQ_CANDIDATO, CD_DETALHE_SITUACAO_CAND, 
         DS_COR_RACA, ST_REELEICAO, DS_GENERO, TP_AGREMIACAO, SG_PARTIDO,
         NM_PARTIDO, NM_CANDIDATO, NR_IDADE_DATA_POSSE, DS_SIT_TOT_TURNO) %>%
    filter(NR_TURNO == 1, CD_CARGO %in% c('6', '7'), 
           CD_DETALHE_SITUACAO_CAND %in% c('2', '16'))

# O segundo banco a ser baixado é o 
# "despesas_contratadas_candidatos_2018_BRASIL", do qual utilizaremos as
# variáveis "SQ_CANDIDATO", "SQ_PRESTADOR_CONTAS", "SG_UF", VR_DESPESA_CONTRATADA,
# e "DS_DESPESA" para podermos 'unificar' os bancos.

download.file(link_2, "despesas_contratadas_candidatos_2018_BRASIL.csv", mode = "wb")

BANCO_DESPESAS_CONTRATADAS <- 
  read.csv2("despesas_contratadas_candidatos_2018_BRASIL.csv")

# Como há várias observações por número sequencial, será necessário unificar
# as observações para cada número sequencial.

BANCO_DESPESAS_CONTRATADAS_POR_SEQUENCIAL <- BANCO_DESPESAS_CONTRATADAS %>%
  select(SG_UF, DS_DESPESA, SQ_PRESTADOR_CONTAS, SQ_CANDIDATO, VR_DESPESA_CONTRATADA) %>%
  group_by(SQ_PRESTADOR_CONTAS, SG_UF, SQ_CANDIDATO) %>%
  summarise(sum(VR_DESPESA_CONTRATADA))

# A partir disso, tentaremos conseguir as mesmas informações sobre
# impulsionamento de conteúdo a partir desse novo banco.

BANCO_DESPESAS_CONTRATADAS_IMPULSIONAMENTO <- BANCO_DESPESAS_CONTRATADAS %>%
  select(SG_UF, DS_ORIGEM_DESPESA, SQ_PRESTADOR_CONTAS, SQ_CANDIDATO, 
         VR_DESPESA_CONTRATADA) %>%
  filter(DS_ORIGEM_DESPESA %in% c('Despesa com Impulsionamento de Conteúdos'))

# Como o banco oferece várias informações sobre impulsionamento para
# cada número sequencial, precisaremos agrupar os valores por número 
# sequencial.

BANCO_DESPESAS_CONTRATADAS_IMPULSIONAMENTO_SOMADO <- BANCO_DESPESAS_CONTRATADAS_IMPULSIONAMENTO %>%
  group_by(SQ_PRESTADOR_CONTAS, SG_UF, DS_ORIGEM_DESPESA, SQ_CANDIDATO) %>%
  summarise(sum(VR_DESPESA_CONTRATADA))

# Depois disso, unificaremos os bancos 
# "BANCO_DESPESAS_CONTRATADAS_POR_SEQUENCIAL" e
# "BANCO_DESPESAS_CONTRATADAS_IMPULSIONAMENTO_SOMADO" com a função "merge".

BANCO_UNIFICADO <- merge(BANCO_DESPESAS_CONTRATADAS_POR_SEQUENCIAL,
                         BANCO_DESPESAS_CONTRATADAS_IMPULSIONAMENTO_SOMADO,
                         by="SQ_CANDIDATO")

# A partir do primeiro banco unificado, criaremos o "BANCO_UNIFICADO_2",
# agregando o banco "BANCO_CONSULTA_CANDIDATOS_FILTRADO" com a função "merge".

BANCO_UNIFICADO_2 <- merge(BANCO_UNIFICADO,
                           BANCO_CONSULTA_CANDIDATOS_FILTRADO,
                           by="SQ_CANDIDATO")

# Com base no BANCO_UNIFICADO_2, recodificaremos a variável "DS_SIT_TOT_TURNO",
# para podermos obter valores categorizáveis de forma dummy (ELEITO/NÃO ELEITO).


BANCO_UNIFICADO_2_RECODIFICADO <- BANCO_UNIFICADO_2 %>%
  mutate(DS_SIT_TOT_TURNO  = recode(DS_SIT_TOT_TURNO,
                            "SUPLENTE" = "0",
                            "NÃO ELEITO" = "0",
                            "ELEITO POR QP" = "1",
                            "ELEITO POR MÉDIA" = "1"))

# É necessário também renomear as variáveis "sum(VR_DESPESA_CONTRATADA).x" e
# "sum(VR_DESPESA_CONTRATADA).y" para "Despesa_Total" e 
# "Despesa_Impulsionamento", respectivamente.

BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO <- BANCO_UNIFICADO_2_RECODIFICADO %>%
  rename(DESPESA_TOTAL= "sum(VR_DESPESA_CONTRATADA).x") %>%
  rename(DESPESA_IMPULSIONAMENTO= "sum(VR_DESPESA_CONTRATADA).y")

# A partir do "BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO", começaremos a análise 
# exploratória das variáveis.

# "DS_COR_RACA"

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DS_COR_RACA)) +
  geom_bar()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(fct_infreq(DS_COR_RACA))) +
  geom_bar()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(DS_COR_RACA, ..count../sum(..count..) )) +
  geom_bar(na.rm = T) +
  scale_y_continuous(labels = percent)

# "DS_GENERO"

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DS_GENERO)) +
  geom_bar()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(fct_infreq(DS_GENERO))) +
  geom_bar()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(DS_GENERO, ..count../sum(..count..) )) +
  geom_bar(na.rm = T) +
  scale_y_continuous(labels = percent)

# "ST_REELEICAO"

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(ST_REELEICAO)) +
  geom_bar()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(fct_infreq(ST_REELEICAO))) +
  geom_bar()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(ST_REELEICAO, ..count../sum(..count..) )) +
  geom_bar(na.rm = T) +
  scale_y_continuous(labels = percent)

# "NR_IDADE_DATA_POSSE"

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(NR_IDADE_DATA_POSSE)) +
  geom_histogram()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(NR_IDADE_DATA_POSSE)) +
  geom_density()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(x = "", y = NR_IDADE_DATA_POSSE)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) 

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(y = NR_IDADE_DATA_POSSE)) + 
  geom_boxplot()


# "DESPESA_TOTAL"

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DESPESA_TOTAL)) +
  geom_histogram(bins = 60)

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DESPESA_TOTAL)) +
  geom_density(adjust = 15)

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(x = "", y = DESPESA_TOTAL)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) 

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(y = DESPESA_TOTAL)) + 
  geom_boxplot()


# "DESPESA_IMPULSIONAMENTO"

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(DESPESA_IMPULSIONAMENTO)) +
  geom_histogram(bins = 60)

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(DESPESA_IMPULSIONAMENTO)) +
  geom_density(adjust = 50)

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(x = "", y = DESPESA_IMPULSIONAMENTO)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) 

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(y = DESPESA_IMPULSIONAMENTO)) + 
  geom_boxplot()


# "TP_AGREMIACAO"

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(TP_AGREMIACAO)) +
  geom_bar()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(fct_infreq(TP_AGREMIACAO))) +
  geom_bar()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(TP_AGREMIACAO, ..count../sum(..count..) )) +
  geom_bar(na.rm = T) +
  scale_y_continuous(labels = percent)

# "DS_SIT_TOT_TURNO"


ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DS_SIT_TOT_TURNO)) +
  geom_bar()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(fct_infreq(TP_AGREMIACAO))) +
  geom_bar()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(DS_SIT_TOT_TURNO, ..count../sum(..count..) )) +
  geom_bar(na.rm = T) +
  scale_y_continuous(labels = percent)


# A partir do "BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO", começaremos a análise 
# exploratória entre as variáveis.

# "DS_COR_RACA" e "DS_SIT_TOT_TURNO"

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DS_COR_RACA, fill = DS_SIT_TOT_TURNO)) +
  geom_bar(position = "fill")

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(x = DS_COR_RACA, y = DS_SIT_TOT_TURNO)) + 
  geom_count(aes(group = DS_COR_RACA, size = after_stat(prop))) +
  scale_size_area(max_size = 15)

# "DS_GENERO" e "DS_SIT_TOT_TURNO":

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DS_GENERO, fill = DS_SIT_TOT_TURNO)) +
  geom_bar(position = "fill")

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(x = DS_GENERO, y = DS_SIT_TOT_TURNO)) + 
  geom_count(aes(group = DS_GENERO, size = after_stat(prop))) +
  scale_size_area(max_size = 10)

# "ST_REELEICAO" e "DS_SIT_TOT_TURNO":

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(ST_REELEICAO, fill = DS_SIT_TOT_TURNO)) +
  geom_bar(position = "fill")

# "TP_AGREMIACAO" e "DS_SIT_TOT_TURNO":

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(TP_AGREMIACAO, fill = DS_SIT_TOT_TURNO)) +
  geom_bar(position = "fill")

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(x = TP_AGREMIACAO, y = DS_SIT_TOT_TURNO)) + 
  geom_count(aes(group = TP_AGREMIACAO, size = after_stat(prop))) +
  scale_size_area(max_size = 10)

# "NR_IDADE_DATA_POSSE" e "DS_SIT_TOT_TURNO":

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(NR_IDADE_DATA_POSSE, 
                                           fill = DS_SIT_TOT_TURNO)) +
  geom_density(alpha = 1)

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DS_SIT_TOT_TURNO, 
                                           NR_IDADE_DATA_POSSE)) +
  geom_boxplot()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DS_SIT_TOT_TURNO, 
                                           NR_IDADE_DATA_POSSE)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) 

# "DESPESA_TOTAL" e "DS_SIT_TOT_TURNO":

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DESPESA_TOTAL, 
                                           fill = DS_SIT_TOT_TURNO)) +
  geom_density(alpha = 0.5)

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DS_SIT_TOT_TURNO, 
                                                     DESPESA_TOTAL)) +
  geom_boxplot()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DS_SIT_TOT_TURNO, 
                                                     DESPESA_TOTAL)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) 

# "DESPESA_IMPULSIONAMENTO" e "DS_SIT_TOT_TURNO":

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DESPESA_IMPULSIONAMENTO, 
                                                     fill = DS_SIT_TOT_TURNO)) +
  geom_density(alpha = 0.5)

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DS_SIT_TOT_TURNO, 
                                                     DESPESA_IMPULSIONAMENTO)) +
  geom_boxplot()

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, aes(DS_SIT_TOT_TURNO, 
                                                     DESPESA_IMPULSIONAMENTO)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) 

# "DESPESA_TOTAL" e "DESPESA_IMPULSIONAMENTO":

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(DESPESA_IMPULSIONAMENTO, DESPESA_TOTAL)) +
  geom_point(alpha = 1)

ggplot(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO, 
       aes(DESPESA_IMPULSIONAMENTO, DESPESA_TOTAL)) +
  geom_jitter()

# A partir do "BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO", realizaremos o 
# Teste T entre as variáveis "DESPESA_IMPULSIONAMENTO" e "DS_SIT_TOT_TURNO".

t.test(DESPESA_IMPULSIONAMENTO ~ DS_SIT_TOT_TURNO, 
       data = BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO)

# A partir do "BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO", realizaremos o 
# Teste T entre as variáveis "DESPESA_TOTAL" e "DS_SIT_TOT_TURNO".

t.test(DESPESA_TOTAL ~ DS_SIT_TOT_TURNO, 
       data = BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO)

# A partir do "BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO", realizaremos o 
# Teste r de Pearson entre as variáveis "DESPESA_TOTAL" e 
# "DESPESA_IMPULSIONAMENTO", para observar se há uma possível correlação
# entre essas variáveis.

cor.test(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO$DESPESA_IMPULSIONAMENTO, 
         BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO$DESPESA_TOTAL)

# A partir do "BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO", realizaremos o 
# o teste de ANOVA (ou teste de Kruskal) entre as variáveis
# "DS_COR_RACA" e "DS_SIT_TOT_TURNO".

kruskal.test(DS_SIT_TOT_TURNO ~ DS_COR_RACA, 
             data = BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO)

# A partir do "BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO", realizaremos o 
# Teste de Chi-Quadrado entre as variáveis "DS_GENERO" e "DS_SIT_TOT_TURNO".

TABELA_DS_GENERO_E_DS_SIT_TOT_TURNO <- 
  table(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO$DS_GENERO, 
        BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO$DS_SIT_TOT_TURNO)

prop.table(TABELA_DS_GENERO_E_DS_SIT_TOT_TURNO)

chisq.test(TABELA_DS_GENERO_E_DS_SIT_TOT_TURNO)

assoc(TABELA_DS_GENERO_E_DS_SIT_TOT_TURNO, shade = TRUE)

mosaicplot(TABELA_DS_GENERO_E_DS_SIT_TOT_TURNO, shade = TRUE)

# A partir do "BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO", realizaremos o 
# Teste de Chi-Quadrado entre as variáveis "ST_REELEICAO" e "DS_SIT_TOT_TURNO".

TABELA_ST_REELEICAO_E_DS_SIT_TOT_TURNO <- 
  table(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO$ST_REELEICAO, 
        BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO$DS_SIT_TOT_TURNO)

prop.table(TABELA_ST_REELEICAO_E_DS_SIT_TOT_TURNO)

chisq.test(TABELA_ST_REELEICAO_E_DS_SIT_TOT_TURNO)

assoc(TABELA_ST_REELEICAO_E_DS_SIT_TOT_TURNO, shade = TRUE)

mosaicplot(TABELA_ST_REELEICAO_E_DS_SIT_TOT_TURNO, shade = TRUE)

# A partir do "BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO", realizaremos o 
# Teste de Chi-Quadrado entre as variáveis "TP_AGREMIACAO" e "DS_SIT_TOT_TURNO".

TABELA_TP_AGREMIACAO_E_DS_SIT_TOT_TURNO <- 
  table(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO$TP_AGREMIACAO, 
        BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO$DS_SIT_TOT_TURNO)

prop.table(TABELA_TP_AGREMIACAO_E_DS_SIT_TOT_TURNO)

chisq.test(TABELA_TP_AGREMIACAO_E_DS_SIT_TOT_TURNO)

assoc(TABELA_TP_AGREMIACAO_E_DS_SIT_TOT_TURNO, shade = TRUE)

mosaicplot(TABELA_TP_AGREMIACAO_E_DS_SIT_TOT_TURNO, shade = TRUE)

# A partir do "BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO", realizaremos o 
# Teste T entre as variáveis "NR_IDADE_DATA_POSSE" e "DS_SIT_TOT_TURNO".

t.test(NR_IDADE_DATA_POSSE ~ DS_SIT_TOT_TURNO, 
       data = BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO)

# A partir do "BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG", que
# criaremos agora, começaremos agora a 
# utilizar regressões logísticas entre as variáveis de interesse.

BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG <- 
  BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO %>%
  mutate(DS_SIT_TOT_TURNO  = recode(DS_SIT_TOT_TURNO,
                                    "0" = 0,
                                    "0" = 0,
                                    "1" = 1,
                                    "1" = 1))

# "DS_COR_RACA" e "DS_SIT_TOT_TURNO"

REGRESSAO_LOG_DS_COR_RACA_DS_SIT_TOT_TURNO <- 
  glm(DS_SIT_TOT_TURNO ~ DS_COR_RACA, 
                           data = 
        BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG, 
      family = "binomial")

summary(REGRESSAO_LOG_DS_COR_RACA_DS_SIT_TOT_TURNO) 

confint(REGRESSAO_LOG_DS_COR_RACA_DS_SIT_TOT_TURNO) 

pR2(REGRESSAO_LOG_DS_COR_RACA_DS_SIT_TOT_TURNO)

# "DS_GENERO" e "DS_SIT_TOT_TURNO"

REGRESSAO_LOG_DS_GENERO_DS_SIT_TOT_TURNO <- 
  glm(DS_SIT_TOT_TURNO ~ DS_GENERO, 
      data = BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG, 
      family = "binomial")

summary(REGRESSAO_LOG_DS_GENERO_DS_SIT_TOT_TURNO) 

confint(REGRESSAO_LOG_DS_GENERO_DS_SIT_TOT_TURNO) 

pR2(REGRESSAO_LOG_DS_GENERO_DS_SIT_TOT_TURNO)
 
# "ST_REELEICAO" e "DS_SIT_TOT_TURNO"

REGRESSAO_LOG_ST_REELEICAO_DS_SIT_TOT_TURNO <- 
  glm(DS_SIT_TOT_TURNO ~ ST_REELEICAO, 
      data = BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG, 
      family = "binomial")

summary(REGRESSAO_LOG_ST_REELEICAO_DS_SIT_TOT_TURNO) 

confint(REGRESSAO_LOG_ST_REELEICAO_DS_SIT_TOT_TURNO) 

pR2(REGRESSAO_LOG_ST_REELEICAO_DS_SIT_TOT_TURNO)

# "TP_AGREMIACAO" e "DS_SIT_TOT_TURNO"

REGRESSAO_LOG_TP_AGREMIACAO_DS_SIT_TOT_TURNO <- 
  glm(DS_SIT_TOT_TURNO ~ TP_AGREMIACAO, 
      data = BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG, 
      family = "binomial")

summary(REGRESSAO_LOG_TP_AGREMIACAO_DS_SIT_TOT_TURNO) 

confint(REGRESSAO_LOG_TP_AGREMIACAO_DS_SIT_TOT_TURNO) 

pR2(REGRESSAO_LOG_TP_AGREMIACAO_DS_SIT_TOT_TURNO)

# "NR_IDADE_DATA_POSSE" e "DS_SIT_TOT_TURNO"

REGRESSAO_LOG_NR_IDADE_DATA_POSSE_DS_SIT_TOT_TURNO <- 
  glm(DS_SIT_TOT_TURNO ~ NR_IDADE_DATA_POSSE, 
      data = BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG, 
      family = "binomial")

summary(REGRESSAO_LOG_NR_IDADE_DATA_POSSE_DS_SIT_TOT_TURNO) 

confint(REGRESSAO_LOG_NR_IDADE_DATA_POSSE_DS_SIT_TOT_TURNO) 

pR2(REGRESSAO_LOG_NR_IDADE_DATA_POSSE_DS_SIT_TOT_TURNO)

# "DESPESA_TOTAL" e "DS_SIT_TOT_TURNO"

REGRESSAO_LOG_DESPESA_TOTAL_DS_SIT_TOT_TURNO <- 
  glm(DS_SIT_TOT_TURNO ~ DESPESA_TOTAL, 
      data = BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG, 
      family = "binomial")

summary(REGRESSAO_LOG_DESPESA_TOTAL_DS_SIT_TOT_TURNO) 

confint(REGRESSAO_LOG_DESPESA_TOTAL_DS_SIT_TOT_TURNO) 

pR2(REGRESSAO_LOG_DESPESA_TOTAL_DS_SIT_TOT_TURNO)

# "DESPESA_IMPULSIONAMENTO" e "DS_SIT_TOT_TURNO"

REGRESSAO_LOG_DESPESA_IMPULSIONAMENTO_DS_SIT_TOT_TURNO <- 
  glm(DS_SIT_TOT_TURNO ~ DESPESA_IMPULSIONAMENTO, 
      data = BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG, 
      family = "binomial")

summary(REGRESSAO_LOG_DESPESA_IMPULSIONAMENTO_DS_SIT_TOT_TURNO) 

confint(REGRESSAO_LOG_DESPESA_IMPULSIONAMENTO_DS_SIT_TOT_TURNO) 

pR2(REGRESSAO_LOG_DESPESA_IMPULSIONAMENTO_DS_SIT_TOT_TURNO)

# "DS_COR_RACA" + "DS_GENERO" + "ST_REELEICAO" + "TP_AGREMIACAO" + 
# "NR_IDADE_DATA_POSSE" + "DESPESA_TOTAL" + "DESPESA_IMPULSIONAMENTO" e 
# "DS_SIT_TOT_TURNO".

REGRESSAO_LOG_MULTIVARIADA_DS_SIT_TOT_TURNO <- 
  glm(DS_SIT_TOT_TURNO ~ DS_COR_RACA + DS_GENERO + ST_REELEICAO + TP_AGREMIACAO
      + NR_IDADE_DATA_POSSE + DESPESA_TOTAL + DESPESA_IMPULSIONAMENTO + 
        DESPESA_IMPULSIONAMENTO:ST_REELEICAO + DESPESA_IMPULSIONAMENTO:DESPESA_TOTAL, 
      data = BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG, 
      family = "binomial")

summary(REGRESSAO_LOG_MULTIVARIADA_DS_SIT_TOT_TURNO) 

confint(REGRESSAO_LOG_MULTIVARIADA_DS_SIT_TOT_TURNO) 

pR2(REGRESSAO_LOG_MULTIVARIADA_DS_SIT_TOT_TURNO)

# Para dar continuidade, transformaremos os coeficientes estimados 
# em probabilidade:

margins(REGRESSAO_LOG_MULTIVARIADA_DS_SIT_TOT_TURNO)

summary(margins(REGRESSAO_LOG_MULTIVARIADA_DS_SIT_TOT_TURNO))

# Verificaremos, então, as classificações corretas do modelo em relação 
# ao que poderia ser considerado por um modelo ingênuo:



predicted_prob <- predict(REGRESSAO_LOG_MULTIVARIADA_DS_SIT_TOT_TURNO, 
                          type = "response")


1 - misClassError(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG$DS_SIT_TOT_TURNO, 
                  predicted_prob, 
                  threshold = 0.5)

# A partir desses comandos, observamos as previsões corretas. 
# Corrigiremos, então, o threshold para o valor ideal.

opt_cutoff <- optimalCutoff(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG$DS_SIT_TOT_TURNO, 
                            predicted_prob)


1 - misClassError(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG$DS_SIT_TOT_TURNO, 
                  predicted_prob, 
                  threshold = 0.260131661480244)

# Assim, otimizamos os valores obtidos.


confusionMatrix(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG$DS_SIT_TOT_TURNO, 
                predicted_prob, 
                threshold = opt_cutoff)

# Esse último comando deu a quantidade de acertos e erros do modelo de regressão
# logística por nós utilizado. A partir de então, transformaremos esse 
# resultado em porcentagem.

prop.table(confusionMatrix(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG$DS_SIT_TOT_TURNO, 
                           predicted_prob, threshold = opt_cutoff))


# Agora apresentaremos os resultados básicos de probabilidade de um modelo
# ingênuo:

table(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG$DS_SIT_TOT_TURNO)


prop.table(table(BANCO_UNIFICADO_2_RECODIFICADO_RENOMEADO_REGRESSOES_LOG$DS_SIT_TOT_TURNO))

# Nesse sentido, a construção do modelo por nós utilizado representa uma
# previsibilidade de acerto em relação ao modelo ingênuo dada pela equação:

0.8508/0.8015889

# O modelo de regressão logística dessa pesquisa se mostra 6,1% mais eficiente
# do que a probabilidade pura dada pelo modelo ingênuo (85,36% contra 80,16%).
