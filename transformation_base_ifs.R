library(dplyr)
library(purrr)
library(stringr)

#  create a list of all files in the working directory with the .csv extension
setwd('~/Estudos/PUC/BD/Projeto/if/')
files = list.files(pattern=".csv")

# aggregate xlsx and extract a specific sheet
df = files %>% map_dfr(~ read.csv2(.x,  encoding = 'UTF-8'))

# renomeando a coluna de código
names(df)[2] = 'cod'

# filtrando para excluir linhas com NA ou NI e ordenando pela coluna X.U.FEFF
df2 = df %>% filter(!is.na(cod) & Ativo.Total != 'NI') %>% arrange(X.U.FEFF.Instituição.financeira)

# Excluindo as colunas de índices e patrimônio RWA
df2$Índice.de.Basileia = NULL; df2$Índice.de.Imobilização = NULL; df2$Patrimônio.de.Referência.para.Comparação.com.o.RWA = NULL

# Reorganizando a data
df2$mes = substr(df2$Data, 1, 3)
df2$ano = substr(df2$Data, 5, 6)


df2 = df2 %>% mutate(data_nova = case_when(mes == 'mar' ~ '01/03/20',
                                           mes == 'jun' ~ '01/06/20',
                                           mes == 'set' ~ '01/09/20',
                                           mes == 'dez' ~ '01/12/20'))

df2$data_certa = paste0(df2$data_nova,df2$ano)
df2$data_certa = as.Date(df2$data_certa, '%d/%m/%Y')

# Reordenando as colunas do DF
df2 = df2[, c(1:17, 21)]

# Filtrando as IFs que tiverem dados a partir de 2020 e puxando os valores excluivos para X.U.FEFF e Codigo
ifs_2020 = df2 %>% filter(data_certa > '2019-12-31') %>% select(X.U.FEFF.Instituição.financeira, cod) %>% unique()

# Fazendo join para unir a tabela ifs_2020 com a tabela total
df3 = left_join(ifs_2020, df2, by = c('cod', 'X.U.FEFF.Instituição.financeira'))

# Tirando todos os pontos das colunas numericas
df3$Ativo.Total = gsub('[.]' , '', df3$Ativo.Total)
df3$Carteira.de.Crédito.Classificada = gsub('[.]' , '', df3$Carteira.de.Crédito.Classificada)
df3$Passivo.Circulante.e.Exigível.a.Longo.Prazo.e.Resultados.de.Exercícios.Futuros = gsub('[.]' , '', df3$Passivo.Circulante.e.Exigível.a.Longo.Prazo.e.Resultados.de.Exercícios.Futuros)
df3$Captações = gsub('[.]' , '', df3$Captações)
df3$Patrimônio.Líquido = gsub('[.]' , '', df3$Patrimônio.Líquido)
df3$Lucro.Líquido = gsub('[.]' , '', df3$Lucro.Líquido)
df3$Número.de.Agências = gsub('[.]' , '', df3$Número.de.Agências)
df3$Número.de.Postos.de.Atendimento = gsub('[.]' , '', df3$Número.de.Postos.de.Atendimento)

# Reordenando o dataframe
df3 = df3[, c(1:8, 18, 10:17)]

# Transformando as colunas que estavam como string em numericas
df3[, 10] = as.numeric(df3[, 10])
df3[, 11] = as.numeric(df3[, 11])
df3[, 12] = as.numeric(df3[, 12])
df3[, 13] = as.numeric(df3[, 13])
df3[, 14] = as.numeric(df3[, 14])
df3[, 15] = as.numeric(df3[, 15])
df3[, 16] = as.numeric(df3[, 16])
df3[, 17] = as.numeric(df3[, 17])

# Gerando o csv final
write.csv(df3, 'base_ifs_tratada.csv')
