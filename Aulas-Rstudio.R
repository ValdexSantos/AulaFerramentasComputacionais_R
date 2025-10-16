dados<-read.csv("~/Documents/GitHub/AulaFerramentasComputacionais_R/Pokemon_full.csv")

head(dados)

tail(dados,12)


names(dados)

library(tidyverse)

#selecionar coluna
select(dados,name,hp)

#filtrar colunas
filter(dados,attack<50)


x=mutate(dados,attack+speed) #cria nova variável
mutate(dados, attack=attack/2) #modifica a variável
dados <- mutate(dados, IMC=weight/(height*height)) #IMC DO POKEMON

#comandos pipe
dados %>% 
  select(name,hp,attack,speed) %>% 
  filter(attack<50) %>% 
  mutate(x=attack+speed)

#Grafico-só um exemplo, vai repetir
dados %>% 
  filter(height>10) %>% 
  select(name, height, weight) %>% 
  mutate(imc=weight/(height*height)) %>% 
  ggplot()+
  geom_density((aes(x=imc)))

str(dados) #mostra o tipo de dados

#Retorna a coluna como um vetor enequanto o select retorna como coluna mesmo
dados %>% pull(IMC)

#RESUMINDO OS DADOS COM UMA COLUNA PARA CADA VARIAVEL
dados %>% 
  summarise(media=mean(IMC),desvio=sd(IMC))

#AGRUPANDO DADOS E calculando
dados %>% 
  group_by(type) %>% 
  summarise(media=mean(IMC),desvio=sd(IMC))

df <- dados %>% 
  group_by(type) %>% 
  mutate(media=mean(IMC)) %>% view()




#DESAGRUPANDO
df %>% 
  ungroup() %>% 
  mutate(media=mean(IMC)) %>% view()

#busca padrões - aceita Regular Expressions (ReGex)
grep("saur",dados$name)
grepl("saur", dados$name)

grep("saur|fly",dados$name)

grep("[Ss]aur",dados$name)

dados %>% 
  filter(grepl("saur|fly",name), attack>50)

#juntando dataframes
df1 <- dados %>% 
  filter(attack>70)

df2 <- dados %>% 
  filter(attack<70)

rbind(df1,df2) #juntar linhas


#juntando dataframes com colunas difrentes
df1 <- dados %>% 
  select(attack,speed, weight) %>% 
  filter(attack>70)

df2 <- dados %>% 
  select(attack,hp, weight, height) %>% 
  filter(attack<70)

bind_rows(df1,df2) %>%  view#juntar linhas, as que tiverem mesmo nome(vira uma só) e na que não tiver coloca NA


###########
#Juntando daataframes com uma coluna de referencia - JOIN
df_resumo <- dados %>% 
  group_by(type) %>% 
  summarise(media=mean(IMC),desvio=sd(IMC))

left_join(dados, df_resumo, by="type") %>% view
          