########################################################################
######Usado para todos os anos, ao substituir o 2017 por outro ano######
########################################################################


install.packages("readr")
library(readr)

setwd("~/RAIS") #pasta em que o arquivo está
RAISDF2017 <- read.table("DF2017.txt", header=TRUE, sep=";")

library(dplyr)
RAISDF201718a65 <- RAISDF2017%>% filter(Idade >= 18 & Idade <= 65 & Vínculo.Ativo.31.12==1)


#####TRANSFORMAR RENDIMENTO EM NUMERICA#####
RAISDF201718a65$RemunMediaNom <- as.character(RAISDF201718a65$Vl.Remun.Média.Nom)
RAISDF201718a65$RemunMediaNom2 <- as.numeric(gsub(",", ".", RAISDF201718a65$Vl.Remun.Média.Nom))


quantile(RAISDF201718a65$RemunMediaNom2)

# Top 1% and bottom 1% removed 
RAISDF201718a65b <- RAISDF201718a65 %>% filter(between(RemunMediaNom2, quantile(RemunMediaNom2, .01), quantile(RemunMediaNom2, .99)))

##renda zero retirada
RAISDF201718a65c <- RAISDF201718a65b %>% filter (RemunMediaNom2>0)


####################################################EXCLUIR MILITARES
RAISDF201718a65c$CBOcup2002<- as.character(RAISDF201718a65c$CBO.Ocupação.2002)
RAISDF201718a65c$CBO2002 <- as.numeric( RAISDF201718a65c$CBOcup2002)
class(RAISDF201718a65c$CBO2002)

RAISDF201718a65c$militares <- ifelse (startsWith(RAISDF201718a65c$CBOcup2002,"0"),1,0)


#######Para os anos de 2008 e 2010###########
# RAISDF200818a65c$militares <- ifelse (RAISDF200818a65c$CBO2002 ==10105|
#                                         RAISDF200818a65c$CBO2002 ==10110|
#                                         RAISDF200818a65c$CBO2002 ==10115|
#                                         RAISDF200818a65c$CBO2002 ==10205|
#                                         RAISDF200818a65c$CBO2002 ==10210|
#                                         RAISDF200818a65c$CBO2002 ==10215|
#                                         RAISDF200818a65c$CBO2002 ==10305|
#                                         RAISDF200818a65c$CBO2002 ==10310|
#                                         RAISDF200818a65c$CBO2002 ==10315|
#                                         RAISDF200818a65c$CBO2002 ==20075|
#                                         RAISDF200818a65c$CBO2002 ==20105|
#                                         RAISDF200818a65c$CBO2002 ==20110|
#                                         RAISDF200818a65c$CBO2002 ==20115|
#                                         RAISDF200818a65c$CBO2002 ==20205|
#                                         RAISDF200818a65c$CBO2002 ==20305|
#                                         RAISDF200818a65c$CBO2002 ==20310|
#                                         RAISDF200818a65c$CBO2002 ==21105|
#                                         RAISDF200818a65c$CBO2002 ==21110|
#                                         RAISDF200818a65c$CBO2002 ==21205|
#                                         RAISDF200818a65c$CBO2002 ==21210|
#                                         RAISDF200818a65c$CBO2002 ==30105|
#                                         RAISDF200818a65c$CBO2002 ==30110|
#                                         RAISDF200818a65c$CBO2002 ==30115|
#                                         RAISDF200818a65c$CBO2002 ==30205|
#                                         RAISDF200818a65c$CBO2002 ==30305|
#                                         RAISDF200818a65c$CBO2002 ==31105|
#                                         RAISDF200818a65c$CBO2002 ==31110|
#                                         RAISDF200818a65c$CBO2002 ==31205|
#                                         RAISDF200818a65c$CBO2002 ==31210
#                                       ,1,0)
# table(RAISDF200818a65c$militares)


table(RAISDF201718a65c$militares)

RAISDF201718a65d <- RAISDF201718a65c%>% filter(militares==0)

  
#####TRABALHADORES PUBLICOS ESTATUTARIOS x PRIVADOS#####
table(RAISDF201718a65d$Tipo.Vínculo)

##estatutários:
RAISDF201718a65d$estatutario <- ifelse((RAISDF201718a65d$Tipo.Vínculo == 30),1,0)
RAISDF201718a65d$estatutarioefet <- ifelse((RAISDF201718a65d$Tipo.Vínculo == 30|RAISDF201718a65d$Tipo.Vínculo == 31),1,0)
RAISDF201718a65d$estatutariotot <- ifelse((RAISDF201718a65d$Tipo.Vínculo == 30|RAISDF201718a65d$Tipo.Vínculo == 31| RAISDF201718a65d$Tipo.Vínculo == 35),1,0)
table (RAISDF201718a65d$estatutario)
table (RAISDF201718a65d$estatutarioefet)
table (RAISDF201718a65d$estatutariotot)

##privados
##TABELA DE NATUREZA JURÍDICA 2018
# 2. Entidades Empresariais
# 201-1 - Empresa Pública
# 3. Entidades sem Fins Lucrativos
# 4. Pessoas Físicas

table(RAISDF201718a65d$Natureza.Jurídica)
RAISDF201718a65d$trabprivado <- ifelse((RAISDF201718a65d$Natureza.Jurídica>= 2038 & RAISDF201718a65d$Natureza.Jurídica <= 2330),1,0)
table(RAISDF201718a65d$trabprivado)

##privados:clt
RAISDF201718a65d$trabprivadoclt <- ifelse((RAISDF201718a65d$trabprivado==1) & (RAISDF201718a65d$Tipo.Vínculo ==10|
                                                                                 RAISDF201718a65d$Tipo.Vínculo ==15| 
                                                                                 RAISDF201718a65d$Tipo.Vínculo ==20|
                                                                                 RAISDF201718a65d$Tipo.Vínculo ==25|
                                                                                 RAISDF201718a65d$Tipo.Vínculo ==60|
                                                                                 RAISDF201718a65d$Tipo.Vínculo ==65| 
                                                                                 RAISDF201718a65d$Tipo.Vínculo ==70| 
                                                                                 RAISDF201718a65d$Tipo.Vínculo ==75),1,0)
table(RAISDF201718a65d$trabprivado,RAISDF201718a65d$Tipo.Vínculo)
table(RAISDF201718a65d$trabprivadoclt)
table(RAISDF201718a65d$trabprivadoclt, RAISDF201718a65d$estatutarioefet)
table(RAISDF201718a65d$Tipo.Vínculo, RAISDF201718a65d$trabprivado)
table(RAISDF201718a65d$Tipo.Vínculo, RAISDF201718a65d$trabprivadoclt)



RAISDF201718a65d$trabresPPclt <- ifelse(RAISDF201718a65d$estatutarioefet==1,2,RAISDF201718a65d$trabprivadoclt)


RAISDF201718a65d$trabresPP <- ifelse(RAISDF201718a65d$estatutarioefet==1,2,RAISDF201718a65d$trabprivado)
table (RAISDF201718a65d$trabresPP)

table(RAISDF201718a65d$Tipo.Vínculo,  RAISDF201718a65d$trabresPP) 
table(RAISDF201718a65d$Natureza.Jurídica,   RAISDF201718a65d$trabresPP) 

table(RAISDF201718a65d$Tipo.Vínculo)

#############################################################

#####GINI#####

#chooseCRANmirror()
#install.packages("ineq")

library("ineq")
RAISDF201718a65d$sexo <- as.numeric(as.character(RAISDF201718a65d$Sexo.Trabalhador))
class(RAISDF201718a65d$sexo)

ineq(RAISDF201718a65d[which(RAISDF201718a65d$sexo==2, RAISDF201718a65d$trabresPP==1), ]$RemunMediaNom2 ,type="Gini")


####VARIAVEIS NUMERICAS
RAISDF201718a65d$Idade2 <- as.numeric(as.character(RAISDF201718a65d$Idade))
class(RAISDF201718a65d$Idade2)


RAISDF201718a65d$escolaridade <- as.numeric(as.character(RAISDF201718a65d$Escolaridade.após.2005))
class(RAISDF201718a65d$escolaridade)



#base para homens e para mulheres
mulheresdf201718a65 <- RAISDF201718a65d%>% filter(Sexo.Trabalhador==2)
homensdf201718a65 <- RAISDF201718a65d%>% filter(Sexo.Trabalhador==1)

#############################################################
####COM A BASE TODA
#####mulheresestatutarias
ineq(RAISDF201718a65d[which( RAISDF201718a65d$trabresPP==2, RAISDF201718a65d$sexo==2), ]$RemunMediaNom2 ,type="Gini")
#0.3434608
####SO COM A DE MULHERES

#####mulheresestatutarias
ineq(mulheresdf201718a65[which( mulheresdf201718a65$trabresPP==2), ]$RemunMediaNom2, type="Gini")
#0.3320018
############################################################


#####mulheresestatutarias
RAISDF201718a65d$Gini1 <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==2), ineq(mulheresdf201718a65[which(mulheresdf201718a65$trabresPP==2), ]$RemunMediaNom2 ,type="Gini"), 0 )
table(RAISDF201718a65d$Gini1)

#####mulheresstprivado
RAISDF201718a65d$Gini2 <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==1),ineq(mulheresdf201718a65[which( mulheresdf201718a65$trabresPP==1), ]$RemunMediaNom2 ,type="Gini"), 0 )
table(RAISDF201718a65d$Gini2)

#####mulheresoutros
RAISDF201718a65d$Gini3 <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==0),ineq(mulheresdf201718a65[which(mulheresdf201718a65$trabresPP==0), ]$RemunMediaNom2 ,type="Gini"), 0 )
table(RAISDF201718a65d$Gini3)

#####homensestaturarios
RAISDF201718a65d$Gini4 <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==2),ineq(homensdf201718a65[which(homensdf201718a65$trabresPP==2), ]$RemunMediaNom2 ,type="Gini"), 0 )
table(RAISDF201718a65d$Gini4)

#####homensstprivado
RAISDF201718a65d$Gini5 <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==1),ineq(homensdf201718a65[which( homensdf201718a65$trabresPP==1), ]$RemunMediaNom2 ,type="Gini"), 0 )
table(RAISDF201718a65d$Gini5)

#####homensoutros
RAISDF201718a65d$Gini6 <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==0),ineq(homensdf201718a65[which( homensdf201718a65$trabresPP==0), ]$RemunMediaNom2 ,type="Gini"), 0 )
table(RAISDF201718a65d$Gini6)

#######################juntar
RAISDF201718a65d$Gini <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==2),RAISDF201718a65d$Gini1, 0)

RAISDF201718a65d$Gini <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==1),RAISDF201718a65d$Gini2, RAISDF201718a65d$Gini)

RAISDF201718a65d$Gini <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==0),RAISDF201718a65d$Gini3, RAISDF201718a65d$Gini)

RAISDF201718a65d$Gini <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==2),RAISDF201718a65d$Gini4, RAISDF201718a65d$Gini)

RAISDF201718a65d$Gini <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==1),RAISDF201718a65d$Gini5, RAISDF201718a65d$Gini)

RAISDF201718a65d$Gini <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==0),RAISDF201718a65d$Gini6, RAISDF201718a65d$Gini)


gini <- table( RAISDF201718a65d$Gini, RAISDF201718a65d$trabresPP)

write.table(gini, file='gini.csv',sep=';',na="",quote=TRUE, row.names=TRUE)



### media e mediana dos salarios
mean(RAISDF201718a65d$RemunMediaNom2)
median(RAISDF201718a65d$RemunMediaNom2)

#####mulheresestatutarias
RAISDF201718a65d$mediarem1 <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==2),mean(mulheresdf201718a65[which( mulheresdf201718a65$trabresPP==2), ]$RemunMediaNom2), 0)
RAISDF201718a65d$medianarem1 <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==2),median(mulheresdf201718a65[which( mulheresdf201718a65$trabresPP==2), ]$RemunMediaNom2), 0)

table(RAISDF201718a65d$mediarem1, RAISDF201718a65d$trabresPP)
#####mulheresstprivado
RAISDF201718a65d$mediarem2 <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==1),mean(mulheresdf201718a65[which(mulheresdf201718a65$trabresPP==1), ]$RemunMediaNom2),0)
RAISDF201718a65d$medianarem2 <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==1),median(mulheresdf201718a65[which(mulheresdf201718a65$trabresPP==1), ]$RemunMediaNom2), 0)

#####mulheresoutros
RAISDF201718a65d$mediarem3 <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==0),mean(mulheresdf201718a65[which(mulheresdf201718a65$trabresPP==0), ]$RemunMediaNom2), 0)
RAISDF201718a65d$medianarem3 <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==0),median(mulheresdf201718a65[which(mulheresdf201718a65$trabresPP==0), ]$RemunMediaNom2), 0)

#####homensestaturarios
RAISDF201718a65d$mediarem4 <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==2),mean(homensdf201718a65[which(homensdf201718a65$trabresPP==2), ]$RemunMediaNom2), 0)
RAISDF201718a65d$medianarem4 <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==2),median(homensdf201718a65[which(homensdf201718a65$trabresPP==2), ]$RemunMediaNom2), 0)

#####homensstprivado
RAISDF201718a65d$mediarem5 <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==1),mean(homensdf201718a65[which(homensdf201718a65$trabresPP==1), ]$RemunMediaNom2), 0)
RAISDF201718a65d$medianarem5 <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==1),median(homensdf201718a65[which(homensdf201718a65$trabresPP==1), ]$RemunMediaNom2), 0)

#####homensoutros
RAISDF201718a65d$mediarem6 <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==0),mean(homensdf201718a65[which(homensdf201718a65$trabresPP==0), ]$RemunMediaNom2), 0)
RAISDF201718a65d$medianarem6 <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==0),median(homensdf201718a65[which(homensdf201718a65$trabresPP==0), ]$RemunMediaNom2), 0)

#######################juntar média
RAISDF201718a65d$mediarem <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==2),RAISDF201718a65d$mediarem1, 0)

RAISDF201718a65d$mediarem <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==1),RAISDF201718a65d$mediarem2, RAISDF201718a65d$mediarem)

RAISDF201718a65d$mediarem <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==0),RAISDF201718a65d$mediarem3, RAISDF201718a65d$mediarem)

RAISDF201718a65d$mediarem <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==2),RAISDF201718a65d$mediarem4, RAISDF201718a65d$mediarem)

RAISDF201718a65d$mediarem <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==1),RAISDF201718a65d$mediarem5, RAISDF201718a65d$mediarem)

RAISDF201718a65d$mediarem <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==0),RAISDF201718a65d$mediarem6, RAISDF201718a65d$mediarem)

#######################juntar mediana
RAISDF201718a65d$medianarem <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==2),RAISDF201718a65d$medianarem1, 0)

RAISDF201718a65d$medianarem <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==1),RAISDF201718a65d$medianarem2, RAISDF201718a65d$medianarem)

RAISDF201718a65d$medianarem <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==0),RAISDF201718a65d$medianarem3, RAISDF201718a65d$medianarem)

RAISDF201718a65d$medianarem <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==2),RAISDF201718a65d$medianarem4, RAISDF201718a65d$medianarem)

RAISDF201718a65d$medianarem <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==1),RAISDF201718a65d$medianarem5, RAISDF201718a65d$medianarem)

RAISDF201718a65d$medianarem <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==0),RAISDF201718a65d$medianarem6, RAISDF201718a65d$medianarem)
#########################

table(RAISDF201718a65d$mediarem, RAISDF201718a65d$trabresPP, RAISDF201718a65d$sexo )
table(RAISDF201718a65d$medianarem, RAISDF201718a65d$trabresPP, RAISDF201718a65d$sexo)


############DEFLACIONAR
# install.packages("deflateBR")
library(deflateBR)


#install.packages("tidyverse")
library(tidyverse)

RAISDF201718a65d$data <-  seq.Date(from = as.Date("2017-12-01"), by = "month", length.out = 1)

class(RAISDF201718a65d$data)
table(RAISDF201718a65d$data)

RAISDF201718a65d$remmediadef <- deflate(RAISDF201718a65d$RemunMediaNom2, RAISDF201718a65d$data, "12/2017", "ipca")


### media e mediana dos salarios reais
mean(RAISDF201718a65d$remmediadef)
median(RAISDF201718a65d$remmediadef)


####################LN DO SALÁRIO

#atualizar base para homens e para mulheres
mulheresdf201718a65 <- RAISDF201718a65d%>% filter(Sexo.Trabalhador==2)
homensdf201718a65 <- RAISDF201718a65d%>% filter(Sexo.Trabalhador==1)

# table(RAISDF201718a65d$logremREAL)
class(mulheresdf201718a65$logremREAL)

#####mulheresestatutarias
RAISDF201718a65d$medialogrem1 <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==2),mean(mulheresdf201718a65[which(mulheresdf201718a65$trabresPP==2), ]$logremREAL), 0)
table(RAISDF201718a65d$medialogrem1)

#####mulheresstprivado
RAISDF201718a65d$medialogrem2 <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==1), mean(mulheresdf201718a65[which(mulheresdf201718a65$trabresPP==1), ]$logremREAL), 0)

#####mulheresoutros
RAISDF201718a65d$medialogrem3 <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==0),mean(mulheresdf201718a65[which(mulheresdf201718a65$trabresPP==0), ]$logremREAL), 0)

#####homensestaturarios
RAISDF201718a65d$medialogrem4 <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==2),mean(homensdf201718a65[which(homensdf201718a65$trabresPP==2), ]$logremREAL), 0)


#####homensstprivado
RAISDF201718a65d$medialogrem5 <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==1),mean(homensdf201718a65[which(homensdf201718a65$trabresPP==1), ]$logremREAL), 0)


#####homensoutros
RAISDF201718a65d$medialogrem6 <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==0),mean(RAISDF201718a65d[which(homensdf201718a65$trabresPP==0), ]$logremREAL), 0)

#######################juntar medialogrem
RAISDF201718a65d$medialogrem <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==2),RAISDF201718a65d$medialogrem1, 0)

RAISDF201718a65d$medialogrem <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==1),RAISDF201718a65d$medialogrem2, RAISDF201718a65d$medialogrem)

RAISDF201718a65d$medialogrem <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==0),RAISDF201718a65d$medialogrem3, RAISDF201718a65d$medialogrem)

RAISDF201718a65d$medialogrem <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==2),RAISDF201718a65d$medialogrem4, RAISDF201718a65d$medialogrem)

RAISDF201718a65d$medialogrem <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==1),RAISDF201718a65d$medialogrem5, RAISDF201718a65d$medialogrem)

RAISDF201718a65d$medialogrem <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==0),RAISDF201718a65d$medialogrem6, RAISDF201718a65d$medialogrem)

table(RAISDF201718a65d$medialogrem)
table(RAISDF201718a65d$medialogrem, RAISDF201718a65d$trabresPP, RAISDF201718a65d$sexo)

mean(RAISDF201718a65d$logremREAL)



###################MEDIA E MEDIANA REAL DOS SALARIOS
mean(RAISDF201718a65d$remmediadef)
median(RAISDF201718a65d$remmediadef)

#####mulheresestatutarias
RAISDF201718a65d$mediaremREAL1 <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==2),mean(mulheresdf201718a65[which( mulheresdf201718a65$trabresPP==2), ]$remmediadef), 0)
RAISDF201718a65d$medianaremREAL1 <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==2),median(mulheresdf201718a65[which( mulheresdf201718a65$trabresPP==2), ]$remmediadef), 0)

table(RAISDF201718a65d$mediaremREAL1, RAISDF201718a65d$trabresPP)
#####mulheresstprivado
RAISDF201718a65d$mediaremREAL2 <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==1),mean(mulheresdf201718a65[which(mulheresdf201718a65$trabresPP==1), ]$remmediadef),0)
RAISDF201718a65d$medianaremREAL2 <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==1),median(mulheresdf201718a65[which(mulheresdf201718a65$trabresPP==1), ]$remmediadef), 0)

#####mulheresoutros
RAISDF201718a65d$mediaremREAL3 <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==0),mean(mulheresdf201718a65[which(mulheresdf201718a65$trabresPP==0), ]$remmediadef), 0)
RAISDF201718a65d$medianaremREAL3 <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==0),median(mulheresdf201718a65[which(mulheresdf201718a65$trabresPP==0), ]$remmediadef), 0)

#####homensestaturarios
RAISDF201718a65d$mediaremREAL4 <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==2),mean(homensdf201718a65[which(homensdf201718a65$trabresPP==2), ]$remmediadef), 0)
RAISDF201718a65d$medianaremREAL4 <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==2),median(homensdf201718a65[which(homensdf201718a65$trabresPP==2), ]$remmediadef), 0)

#####homensstprivado
RAISDF201718a65d$mediaremREAL5 <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==1),mean(homensdf201718a65[which(homensdf201718a65$trabresPP==1), ]$remmediadef), 0)
RAISDF201718a65d$medianaremREAL5 <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==1),median(homensdf201718a65[which(homensdf201718a65$trabresPP==1), ]$remmediadef), 0)

#####homensoutros
RAISDF201718a65d$mediaremREAL6 <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==0),mean(homensdf201718a65[which(homensdf201718a65$trabresPP==0), ]$remmediadef), 0)
RAISDF201718a65d$medianaremREAL6 <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==0),median(homensdf201718a65[which(homensdf201718a65$trabresPP==0), ]$remmediadef), 0)

#######################juntar média
RAISDF201718a65d$mediaremREAL <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==2),RAISDF201718a65d$mediaremREAL1, 0)

RAISDF201718a65d$mediaremREAL <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==1),RAISDF201718a65d$mediaremREAL2, RAISDF201718a65d$mediaremREAL)

RAISDF201718a65d$mediaremREAL <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==0),RAISDF201718a65d$mediaremREAL3, RAISDF201718a65d$mediaremREAL)

RAISDF201718a65d$mediaremREAL <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==2),RAISDF201718a65d$mediaremREAL4, RAISDF201718a65d$mediaremREAL)

RAISDF201718a65d$mediaremREAL <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==1),RAISDF201718a65d$mediaremREAL5, RAISDF201718a65d$mediaremREAL)

RAISDF201718a65d$mediaremREAL <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==0),RAISDF201718a65d$mediaremREAL6, RAISDF201718a65d$mediaremREAL)

#######################juntar mediana
RAISDF201718a65d$medianaremREAL <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==2),RAISDF201718a65d$medianaremREAL1, 0)

RAISDF201718a65d$medianaremREAL <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==1),RAISDF201718a65d$medianaremREAL2, RAISDF201718a65d$medianaremREAL)

RAISDF201718a65d$medianaremREAL <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==0),RAISDF201718a65d$medianaremREAL3, RAISDF201718a65d$medianaremREAL)

RAISDF201718a65d$medianaremREAL <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==2),RAISDF201718a65d$medianaremREAL4, RAISDF201718a65d$medianaremREAL)

RAISDF201718a65d$medianaremREAL <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==1),RAISDF201718a65d$medianaremREAL5, RAISDF201718a65d$medianaremREAL)

RAISDF201718a65d$medianaremREAL <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==0),RAISDF201718a65d$medianaremREAL6, RAISDF201718a65d$medianaremREAL)
#########################

mediasalarioreal <-table(RAISDF201718a65d$mediaremREAL, RAISDF201718a65d$trabresPP, RAISDF201718a65d$sexo )
write.table(mediasalarioreal, file='mediasalarioreal.csv',sep=';',na="",quote=TRUE, row.names=TRUE)



medianasalarioreal <- table(RAISDF201718a65d$medianaremREAL, RAISDF201718a65d$trabresPP, RAISDF201718a65d$sexo)
write.table(medianasalarioreal, file='medianasalarioreal.csv',sep=';',na="",quote=TRUE, row.names=TRUE)


#################################Porcentagens por sexo###############
frequencias <- table(RAISDF201718a65d$trabresPP, RAISDF201718a65d$Sexo.Trabalhador )
write.table(frequencias, file='frequencias.csv',sep=';',na="",quote=TRUE, row.names=TRUE)


#########################################################
########MEDIA DA IDADE e ESCOLARIDADE###################
#########################################################

#####IDADE
#####mulheresestatutarias
RAISDF201718a65d$mediaidade1 <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==2), mean(mulheresdf201718a65[which(mulheresdf201718a65$trabresPP==2), ]$Idade2), 0)
table(RAISDF201718a65d$mediaidade1)

#####mulheresstprivado
RAISDF201718a65d$mediaidade2 <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==1),mean(mulheresdf201718a65[which(mulheresdf201718a65$trabresPP==1), ]$Idade2), 0)

#####mulheresoutros
RAISDF201718a65d$mediaidade3 <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==0),mean(mulheresdf201718a65[which(mulheresdf201718a65$trabresPP==0), ]$Idade2), 0)

#####homensestaturarios
RAISDF201718a65d$mediaidade4 <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==2),mean(homensdf201718a65[which(homensdf201718a65$trabresPP==2), ]$Idade2), 0)


#####homensstprivado
RAISDF201718a65d$mediaidade5 <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==1),mean(homensdf201718a65[which(homensdf201718a65$trabresPP==1), ]$Idade2), 0)

#####homensoutros
RAISDF201718a65d$mediaidade6 <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==0),mean(homensdf201718a65[which(homensdf201718a65$trabresPP==0), ]$Idade2),0)

#######################juntar mediaidade
RAISDF201718a65d$mediaidade <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==2),RAISDF201718a65d$mediaidade1, 0)

RAISDF201718a65d$mediaidade <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==1),RAISDF201718a65d$mediaidade2, RAISDF201718a65d$mediaidade)

RAISDF201718a65d$mediaidade <- ifelse( (RAISDF201718a65d$sexo==2  & RAISDF201718a65d$trabresPP==0),RAISDF201718a65d$mediaidade3, RAISDF201718a65d$mediaidade)

RAISDF201718a65d$mediaidade <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==2),RAISDF201718a65d$mediaidade4, RAISDF201718a65d$mediaidade)

RAISDF201718a65d$mediaidade <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==1),RAISDF201718a65d$mediaidade5, RAISDF201718a65d$mediaidade)

RAISDF201718a65d$mediaidade <- ifelse( (RAISDF201718a65d$sexo==1  & RAISDF201718a65d$trabresPP==0),RAISDF201718a65d$mediaidade6, RAISDF201718a65d$mediaidade)

idade <- table(RAISDF201718a65d$mediaidade, RAISDF201718a65d$trabresPP, RAISDF201718a65d$sexo)

write.table(idade, file='idade.csv',sep=';',na="",quote=TRUE, row.names=TRUE)



write.table(RAISDF201718a65d, file='RAISDF201718a65d.csv', sep=';', row.names=FALSE)
