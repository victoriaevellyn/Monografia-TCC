#########################################################
######Específicos para comparação entre 2008 e 2017######
#########################################################

install.packages("readr")
library(readr)

setwd("~/RAIS") #pasta em que o arquivo está
RAISDF201718a65d <- read.table("RAISDF201718a65d.csv", header=TRUE, sep=";")


#base para homens e para mulheres
mulheresdf201718a65 <- RAISDF201718a65d%>% filter(Sexo.Trabalhador==2)
homensdf201718a65 <- RAISDF201718a65d%>% filter(Sexo.Trabalhador==1)


#####################ESCOLARIDADE
escoltotal <- table(RAISDF201718a65d$escolaridade, RAISDF201718a65d$trabresPP)

# escolmulheres <- 
table( mulheresdf201718a65$escolaridade, mulheresdf201718a65$trabresPP)

# escolhomens <- 
table( homensdf201718a65$escolaridade, homensdf201718a65$trabresPP)

write.table(escoltotal , file='escolaridade.csv',sep=';',na="",quote=TRUE, row.names=TRUE)


#####CATEGORIAS CBO GrGRUPO#####
RAISDF201718a65d$grgrupo1 <- ifelse (startsWith(RAISDF201718a65d$CBOcup2002,"1"),1,0)
table(RAISDF201718a65d$grgrupo1)

RAISDF201718a65d$grgrupo2 <- ifelse (startsWith(RAISDF201718a65d$CBOcup2002,"2"),2,RAISDF201718a65d$grgrupo1)
table(RAISDF201718a65d$grgrupo2)

RAISDF201718a65d$grgrupo3 <- ifelse (startsWith(RAISDF201718a65d$CBOcup2002,"3"),3,RAISDF201718a65d$grgrupo2)
table(RAISDF201718a65d$grgrupo3)

RAISDF201718a65d$grgrupo4 <- ifelse (startsWith(RAISDF201718a65d$CBOcup2002,"4"),4,RAISDF201718a65d$grgrupo3)
table(RAISDF201718a65d$grgrupo4)

RAISDF201718a65d$grgrupo5 <- ifelse (startsWith(RAISDF201718a65d$CBOcup2002,"5"),5,RAISDF201718a65d$grgrupo4)
table(RAISDF201718a65d$grgrupo5)

RAISDF201718a65d$grgrupo6 <- ifelse (startsWith(RAISDF201718a65d$CBOcup2002,"6"),6,RAISDF201718a65d$grgrupo5)
table(RAISDF201718a65d$grgrupo6)

RAISDF201718a65d$grgrupo7 <- ifelse (startsWith(RAISDF201718a65d$CBOcup2002,"7"),7,RAISDF201718a65d$grgrupo6)
table(RAISDF201718a65d$grgrupo7)

RAISDF201718a65d$grgrupo8 <- ifelse (startsWith(RAISDF201718a65d$CBOcup2002,"8"),8,RAISDF201718a65d$grgrupo7)
table(RAISDF201718a65d$grgrupo8)

RAISDF201718a65d$CBOGrGrupo2002 <- ifelse (startsWith(RAISDF201718a65d$CBOcup2002,"9"),9,RAISDF201718a65d$grgrupo8)
table(RAISDF201718a65d$CBOGrGrupo2002)




#####POR CBO
CBOtotal <- table(RAISDF201718a65d$CBOGrGrupo2002, RAISDF201718a65d$trabresPP)

CBOmulheres <- table(mulheresdf201718a65$CBOGrGrupo2002, mulheresdf201718a65$trabresPP)

CBOhomens <- table(homensdf201718a65$CBOGrGrupo2002, homensdf201718a65$trabresPP)


write.table(CBOtotal , file='CBO.csv',sep=';',na="",quote=TRUE, row.names=TRUE)

write.table(CBOmulheres, file='CBOmulheres.csv',sep=';',na="",quote=TRUE, row.names=TRUE)

write.table(CBOhomens, file='CBOhomens.csv',sep=';',na="",quote=TRUE, row.names=TRUE)

#################################salario por cbo
# install.packages("srvyr")
library(srvyr)

cbomediassal <- RAISDF201718a65d %>% 
  group_by(sexo, CBOGrGrupo2002, trabresPP) %>% 
  summarise(media=mean(remmediadef, vartype = "se", na.rm = T),
            mediana=median(remmediadef, vartype = "se", na.rm = T))

write.table(cbomediassal, file= 'CBOmediasal.csv', sep=";", dec= ",",  row.names=F, fileEncoding ="latin1")

cbomediassalTOT <- RAISDF201718a65d %>% 
  group_by(CBOGrGrupo2002) %>% 
  summarise(media=mean(remmediadef, vartype = "se", na.rm = T),
            mediana=median(remmediadef, vartype = "se", na.rm = T))

write.table(cbomediassalTOT, file= 'cbomediassalTOT.csv', sep=";", dec= ",",  row.names=F, fileEncoding ="latin1")

