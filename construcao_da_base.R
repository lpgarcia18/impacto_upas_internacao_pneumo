library(data.table)
library(readr)
library(readxl)
library(tidyverse)
library(reshape2)


# horas-médico ----------------------------------------------------------------
profissionais <- fread("dados/ministerio_da_saude/cnes/tbCargaHorariaSus201708.csv")
profissionais <- profissionais %>% select(CO_UNIDADE, CO_CBO, QT_CARGA_HORARIA_AMBULATORIAL, CO_PROFISSIONAL_SUS)
profissionais$CO_CBO <- substr(profissionais$CO_CBO,0,3) 
profissionais <- subset(profissionais, profissionais$CO_CBO == 225) #CBO de todos os médicos
num_profissionais <- unique(profissionais$CO_PROFISSIONAL_SUS)

estabelecimento <- fread("dados/ministerio_da_saude/cnes/tbEstabelecimento201708.csv")
estabelecimento <- estabelecimento %>% select(CO_UNIDADE, CO_NATUREZA_JUR,
					      CO_ESTADO_GESTOR, CO_MUNICIPIO_GESTOR, TP_UNIDADE)
num_estabelecimento <- unique(estabelecimento$CO_UNIDADE)
tipo_unidade <- fread("dados/ministerio_da_saude/cnes/tbTipoUnidade201708.csv")
#Unidades que serão analisadas
# CO_TIPO_UNIDADE;"DS_TIPO_UNIDADE"
# 01;"POSTO DE SAUDE"
# 02;"CENTRO DE SAUDE/UNIDADE BASICA"
# 20;"PRONTO SOCORRO GERAL"
# 21;"PRONTO SOCORRO ESPECIALIZADO"
# 05;"HOSPITAL GERAL"
# 07;"HOSPITAL ESPECIALIZADO"
# 73;"PRONTO ATENDIMENTO"
tipo_unidade <- subset(tipo_unidade, tipo_unidade$CO_TIPO_UNIDADE %in% c(01,02,20,21,05,07,73))
tipo_unidade$TIPO_ESTABELECIMENTO <- NA
for(i in seq_along(tipo_unidade$DS_TIPO_UNIDADE)){
	if(tipo_unidade$DS_TIPO_UNIDADE[i] == "POSTO DE SAUDE" |
	   tipo_unidade$DS_TIPO_UNIDADE[i] == "CENTRO DE SAUDE/UNIDADE BASICA"){
	tipo_unidade$TIPO_ESTABELECIMENTO[i] <- "UBS"
	}else if(tipo_unidade$DS_TIPO_UNIDADE[i] == "PRONTO SOCORRO GERAL" |
	   tipo_unidade$DS_TIPO_UNIDADE[i] == "PRONTO SOCORRO ESPECIALIZADO"|
	   tipo_unidade$DS_TIPO_UNIDADE[i] == "HOSPITAL GERAL"|
	   tipo_unidade$DS_TIPO_UNIDADE[i] == "HOSPITAL ESPECIALIZADO"){
	tipo_unidade$TIPO_ESTABELECIMENTO[i] <- "Hospital/Emergência"	
	}else{
	tipo_unidade$TIPO_ESTABELECIMENTO[i] <- "UPA"	
	}
}

natureza_juridica <- fread("dados/ministerio_da_saude/cnes/tbNaturezaJuridica201708.csv")
#Natureza jurídica analisada "ORGANIZAÇÃO SOCIAL (OS)", "ENTIDADES SEM FINS LUCRATIVOS" e "FUNDACAO PRIVADA" foram mantidas, pois UPAS e UBS têm sido entregues 
#para administração destas
# CO_NATUREZA_JUR;"DS_NATUREZA_JUR"		
# 1000;"ADMINISTRACAO PUBLICA"		
# 1228;"CONSORCIO PUBLICO DE DIREITO PRIVADO"		
# 1236;"ESTADO OU DISTRITO FEDERAL"		
# 1244;"MUNICIPIO"		
# 1252;"FUNDACAO PUBLICA DE DIREITO PRIVADO FEDERAL"		
# 1260;"FUNDACAO PUBLICA DE DIREITO PRIVADO ESTADUAL OU DO DISTRITO FEDERAL"		
# 1279;"FUNDACAO PUBLICA DE DIREITO PRIVADO MUNICIPAL"		
# 2305;"EMPRESA INDIVIDUAL DE RESPONSABILIDADE LIMITADA (DE NATUREZA EMPRESARIA)"		
# 2313;"EMPRESA INDIVIDUAL DE RESPONSABILIDADE LIMITADA (DE NATUREZA SIMPLES)"		
# 3301;"ORGANIZAÇÃO SOCIAL (OS)"		
# 1015;"ORGAO PUBLICO DO PODER EXECUTIVO FEDERAL"		
# 1023;"ORGAO PUBLICO DO PODER EXECUTIVO ESTADUAL OU DO DISTRITO FEDERAL"		
# 1031;"ORGAO PUBLICO DO PODER EXECUTIVO MUNICIPAL"		
# 1040;"ORGAO PUBLICO DO PODER LEGISLATIVO FEDERAL"		
# 1058;"ORGAO PUBLICO DO PODER LEGISLATIVO ESTADUAL OU DO DISTRITO FEDERAL"		
# 1066;"ORGAO PUBLICO DO PODER LEGISLATIVO MUNICIPAL"		
# 1074;"ORGAO PUBLICO DO PODER JUDICIARIO FEDERAL"		
# 1082;"ORGAO PUBLICO DO PODER JUDICIARIO ESTADUAL"		
# 1104;"AUTARQUIA FEDERAL"		
# 1112;"AUTARQUIA ESTADUAL OU DO DISTRITO FEDERAL"		
# 1120;"AUTARQUIA MUNICIPAL"		
# 1139;"FUNDACAO FEDERAL"		
# 1147;"FUNDACAO ESTADUAL OU DO DISTRITO FEDERAL"		
# 1155;"FUNDACAO MUNICIPAL"		
# 1163;"ORGAO PUBLICO AUTONOMO FEDERAL"		
# 1171;"ORGAO PUBLICO AUTONOMO ESTADUAL OU DO DISTRITO FEDERAL"		
# 1180;"ORGAO PUBLICO AUTONOMO MUNICIPAL"		
# 1201;"FUNDO PUBLICO"		
# 1210;"ASSOCIACAO PUBLICA"		
# 2011;"EMPRESA PUBLICA"		
# 2038;"SOCIEDADE DE ECONOMIA MISTA"		
# 3000;"ENTIDADES SEM FINS LUCRATIVOS"	
# 3069;"FUNDACAO PRIVADA"
natureza_juridica <- subset(natureza_juridica, natureza_juridica$CO_NATUREZA_JUR %in% c(1000, 1228, 1236, 1244, 1252, 1260, 
											1279, 2305, 2313, 3301, 1015, 1023, 
											1031, 1040, 1058, 1066, 1074, 1082, 
											1104, 1112, 1120, 1139, 1147, 1155, 
											1163, 1171, 1180, 1201, 1210, 2011, 
											2038, 3000, 3069))

estabelecimento <- merge(estabelecimento, tipo_unidade, by.x = "TP_UNIDADE", by.y =  "CO_TIPO_UNIDADE", all.y = T)
estabelecimento <- merge(estabelecimento, natureza_juridica, by = "CO_NATUREZA_JUR", all.y = T)
estabelecimento<- estabelecimento %>% select(CO_ESTADO_GESTOR, CO_MUNICIPIO_GESTOR, TIPO_ESTABELECIMENTO, CO_UNIDADE)

carga_horaria <- merge(profissionais, estabelecimento, by = "CO_UNIDADE")
carga_horaria <- carga_horaria %>%
	group_by(CO_ESTADO_GESTOR, CO_MUNICIPIO_GESTOR, TIPO_ESTABELECIMENTO) %>%
	summarise(HORA_MEDICO = sum(QT_CARGA_HORARIA_AMBULATORIAL))

cod_municipios <- read_csv("dados/bases_ibge/RELATORIO_DTB_BRASIL_MUNICIPIO.csv")
cod_municipios <- cod_municipios %>% select(UF, Nome_UF, Município, Nome_Município)
names(cod_municipios) <- c("CO_ESTADO_GESTOR", "ESTADO", "CO_MUNICIPIO_GESTOR", "MUNICIPIO")
cod_municipios$CO_MUNICIPIO_GESTOR <- paste0(cod_municipios$CO_ESTADO_GESTOR,cod_municipios$CO_MUNICIPIO_GESTOR)
cod_municipios$CO_MUNICIPIO_GESTOR <- substr(cod_municipios$CO_MUNICIPIO_GESTOR, 0,6)
carga_horaria$CO_MUNICIPIO_GESTOR <- as.character(carga_horaria$CO_MUNICIPIO_GESTOR)
carga_horaria <- merge(cod_municipios, carga_horaria, by = c("CO_ESTADO_GESTOR","CO_MUNICIPIO_GESTOR"))
carga_horaria$ESTADO <- NULL
carga_horaria$MUNICIPIO <- NULL
carga_horaria$CO_ESTADO_GESTOR <- NULL
names(carga_horaria)[1] <- "COD_MUNICIPIO"
carga_horaria <- dcast(carga_horaria, COD_MUNICIPIO+HORA_MEDICO~TIPO_ESTABELECIMENTO,value.var = "HORA_MEDICO")
carga_horaria$HORA_MEDICO <- NULL
names(carga_horaria)[2] <- "HOSP_EMERG" 
carga_horaria$ANO <- "2017"

#População por município proj tcu - http://tabnet.datasus.gov.br/cgi/tabcgi.exe?ibge/cnv/poptbr.def
est_pop <- read_delim("dados/ministerio_da_saude/est_pop_municipio_2016_2017.csv", 
    ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
    trim_ws = TRUE)
names(est_pop) <- c("MUNICIPIO", "2016", "2017")
est_pop <- melt(est_pop)
names(est_pop) <- c("MUNICIPIO", "ANO", "POPULACAO")
est_pop$ANO <- as.numeric(as.character(est_pop$ANO))
est_pop$COD_MUNICIPIO <- substr(est_pop$MUNICIPIO, 0,6)
est_pop$MUNICIPIO <- substr(est_pop$MUNICIPIO, 8,100)


##População por faixa etária e sexo censo - 2010 #https://sidra.ibge.gov.br/tabela/200
faixa_etaria_sexo <- read_delim("dados/bases_ibge/faixa_etaria_municipio_censo_2010.csv", 
    ";", escape_double = FALSE, trim_ws = TRUE)
names(faixa_etaria_sexo) <- c("MUNICIPIO", "POP_FAIXA_ETARIA", "POP_MASC", "POP_FEM")
faixa_etaria_sexo$FAIXA_ETARIA <- sub("^[^;]*", "", faixa_etaria_sexo$MUNICIPIO)
faixa_etaria_sexo$FAIXA_ETARIA <- substr(faixa_etaria_sexo$FAIXA_ETARIA, 4,100)
faixa_etaria_sexo$FAIXA_ETARIA <- substr(faixa_etaria_sexo$FAIXA_ETARIA, 0,nchar(faixa_etaria_sexo$FAIXA_ETARIA)-2)
faixa_etaria_sexo$MUNICIPIO <- sub("\\;.*", "", faixa_etaria_sexo$MUNICIPIO)
faixa_etaria_sexo$MUNICIPIO <- substr(faixa_etaria_sexo$MUNICIPIO, 2,100) 
faixa_etaria_sexo$ESTADO <- sub(".*(?:\\((.*)\\)).*|.*", "\\1", faixa_etaria_sexo$MUNICIPIO)
faixa_etaria_sexo$MUNICIPIO <- sub(" *\\(.*", "", faixa_etaria_sexo$MUNICIPIO)
faixa_etaria_sexo$POP_FAIXA_ETARIA <- substr(faixa_etaria_sexo$POP_FAIXA_ETARIA, 3,100)
faixa_etaria_sexo$POP_FAIXA_ETARIA <- substr(faixa_etaria_sexo$POP_FAIXA_ETARIA, 0,nchar(faixa_etaria_sexo$POP_FAIXA_ETARIA)-2)
faixa_etaria_sexo$POP_MASC <- substr(faixa_etaria_sexo$POP_MASC, 3,100)
faixa_etaria_sexo$POP_MASC <- substr(faixa_etaria_sexo$POP_MASC, 0,nchar(faixa_etaria_sexo$POP_MASC)-2)
faixa_etaria_sexo$POP_FEM <- substr(faixa_etaria_sexo$POP_FEM, 3,100)
faixa_etaria_sexo$POP_FEM <- substr(faixa_etaria_sexo$POP_FEM, 0,nchar(faixa_etaria_sexo$POP_FEM)-6)
faixa_etaria_sexo[which(faixa_etaria_sexo$ESTADO == ""), colnames(faixa_etaria_sexo) == "ESTADO"] <- "BR"
faixa_etaria_sexo[which(faixa_etaria_sexo$POP_FAIXA_ETARIA == "..."),colnames(faixa_etaria_sexo) =="POP_FAIXA_ETARIA"] <- 1 #inserido 1 para evitar divisáo por 0 no cálculo das taxas
faixa_etaria_sexo[which(faixa_etaria_sexo$POP_MASC == "..."),colnames(faixa_etaria_sexo) =="POP_MASC"] <- 1 #inserido 1 para evitar divisáo por 0 no cálculo das taxas
faixa_etaria_sexo[which(faixa_etaria_sexo$POP_FEM == "..."),colnames(faixa_etaria_sexo) =="POP_FEM"] <- 1 #inserido 1 para evitar divisáo por 0 no cálculo das taxas
faixa_etaria_sexo[which(faixa_etaria_sexo$POP_FAIXA_ETARIA == "-"),colnames(faixa_etaria_sexo) =="POP_FAIXA_ETARIA"] <- 1 #inserido 1 para evitar divisáo por 0 no cálculo das taxas
faixa_etaria_sexo[which(faixa_etaria_sexo$POP_MASC == "-"),colnames(faixa_etaria_sexo) =="POP_MASC"] <- 1 #inserido 1 para evitar divisáo por 0 no cálculo das taxas
faixa_etaria_sexo[which(faixa_etaria_sexo$POP_FEM == "-"),colnames(faixa_etaria_sexo) =="POP_FEM"] <- 1 #inserido 1 para evitar divisáo por 0 no cálculo das taxas
faixa_etaria_sexo$POP_FAIXA_ETARIA <- as.numeric(as.character(faixa_etaria_sexo$POP_FAIXA_ETARIA))
faixa_etaria_sexo$POP_MASC <- as.numeric(as.character(faixa_etaria_sexo$POP_MASC))
faixa_etaria_sexo$POP_FEM <- as.numeric(as.character(faixa_etaria_sexo$POP_FEM))
faixa_etaria <- faixa_etaria_sexo[,colnames(faixa_etaria_sexo) %in% c("ESTADO", "MUNICIPIO", "FAIXA_ETARIA", "POP_FAIXA_ETARIA")]
sexo <- faixa_etaria_sexo[,colnames(faixa_etaria_sexo) %in% c("ESTADO", "MUNICIPIO", "POP_MASC", "POP_FEM", "FAIXA_ETARIA")]
faixa_etaria <- dcast(faixa_etaria,formula = ESTADO + MUNICIPIO ~ FAIXA_ETARIA, value.var = "POP_FAIXA_ETARIA", fun.aggregate = sum, na.rm = T)
faixa_etaria$Var.3 <- NULL
faixa_etaria$`Idade ignorada` <- NULL
faixa_etaria$`Grupo de idade` <- NULL


faixa_etaria <- faixa_etaria[complete.cases(faixa_etaria),]
faixa_etaria <- melt(faixa_etaria, id.vars = c("ESTADO", "MUNICIPIO"))
names(faixa_etaria) <- c("ESTADO", "MUNICIPIO", "FAIXA_ETARIA", "POP_FAIXA_ETARIA")

faixa_etaria_total <- subset(faixa_etaria, faixa_etaria$FAIXA_ETARIA == "Total") 

faixa_etaria_0a4 <- subset(faixa_etaria, faixa_etaria$FAIXA_ETARIA %in% c("0 a 4 anos"))


faixa_etaria_mais60 <- subset(faixa_etaria, faixa_etaria$FAIXA_ETARIA %in% c("60 a 64 anos","65 a 69 anos", "70 a 74 anos",
									    "75 a 79 anos", "80 a 84 anos", "85 a 89 anos", 
   									     "90 a 94 anos", "95 a 99 anos", "100 anos ou mais"))	
faixa_etaria_mais60 <- faixa_etaria_mais60 %>%
	group_by(ESTADO, MUNICIPIO) %>%
	summarize(POP_FAIXA_ETARIA = sum(POP_FAIXA_ETARIA))
faixa_etaria_mais60$FAIXA_ETARIA <- "mais de 60 anos"
faixa_etaria_mais60 <- as.data.frame(faixa_etaria_mais60)

faixa_etaria <- rbind(faixa_etaria_0a4, faixa_etaria_mais60, faixa_etaria_total) %>% as.data.frame()
 
faixa_etaria <- dcast(faixa_etaria, ESTADO + MUNICIPIO ~ FAIXA_ETARIA, value.var = "POP_FAIXA_ETARIA")
faixa_etaria$TX_MENOS_05 <- faixa_etaria$`0 a 4 anos`/faixa_etaria$Total
faixa_etaria$TX_MAIS_60 <- faixa_etaria$`mais de 60 anos`/faixa_etaria$Total
faixa_etaria$`0 a 4 anos` <- NULL
faixa_etaria$`mais de 60 anos` <- NULL

#Base com código dos estados para fazer merge das faixas etárias com as demais bases, uma vez que há municípios com o mesmo nome
estados <- read_csv("dados/estados.csv")
faixa_etaria <- merge(estados, faixa_etaria, by.x = "SIGLA", by.y = "ESTADO", all = T)
faixa_etaria$ANO <- 2016

##PIB por município 2010 a 2017 - https://www.ibge.gov.br/estatisticas/economicas/contas-nacionais/9088-produto-interno-bruto-dos-municipios.html?=&t=downloads
pib <- read_csv("dados/bases_ibge/pib_municipios_2010_2017.csv", 
    locale = locale())
pib <- pib[,c(7,8,1,39,40)]
pib <- subset(pib, pib$Ano %in% c("2016", "2017"))
names(pib) <- c("COD_MUNICIPIO","MUNICIPIO", "ANO", "PIB", "PIB_PER_CAPITA")
pib$COD_MUNICIPIO <- as.character(pib$COD_MUNICIPIO)
pib$COD_MUNICIPIO <- substr(pib$COD_MUNICIPIO, 0,6) #O código aqui tem um número a mais
pib <- as.data.frame(pib)
pib$MUNICIPIO <- NULL

##Internações por doenças pulmonares 2016-2017
internacao <- read_csv("dados/ministerio_da_saude/internacao_doenca_pulmonar_cap_x.csv")
internacao <- internacao %>% select(Município, "2016", "2017")
internacao[is.na(internacao$`2016`), colnames(internacao) == "2016"] <- 0
internacao[is.na(internacao$`2017`), colnames(internacao) == "2017"] <- 0
internacao$`2016` <- as.numeric(internacao$`2016`)
internacao$`2017` <- as.numeric(internacao$`2017`)
internacao <- melt(internacao)
names(internacao) <- c("MUNICIPIO", "ANO", "INTERNACAO")
internacao$COD_MUNICIPIO <- substr(internacao$MUNICIPIO,0,6)
internacao$MUNICIPIO <- substr(internacao$MUNICIPIO,8,100)

#Beneficiários de Plano de Saúde - será utilizado junho de 2016 http://www.ans.gov.br/anstabnet/cgi-bin/tabnet?dados/tabnet_02.def
plano_saude <- read_delim("dados/beneficiarios_plano_saude.csv", 
    ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
    trim_ws = TRUE, skip = 3)
plano_saude <- plano_saude[,c(1,4)]
names(plano_saude) <- c("MUNICIPIO", "PLANO_SAUDE")
plano_saude$COD_MUNICIPIO <- substr(plano_saude$MUNICIPIO, 0,6)
plano_saude$MUNICIPIO <- substr(plano_saude$MUNICIPIO, 8,100)
plano_saude$ANO <- "2016"
plano_saude$MUNICIPIO <- NULL

##Merge das bases
base <- merge(internacao, carga_horaria, by = c("COD_MUNICIPIO", "ANO"), all = T)
base <- merge(base, pib, by = c("COD_MUNICIPIO", "ANO"), all = T)
base <- merge(base, plano_saude, by = c("COD_MUNICIPIO", "ANO"), all = T)
base <- merge(base, est_pop, by = c("COD_MUNICIPIO", "MUNICIPIO", "ANO"), all = T)
base$COD_ESTADO <- substr(base$COD_MUNICIPIO,0,2)
base$ANO <- as.numeric(as.character(base$ANO))
base <- merge(base, faixa_etaria, by = c("COD_ESTADO", "MUNICIPIO", "ANO"), all = T)

base <- base[!(base$MUNICIPIO %like% c("Município ignorado%")),]

base <- base[complete.cases(base$MUNICIPIO),]



base$TX_BENEF_PLANO_SAUDE <- base$PLANO_SAUDE/base$POPULACAO 
base$TX_HOSP_EMERG <- base$HOSP_EMERG/base$POPULACAO * 1000
base$TX_UBS <- base$UBS/base$POPULACAO * 1000 
base$TX_UPA <- base$UPA/base$POPULACAO * 1000 
base$TX_INTERNACAO <- base$INTERNACAO/base$POPULACAO * 1000 

base_2016 <- subset(base, base$ANO == 2016)
base_2017 <- subset(base, base$ANO == 2017)
base_2016 <- base_2016[,colnames(base_2016) %in% c("COD_MUNICIPIO", "MUNICIPIO", "POPULACAO", "PIB_PER_CAPITA", "TX_BENEF_PLANO_SAUDE", "TX_INTERNACAO",
						   "TX_MENOS_05","TX_MAIS_60")]
names(base_2016)[-c(1,2)] <- paste0(names(base_2016)[-c(1,2)],"_2016")
base_2017 <- base_2017[,colnames(base_2017) %in% c("COD_MUNICIPIO", "POPULACAO", "TX_HOSP_EMERG", "TX_UBS", "TX_UPA",
						   "TX_INTERNACAO", "TIPO_ESTABELECIMENTO")]
names(base_2017)[-1] <- paste0(names(base_2017)[-1],"_2017")

base_2017_hosp <- base_2017 %>% select(COD_MUNICIPIO, POPULACAO_2017, TX_INTERNACAO_2017, TX_HOSP_EMERG_2017)
base_2017_hosp <- na.omit(base_2017_hosp)
base_2017_ubs <- base_2017 %>% select(COD_MUNICIPIO, POPULACAO_2017, TX_INTERNACAO_2017, TX_UBS_2017)
base_2017_ubs <- na.omit(base_2017_ubs)
base_2017_upa <- base_2017 %>% select(COD_MUNICIPIO, POPULACAO_2017, TX_INTERNACAO_2017, TX_UPA_2017)
base_2017_upa <- na.omit(base_2017_upa)

dose_resp <- merge(base_2017_hosp, base_2017_ubs, by = c("COD_MUNICIPIO", "POPULACAO_2017", "TX_INTERNACAO_2017"), all = T)
dose_resp <- merge(dose_resp, base_2017_upa, by = c("COD_MUNICIPIO", "POPULACAO_2017", "TX_INTERNACAO_2017"), all = T)
dose_resp <- merge(dose_resp, base_2016, by = c("COD_MUNICIPIO"), all = T)

names(dose_resp)[10] <- "TX_MENOS_05_2010"
names(dose_resp)[11] <- "TX_MAIS_60_2010"

dose_resp[which(is.na(dose_resp$TX_HOSP_EMERG_2017)), names(dose_resp) == "TX_HOSP_EMERG_2017"] <-  0
dose_resp[which(is.na(dose_resp$TX_UBS_2017)), names(dose_resp) == "TX_UBS_2017"] <-  0
dose_resp[which(is.na(dose_resp$TX_UPA_2017)), names(dose_resp) == "TX_UPA_2017"] <-  0

dose_resp <- na.omit(dose_resp)

municipios <- unique(dose_resp$MUNICIPIO)

write.csv(dose_resp, "dados/dose_resp.csv", row.names = F)

