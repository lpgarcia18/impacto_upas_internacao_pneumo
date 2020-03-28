# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)
# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(causaldrf)
library(MASS)
library(caret)
library(gbm)
library(WeightIt)
library(twang)
library(ggpubr)
library(stringr)
library(matrixStats)
library(reshape2)
library(ggpubr)
library(wBoot)

# Base --------------------------------------------------------------------
dose_resp <- read_csv("dados/dose_resp.csv", locale = locale(encoding = "WINDOWS-1252"))
dose_resp <- subset(dose_resp, dose_resp$POPULACAO_2017 > 100000)

# Descrição ---------------------------------------------------------------
ggplot(dose_resp, aes(TX_HOSP_EMERG_2017))+
	geom_density()
	
ggplot(dose_resp, aes(TX_UBS_2017))+
	geom_density()

ggplot(dose_resp, aes(TX_UPA_2017))+
	geom_density()


descricao <- summary(dose_resp)
write.csv(descricao, "resultados/descricao.csv", row.names = F)

dose_resp$MUNICIPIO <- as.factor(dose_resp$MUNICIPIO)


# Análise -----------------------------------------------------------------
grid <- c(0,1,2,3,4,5)
numb_tree <- 1000


# UPAs --------------------------------------------------------------------
set.seed(1)
upa_total_bart <- bart_est(Y = TX_INTERNACAO_2017,
		      treat = TX_UPA_2017, 
		      outcome_formula = TX_INTERNACAO_2017 ~
					  TX_UPA_2017 +
				 		TX_HOSP_EMERG_2017 +
					  	TX_UBS_2017 + 
				      		TX_BENEF_PLANO_SAUDE_2016 +
						PIB_PER_CAPITA_2016 +
					 	POPULACAO_2016 +
			   			TX_MENOS_05_2010 +
			   			TX_MAIS_60_2010,
		      data = dose_resp,
		      ntree = numb_tree,
		      grid_val = grid)

save(upa_total_bart, file = "upa_total_bart.RData")

 
# load(file = "upa_total_bart.RData")



data_plot_func <- function(base, base_bart){
	
	nt <- nrow(base) #os cálculos são feitos para cada município e para cada nível de tratamento. Por isso, utiliza-se o número de linhas da base que é o número de municípios
	
	base_0 <- base_bart[[2]]$yhat.test[,1:(nt)] #Primeiro nível de tratamento
	base_1 <- base_bart[[2]]$yhat.test[,(nt+1):(2*nt)] # Segundo nível de tratamento e assim por diante
	base_2 <- base_bart[[2]]$yhat.test[,(2*nt+1):(3*nt)]
	base_3 <- base_bart[[2]]$yhat.test[,(3*nt+1):(4*nt)]
	base_4 <- base_bart[[2]]$yhat.test[,(4*nt+1):(5*nt)]
	base_5 <- base_bart[[2]]$yhat.test[,(5*nt+1):(6*nt)]
	
	ci.fun <- function(a){
	c(quantile(a,.025),quantile(a,.975))
	}
	
	base_cis_0 <- ci.fun(base_0)
	base_cis_1 <- ci.fun(base_1)
	base_cis_2 <- ci.fun(base_2)
	base_cis_3 <- ci.fun(base_3)
	base_cis_4 <- ci.fun(base_4)
	base_cis_5 <- ci.fun(base_5)
	
	base_plot <- rbind(base_cis_0, 
			      base_cis_1, 
			      base_cis_2, 
			      base_cis_3, 
			      base_cis_4,
			      base_cis_5) %>% as.data.frame()
	
	base_plot$ADRF <- summary(base_bart)
	
	return(base_plot)
}




upa_total_plot <- data_plot_func(dose_resp, upa_total_bart)

# Resultados --------------------------------------------------------------
resultados <- upa_total_plot
resultados
resultados$TAXA <- grid
write.csv(resultados, "resultados/adrf.csv", row.names = F)



#Comparação dos efeitso de cada níveL de tratamento dos tipos de pontos de atenção 
comp_efeito_func <- function(base, base_bart){
	nt <- nrow(base) #os cálculos são feitos para cada município e para cada nível de tratamento. Por isso, utiliza-se o número de linhas da base que é o número de municípios
	
	base_0 <- base_bart[[2]]$yhat.test[,1:(nt)] #Primeiro nível de tratamento
	base_1 <- base_bart[[2]]$yhat.test[,(nt+1):(2*nt)] # Segundo nível de tratamento e assim por diante
	base_2 <- base_bart[[2]]$yhat.test[,(2*nt+1):(3*nt)]
	base_3 <- base_bart[[2]]$yhat.test[,(3*nt+1):(4*nt)]
	base_4 <- base_bart[[2]]$yhat.test[,(4*nt+1):(5*nt)]
	base_5 <- base_bart[[2]]$yhat.test[,(5*nt+1):(6*nt)]

	base_0_med <- colMeans(base_0)
	base_1_med <- colMeans(base_1)
	base_2_med <- colMeans(base_2)
	base_3_med <- colMeans(base_3)
	base_4_med <- colMeans(base_4)
	base_5_med <- colMeans(base_5)
	
	base_med <- rbind(base_0_med,
	      base_1_med,
	      base_2_med,
	      base_3_med,
	      base_4_med,
	      base_5_med) %>% t() %>% as.data.frame()
	
	return(base_med)
}
	

upa_total_med <- comp_efeito_func(dose_resp, upa_total_bart)


adrf_municipios <- upa_total_med
adrf_municipios$MUNICIPIOS <- dose_resp$MUNICIPIO

#Bias-corrected with acceleration constant bootstrap (BCa)  para cálculo do intervalo de confiança
nboost <- 5000
#Nível 1
set.seed(1)
pb_table_0_1 <- boot.paired.bca(adrf_municipios$base_0_med, adrf_municipios$base_1_med, alternative = "two.sided",
				conf.level = 0.95, type = NULL, R = nboost)
#Nível 2
set.seed(1)
pb_table_0_2 <- boot.paired.bca(adrf_municipios$base_0_med, adrf_municipios$base_2_med, alternative = "two.sided",
				conf.level = 0.95, type = NULL, R = nboost)
#Nível 3
set.seed(1)
pb_table_0_3 <- boot.paired.bca(adrf_municipios$base_0_med, adrf_municipios$base_3_med, alternative = "two.sided",
				conf.level = 0.95, type = NULL, R = nboost)
#Nível 4
set.seed(1)
pb_table_0_4 <- boot.paired.bca(adrf_municipios$base_0_med, adrf_municipios$base_4_med, alternative = "two.sided",
				conf.level = 0.95, type = NULL, R = nboost)
#Nível 5
set.seed(1)
pb_table_0_5 <- boot.paired.bca(adrf_municipios$base_0_med, adrf_municipios$base_5_med, alternative = "two.sided",
				conf.level = 0.95, type = NULL, R = nboost)

pb_table <- rbind(
	cbind(pb_table_0_1$Mean, pb_table_0_1$Confidence.interval),
	cbind(pb_table_0_2$Mean, pb_table_0_2$Confidence.interval),
	cbind(pb_table_0_3$Mean, pb_table_0_3$Confidence.interval),
	cbind(pb_table_0_4$Mean, pb_table_0_4$Confidence.interval),
	cbind(pb_table_0_5$Mean, pb_table_0_5$Confidence.interval)) %>% as.data.frame() 
pb_table$V1 <-  round(as.numeric(as.character(pb_table$V1)), digits = 4)
pb_table$V2 <- as.character(pb_table$V2)
pb_table$V2 <- substring(pb_table$V2, 2, nchar(pb_table$V2)-1)
pb_ic <- strsplit(pb_table$V2, ",")
pb_dif <- do.call(rbind, pb_ic) %>% as.data.frame()
names(pb_dif) <- c("2.5%", "97.5%")
pb_dif$DIFERENCA <- pb_table$V1
pb_dif$NIVEL <- c(1:5)

write.csv(pb_dif, "resultados/diferenca_adrf.csv", row.names = F)



# Florianópolis -----------------------------------------------------------
# O upa_total_bart contém uma base com 1000 linhas (representando as 1000 árvores utilizadas) e
#310 * 6 colunas (310 são os municípios com mais de 100.000hab e 6 são os níveis e tratamento)
#Floripa é o município 262 na base dose resposta. Assim, na upa_total_bart as colunas
#262 correspode ao nível 0 de tratamento para Floripa
#262+310 ao nível 1 para Floripa
#...
#262+310*5 ao nível 5 para Floripa

floripa <- 262
munic <- nrow(dose_resp)
grid_lenght <- length(grid)

index <- seq(floripa, grid_lenght*munic, by = munic)


ci.fun <- function(a){
	c(quantile(a,.025),quantile(a,.975))
}

adrf_floripa_ic <- list()
for (i in 1:grid_lenght){
  adrf_floripa_ic[[i]] <- ci.fun(upa_total_bart[[2]]$yhat.test[,index[i]])

}

adrf_floripa_ic <- do.call(rbind, adrf_floripa_ic) %>% as.data.frame() 
adrf_floripa <- adrf_floripa_ic
adrf_floripa$ADRF <- unlist(c(adrf_municipios[262,-7][1,])) 	
adrf_floripa$TAXA <- grid 
write.csv(adrf_floripa, "resultados/floripa_adrf.csv", row.names = F)


res_tot <- ggplot(resultados, aes(x = TAXA, y = ADRF, group = 1))+
	geom_line(size = 1.2, color = "red")+
	geom_ribbon(aes(ymin=resultados$`2.5%`, ymax= resultados$`97.5%`), alpha = 0.3)+
	geom_hline(yintercept= 0, linetype="dashed", color = "red")+
	theme_bw()+
	labs(y = "Taxa de Internação/ 100.000hab", 
	     x = "Taxa de Horas-Médico")+
	ylim(-1,10)


res_floripa <- ggplot(adrf_floripa, aes(x = TAXA, y = ADRF, group = 1))+
	geom_line(size = 1.2, color = "red")+
	geom_ribbon(aes(ymin=adrf_floripa$`2.5%`, ymax= adrf_floripa$`97.5%`), alpha = 0.3)+
	geom_hline(yintercept= 0, linetype="dashed", color = "red")+
	theme_bw()+
	labs(y = "Taxa de Internação/ 100.000hab", 
	     x = "Taxa de Horas-Médico")+
	ylim(-1,10)



jpeg("resultados/adrf.jpg" ,width = 36, height = 24, units = "cm", res = 1200)
ggarrange(res_tot, res_floripa,
          labels = c("A", "B"), nrow = 1, ncol = 2)
dev.off()

#Bias-corrected with acceleration constant bootstrap (BCa)  para cálculo do intervalo de confiança

#Nível 1
set.seed(1)
floripa_table_0_1 <- boot.paired.bca(upa_total_bart[[2]]$yhat.test[,index[1]], upa_total_bart[[2]]$yhat.test[,index[2]], alternative = "two.sided",
				conf.level = 0.95, type = NULL, R = nboost)
#Nível 2
set.seed(1)
floripa_table_0_2 <- boot.paired.bca(upa_total_bart[[2]]$yhat.test[,index[1]], upa_total_bart[[2]]$yhat.test[,index[3]], alternative = "two.sided",
				conf.level = 0.95, type = NULL, R = nboost)
#Nível 3
set.seed(1)
floripa_table_0_3 <- boot.paired.bca(upa_total_bart[[2]]$yhat.test[,index[1]], upa_total_bart[[2]]$yhat.test[,index[4]], alternative = "two.sided",
				conf.level = 0.95, type = NULL, R = nboost)
#Nível 4
set.seed(1)
floripa_table_0_4 <- boot.paired.bca(upa_total_bart[[2]]$yhat.test[,index[1]], upa_total_bart[[2]]$yhat.test[,index[5]], alternative = "two.sided",
				conf.level = 0.95, type = NULL, R = nboost)
#Nível 5
set.seed(1)
floripa_table_0_5 <- boot.paired.bca(upa_total_bart[[2]]$yhat.test[,index[1]], upa_total_bart[[2]]$yhat.test[,index[6]], alternative = "two.sided",
				conf.level = 0.95, type = NULL, R = nboost)

floripa_table <- rbind(
	cbind(floripa_table_0_1$Mean, floripa_table_0_1$Confidence.interval),
	cbind(floripa_table_0_2$Mean, floripa_table_0_2$Confidence.interval),
	cbind(floripa_table_0_3$Mean, floripa_table_0_3$Confidence.interval),
	cbind(floripa_table_0_4$Mean, floripa_table_0_4$Confidence.interval),
	cbind(floripa_table_0_5$Mean, floripa_table_0_5$Confidence.interval)) %>% as.data.frame() 
floripa_table$V1 <-  round(as.numeric(as.character(floripa_table$V1)), digits = 4)
floripa_table$V2 <- as.character(floripa_table$V2)
floripa_table$V2 <- substring(floripa_table$V2, 2, nchar(floripa_table$V2)-1)
floripa_ic <- strsplit(floripa_table$V2, ",")
floripa_dif <- do.call(rbind, floripa_ic) %>% as.data.frame()
names(floripa_dif) <- c("2.5%", "97.5%")
floripa_dif$DIFERENCA <- floripa_table$V1
floripa_dif$NIVEL <- c(1:5)

write.csv(floripa_dif, "resultados/diferenca_floripa_adrf.csv", row.names = F)


dif_tot <- ggplot(pb_dif, aes(x = NIVEL, y = DIFERENCA, group = 1))+
	geom_line(size = 1.2, color = "red")+
	geom_ribbon(aes(ymin=as.numeric(as.character(pb_dif$`2.5%`)), 
			ymax= as.numeric(as.character(pb_dif$`97.5%`))), alpha = 0.3)+
	geom_hline(yintercept= 0, linetype="dashed", color = "red")+
	theme_bw()+
	labs(y = "Diferença entre um dado nível e o nível 0", 
	     x = "Nível")+
	ylim(0,1.6)


dif_floripa <- ggplot(floripa_dif, aes(x = NIVEL, y = DIFERENCA, group = 1))+
	geom_line(size = 1.2, color = "red")+
	geom_ribbon(aes(ymin=as.numeric(as.character(floripa_dif$`2.5%`)), 
			ymax= as.numeric(as.character(floripa_dif$`97.5%`))), alpha = 0.3)+
	geom_hline(yintercept= 0, linetype="dashed", color = "red")+
	theme_bw()+
	labs(y = "Diferença entre um dado nível e o nível 0", 
	     x = "Nível")+
	ylim(0,1.6)



jpeg("resultados/dif_adrf.jpg" ,width = 36, height = 24, units = "cm", res = 1200)
ggarrange(dif_tot, dif_floripa,
          labels = c("A", "B"), nrow = 1, ncol = 2)
dev.off()



