
PQS_BRASIL <- import(file = "PQS.BRASIL. 01.05.2020.sav")


#RECODIFICANDO VARIAVEL 
PQS_BRASIL$Sexo <- as.character(PQS_BRASIL$Sexo)
PQS_BRASIL$Sexo_Rec <- as.factor(PQS_BRASIL$Sexo)
PQS_BRASIL$Sexo_Rec <- factor(PQS_BRASIL$Sexo_Rec , levels = c("Feminino", "Masculino"))

#calculando a frequencia
PQS_BRASIL$Freq <- 1

OBJETO_1 <- PQS_BRASIL %>% group_by(Sexo) %>% summarise(qtd = sum(Freq))
OBJETO_1$percentual <- round(OBJETO_1$qtd*100/601, digits = 1)
OBJETO_1$Sexo_Rec <-
  ifelse((OBJETO_1$Sexo == "1"),"Feminino","Masculino")


#grafico de pizza
graf.distribuicao.bolsas.sexo <- 
  ggplot(OBJETO_1, aes(x ="", y = qtd, fill = Sexo_Rec)) + 
  geom_bar(width = 1, stat = "identity", color = "black") + 
  coord_polar("y", start = 0, direction = -1)+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.text.x=element_blank(),
    legend.title = element_blank())+
    scale_fill_manual(values = c("khaki1","khaki4"))

grafico1 <- grid.arrange(graf.distribuicao.bolsas.sexo,ncol=1,
             top = textGrob("Gráfico 1 - Distribuição dos bolsistas PQ por sexo",
                            gp=gpar(fontsize=12,font=1)),
             bottom = textGrob("Dados Obtidos a partir da pesquisa (2020)",
                               gp=gpar(fontsize=10,font=1)))


PQS_BRASIL$REC1 <-
  ifelse((PQS_BRASIL$Tipo.Bolsa == "1"),"PQ-1D",PQS_BRASIL$Tipo.Bolsa)
PQS_BRASIL$REC2 <-
  ifelse((PQS_BRASIL$REC1 == "2"),"PQ-1C",PQS_BRASIL$REC1)
PQS_BRASIL$REC3 <-
  ifelse((PQS_BRASIL$REC2 == "3"),"PQ-1B",PQS_BRASIL$REC2)
PQS_BRASIL$REC4 <-
  ifelse((PQS_BRASIL$REC3 == "4"),"PQ-1A",PQS_BRASIL$REC3)
PQS_BRASIL$REC5 <-
  ifelse((PQS_BRASIL$REC4 == "5"),"PQ-SR",PQS_BRASIL$REC4)
PQS_BRASIL$REC6 <-
  ifelse((PQS_BRASIL$REC5 == "6"),"PQ-2",PQS_BRASIL$REC5)

PQS_BRASIL[35:39] <- NULL
PQS_BRASIL$count <- NULL

PQS_BRASIL$Freq <- NULL

PQS_BRASIL$Contando <- 1
PQS_BRASIL$Contando <- as.character(PQS_BRASIL$Contando)
PQS_BRASIL$Contando <- as.numeric(PQS_BRASIL$Contando)

#grafico barras2
PQS_BRASIL %>% group_by(REC6)%>%
  mutate(REC6 = factor(REC6, levels=c(
    "PQ-1D", "PQ-1C", "PQ-1B", 
    "PQ-1A", "PQ-SR", "PQ-2")))%>% 
  summarise(qtd = sum(Contando))

TipoBolsa <- c("PQ-1D", "PQ-1C", "PQ-1B", "PQ-1A", "PQ-SR", "PQ-2")
Quantidade <- c(95,52,41,61,9,343)

OBJETO_2 <- data.frame(TipoBolsa, Quantidade)


graf.distribuicao.tipo.bolsa <- 
  ggplot(OBJETO_2, aes(x = TipoBolsa, y = Quantidade))+
  geom_bar(width = 0.5, stat = "identity",color = "black", fill = "khaki3")+
  scale_y_continuous(limits = c(0,400), expand = c(0,0))+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 10, colour = "black"),
      axis.text.y = element_text(size = 10, colour = "black"),
      axis.title.y = element_text(size=11, face="bold", colour = "black"))+
  labs(x = "", y = "Frequência")

grafico2 <- grid.arrange(graf.distribuicao.tipo.bolsa,ncol=1,
             top = textGrob("Gráfico 2 - Distribuição por tipos de bolsa PQ",
                            gp=gpar(fontsize=12,font=1)),
             bottom = textGrob("Dados Obtidos a partir da pesquisa (2020)",
                               gp=gpar(fontsize=10,font=1)))


#recodificando a variavel regiao
PQS_BRASIL$REG1 <-
  ifelse((PQS_BRASIL$Região == "1"),"Centro-Oeste",PQS_BRASIL$Região)
PQS_BRASIL$REG2 <-
  ifelse((PQS_BRASIL$REG1 == "2"),"Nordeste",PQS_BRASIL$REG1)
PQS_BRASIL$REG3 <-
  ifelse((PQS_BRASIL$REG2 == "3"),"Norte",PQS_BRASIL$REG2)
PQS_BRASIL$REG4 <-
  ifelse((PQS_BRASIL$REG3 == "4"),"Sudeste",PQS_BRASIL$REG3)
PQS_BRASIL$REG5 <-
  ifelse((PQS_BRASIL$REG4 == "5"),"Sul",PQS_BRASIL$REG4)
PQS_BRASIL$REG6 <-
  ifelse((PQS_BRASIL$REG5 == "6"),"99",PQS_BRASIL$REG5)

PQS_BRASIL[35:39] <- NULL

PQS_BRASIL %>%
  mutate(REG6 = factor(REG6, levels=c(
    "Centro-Oeste", "Nordeste", "Norte", 
    "Sudeste", "Sul", "99")))%>%
  group_by(REG6)%>%
  summarise(qtd = sum(Contando))

Regiao <- c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul")
Quantidade2 <- c(24,95,8,346,127)

OBJETO_3 <- data.frame(Regiao, Quantidade2)


graf.bolsa.regiao <- 
  ggplot(OBJETO_3, aes(x = Regiao, y = Quantidade2))+
  geom_bar(width = 0.5, stat = "identity", color = "black", fill = "khaki3")+
  scale_y_continuous(limits = c(0,400), expand = c(0,0))+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size=11, face="bold", colour = "black"))+
  labs(x = "", y = "Frequência")

grafico3 <- grid.arrange(graf.bolsa.regiao,ncol=1,
             top = textGrob("Gráfico 3 - Distribuição dos bolsistas PQ por região do Brasil",
                            gp=gpar(fontsize=12,font=1)),
             bottom = textGrob("Dados Obtidos a partir da pesquisa (2020)",
                               gp=gpar(fontsize=10,font=1)))

  

