install.packages("stargazer")
install.packages("haven")
install.packages("ggthemes")
install.packages("lmtest")
install.packages("margins")
install.packages("tidyverse")
install.packages("InformationValue")
install.packages("sjPlot")
install.packages("margins")


library(tidyverse)
library(readr)
library(sjPlot)
library(haven)
library(lmtest)
library(sandwich)
library(car)
library(stargazer)
library(ggthemes)
library(InformationValue)
library(margins)
library(dotwhisker)


# Tratamento dos dados do ESEB de 2022

ESEB2022 <- read_sav("https://github.com/ulissesgdm/Analise-de-dados/raw/master/Eseb_2022%20(2).sav")


########################################################################################


#Tratamento

Eseb1 <- select(ESEB2022, SbjNum, D01A_IDADE, Q01, Q03, Q02b, Q02f, D02, D03, REG, Q02a, Q02g, Q07d, Q07j, Q07e, Q07f, Q10P1b, Q10P2b, D10, D09_RENDAF, D12a, Q19, Q29, Q02g, Q17_2, Q17_5)


# Filtragem de entrevistados que não responderam as variáveis utilizadas na pesquisa

Eseb1 <- Eseb1 %>% filter(Q02b < 90, Q02f < 90, D03 < 90, Q17_5 <20, Q17_2 <20,
                          D09_RENDAF < 200000, D03 < 11) %>%
  mutate(D10 = as_factor(D10),
         D12A = as_factor(D12a),
         REG = as_factor(REG),
         Q10P2b = as_factor(Q10P2b),
         D02 = as_factor(D02),
         Q01 = as_factor(Q01),
         Q02g = as_factor(Q02g))

#Variável Voto


Eseb1 <- Eseb1 %>%
  mutate(Q10P2b = case_when(#Q10P2b == "Anulou o Voto" ~ "Branco/Nulo",
    #Q10P2b == "Votou em branco" ~ "Branco/Nulo",
    #Q10P2b == "Não Sabe/Não lembra" ~ "Não sabe/Não respondeu",
    #Q10P2b == "Não respondeu" ~ "Não sabe/Não respondeu",
    Q10P2b == "Lula" ~ "Lula",
    Q10P2b == "Jair Bolsonaro" ~ "Jair Bolsonaro",
  ))


#Variável religião

Outras <- levels(Eseb1$D10)[-c(3,5,96,97,98)]

Eseb1 <- Eseb1 %>%
  mutate(religiao = case_when(D10 %in% Outras ~ "Outras",
                              D10 == "Católica" ~ "Católica",
                              D10 == "Evangélica" ~ "Evangélica",
                              D10 == "Não tem religião" ~ "Não tem religião",
                              D10 == "É ATEU/AGNÓSTICO/ NÃO ACREDITA EM DEUS " ~ "Não tem religião"))


#Variável Raça

Outros <- levels(Eseb1$D12A)[c(4,5, 97, 98)]

Eseb1 <- Eseb1 %>%
  mutate(raca = case_when(D12A %in% Outros ~ "Outros",
                          D12A == "Preto" ~ "Preto",
                          D12A == "Pardo" ~ "Pardo",
                          D12A == "Branco" ~ "Branco"))




#Variável Polarização

Eseb1$apoio <- Eseb1$Q17_2 - Eseb1$Q17_5

Eseb1$polarizacao <- abs(Eseb1$apoio)


ggplot(Eseb1, aes(Q02g)) + geom_bar()


reg_apoio_tv <- lm(apoio ~ Q02b + Q01 + D09_RENDAF + D01A_IDADE + D02 + REG + D03 + religiao + raca, data = Eseb1)

summary(reg_apoio_tv)

plot_model(reg_apoio_tv)

reg_apoio_rs <- lm(apoio ~ Q02f + Q01 + D09_RENDAF + D01A_IDADE + D02 + REG + D03 + religiao + raca, data = Eseb1)

summary(reg_apoio_rs)

plot_model(reg_apoio_rs)


#Polarização Afetiva e Consumo de TV por voto

reg_pol_tv <- lm(polarizacao ~ Q02b*Q10P2b + Q01+ D09_RENDAF + D01A_IDADE + D02 + REG + D03 + religiao + raca, data = Eseb1)

summary(reg_pol_tv)

plot_model(reg_pol_tv,  type = "pred",
           terms = c("Q10P2b","Q02b"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) +labs(title ="TV e Polarização", x= "Voto 2º turno", y = "Polarização Afetiva") + ggplot2::labs(colour = "Dias de consumo - TV")


# Polarização Afetiva e Consumo de Rede Social por voto

reg_pol_rs <- lm(polarizacao ~ Q02f*Q10P2b + Q01+  D09_RENDAF + D01A_IDADE + D02 + REG + D03 + religiao + raca, data = Eseb1)

summary(reg_pol_rs)

plot_model(reg_pol_rs,  type = "pred",
           terms = c("Q10P2b","Q02f"), ci.lvl = 0.9)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) +labs(title ="Redes Sociais e Polarização", x= "Voto 2º turno", y = "Polarização Afetiva") + ggplot2::labs(colour = "Dias de consumo - TV")


#Polarização afetiva e uso diário de redes sociais


reg_pol_dia <- lm(polarizacao ~ Q02g + Q01+  D09_RENDAF + D01A_IDADE + D02 + REG + D03 + religiao + raca, data = Eseb1)

summary(reg_pol_dia)

plot_model(reg_pol_dia, type = "pred",
           terms = c("Q02g"), ci.lvl = 0.9) + labs(title = "Polarização e uso diário de Redes Sociais", x = "Uso diário de Redes Sociais", y = 'Polarização') + theme(axis.text.x = element_text(angle = 45, hjust = 1))









