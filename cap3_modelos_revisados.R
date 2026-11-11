###############################################################################
# Capítulo III — TV, mídias sociais e polarização (ESEB 2022)
# Script revisado: um único tratamento de dados, amostra comum, pesos,
# erros-padrão agrupados por município, efeitos marginais com IC 95%,
# teste direto do "afastamento" entre eleitores de Lula e Bolsonaro,
# índices de posicionamento e medidas alternativas de polarização afetiva.
#
# ATENÇÃO: este script não pôde ser executado no ambiente onde foi escrito
# (sem R). Rode bloco a bloco e confira as tabelas de verificação (table()).
###############################################################################

# Pacotes (instale uma vez; InformationValue saiu do CRAN e não é necessário)
# install.packages(c("tidyverse", "haven", "marginaleffects", "sandwich",
#                    "modelsummary", "psych"))

library(tidyverse)
library(haven)
library(marginaleffects)   # efeitos marginais, previsões e testes de diferença
library(sandwich)
library(modelsummary)

set.seed(2022)

# -----------------------------------------------------------------------------
# 1. Dados
# -----------------------------------------------------------------------------
ESEB2022 <- read_sav(
  "https://github.com/ulissesgdm/Analise-de-dados/raw/master/Eseb_2022%20(2).sav"
)

# Funções auxiliares ----------------------------------------------------------
# Converte códigos de "não sabe / não respondeu / não conhece" em NA
limpa <- function(x, max_valido) {
  x <- as.numeric(zap_labels(x))
  if_else(x > max_valido, NA_real_, x)
}
rotulo <- function(x) str_squish(as.character(as_factor(x)))

# -----------------------------------------------------------------------------
# 2. Tratamento (feito UMA vez para todos os modelos)
# -----------------------------------------------------------------------------
eseb <- ESEB2022 %>%
  transmute(
    id    = SbjNum,
    MUN   = MUN,
    peso  = as.numeric(peso),

    # Consumo semanal de notícias (0 a 7 dias)
    tv    = limpa(Q02b, 7),     # TV aberta/privada (Globo, Record)
    redes = limpa(Q02f, 7),     # redes sociais (Facebook, Twitter, WhatsApp, Telegram)
    # Q02g: só para quem respondeu 7 dias em Q02f (1 = uma vez/dia, 2 = várias, 3 = 10+ vezes)
    redes_freq = limpa(Q02g, 3),
    # Outros meios: controlam o "consumidor geral de notícias".
    # Confira no questionário o que é cada item (Q02a, Q02c, Q02d, Q02e).
    outros_meios = rowMeans(cbind(limpa(Q02a, 7), limpa(Q02c, 7),
                                  limpa(Q02d, 7), limpa(Q02e, 7)), na.rm = TRUE),

    # Voto no 2º turno: rótulos lidos do próprio arquivo (sem depender de grafia)
    voto = case_when(
      str_detect(rotulo(Q10P2b), regex("lula", ignore_case = TRUE))      ~ "Lula",
      str_detect(rotulo(Q10P2b), regex("bolsonaro", ignore_case = TRUE)) ~ "Bolsonaro",
      TRUE ~ "Nulo/branco/não votou/NR"
    ),

    # Controles
    interesse = factor(limpa(Q01, 4)),   # 1 = muito interessado ... 4 = nada
    idade     = as.numeric(D01A_IDADE),
    mulher    = as.numeric(str_detect(rotulo(D02), regex("^fem", ignore_case = TRUE))),
    educ      = limpa(D03, 10),
    renda_r   = as.numeric(zap_labels(D09_RENDAF)),
    regiao    = as_factor(REG),
    religiao  = case_when(
      str_detect(rotulo(D10), regex("^cat", ignore_case = TRUE))   ~ "Católica",
      str_detect(rotulo(D10), regex("^evang", ignore_case = TRUE)) ~ "Evangélica",
      str_detect(rotulo(D10), regex("ateu|tem religi", ignore_case = TRUE)) ~ "Sem religião",
      str_detect(rotulo(D10), regex("n.o sabe|n.o respondeu", ignore_case = TRUE)) ~ NA_character_,
      !is.na(D10) ~ "Outras"
    ),
    raca = case_when(
      rotulo(D12a) == "Preto"  ~ "Preto",
      rotulo(D12a) == "Pardo"  ~ "Pardo",
      rotulo(D12a) == "Branco" ~ "Branco",
      str_detect(rotulo(D12a), regex("n.o sabe|n.o respondeu", ignore_case = TRUE)) ~ NA_character_,
      !is.na(D12a) ~ "Outros"
    ),

    # Confiança nos meios (Q07f = mídia tradicional; Q07g = mídias sociais)
    # 1 = muita, 2 = alguma, 3 = pouca, 4 = nenhuma; 97/98 = NS/NR
    conf_trad_cat = limpa(Q07f, 4),
    conf_soc_cat  = limpa(Q07g, 4),

    # Termômetros (0–10): lideranças (Q17) e partidos (Q16)
    lula_t  = limpa(Q17_5, 10),
    bolso_t = limpa(Q17_2, 10),
    pt_t    = limpa(Q16_5, 10),
    pl_t    = limpa(Q16_2, 10),

    # 14 pautas (1 = a favor, 2 = contra; 3 = "depende", espontânea; 97/98 = NS/NR)
    across(Q31_1:Q31_14, ~ limpa(.x, 3), .names = "{.col}_dep"),   # guarda o "depende"
    across(Q31_1:Q31_14, ~ limpa(.x, 2))
  ) %>%
  mutate(
    voto      = factor(voto, levels = c("Lula", "Bolsonaro", "Nulo/branco/não votou/NR")),
    # Renda: valores absurdos/códigos de NS-NR viram NA; usa log
    renda_r   = if_else(renda_r >= 200000, NA_real_, renda_r),
    log_renda = log1p(renda_r),
    peso      = peso / mean(peso, na.rm = TRUE),
    # 1 = muita ou alguma confiança; 0 = pouca ou nenhuma
    conf_trad = as.numeric(conf_trad_cat <= 2),
    conf_soc  = as.numeric(conf_soc_cat <= 2)
  )

# VERIFICAÇÕES — rode e confira antes de seguir
table(eseb$voto, useNA = "ifany")
table(eseb$religiao, useNA = "ifany")   # "Sem religião" precisa aparecer
table(eseb$raca, useNA = "ifany")
summary(eseb[, c("tv", "redes", "outros_meios", "educ", "log_renda", "peso")])
table(eseb$redes)                        # distribuição de 0 a 7 dias

# -----------------------------------------------------------------------------
# 3. Pautas recodificadas: 1 = posição conservadora / à direita
# -----------------------------------------------------------------------------
# "favor = conservador" nas pautas 1, 4, 8, 10, 11, 14; nas demais, "contra".
# Auxílio Brasil (13) é programa do governo Bolsonaro: mede alinhamento
# governista, não ideologia — fica fora dos índices.
pautas <- tribble(
  ~var,     ~nome,                         ~favor_conserv, ~dimensao,
  "Q31_1",  "Redução da maioridade penal", TRUE,  "Costumes/segurança",
  "Q31_2",  "Casamento homoafetivo",       FALSE, "Costumes/segurança",
  "Q31_3",  "Adoção por casal gay",        FALSE, "Costumes/segurança",
  "Q31_4",  "Pena de morte",               TRUE,  "Costumes/segurança",
  "Q31_5",  "Descriminalização das drogas",FALSE, "Costumes/segurança",
  "Q31_6",  "Proibição da venda de armas", FALSE, "Costumes/segurança",
  "Q31_7",  "Legalização do aborto",       FALSE, "Costumes/segurança",
  "Q31_8",  "Prisão de mulher que abortou",TRUE,  "Costumes/segurança",
  "Q31_9",  "Cotas raciais",               FALSE, "Costumes/segurança",
  "Q31_10", "Reza nas escolas públicas",   TRUE,  "Costumes/segurança",
  "Q31_11", "Privatizações",               TRUE,  "Economia",
  "Q31_12", "Bolsa Família",               FALSE, "Economia",
  "Q31_13", "Auxílio Brasil",              TRUE,  "Governista",
  "Q31_14", "Militarização das escolas",   TRUE,  "Costumes/segurança"
)

for (i in seq_len(nrow(pautas))) {
  v <- pautas$var[i]
  eseb[[paste0("c_", v)]] <- if (pautas$favor_conserv[i]) {
    as.numeric(eseb[[v]] == 1)
  } else {
    as.numeric(eseb[[v]] == 2)
  }
}

# Índices: média das posições conservadoras (exige ao menos 2/3 das respostas)
itens_cost <- paste0("c_", pautas$var[pautas$dimensao == "Costumes/segurança"])
itens_econ <- paste0("c_", pautas$var[pautas$dimensao == "Economia"])

eseb <- eseb %>%
  mutate(
    n_cost      = rowSums(!is.na(across(all_of(itens_cost)))),
    idx_cost    = if_else(n_cost >= 8, rowMeans(across(all_of(itens_cost)), na.rm = TRUE), NA_real_),
    idx_econ    = rowMeans(across(all_of(itens_econ)), na.rm = TRUE),
    idx_conserv = rowMeans(across(all_of(c(itens_cost, itens_econ))), na.rm = TRUE)
  )

# Consistência interna do índice de costumes (alfa de Cronbach)
psych::alpha(eseb[, itens_cost], check.keys = FALSE)$total

# -----------------------------------------------------------------------------
# 4. Fórmula comum e amostra comum
# -----------------------------------------------------------------------------
controles <- "interesse + idade + mulher + educ + log_renda + religiao + raca + regiao + outros_meios"

# Amostra analítica comum para as pautas (casos completos nos regressores)
base_x <- eseb %>%
  filter(if_all(c(tv, redes, voto, interesse, idade, mulher, educ,
                  log_renda, religiao, raca, regiao, outros_meios, peso),
                ~ !is.na(.x)))
nrow(base_x)   # informe este N no texto

# -----------------------------------------------------------------------------
# 4b. Estatísticas descritivas (Tabelas da seção "Descrição dos dados")
# -----------------------------------------------------------------------------
wpct <- function(x, w) 100 * weighted.mean(x, w, na.rm = TRUE)

desc_grupo <- function(dd) {
  w <- dd$peso
  tibble(
    `N (entrevistas)`               = nrow(dd),
    `TV: média de dias`             = weighted.mean(dd$tv, w),
    `TV: % nenhum dia`              = wpct(dd$tv == 0, w),
    `TV: % todos os dias`           = wpct(dd$tv == 7, w),
    `Redes: média de dias`          = weighted.mean(dd$redes, w),
    `Redes: % nenhum dia`           = wpct(dd$redes == 0, w),
    `Redes: % todos os dias`        = wpct(dd$redes == 7, w),
    `Outros meios: média de dias`   = weighted.mean(dd$outros_meios, w),
    `% interessado ou muito`        = wpct(as.numeric(dd$interesse) <= 2, w),
    `Idade média`                   = weighted.mean(dd$idade, w),
    `% mulheres`                    = wpct(dd$mulher, w),
    `Renda mediana (R$)`            = median(dd$renda_r, na.rm = TRUE),
    `% católicos`                   = wpct(dd$religiao == "Católica", w),
    `% evangélicos`                 = wpct(dd$religiao == "Evangélica", w),
    `% sem religião`                = wpct(dd$religiao == "Sem religião", w),
    `% brancos`                     = wpct(dd$raca == "Branco", w),
    `% pardos`                      = wpct(dd$raca == "Pardo", w),
    `% pretos`                      = wpct(dd$raca == "Preto", w),
    `Índice de costumes`            = weighted.mean(dd$idx_cost, w, na.rm = TRUE),
    `Índice econômico`              = weighted.mean(dd$idx_econ, w, na.rm = TRUE),
    `Nota a Lula`                   = weighted.mean(dd$lula_t, w, na.rm = TRUE),
    `Nota a Bolsonaro`              = weighted.mean(dd$bolso_t, w, na.rm = TRUE),
    `Nota ao PT`                    = weighted.mean(dd$pt_t, w, na.rm = TRUE),
    `Nota ao PL`                    = weighted.mean(dd$pl_t, w, na.rm = TRUE),
    `|Lula − Bolsonaro|`            = weighted.mean(abs(dd$lula_t - dd$bolso_t), w, na.rm = TRUE)
  )
}

tab_descritiva <- bind_rows(
  desc_grupo(base_x) %>% mutate(grupo = "Total"),
  base_x %>% group_split(voto) %>% map_dfr(~ desc_grupo(.x) %>% mutate(grupo = as.character(.x$voto[1])))
) %>%
  pivot_longer(-grupo, names_to = "variavel") %>%
  pivot_wider(names_from = grupo, values_from = value)
print(tab_descritiva, n = 30)

# % com posição conservadora em cada pauta, por grupo
tab_pautas <- pautas %>%
  mutate(map_dfr(var, function(v) {
    x <- base_x[[paste0("c_", v)]]
    tibble(Total     = wpct(x, base_x$peso),
           Lula      = wpct(x[base_x$voto == "Lula"], base_x$peso[base_x$voto == "Lula"]),
           Bolsonaro = wpct(x[base_x$voto == "Bolsonaro"], base_x$peso[base_x$voto == "Bolsonaro"]),
           Outros    = wpct(x[base_x$voto == "Nulo/branco/não votou/NR"],
                            base_x$peso[base_x$voto == "Nulo/branco/não votou/NR"]))
  })) %>%
  mutate(`Dif. B − L` = Bolsonaro - Lula) %>%
  select(nome, Total:`Dif. B − L`)
print(tab_pautas)

# Exporta para o LaTeX (vírgula decimal)
# install.packages("kableExtra")
# kableExtra::kbl(tab_pautas, format = "latex", booktabs = TRUE, digits = 1,
#                 format.args = list(decimal.mark = ","))

# -----------------------------------------------------------------------------
# 5. Polarização ideológica — modelo por pauta (logit ponderado, EP por município)
# -----------------------------------------------------------------------------
roda_pauta <- function(v) {
  f <- as.formula(paste0("c_", v, " ~ tv*voto + redes*voto + ", controles))
  glm(f, data = base_x, weights = peso, family = quasibinomial)
}
modelos_pauta <- set_names(map(pautas$var, roda_pauta), pautas$nome)

# 5a. Efeito de 1 dia a mais de consumo sobre P(posição conservadora), por grupo
efeitos_pauta <- imap_dfr(modelos_pauta, function(m, nome) {
  avg_slopes(m, variables = c("tv", "redes"), by = "voto",
             vcov = ~MUN, wts = "peso") %>%
    as_tibble() %>% mutate(pauta = nome)
})

# 5b. TESTE DIRETO DA "BOCA DE JACARÉ":
# o efeito do consumo é diferente entre eleitores de Bolsonaro e de Lula?
# (diferença Bolsonaro − Lula nas inclinações; positivo = grupos se afastam,
#  já que Bolsonaro tende ao polo conservador)
# Obs.: a ordem das linhas segue os níveis de 'voto' (b1 = Lula, b2 = Bolsonaro).
# Em versões recentes do marginaleffects, confira a sintaxe de 'hypothesis'.
afastamento <- imap_dfr(modelos_pauta, function(m, nome) {
  map_dfr(c("tv", "redes"), function(meio) {
    avg_slopes(m, variables = meio, by = "voto", vcov = ~MUN, wts = "peso",
               hypothesis = "b2 - b1 = 0") %>%
      as_tibble() %>% mutate(pauta = nome, meio = meio)
  })
}) %>%
  # Correção para múltiplos testes (14 pautas × 2 meios)
  mutate(p_ajustado_BH = p.adjust(p.value, method = "BH"))

afastamento %>%
  select(pauta, meio, estimate, conf.low, conf.high, p.value, p_ajustado_BH) %>%
  arrange(meio, desc(estimate)) %>%
  print(n = 28)

# Gráfico de CONFERÊNCIA (a versão para a tese é a Figura B, na seção 11)
efeitos_pauta %>%
  filter(voto %in% c("Lula", "Bolsonaro")) %>%
  mutate(meio = if_else(term == "tv", "TV aberta", "Redes sociais")) %>%
  ggplot(aes(x = estimate, y = fct_rev(pauta), colour = voto)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(width = .5)) +
  facet_wrap(~ meio) +
  labs(x = "Variação na prob. de posição conservadora por dia adicional de consumo (IC 95%)",
       y = NULL, colour = "Voto 2º turno") +
  theme_minimal()

# -----------------------------------------------------------------------------
# 6. Modelo empilhado: uma estimativa geral para as 13 pautas ideológicas
# -----------------------------------------------------------------------------
longo <- base_x %>%
  select(id, MUN, peso, tv, redes, voto, interesse, idade, mulher, educ,
         log_renda, religiao, raca, regiao, outros_meios,
         all_of(c(itens_cost, itens_econ))) %>%
  pivot_longer(all_of(c(itens_cost, itens_econ)), names_to = "pauta",
               values_to = "conserv") %>%
  filter(!is.na(conserv))

m_empilhado <- glm(
  as.formula(paste("conserv ~ pauta + tv*voto + redes*voto +", controles)),
  data = longo, weights = peso, family = quasibinomial
)
# EP agrupado por município (que também agrupa as respostas do mesmo indivíduo)
avg_slopes(m_empilhado, variables = c("tv", "redes"), by = "voto",
           vcov = ~MUN, wts = "peso")
avg_slopes(m_empilhado, variables = "redes", by = "voto",
           vcov = ~MUN, wts = "peso", hypothesis = "b2 - b1 = 0")

# -----------------------------------------------------------------------------
# 7. Índices de posicionamento (MQO, como descrito no texto do capítulo)
# -----------------------------------------------------------------------------
m_idx_cost <- lm(as.formula(paste("idx_cost ~ tv*voto + redes*voto +", controles)),
                 data = base_x, weights = peso)
m_idx_econ <- lm(as.formula(paste("idx_econ ~ tv*voto + redes*voto +", controles)),
                 data = base_x, weights = peso)

modelsummary(list("Costumes" = m_idx_cost, "Economia" = m_idx_econ),
             vcov = ~MUN, stars = TRUE,
             coef_omit = "interesse|religiao|raca|regiao")

# Gráfico de CONFERÊNCIA (não é figura da tese; as figuras finais estão na seção 11)
plot_predictions(m_idx_cost, condition = c("redes", "voto"), vcov = ~MUN) +
  labs(x = "Dias por semana de notícias via redes sociais",
       y = "Índice de posições conservadoras (0–1)", colour = "Voto", fill = "Voto") +
  theme_minimal()

# Distância Bolsonaro − Lula em 0 e 7 dias, e se ela muda
avg_comparisons(m_idx_cost, variables = list(voto = c("Lula", "Bolsonaro")),
                by = "redes", newdata = datagrid(redes = c(0, 7), grid_type = "counterfactual"),
                vcov = ~MUN)

# -----------------------------------------------------------------------------
# 8. Não linearidade: consumo em faixas (0, 1–3, 4–6, 7 dias)
# -----------------------------------------------------------------------------
base_x <- base_x %>%
  mutate(redes_cat = cut(redes, c(-Inf, 0, 3, 6, 7), labels = c("0", "1–3", "4–6", "7")),
         tv_cat    = cut(tv,    c(-Inf, 0, 3, 6, 7), labels = c("0", "1–3", "4–6", "7")))
m_idx_cat <- lm(as.formula(paste("idx_cost ~ tv_cat*voto + redes_cat*voto +", controles)),
                data = base_x, weights = peso)
plot_predictions(m_idx_cat, condition = c("redes_cat", "voto"), vcov = ~MUN)   # conferência

# -----------------------------------------------------------------------------
# 8b. Intensidade: separa quem usa redes todos os dias pela frequência diária (Q02g)
# -----------------------------------------------------------------------------
base_x <- base_x %>%
  mutate(redes_int = case_when(
    redes == 0 ~ "Nenhum dia",
    redes < 7  ~ "1–6 dias",
    redes_freq == 1 ~ "Todo dia, 1 vez",
    redes_freq == 2 ~ "Todo dia, várias vezes",
    redes_freq == 3 ~ "Todo dia, 10+ vezes"
  ) %>% factor(levels = c("Nenhum dia", "1–6 dias", "Todo dia, 1 vez",
                          "Todo dia, várias vezes", "Todo dia, 10+ vezes")))
table(base_x$redes_int, useNA = "ifany")

m_int <- lm(as.formula(paste("idx_cost ~ tv*voto + redes_int*voto +", controles)),
            data = base_x, weights = peso)
prev_int <- avg_predictions(m_int,
  variables = list(redes_int = levels(base_x$redes_int), voto = c("Lula", "Bolsonaro")),
  vcov = ~MUN, wts = "peso")
prev_int
# Distância Bolsonaro − Lula em cada nível de intensidade
avg_comparisons(m_int, variables = list(voto = c("Lula", "Bolsonaro")), by = "redes_int",
                newdata = datagrid(redes_int = levels(base_x$redes_int), grid_type = "counterfactual"),
                vcov = ~MUN)

# -----------------------------------------------------------------------------
# 9. Polarização afetiva
# -----------------------------------------------------------------------------
base_af <- base_x %>%
  mutate(
    # (a) medida atual: diferença absoluta Lula × Bolsonaro
    af_lideres = abs(lula_t - bolso_t),
    af_partidos = abs(pt_t - pl_t),
    # (b) separando "amor ao próprio lado" e "rejeição ao outro lado"
    in_lider  = case_when(voto == "Lula" ~ lula_t,  voto == "Bolsonaro" ~ bolso_t),
    out_lider = case_when(voto == "Lula" ~ 10 - bolso_t, voto == "Bolsonaro" ~ 10 - lula_t),
    in_part   = case_when(voto == "Lula" ~ pt_t,  voto == "Bolsonaro" ~ pl_t),
    out_part  = case_when(voto == "Lula" ~ 10 - pl_t, voto == "Bolsonaro" ~ 10 - pt_t)
  )

m_af_lid <- lm(as.formula(paste("af_lideres ~ tv*voto + redes*voto +", controles)),
               data = base_af, weights = peso)
m_af_par <- lm(as.formula(paste("af_partidos ~ tv*voto + redes*voto +", controles)),
               data = base_af, weights = peso)

eleitores <- filter(base_af, voto %in% c("Lula", "Bolsonaro")) %>% droplevels()
m_in  <- lm(as.formula(paste("in_lider ~ tv*voto + redes*voto +", controles)),
            data = eleitores, weights = peso)
m_out <- lm(as.formula(paste("out_lider ~ tv*voto + redes*voto +", controles)),
            data = eleitores, weights = peso)
m_in_p  <- lm(as.formula(paste("in_part ~ tv*voto + redes*voto +", controles)),
              data = eleitores, weights = peso)
m_out_p <- lm(as.formula(paste("out_part ~ tv*voto + redes*voto +", controles)),
              data = eleitores, weights = peso)

modelsummary(list("Afetiva (líderes)" = m_af_lid, "Afetiva (partidos)" = m_af_par,
                  "Afeto ao próprio" = m_in, "Rejeição ao outro" = m_out),
             vcov = ~MUN, stars = TRUE,
             coef_omit = "interesse|religiao|raca|regiao")

# Efeito de 1 dia a mais de redes, por grupo, em cada medida
map(list(lideres = m_af_lid, partidos = m_af_par, in_lider = m_in, out_lider = m_out),
    ~ avg_slopes(.x, variables = c("redes", "tv"), by = "voto", vcov = ~MUN, wts = "peso"))

# Gráfico de CONFERÊNCIA (a versão para a tese é a Figura C, na seção 11)
plot_predictions(m_af_lid, condition = c("redes", "voto"), vcov = ~MUN) +
  labs(x = "Dias por semana de notícias via redes sociais",
       y = "Polarização afetiva (|Lula − Bolsonaro|, 0–10)",
       colour = "Voto", fill = "Voto") +
  theme_minimal()

# -----------------------------------------------------------------------------
# 9b. Confiança nos meios de comunicação (Q07f e Q07g)
# -----------------------------------------------------------------------------
# Tabela descritiva: distribuição ponderada (%) por voto, na amostra analítica
tab_conf <- base_x %>%
  filter(voto %in% c("Lula", "Bolsonaro")) %>%
  select(voto, peso, `Mídia tradicional` = conf_trad_cat, `Mídias sociais` = conf_soc_cat) %>%
  pivot_longer(c(`Mídia tradicional`, `Mídias sociais`), names_to = "meio", values_to = "resp") %>%
  filter(!is.na(resp)) %>%
  mutate(resp = factor(resp, 1:4, c("Muita", "Alguma", "Pouca", "Nenhuma"))) %>%
  count(meio, voto, resp, wt = peso) %>%
  group_by(meio, voto) %>% mutate(pct = round(100 * n / sum(n))) %>% ungroup() %>%
  select(-n) %>% pivot_wider(names_from = resp, values_from = pct)
tab_conf

# Logit com a mesma especificação dos demais modelos
m_conf <- list(
  "Mídia tradicional" = glm(as.formula(paste("conf_trad ~ tv*voto + redes*voto +", controles)),
                            data = base_x, weights = peso, family = quasibinomial),
  "Mídias sociais"    = glm(as.formula(paste("conf_soc ~ tv*voto + redes*voto +", controles)),
                            data = base_x, weights = peso, family = quasibinomial)
)

# (a) Diferença média entre eleitores de Lula e de Bolsonaro
map(m_conf, ~ avg_comparisons(.x, variables = list(voto = c("Bolsonaro", "Lula")),
                              vcov = ~MUN, wts = "peso"))
# (b) Consumo diário vs. nenhum consumo, por grupo (redes e TV)
map(m_conf, ~ avg_comparisons(.x, variables = list(redes = c(0, 7), tv = c(0, 7)),
                              by = "voto", vcov = ~MUN, wts = "peso"))
# (c) As inclinações das redes diferem entre os grupos?
map(m_conf, ~ avg_slopes(.x, variables = "redes", by = "voto", vcov = ~MUN, wts = "peso",
                         hypothesis = "b2 - b1 = 0"))

# -----------------------------------------------------------------------------
# 10. Robustez (para nota de rodapé ou apêndice)
# -----------------------------------------------------------------------------
# (a) sem pesos; (b) sem "outros meios"; (c) controlando autoposição ideológica
#     (Q19) — atenção: pode ser parte do mecanismo, então é teste conservador.
m_rob_sempeso <- update(m_idx_cost, weights = NULL)
m_rob_semmeios <- update(m_idx_cost, . ~ . - outros_meios)
modelsummary(list("Principal" = m_idx_cost, "Sem pesos" = m_rob_sempeso,
                  "Sem outros meios" = m_rob_semmeios),
             vcov = ~MUN, stars = TRUE, coef_omit = "interesse|religiao|raca|regiao")

# (c2) "Depende" (resposta espontânea) como posição intermediária (0,5) em vez de ausente
for (i in seq_len(nrow(pautas))) {
  v <- paste0(pautas$var[i], "_dep")
  x <- base_x[[v]]
  base_x[[paste0("d_", pautas$var[i])]] <- case_when(
    x == 3 ~ 0.5,
    x == 1 ~ as.numeric(pautas$favor_conserv[i]),
    x == 2 ~ as.numeric(!pautas$favor_conserv[i]))
}
itens_cost_dep <- paste0("d_", pautas$var[pautas$dimensao == "Costumes/segurança"])
base_x <- base_x %>%
  mutate(idx_cost_dep = if_else(rowSums(!is.na(across(all_of(itens_cost_dep)))) >= 8,
                                rowMeans(across(all_of(itens_cost_dep)), na.rm = TRUE), NA_real_))
m_rob_dep <- update(m_idx_cost, idx_cost_dep ~ .)
avg_slopes(m_rob_dep, variables = "redes", by = "voto", vcov = ~MUN, wts = "peso",
           hypothesis = "b2 - b1 = 0")

# (d) Sensibilidade a confundidores não observados (opcional):
# install.packages("sensemakr")
# sensemakr::sensemakr(lm(idx_cost ~ redes + voto + tv + idade + mulher + educ +
#                         log_renda + religiao + raca + regiao + outros_meios + interesse,
#                         data = base_x),
#                      treatment = "redes", benchmark_covariates = "educ")

###############################################################################
# 11. FIGURAS PARA A TESE
# Geram as Figuras A–F: aparecem no painel Plots e são salvas na pasta
# figuras/ dentro do diretório de trabalho (veja com getwd()),
# em PNG 300 dpi e PDF vetorial para o LaTeX.
# Rode depois das seções 1–9 (usam base_x, afastamento, eleitores etc.).
###############################################################################
dir.create("figuras", showWarnings = FALSE)

cores  <- c("Lula" = "#eb6834", "Bolsonaro" = "#2a78d6")   # testadas para daltonismo
formas <- c("Lula" = 16, "Bolsonaro" = 15)                 # forma diferente: legível em P&B
virgula <- scales::label_number(decimal.mark = ",", big.mark = ".", style_negative = "minus")
virgula2 <- scales::label_number(accuracy = .01, decimal.mark = ",")

tema_tese <- theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#e6e5e0", linewidth = .3),
        axis.line = element_line(colour = "#8a8984", linewidth = .3),
        strip.text = element_text(face = "bold", hjust = 0, size = 10),
        legend.position = "top", legend.title = element_blank(),
        plot.caption = element_text(hjust = 0, colour = "#52514e", size = 8))

salva <- function(p, nome, larg = 18, alt = 13) {
  print(p)   # mostra também no painel Plots do RStudio
  ggsave(file.path("figuras", paste0(nome, ".png")), p, width = larg, height = alt, units = "cm", dpi = 300)
  ggsave(file.path("figuras", paste0(nome, ".pdf")), p, width = larg, height = alt, units = "cm",
         device = cairo_pdf)
}

# -----------------------------------------------------------------------------
# Figura A — Índices de posicionamento por faixa de consumo (figura principal)
# -----------------------------------------------------------------------------
faixas <- c("0", "1–3", "4–6", "7")
rotulos_faixa <- c("0" = "Nenhum", "1–3" = "1–3 dias", "4–6" = "4–6 dias", "7" = "Todos os dias")

m_cat <- list(
  "Costumes e segurança" = lm(as.formula(paste("idx_cost ~ tv_cat*voto + redes_cat*voto +", controles)),
                              data = base_x, weights = peso),
  "Economia"             = lm(as.formula(paste("idx_econ ~ tv_cat*voto + redes_cat*voto +", controles)),
                              data = base_x, weights = peso)
)

prev_faixa <- function(m, var_cat, rotulo_meio) {
  vars <- list(factor(faixas, levels = faixas), factor(c("Lula", "Bolsonaro"), levels = levels(base_x$voto)))
  names(vars) <- c(var_cat, "voto")
  avg_predictions(m, variables = vars, vcov = ~MUN, wts = "peso") %>%
    as_tibble() %>%
    rename(faixa = all_of(var_cat)) %>%
    mutate(meio = rotulo_meio)
}

dados_A <- imap_dfr(m_cat, function(m, indice) {
  bind_rows(prev_faixa(m, "redes_cat", "Redes sociais"),
            prev_faixa(m, "tv_cat", "TV aberta")) %>%
    mutate(indice = indice)
}) %>%
  mutate(faixa = factor(faixa, levels = faixas),
         voto  = factor(voto, levels = c("Bolsonaro", "Lula")),
         meio  = factor(meio, levels = c("Redes sociais", "TV aberta")))

rotulo_dist <- dados_A %>%
  filter(faixa %in% c("0", "7")) %>%
  select(indice, meio, faixa, voto, estimate) %>%
  pivot_wider(names_from = c(voto, faixa), values_from = estimate) %>%
  mutate(txt = paste0("Distância: ", virgula2(Bolsonaro_0 - Lula_0),
                      " → ", virgula2(Bolsonaro_7 - Lula_7)))

fig_A <- ggplot(dados_A, aes(faixa, estimate, colour = voto, shape = voto, group = voto)) +
  geom_line(linewidth = .8, position = position_dodge(width = .15)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = .45, linewidth = .5,
                  position = position_dodge(width = .15)) +
  geom_text(data = rotulo_dist, aes(x = 0.6, y = Inf, label = txt), inherit.aes = FALSE,
            hjust = 0, vjust = 1.5, size = 2.9, colour = "#52514e") +
  facet_grid(indice ~ meio, scales = "free_y", switch = "y") +
  scale_x_discrete(labels = rotulos_faixa) +
  scale_y_continuous(labels = virgula) +
  scale_colour_manual(values = cores) + scale_shape_manual(values = formas) +
  labs(x = "Dias por semana em que acompanhou notícias da campanha pelo meio",
       y = "Índice de posições (0 = progressista, 1 = conservador)",
       caption = "Valores previstos com controles; barras = IC 95% (EP agrupados por município). Fonte: ESEB 2022.") +
  tema_tese + theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 90, hjust = .5))
salva(fig_A, "figA_indices_faixas", alt = 14)

# -----------------------------------------------------------------------------
# Figura B — Afastamento entre os grupos, pauta a pauta (substitui Figs. 2 e 3)
# -----------------------------------------------------------------------------
dados_B <- afastamento %>%
  filter(pauta != "Auxílio Brasil") %>%
  group_by(meio) %>%
  mutate(p_BH = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  mutate(meio = if_else(meio == "redes", "Redes sociais", "TV aberta"),
         sig  = if_else(p_BH < .05, "Significativo após correção", "Não significativo"),
         across(c(estimate, conf.low, conf.high), ~ .x * 100))

ordem <- dados_B %>% filter(meio == "Redes sociais") %>% arrange(estimate) %>% pull(pauta)

fig_B <- ggplot(dados_B, aes(estimate, factor(pauta, levels = ordem))) +
  geom_vline(xintercept = 0, colour = "#52514e", linewidth = .4) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), colour = "#52514e", linewidth = .6) +
  geom_point(aes(fill = sig), shape = 21, size = 2.6, stroke = .7, colour = "black") +
  scale_fill_manual(values = c("Significativo após correção" = "black", "Não significativo" = "white")) +
  scale_x_continuous(labels = virgula) +
  facet_wrap(~ meio) +
  labs(x = "Afastamento entre eleitores de Bolsonaro e de Lula por dia adicional de consumo\n(pontos percentuais; IC 95%; valores > 0 = grupos se afastam)",
       y = NULL,
       caption = "Correção de Benjamini-Hochberg para múltiplos testes. Fonte: ESEB 2022.") +
  tema_tese
salva(fig_B, "figB_pautas_afastamento", alt = 12)

# -----------------------------------------------------------------------------
# Figura C — Polarização afetiva decomposta (substitui Figs. 4 e 5)
# -----------------------------------------------------------------------------
m_afet <- list("Afeto pelo próprio líder"       = m_in,
               "Rejeição ao líder adversário"   = m_out,
               "Afeto pelo próprio partido"     = m_in_p,
               "Rejeição ao partido adversário" = m_out_p)

dados_C <- imap_dfr(m_afet, function(m, medida) {
  avg_slopes(m, variables = c("redes", "tv"), by = "voto", vcov = ~MUN, wts = "peso") %>%
    as_tibble() %>% mutate(medida = medida)
}) %>%
  mutate(across(c(estimate, conf.low, conf.high), ~ .x * 7),   # 7 dias vs. nenhum
         meio   = if_else(term == "redes", "Redes sociais", "TV aberta"),
         medida = factor(medida, levels = rev(names(m_afet))),
         voto   = factor(voto, levels = c("Lula", "Bolsonaro")))

fig_C <- ggplot(dados_C, aes(estimate, medida, colour = voto, shape = voto)) +
  geom_vline(xintercept = 0, colour = "#52514e", linewidth = .4) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = .45, linewidth = .6,
                  position = position_dodge(width = .5)) +
  facet_wrap(~ meio) +
  scale_colour_manual(values = cores, breaks = c("Bolsonaro", "Lula")) +
  scale_shape_manual(values = formas, breaks = c("Bolsonaro", "Lula")) +
  scale_x_continuous(labels = virgula) +
  labs(x = "Diferença prevista entre consumo diário e nenhum consumo (escala 0–10; IC 95%)",
       y = NULL, caption = "Apenas eleitores de Lula e de Bolsonaro no 2º turno. Fonte: ESEB 2022.") +
  tema_tese
salva(fig_C, "figC_afetiva_decomposta", alt = 10)

# -----------------------------------------------------------------------------
# Figura D (apêndice) — Distribuição do consumo semanal
# -----------------------------------------------------------------------------
dados_D <- base_x %>%
  select(`Redes sociais` = redes, `TV aberta` = tv) %>%
  pivot_longer(everything(), names_to = "meio", values_to = "dias") %>%
  count(meio, dias) %>% group_by(meio) %>% mutate(pct = n / sum(n) * 100)

fig_D <- ggplot(dados_D, aes(factor(dias), pct)) +
  geom_col(fill = "#2a78d6", width = .7) +
  facet_wrap(~ meio) +
  scale_y_continuous(labels = virgula) +
  labs(x = "Dias por semana", y = "% dos entrevistados", caption = "Fonte: ESEB 2022.") +
  tema_tese
salva(fig_D, "figD_distribuicao_consumo", alt = 7)

# -----------------------------------------------------------------------------
# Figura E — Gradiente de intensidade do uso de redes (Q02f + Q02g)
# -----------------------------------------------------------------------------
dados_E <- as_tibble(prev_int) %>%
  mutate(voto = factor(voto, levels = c("Bolsonaro", "Lula")),
         redes_int = factor(redes_int, levels = levels(base_x$redes_int)))

fig_E <- ggplot(dados_E, aes(redes_int, estimate, colour = voto, shape = voto, group = voto)) +
  geom_line(linewidth = .8, position = position_dodge(width = .15)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = .45, linewidth = .5,
                  position = position_dodge(width = .15)) +
  scale_colour_manual(values = cores) + scale_shape_manual(values = formas) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, 12)) +
  scale_y_continuous(labels = virgula) +
  labs(x = "Uso de redes sociais para acompanhar notícias da campanha",
       y = "Índice de costumes e segurança\n(0 = progressista, 1 = conservador)",
       caption = "Valores previstos com controles; barras = IC 95%. Fonte: ESEB 2022 (Q02f e Q02g).") +
  tema_tese
salva(fig_E, "figE_intensidade_redes", alt = 10)

# -----------------------------------------------------------------------------
# Figura F — Confiança nos meios de comunicação por uso de redes sociais
# -----------------------------------------------------------------------------
dados_F <- imap_dfr(m_conf, function(m, meio) {
  avg_predictions(m, variables = list(redes = 0:7,
                                      voto = factor(c("Lula", "Bolsonaro"), levels = levels(base_x$voto))),
                  vcov = ~MUN, wts = "peso") %>%
    as_tibble() %>% mutate(meio = meio)
}) %>%
  mutate(voto = factor(voto, levels = c("Lula", "Bolsonaro")),
         meio = factor(meio, levels = names(m_conf)))

rotulos_F <- dados_F %>% filter(redes %in% c(0, 7))

fig_F <- ggplot(dados_F, aes(redes, estimate, colour = voto, fill = voto, shape = voto)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .14, colour = NA) +
  geom_line(linewidth = .9) +
  geom_point(data = rotulos_F, size = 2.6) +
  geom_text(data = rotulos_F, aes(label = scales::percent(estimate, accuracy = 1),
                                  hjust = if_else(redes == 0, 1.4, -0.4)),
            size = 2.8, colour = "#3a3a38", show.legend = FALSE) +
  facet_wrap(~ meio) +
  scale_x_continuous(breaks = 0:7, expand = expansion(add = c(1, 1))) +
  scale_y_continuous(labels = scales::percent, limits = c(0, .8)) +
  scale_colour_manual(values = cores) + scale_fill_manual(values = cores) +
  scale_shape_manual(values = formas) +
  labs(x = "Dias por semana de uso de mídias sociais para acompanhar a campanha",
       y = "Muita ou alguma confiança",
       caption = "Valores previstos com controles; faixas = IC 95% (EP agrupados por município). Fonte: ESEB 2022 (Q07f e Q07g).") +
  tema_tese
salva(fig_F, "figF_confianca_midia", alt = 9)
