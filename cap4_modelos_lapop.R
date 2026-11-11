###############################################################################
# Capítulo IV — Mídia, partidarismo e ressentimento (LAPOP Brasil 2025/2026)
#
# Reproduz os resultados descritos no texto do capítulo:
#   seção "Os dados" e "A variável dependente"  -> blocos 1 e 2
#   Figura 4.1 e perfil dos públicos            -> bloco 3
#   Figura 4.2 (lulistas x bolsonaristas)       -> bloco 4
#   Figuras 4.3 e 4.4 (escala de simpatia)      -> bloco 5
#   Figura 4.5 (frequência de consumo)          -> bloco 6
#   Figura 4.6 (percepção de mídia hostil)      -> bloco 7
#   Testes de robustez                          -> bloco 8
#   Figuras                                     -> bloco 9
#   Conferência com os valores do texto         -> bloco 10
#
# Estimação: MQO com o desenho amostral da LAPOP (estratos + UPM), via survey.
# Os valores pontuais foram conferidos fora do R (Python) contra o texto; os
# valores esperados estão no bloco 10. Rode bloco a bloco.
###############################################################################

# install.packages(c("tidyverse", "haven", "survey", "lavaan", "psych", "mice"))
library(tidyverse)
library(haven)
library(survey)

options(survey.lonely.psu = "adjust")

# -----------------------------------------------------------------------------
# 1. Dados e recodificação
# -----------------------------------------------------------------------------
# Base obtida diretamente do repositório da tese no GitHub
url_base <- "https://github.com/ulissesgdm/Tese/raw/refs/heads/main/BRA%202026%20CGD%20AmericasBarometer%20(v0.9.2%20PAVAO)%20(2).dta"
arq <- tempfile(fileext = ".dta")
download.file(url_base, arq, mode = "wb")   # mode = "wb": necessário no Windows (arquivo binário)
bruto <- read_dta(arq)            # .a/.b/.c (NS/NR/NA) entram como NA
stopifnot(nrow(bruto) == 1512)    # confere se a versão baixada é a usada no texto
num <- function(x) as.numeric(zap_labels(x))

lapop <- bruto %>%
  transmute(
    idnum, strata = num(strata), upm = num(upm), wt = num(wt),

    # Ressentimento: escala invertida (5 = concorda muito = mais ressentimento)
    across(braresstl1:braresstl9, ~ 6 - num(.x), .names = "r{str_remove(.col, 'braresstl')}"),

    # Fonte principal de notícias políticas (BRAMEDIA1); "Outro" (7) fica de fora
    midia = case_when(num(bramedia1) == 2 ~ "tv",
                      num(bramedia1) == 1 ~ "redes",
                      num(bramedia1) %in% 3:5 ~ "prof",   # jornais, rádio/podcasts, sites
                      num(bramedia1) == 0 ~ "nao"),
    midia_det = num(bramedia1),

    # Frequência de atenção às notícias (GI0N): diária x não diária
    gi0n   = num(gi0n),
    diario = if_else(gi0n == 1, 1, 0),
    freq3  = case_when(gi0n == 1 ~ "diaria", gi0n == 2 ~ "semanal", gi0n %in% 3:5 ~ "rara"),

    # Simpatia (1–10)
    lula = num(ptyidlula), bols = num(ptyidbols),

    # Notícias falsas
    b3 = num(bramedia3), b4 = num(bramedia4),
    conf_eleicoes = num(b47a), conf_stf = num(b31),

    # Controles
    renda    = num(q10inc) - 1500,                         # faixas 1–15
    raca     = case_when(num(etid) == 1 ~ "branca", num(etid) == 4 ~ "preta",
                         num(etid) == 5 ~ "parda",  !is.na(etid) ~ "outra"),
    mulher   = case_when(num(q1tc) == 2 ~ 1, num(q1tc) == 1 ~ 0),
    idade    = num(q2),
    religiao = case_when(num(q3cn) == 1 ~ "catolica",
                         num(q3cn) %in% c(2, 5) ~ "evangelica",
                         num(q3cn) %in% c(4, 11) ~ "sem",
                         !is.na(q3cn) ~ "outra"),
    regiao   = factor(num(strata)),
    educ     = num(edre),
    interesse = 5 - num(pol1)                               # 4 = muito interessado
  ) %>%
  mutate(
    econ = if_else(rowSums(!is.na(across(r1:r5))) >= 4, rowMeans(across(r1:r5), na.rm = TRUE), NA_real_),
    cult = if_else(rowSums(!is.na(across(r6:r9))) >= 3, rowMeans(across(r6:r9), na.rm = TRUE), NA_real_),
    alin   = lula - bols,                 # -9 (só Bolsonaro) a +9 (só Lula)
    intens = abs(alin),
    grupo = case_when(lula >= 6 & bols <= 5 ~ "lulista",
                      bols >= 6 & lula <= 5 ~ "bolsonarista",
                      !is.na(lula) & !is.na(bols) ~ "outros"),
    midia  = factor(midia, levels = c("tv", "redes", "prof", "nao")),
    grupo  = factor(grupo, levels = c("outros", "lulista", "bolsonarista")),
    raca     = factor(raca, levels = c("branca", "parda", "preta", "outra")),
    religiao = factor(religiao, levels = c("catolica", "evangelica", "sem", "outra"))
  )

# Verificações
nrow(lapop)                                  # 1.512
table(lapop$midia, useNA = "ifany")          # tv 440, redes 694, prof 200, nao 159
table(lapop$grupo, useNA = "ifany")          # lulista 537, bolsonarista 401, outros 557

des <- svydesign(ids = ~upm, strata = ~strata, weights = ~wt, data = lapop, nest = TRUE)
GL <- degf(des)                              # 120 graus de liberdade (UPM − estratos)

controles <- "renda + raca + mulher + idade + I(idade^2) + religiao + regiao + educ + interesse"
VDS <- c(econ = "Índice econômico", cult = "Índice cultural",
         r1 = "1. Apoio a quem não trabalha", r2 = "2. Assistência a quem não trabalha",
         r3 = "3. Impostos sustentam quem não precisa", r4 = "4. Impostos ajudam quem não precisa",
         r5 = "5. Taxados para financiar quem não precisa",
         r6 = "6. Gays e trans x família tradicional", r7 = "7. Identidades masculina/feminina",
         r8 = "8. Ações afirmativas", r9 = "9. Igualdade de gênero")

# -----------------------------------------------------------------------------
# Funções auxiliares
# -----------------------------------------------------------------------------
# Amostra analítica como domínio do desenho (mantém a variância correta)
dominio <- function(vars) {
  ok <- complete.cases(lapop[, vars])
  subset(des, ok)
}
vars_ctrl <- c("renda", "raca", "mulher", "idade", "religiao", "regiao", "educ", "interesse")

# Combinação linear dos coeficientes com teste t (gl do desenho)
comb <- function(m, cvec, rotulo = "") {
  b <- coef(m); V <- vcov(m)
  cvec <- cvec[names(b)]; cvec[is.na(cvec)] <- 0
  est <- sum(cvec * b); se <- sqrt(as.numeric(t(cvec) %*% V %*% cvec))
  tibble(termo = rotulo, est = est, ep = se, t = est / se,
         p = 2 * pt(-abs(est / se), df = GL),
         ic_inf = est - qt(.975, GL) * se, ic_sup = est + qt(.975, GL) * se)
}
vetor <- function(m, pesos) {             # pesos = c(nome_coef = peso, ...)
  faltam <- setdiff(names(pesos), names(coef(m)))
  if (length(faltam)) stop("Coeficientes inexistentes: ", paste(faltam, collapse = ", "))
  v <- setNames(rep(0, length(coef(m))), names(coef(m)))
  v[names(pesos)] <- pesos; v
}

# Valor esperado médio: previsão média na amostra analítica (nd = amostra com
# as variáveis de interesse já fixadas)
preve <- function(m, nd, rotulo = "") {
  tt <- delete.response(terms(m))
  X  <- model.matrix(tt, model.frame(tt, nd, xlev = m$xlevels, na.action = na.pass))
  cvec <- colMeans(X)
  comb(m, setNames(cvec, colnames(X)), rotulo)
}
# Diferença entre duas previsões médias (ex.: bolsonarista − lulista)
difere <- function(m, dados, a, b, rotulo = "") {
  tt <- delete.response(terms(m))
  mm <- function(nd) colMeans(model.matrix(tt, model.frame(tt, nd, xlev = m$xlevels, na.action = na.pass)))
  cvec <- mm(a(dados)) - mm(b(dados))
  comb(m, cvec, rotulo)
}

# -----------------------------------------------------------------------------
# 2. Medidas: consistência interna e análise fatorial confirmatória
# -----------------------------------------------------------------------------
psych::alpha(lapop %>% select(r1:r5))$total          # dimensão econômica
psych::alpha(lapop %>% select(r6:r9))$total          # dimensão cultural
psych::omega(lapop %>% select(r1:r9), nfactors = 2, plot = FALSE)$omega.tot

library(lavaan)
afc1 <- cfa("ress =~ r1 + r2 + r3 + r4 + r5 + r6 + r7 + r8 + r9",
            data = lapop, estimator = "MLR", missing = "fiml")
afc2 <- cfa("econ =~ r1 + r2 + r3 + r4 + r5
             cult =~ r6 + r7 + r8 + r9",
            data = lapop, estimator = "MLR", missing = "fiml")
fitMeasures(afc1, c("cfi.robust", "tli.robust", "rmsea.robust", "srmr"))
fitMeasures(afc2, c("cfi.robust", "tli.robust", "rmsea.robust", "srmr"))
anova(afc1, afc2)                               # 2 fatores x 1 fator
standardizedSolution(afc2) %>% filter(op %in% c("=~", "~~"), lhs != rhs | op == "=~")
# Texto: cargas fortes; r8 (ações afirmativas) é a mais fraca; dimensões correlacionadas

# -----------------------------------------------------------------------------
# 3. Onde o brasileiro se informa (Figura 4.1) e perfil dos públicos
# -----------------------------------------------------------------------------
fontes <- lapop %>% filter(!is.na(midia_det)) %>%
  count(midia_det) %>% mutate(pct = 100 * n / sum(n))
fontes        # redes 46%, TV 29%, profissional 3+4+6 = 13%, não consome 11%

lapop %>% filter(midia %in% c("redes", "tv")) %>% group_by(midia) %>%
  summarise(idade = mean(idade, na.rm = TRUE), educ = mean(educ, na.rm = TRUE),
            nota_lula = mean(lula, na.rm = TRUE), nota_bols = mean(bols, na.rm = TRUE))
# Texto: redes 35 anos, notas 4,3/4,3; TV 50 anos, Lula 5,7 x Bolsonaro 3,5

# -----------------------------------------------------------------------------
# 4. Modelo por grupos: lulistas x bolsonaristas em cada fonte (Figura 4.2)
#    y ~ mídia × grupo + controles
# -----------------------------------------------------------------------------
roda_grupo <- function(y) {
  vars <- c(y, "midia", "grupo", vars_ctrl)
  d_ <- dominio(vars)
  m <- svyglm(as.formula(paste(y, "~ midia * grupo +", controles)), design = d_)
  dados <- lapop[complete.cases(lapop[, vars]), ]       # amostra analítica
  prev <- expand_grid(md = c("tv", "redes", "prof"), gr = c("lulista", "bolsonarista")) %>%
    pmap_dfr(function(md, gr) preve(m, dados %>% mutate(midia = factor(md, levels(lapop$midia)),
                                                        grupo = factor(gr, levels(lapop$grupo))),
                                    rotulo = paste(md, gr, sep = "_")))
  fosso <- map_dfr(c("tv", "redes", "prof"), function(md)
    difere(m, dados,
           function(x) mutate(x, midia = factor(md, levels(lapop$midia)), grupo = factor("bolsonarista", levels(lapop$grupo))),
           function(x) mutate(x, midia = factor(md, levels(lapop$midia)), grupo = factor("lulista", levels(lapop$grupo))),
           rotulo = paste("fosso", md)))
  # O fosso muda entre as fontes? (diferença das diferenças contra a TV)
  dd <- map_dfr(c("redes", "prof"), function(md)
    comb(m, vetor(m, setNames(c(1, -1), c(paste0("midia", md, ":grupobolsonarista"),
                                          paste0("midia", md, ":grupolulista")))),
         rotulo = paste("fosso", md, "- fosso tv")))
  list(modelo = m, n = nrow(dados), prev = prev, fosso = bind_rows(fosso, dd))
}
res_grupo <- map(set_names(names(VDS)), roda_grupo)

res_grupo$econ$fosso      # Texto: TV 0,6; redes 0,7; profissional 1,2 (única diferença significativa)
res_grupo$r6$prev         # Texto: lulistas 2,7 (TV e redes); bolsonaristas 3,1 (TV) e 3,6 (redes)
res_grupo$r6$fosso        # Texto: TV 0,4 (n.s.); redes 1,0; redes − TV significativo

# Holm por família (fosso redes − TV, 11 variáveis dependentes)
tab_grupo <- imap_dfr(res_grupo, ~ .x$fosso %>% mutate(vd = .y, n = .x$n)) %>%
  group_by(termo) %>% mutate(p_holm = p.adjust(p, "holm"), p_bh = p.adjust(p, "BH")) %>% ungroup()
tab_grupo %>% filter(str_detect(termo, "- fosso tv")) %>% arrange(p)

# Análise complementar (ressentimento econômico e as duas simpatias juntas)
d_econ <- dominio(c("econ", "midia", "lula", "bols", vars_ctrl))
m_econ_simp <- svyglm(as.formula(paste("econ ~ lula + bols + midia +", controles)), design = d_econ)
summary(m_econ_simp)$coefficients[c("lula", "bols"), ]
# Texto: associado à rejeição a Lula; a simpatia por Bolsonaro deixa de ter relação

# -----------------------------------------------------------------------------
# 5. Escala contínua de simpatia (Figuras 4.3 e 4.4)
#    y ~ mídia × (alinhamento + intensidade) + controles, moderadores centrados
#    Como intensidade = |alinhamento|, o modelo é linear por partes, com dobra no 0:
#      inclinação "em direção a Bolsonaro" (lado alin < 0) = −b_alin + b_intens
#      inclinação "em direção a Lula"      (lado alin > 0) =  b_alin + b_intens
# -----------------------------------------------------------------------------
roda_continuo <- function(y, fonte = "midia", niveis = c("tv", "redes", "prof"),
                          extra = "", dados_base = lapop) {
  # Todos os públicos entram no modelo (inclusive "não consome", quando presente);
  # 'niveis' define apenas quais públicos são reportados.
  vars <- c(y, fonte, "lula", "bols", vars_ctrl)
  ok <- complete.cases(dados_base[, vars])
  d_ <- subset(svydesign(ids = ~upm, strata = ~strata, weights = ~wt, data = dados_base, nest = TRUE), ok)
  dados <- dados_base[ok, ]
  ma <- mean(dados$alin); mi <- mean(dados$intens)
  d_ <- update(d_, alin_c = alin - ma, intens_c = intens - mi)
  dados <- dados %>% mutate(alin_c = alin - ma, intens_c = intens - mi)
  f <- as.formula(paste(y, "~", fonte, "* (alin_c + intens_c) +", controles, extra))
  m <- svyglm(f, design = d_)
  ref <- levels(dados[[fonte]])[1]
  incl <- map_dfr(niveis, function(nv) {
    p_b <- c(alin_c = -1, intens_c = 1); p_l <- c(alin_c = 1, intens_c = 1)
    if (nv != ref) {
      p_b <- c(p_b, setNames(c(-1, 1), paste0(fonte, nv, c(":alin_c", ":intens_c"))))
      p_l <- c(p_l, setNames(c(1, 1),  paste0(fonte, nv, c(":alin_c", ":intens_c"))))
    }
    bind_rows(comb(m, vetor(m, p_b), paste("incl_bolsonaro", nv)),
              comb(m, vetor(m, p_l), paste("incl_lula", nv)))
  })
  # A fonte altera a inclinação do lado bolsonarista? (contra a referência)
  dif <- map_dfr(setdiff(niveis, ref), function(nv)
    comb(m, vetor(m, setNames(c(-1, 1), paste0(fonte, nv, c(":alin_c", ":intens_c")))),
         paste("dif_incl_bolsonaro", nv, "-", ref)))
  # Curvas (−9 a +9) e "convictos"
  curva <- expand_grid(nv = niveis, a = -9:9) %>%
    pmap_dfr(function(nv, a) {
      nd <- dados %>% mutate(alin_c = a - ma, intens_c = abs(a) - mi)
      nd[[fonte]] <- factor(nv, levels(dados[[fonte]]))
      preve(m, nd, rotulo = nv) %>% mutate(alin = a)
    })
  list(modelo = m, n = nrow(dados), incl = incl, dif = dif, curva = curva,
       convictos = curva %>% filter(alin %in% c(-9, 9)))
}

res_cont <- map(set_names(names(VDS)), roda_continuo)

res_cont$r6$incl      # Texto: 0,15 (redes) x ~0,03 (TV) por ponto em direção a Bolsonaro
res_cont$r6$convictos # Texto: extremo bolsonarista 3,9 (redes) x 3,1 (TV); mídia prof. lado Lula ~1,4
res_cont$econ$convictos  # Texto: ~4,1–4,2 (só Bolsonaro) e ~3,3 (só Lula), TV e redes sobrepostas

# Figura 4.4: inclinações item a item + teste redes − TV com correção de Holm
tab_incl <- imap_dfr(res_cont, ~ bind_rows(.x$incl, .x$dif) %>% mutate(vd = .y, n = .x$n))
tab_dif <- tab_incl %>% filter(termo == "dif_incl_bolsonaro redes - tv") %>%
  mutate(p_holm = p.adjust(p, "holm"), p_bh = p.adjust(p, "BH")) %>% arrange(p)
tab_dif   # Texto: só r6 se mantém após a correção

# Teste conjunto (Wald) de todas as interações mídia × simpatia, por VD
wald_midia <- imap_dfr(res_cont, function(r, vd) {
  w <- regTermTest(r$modelo, ~ midia:alin_c + midia:intens_c, method = "Wald")
  tibble(vd = vd, F = as.numeric(w$Ftest), p = as.numeric(w$p))
}) %>% mutate(p_holm = p.adjust(p, "holm"))
wald_midia

# Polarização afetiva (|Lula − Bolsonaro|): redes x TV (texto: sem diferença significativa)
d_af <- dominio(c("intens", "midia", vars_ctrl))
m_af <- svyglm(as.formula(paste("intens ~ midia +", controles)), design = d_af)
summary(m_af)$coefficients[c("midiaredes", "midiaprof"), ]

# -----------------------------------------------------------------------------
# 6. Frequência de consumo (Figura 4.5)
#    Seis grupos: fonte (TV, redes, profissional) × frequência (diária, não diária)
#    Quem não consome notícias políticas fica fora desta etapa.
# -----------------------------------------------------------------------------
lapop_f <- lapop %>%
  filter(midia %in% c("tv", "redes", "prof"), !is.na(diario)) %>%
  mutate(f6 = factor(paste(midia, if_else(diario == 1, "d", "n"), sep = "_"),
                     levels = c("tv_n", "tv_d", "redes_n", "redes_d", "prof_n", "prof_d")))
NIV6 <- levels(lapop_f$f6)

res_freq <- map(set_names(c("r6", "r7", "r8", "r9", "cult", "econ")),
                ~ roda_continuo(.x, fonte = "f6", niveis = NIV6, dados_base = lapop_f))

res_freq$r6$convictos %>% select(termo, alin, est) %>%
  pivot_wider(names_from = alin, values_from = est) %>% mutate(dist = `-9` - `9`)
# Texto: TV não diária ~0; TV diária 0,8; redes diária 1,6;
#        bolsonarista convicto nas redes diárias 4,1; lulista convicto 2,4

# Redes diárias x TV diária, lado bolsonarista (com Holm entre as VDs)
comp_diaria <- function(r) {
  m <- r$modelo
  comb(m, vetor(m, c("f6redes_d:alin_c" = -1, "f6redes_d:intens_c" = 1,
                     "f6tv_d:alin_c" = 1, "f6tv_d:intens_c" = -1)),
       "redes diária − TV diária (lado Bolsonaro)")
}
tab_freq <- imap_dfr(res_freq, ~ comp_diaria(.x) %>% mutate(vd = .y)) %>%
  mutate(p_holm = p.adjust(p, "holm")) %>% arrange(p)
tab_freq   # Texto: significativo para r6 e r7 (identidades masculina e feminina)

# Robustez: interesse e escolaridade também interagindo com a simpatia
res_freq_rob <- roda_continuo("r6", fonte = "f6", niveis = NIV6, dados_base = lapop_f,
                              extra = "+ interesse:(alin_c + intens_c) + educ:(alin_c + intens_c)")
comp_diaria(res_freq_rob)

# Três níveis de frequência (diária, semanal, rara) entre usuários de redes e telespectadores
lapop_f3 <- lapop %>%
  filter(midia %in% c("tv", "redes"), !is.na(freq3)) %>%
  mutate(f3 = factor(paste(midia, freq3, sep = "_"),
                     levels = c("tv_rara", "tv_semanal", "tv_diaria",
                                "redes_rara", "redes_semanal", "redes_diaria")))
res_f3 <- roda_continuo("r6", fonte = "f3", niveis = levels(lapop_f3$f3), dados_base = lapop_f3)
res_f3$incl %>% filter(str_detect(termo, "bolsonaro"))
# Texto: não há aumento gradual e consistente; destaca-se apenas o grupo diário das redes

# -----------------------------------------------------------------------------
# 7. Percepção de mídia hostil (Figura 4.6)
# -----------------------------------------------------------------------------
# 7a. Descritivo: quem e qual meio mais dissemina notícias falsas, por grupo
desc_hostil <- lapop %>% filter(grupo %in% c("lulista", "bolsonarista")) %>%
  group_by(grupo) %>%
  summarise(midia_jornalistas = 100 * mean(b3 == 3, na.rm = TRUE),
            nenhum_grupo      = 100 * mean(b3 == 10, na.rm = TRUE),
            meio_tv           = 100 * mean(b4 == 2, na.rm = TRUE),
            meio_redes        = 100 * mean(b4 == 1, na.rm = TRUE))
desc_hostil
# Texto: mídia/jornalistas 27% x 11%; TV 23% x 5%; redes 59% x 76%; nenhum 12% x 25%

# Testes das diferenças entre os grupos (design-based), com Holm
lapop <- lapop %>% mutate(bolsonarista = as.numeric(grupo == "bolsonarista"),
                          h_midia = as.numeric(b3 == 3), h_nenhum = as.numeric(b3 == 10),
                          h_tv = as.numeric(b4 == 2), h_redes = as.numeric(b4 == 1))
des <- svydesign(ids = ~upm, strata = ~strata, weights = ~wt, data = lapop, nest = TRUE)
testes_hostil <- map_dfr(c("h_midia", "h_nenhum", "h_tv", "h_redes"), function(y) {
  ok <- lapop$grupo %in% c("lulista", "bolsonarista") & !is.na(lapop[[y]])
  m <- svyglm(as.formula(paste(y, "~ bolsonarista")), design = subset(des, ok))
  comb(m, vetor(m, c(bolsonarista = 1)), y)        # diferença em proporção
}) %>% mutate(p_holm = p.adjust(p, "holm"))
testes_hostil

# 7b. "Culpa o próprio meio": o meio apontado coincide com a fonte principal
lapop_h <- lapop %>%
  filter(midia %in% c("tv", "redes", "prof"), !is.na(b4)) %>%
  mutate(midia = droplevels(midia),
         culpa_proprio = case_when(midia == "redes" ~ as.numeric(b4 == 1),
                                   midia == "tv"    ~ as.numeric(b4 == 2),
                                   midia == "prof"  ~ as.numeric(b4 %in% 3:5)))
res_hostil <- roda_continuo("culpa_proprio", fonte = "midia", dados_base = lapop_h)
res_hostil$convictos
# Texto: redes — lulista convicto 83% x bolsonarista convicto 53%;
#        TV — bolsonarista 23% x lulista 6%; mídia profissional: quase ninguém

# Diferença entre convictos em cada meio, com Holm
conv_dif <- function(r, dados, nv) {
  ma <- mean(dados$alin); mi <- mean(dados$intens)
  difere(r$modelo, dados %>% mutate(alin_c = alin - ma, intens_c = intens - mi),
         function(x) mutate(x, midia = factor(nv, levels(dados$midia)), alin_c = -9 - ma, intens_c = 9 - mi),
         function(x) mutate(x, midia = factor(nv, levels(dados$midia)), alin_c =  9 - ma, intens_c = 9 - mi),
         rotulo = paste("bolsonarista − lulista convicto", nv))
}
dados_h <- lapop_h[complete.cases(lapop_h[, c("culpa_proprio", "midia", "lula", "bols", vars_ctrl)]), ]
tab_hostil <- map_dfr(c("tv", "redes", "prof"), ~ conv_dif(res_hostil, dados_h, .x)) %>%
  mutate(p_holm = p.adjust(p, "holm"))
tab_hostil

# Modelo alternativo para resposta sim/não (logit)
d_h <- subset(svydesign(ids = ~upm, strata = ~strata, weights = ~wt, data = lapop_h, nest = TRUE),
              complete.cases(lapop_h[, c("culpa_proprio", "midia", "lula", "bols", vars_ctrl)]))
ma_h <- mean(dados_h$alin); mi_h <- mean(dados_h$intens)
d_h <- update(d_h, alin_c = alin - ma_h, intens_c = intens - mi_h)
m_hostil_logit <- svyglm(as.formula(paste("culpa_proprio ~ midia * (alin_c + intens_c) +", controles)),
                         design = d_h, family = quasibinomial())
expand_grid(md = c("tv", "redes", "prof"), a = c(-9, 9)) %>%
  pmap_dfr(function(md, a) {
    nd <- dados_h %>% mutate(midia = factor(md, levels(dados_h$midia)),
                             alin_c = a - ma_h, intens_c = 9 - mi_h)
    tibble(midia = md, alin = a, prob = mean(as.numeric(predict(m_hostil_logit, newdata = nd, type = "response", se.fit = FALSE))))
  })
regTermTest(m_hostil_logit, ~ midia:alin_c + midia:intens_c)

# 7c. Percepção de mídia hostil e outras atitudes, dentro de cada grupo
#     hostil = aponta mídia/jornalistas (BRAMEDIA3) ou um meio tradicional (BRAMEDIA4: TV,
#     jornais, rádio ou sites) como principal disseminador de notícias falsas
lapop <- lapop %>%
  mutate(hostil = if_else(is.na(b3) & is.na(b4), NA_real_,
                          as.numeric(coalesce(b3 == 3, FALSE) | coalesce(b4 %in% 2:5, FALSE))))
des <- svydesign(ids = ~upm, strata = ~strata, weights = ~wt, data = lapop, nest = TRUE)
tab_hostil_atit <- expand_grid(g = c("lulista", "bolsonarista", "outros"),
                               y = c("conf_eleicoes", "conf_stf", "cult")) %>%
  pmap_dfr(function(g, y) {
    ok <- lapop$grupo %in% g & complete.cases(lapop[, c(y, "hostil", vars_ctrl)])
    m <- svyglm(as.formula(paste(y, "~ hostil +", controles)), design = subset(des, ok))
    comb(m, vetor(m, c(hostil = 1)), paste(g, y))
  }) %>% mutate(p_holm = p.adjust(p, "holm"))
tab_hostil_atit
# Texto: sem relação com confiança nas eleições/STF; exceção: entre os que não simpatizam
#        com nenhum dos dois líderes ("outros"), mídia hostil -> maior ressentimento cultural

# -----------------------------------------------------------------------------
# 8. Robustez do resultado principal (r6: redes − TV, lado bolsonarista)
# -----------------------------------------------------------------------------
dif_r6 <- function(r) r$dif %>% filter(str_detect(termo, "redes"))

# (a) Erros-padrão por bootstrap (Rao-Wu, 500 réplicas)
set.seed(2026)
des_boot <- as.svrepdesign(des, type = "subbootstrap", replicates = 500)
ok6 <- complete.cases(lapop[, c("r6", "midia", "lula", "bols", vars_ctrl)])
ma6 <- mean(lapop$alin[ok6]); mi6 <- mean(lapop$intens[ok6])
des_boot6 <- update(subset(des_boot, ok6), alin_c = alin - ma6, intens_c = intens - mi6)
m_boot <- svyglm(as.formula(paste("r6 ~ midia * (alin_c + intens_c) +", controles)), design = des_boot6)
comb(m_boot, vetor(m_boot, c("midiaredes:alin_c" = -1, "midiaredes:intens_c" = 1)), "bootstrap")

# (b) Sem a renda (a variável de controle com mais ausentes: 73)
controles_bkp <- controles
controles <- str_remove(controles, "renda \\+ ")
vars_ctrl_bkp <- vars_ctrl; vars_ctrl <- setdiff(vars_ctrl, "renda")
dif_r6(roda_continuo("r6"))
controles <- controles_bkp; vars_ctrl <- vars_ctrl_bkp

# (c) Imputação múltipla da renda (m = 20), combinando pelas regras de Rubin
library(mice)
imp <- mice(lapop %>% select(renda, raca, mulher, idade, religiao, regiao, educ, interesse,
                             midia, lula, bols, r1:r9),
            m = 20, method = "pmm", seed = 2026, printFlag = FALSE)
est_imp <- map(1:20, function(i) {
  li <- lapop; li$renda <- complete(imp, i)$renda
  r <- roda_continuo("r6", dados_base = li)
  dif_r6(r)
})
q <- map_dbl(est_imp, "est"); u <- map_dbl(est_imp, ~ .x$ep^2)
tibble(est = mean(q), ep = sqrt(mean(u) + (1 + 1/20) * var(q)))

# -----------------------------------------------------------------------------
# 9. Figuras (salvas em figuras_R/, para não sobrescrever as figuras do texto)
# -----------------------------------------------------------------------------
dir.create("figuras_R", showWarnings = FALSE)
cores <- c(lulista = "#eb6834", bolsonarista = "#2a78d6")
cores_midia <- c(tv = "#52514e", redes = "#2a78d6", prof = "#c2410c")
rot_midia <- c(tv = "Televisão", redes = "Redes sociais", prof = "Mídia profissional")
virg <- scales::label_number(accuracy = .1, decimal.mark = ",")
tema <- theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#e6e5e0", linewidth = .3),
        legend.position = "top", legend.title = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0))
salva <- function(p, nome, larg = 18, alt = 11) {
  print(p)
  ggsave(file.path("figuras_R", paste0(nome, ".png")), p, width = larg, height = alt, units = "cm", dpi = 300)
}

# Figura 4.1
rot_fonte <- c(`1` = "Redes sociais", `2` = "Televisão", `3` = "Jornais e revistas",
               `4` = "Rádio e podcasts", `5` = "Sites de notícias", `7` = "Outro", `0` = "Não consome")
f41 <- fontes %>% mutate(fonte = rot_fonte[as.character(midia_det)]) %>%
  ggplot(aes(pct, reorder(fonte, pct))) + geom_col(fill = "#2a78d6", width = .65) +
  geom_text(aes(label = paste0(round(pct), "%")), hjust = -.2, size = 3) +
  scale_x_continuous(limits = c(0, 55)) + labs(x = "% dos entrevistados", y = NULL) + tema
salva(f41, "fig4_1_fontes_noticias", alt = 8)

# Figura 4.2
f42 <- imap_dfr(res_grupo[c("econ", "cult", "r6")], ~ .x$prev %>% mutate(vd = VDS[.y])) %>%
  separate(termo, c("midia", "grupo"), sep = "_") %>%
  ggplot(aes(factor(midia, names(rot_midia), rot_midia), est, colour = grupo, shape = grupo)) +
  geom_pointrange(aes(ymin = ic_inf, ymax = ic_sup), position = position_dodge(.4)) +
  facet_wrap(~ vd) + scale_colour_manual(values = cores) +
  scale_y_continuous(labels = virg) + labs(x = NULL, y = "Ressentimento esperado (1–5)") + tema
salva(f42, "fig4_2_fosso_grupos")

# Figura 4.3
f43 <- imap_dfr(res_cont[c("econ", "r6")], ~ .x$curva %>% mutate(vd = VDS[.y])) %>%
  ggplot(aes(alin, est, colour = termo, fill = termo)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "#8a8984") +
  geom_ribbon(aes(ymin = ic_inf, ymax = ic_sup), alpha = .12, colour = NA) +
  geom_line(linewidth = .9) + facet_wrap(~ vd) +
  scale_colour_manual(values = cores_midia, labels = rot_midia) +
  scale_fill_manual(values = cores_midia, labels = rot_midia) +
  scale_y_continuous(labels = virg) +
  labs(x = "Alinhamento (−9 = só Bolsonaro; +9 = só Lula)", y = "Ressentimento esperado (1–5)") + tema
salva(f43, "fig4_3_curvas_alinhamento")

# Figura 4.4
f44 <- tab_incl %>% filter(termo %in% c("incl_bolsonaro tv", "incl_bolsonaro redes")) %>%
  left_join(tab_dif %>% select(vd, p_holm), by = "vd") %>%
  mutate(vd = factor(VDS[vd], rev(VDS)), midia = if_else(str_detect(termo, "redes"), "Redes sociais", "Televisão")) %>%
  ggplot(aes(est, vd, shape = midia)) +
  geom_vline(xintercept = 0, colour = "#8a8984") +
  geom_pointrange(aes(xmin = ic_inf, xmax = ic_sup), position = position_dodge(.5)) +
  geom_text(aes(x = .32, y = vd, label = scales::pvalue(p_holm, decimal.mark = ",")), size = 2.8,
            data = ~ distinct(.x, vd, p_holm), inherit.aes = FALSE) +
  scale_shape_manual(values = c("Redes sociais" = 16, "Televisão" = 1)) +
  labs(x = "Variação do ressentimento por ponto em direção a Bolsonaro", y = NULL,
       caption = "Coluna à direita: p-valor (Holm) do teste redes − TV.") + tema
salva(f44, "fig4_4_inclinacoes_itens", alt = 12)

# Figura 4.5
f45 <- res_freq$r6$convictos %>%
  mutate(grupo = if_else(alin == -9, "bolsonarista", "lulista"),
         fonte = str_remove(termo, "_[dn]$"), freq = if_else(str_detect(termo, "_d$"), "Diária", "Não diária")) %>%
  ggplot(aes(factor(fonte, names(rot_midia), rot_midia), est, colour = grupo, shape = grupo)) +
  geom_pointrange(aes(ymin = ic_inf, ymax = ic_sup), position = position_dodge(.4)) +
  facet_wrap(~ freq) + scale_colour_manual(values = cores, labels = c("Bolsonarista convicto", "Lulista convicto")) +
  scale_shape_discrete(labels = c("Bolsonarista convicto", "Lulista convicto")) +
  scale_y_continuous(labels = virg) + labs(x = NULL, y = "Ressentimento — pauta LGBT+ (1–5)") + tema
salva(f45, "fig4_5_dose_item6")

# Figura 4.6
f46 <- res_hostil$convictos %>%
  mutate(grupo = if_else(alin == -9, "bolsonarista", "lulista")) %>%
  ggplot(aes(factor(termo, names(rot_midia), rot_midia), est, fill = grupo)) +
  geom_col(position = position_dodge(.75), width = .7) +
  geom_text(aes(label = paste0(round(100 * est), "%")), position = position_dodge(.75), vjust = -.4, size = 3) +
  scale_fill_manual(values = cores, labels = c("Bolsonarista convicto", "Lulista convicto")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = "Principal fonte de notícias", y = "Culpa o próprio meio") + tema
salva(f46, "fig4_6_midia_hostil")

# -----------------------------------------------------------------------------
# 10. Conferência com os valores do texto (estimativas pontuais)
#     Valores de referência obtidos na reestimação feita fora do R.
# -----------------------------------------------------------------------------
pega <- function(tab, rot) tab$est[tab$termo == rot]
conv <- function(r, nv, a) r$convictos$est[r$convictos$termo == nv & r$convictos$alin == a]
conferencia <- tribble(
  ~resultado,                                  ~esperado, ~obtido,
  "N índice econômico (grupos)",                    1334, res_grupo$econ$n,
  "N item 6 (grupos)",                              1289, res_grupo$r6$n,
  "Fosso econômico — TV",                           0.60, pega(res_grupo$econ$fosso, "fosso tv"),
  "Fosso econômico — redes",                        0.70, pega(res_grupo$econ$fosso, "fosso redes"),
  "Fosso econômico — profissional",                 1.17, pega(res_grupo$econ$fosso, "fosso prof"),
  "Item 6 — lulistas, TV",                          2.67, pega(res_grupo$r6$prev, "tv_lulista"),
  "Item 6 — lulistas, redes",                       2.64, pega(res_grupo$r6$prev, "redes_lulista"),
  "Item 6 — bolsonaristas, TV",                     3.07, pega(res_grupo$r6$prev, "tv_bolsonarista"),
  "Item 6 — bolsonaristas, redes",                  3.60, pega(res_grupo$r6$prev, "redes_bolsonarista"),
  "Item 6 — fosso TV",                              0.40, pega(res_grupo$r6$fosso, "fosso tv"),
  "Item 6 — fosso redes",                           0.95, pega(res_grupo$r6$fosso, "fosso redes"),
  "Item 6 — inclinação p/ Bolsonaro, redes",        0.146, pega(res_cont$r6$incl, "incl_bolsonaro redes"),
  "Item 6 — inclinação p/ Bolsonaro, TV",           0.024, pega(res_cont$r6$incl, "incl_bolsonaro tv"),
  "Item 6 — só Bolsonaro, redes",                   3.93, conv(res_cont$r6, "redes", -9),
  "Item 6 — só Bolsonaro, TV",                      3.11, conv(res_cont$r6, "tv", -9),
  "Item 6 — só Lula, mídia profissional",           1.46, conv(res_cont$r6, "prof", 9),
  "Econômico — só Lula, mídia profissional",        2.57, conv(res_cont$econ, "prof", 9),
  "N frequência (item 6)",                          1159, res_freq$r6$n,
  "Frequência — bolsonarista convicto, redes diária", 4.10, conv(res_freq$r6, "redes_d", -9),
  "Frequência — lulista convicto, redes diária",    2.46, conv(res_freq$r6, "redes_d", 9),
  "Frequência — distância, TV diária",              0.84, conv(res_freq$r6, "tv_d", -9) - conv(res_freq$r6, "tv_d", 9),
  "N mídia hostil",                                 1190, res_hostil$n,
  "Culpa o próprio meio — redes, lulista convicto", 0.83, conv(res_hostil, "redes", 9),
  "Culpa o próprio meio — redes, bolsonarista conv.", 0.53, conv(res_hostil, "redes", -9),
  "Culpa o próprio meio — TV, bolsonarista convicto", 0.23, conv(res_hostil, "tv", -9),
  "Culpa o próprio meio — TV, lulista convicto",    0.06, conv(res_hostil, "tv", 9)
) %>% mutate(confere = abs(esperado - obtido) < .015 | (esperado > 100 & esperado == obtido))
print(conferencia, n = Inf)
