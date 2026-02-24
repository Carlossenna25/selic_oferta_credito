## Pacotes utilizados ##
library(GetBCBData)
library(sidrar)
library(tidyverse)
library(scales)
library(quantmod)
library(tidyr)
library(dplyr)
library(forecast)
library(patchwork)
library(seasonal)
library(vars)
library(lubridate)
library(scales)
library(ggplot2)
library(urca)
library(zoo)
library(corrplot)
library(purrr)
library(dynlm)
library(httr2)
library(readxl)
library(stringr)
library(janitor)
library(fixest)
library(DescTools)
library(marginaleffects)
library(patchwork)
library(scales)
library(purrr)

## Dados financeiros e macroeconômicos ##
series_BC <- c(
  spread_medio = 20786,
  icc = 25354,
  saldo_publico = 2007,
  saldo_privado = 2043,
  ibc_Br_sazonal = 24364,
  ipca = 433,
  selic = 4189
)

GetBCBData::gbcbd_get_series(
  id = series_BC,
  first.date = "2015-01-01",
  last.date = Sys.Date()
) %>%
  rename(data = ref.date,
         valor = value,
         serie = series.name) -> dados_BC

dados_BC %>%
  dplyr::select(data, serie, valor) %>%
  pivot_wider(names_from = serie, 
              values_from = valor) %>%
  arrange(data) -> dados_BC


dados_BC %>%
  mutate(ano = year(data),
         trimestre = quarter(data)) %>%
  group_by(ano, trimestre) %>%
  summarise(
    spread_medio = mean(spread_medio, na.rm = TRUE),
    icc = mean(icc, na.rm = TRUE),
    selic = mean(selic, na.rm = TRUE),
    ipca_tri = prod(1 + ipca/100, na.rm = TRUE) - 1,
    ibc_Br_sazonal = mean(ibc_Br_sazonal, na.rm = TRUE),
    saldo_publico = last(saldo_publico),
    saldo_privado = last(saldo_privado),
    .groups = "drop"
  ) %>%
  arrange(ano, trimestre) %>%
  mutate(
    selic = selic/100,                 
    d_selic = selic - lag(selic),
    d_selic_lag1 = lag(d_selic)
  ) -> dados_macro_tri

## Funções para importação de planilhas do IF.Data ##

fill_right <- function(x) {
  x <- str_squish(replace_na(as.character(x), ""))
  x[x == ""] <- NA_character_
  for (i in seq_along(x)) if (i > 1 && is.na(x[i])) x[i] <- x[i - 1]
  x
}

make_names_2row <- function(h1, h2) {
  h1 <- fill_right(h1)
  h2 <- str_squish(replace_na(as.character(h2), ""))
  h2[h2 == ""] <- NA_character_

  nm <- map_chr(seq_along(h1), \(i) {
    parts <- c(h1[i], h2[i])
    parts <- parts[!is.na(parts) & parts != ""]
    paste(parts, collapse = " - ")
  })

  nm[nm == ""] <- NA_character_
  nm <- make.unique(nm, sep = " | ")
  nm[is.na(nm)] <- paste0("V", which(is.na(nm)))
  nm
}

make_names_3row <- function(h1, h2, h3) {
  h1 <- fill_right(h1)
  h2 <- fill_right(h2)
  h3 <- str_squish(replace_na(as.character(h3), ""))
  h3[h3 == ""] <- NA_character_

  nm <- map_chr(seq_along(h1), function(i) {
    parts <- c(h1[i], h2[i], h3[i])
    parts <- parts[!is.na(parts) & parts != ""]
    paste(parts, collapse = " - ")
  })

  nm[nm == ""] <- NA_character_
  nm <- make.unique(nm, sep = " | ")
  nm[is.na(nm)] <- paste0("V", which(is.na(nm)))
  nm
}

to_num_br_safe <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)

  neg <- str_detect(x, "^\\(.*\\)$")
  x <- str_replace_all(x, "^\\((.*)\\)$", "\\1")

  x <- str_replace_all(x, "[^0-9,\\.\\-]", "")
  x <- str_replace_all(x, "\\.", "")
  x <- str_replace_all(x, ",", ".")
  out <- suppressWarnings(as.numeric(x))
  out[neg & !is.na(out)] <- -out[neg & !is.na(out)]
  out
}

to_pct_br_0_1 <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "%", "")
  to_num_br_safe(x) / 100
}

parse_data_mista <- function(x) {
  x <- as.character(x)

  out1 <- suppressWarnings(as.Date(paste0("01/", x), format = "%d/%m/%Y"))

  mon_map <- c(
    jan="01", fev="02", mar="03", abr="04", mai="05", jun="06",
    jul="07", ago="08", set="09", out="10", nov="11", dez="12"
  )

  m <- str_match(tolower(x), "^([a-z]{3})/(\\d{2})$")
  out2 <- rep(as.Date(NA), length(x))
  ok <- !is.na(m[,1])
  if (any(ok)) {
    mm <- mon_map[m[ok,2]]
    yy <- as.integer(m[ok,3])
    yy <- ifelse(yy <= 69, 2000 + yy, 1900 + yy)
    out2[ok] <- as.Date(paste0(yy, "-", mm, "-01"))
  }

  out <- out1
  out[is.na(out)] <- out2[is.na(out)]
  out
}

pick1 <- function(df, pattern, warn = FALSE) {
  nm <- names(df)[str_detect(names(df), pattern)]
  if (length(nm) == 0) {
    if (warn) warning("Nenhuma coluna encontrada para padrão: ", pattern, call. = FALSE)
    return(rep(NA_character_, nrow(df)))
  }
  df[[nm[1]]]
}

ym_from_file <- function(fp) str_match(basename(fp), "dados_(\\d{6})\\.csv")[,2]

add_file_meta <- function(df, fp) {
  ym <- ym_from_file(fp)
  df %>%
    mutate(
      arquivo = basename(fp),
      ano_mes_arquivo = ym,
      trimestre_arquivo = paste0(substr(ym, 1, 4), "-T",
                                 ((as.integer(substr(ym, 5, 6)) - 1) %/% 3 + 1))
    )
}

## Dados referentes a Conglomerados Prudenciais e Instituições Independentes - Resumo ##
path_resumo <- "C:/Users/carlo/OneDrive/Área de Trabalho/Estudos/FGC - resumo"
files_resumo <- list.files(path_resumo, pattern = "^dados_\\d{6}\\.csv$", full.names = TRUE)

conglomerados_resumo <- map_dfr(files_resumo, ~{
  read_delim(
    .x,
    delim = ";",
    locale = locale(encoding = "UTF-8"),
    col_types = cols(.default = col_character()),
    show_col_types = FALSE,
    name_repair = "unique"
  ) %>%
    add_file_meta(.x)
})

conglomerados_resumo <- conglomerados_resumo %>%
  dplyr::select(-tidyselect::matches("^\\.\\.\\.[0-9]+$")) %>%
  dplyr::filter(!is.na(`Código`), `Código` != "") %>%
  dplyr::transmute(
    Instituição = `Instituição`,
    Código = as.character(`Código`),
    TCB = as.character(`TCB`),
    `Ativo Total` = to_num_br_safe(`Ativo Total`),
    `Carteira de Crédito Classificada` = to_num_br_safe(`Carteira de Crédito Classificada`),
    `Captações` = to_num_br_safe(`Captações`),
    `Lucro Líquido` = to_num_br_safe(`Lucro Líquido`),
    `Patrimônio Líquido` = to_num_br_safe(`Patrimônio Líquido`),
    `Índice de Basileia` = to_pct_br_0_1(`Índice de Basileia`),
    Data = parse_data_mista(`Data`)
  ) %>%
  dplyr::distinct() %>%
  dplyr::group_by(Código, Data) %>%
  dplyr::slice_max(`Ativo Total`, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup()

## Dados referentes a Conglomerados Prudenciais e Instituições Independentes - Passivo ##
path_passivo <- "C:/Users/carlo/OneDrive/Área de Trabalho/Estudos/FGC - Passivo"
files_passivo <- list.files(path_passivo, pattern = "^dados_\\d{6}\\.csv$", full.names = TRUE)

read_passivo_file <- function(fp) {
  raw <- read_delim(
    fp, delim = ";", col_names = FALSE,
    locale = readr::locale(encoding = "UTF-8"),
    show_col_types = FALSE, 
    quote = "\""
  )
  
  h1 <- raw[1,] |> unlist(use.names = FALSE)
  h2 <- raw[2,] |> unlist(use.names = FALSE)
  h3 <- raw[3,] |> unlist(use.names = FALSE)

  new_names <- make_names_3row(h1, h2, h3)
  dat <- raw[-c(1,2,3),] |> as_tibble()
  names(dat) <- new_names
  dat <- dat |> dplyr::select(dplyr::where(~ !all(is.na(.x) | .x == "")))
  dat
}

conglomerados_passivo_raw <- map_dfr(files_passivo, ~ read_passivo_file(.x) %>% add_file_meta(.x))

conglomerados_passivo <- conglomerados_passivo_raw %>%
  dplyr::filter(!is.na(`Código`), `Código` != "") %>%
  transmute(
    Instituição = `Instituição`,
    Código = as.character(`Código`),
    Data = parse_data_mista(`Data`),

    dep_vista    = to_num_br_safe(pick1(., "Depósitos à Vista \\(a1\\)")),
    dep_total    = to_num_br_safe(pick1(., "Depósito Total \\(a\\)")),
    dep_prazo    = to_num_br_safe(pick1(., "Depósitos a Prazo \\(a4\\)")),
    dep_poupanca = to_num_br_safe(pick1(., "Depósitos de Poupança \\(a2\\)")),
    passivo_total = to_num_br_safe(pick1(., "^Passivo Total \\(k\\)"))
  ) %>%
  distinct() %>%
  group_by(Código, Data) %>%
  slice_max(coalesce(passivo_total, -Inf), n = 1, with_ties = FALSE) %>%
  ungroup()

## Dados referentes a Conglomerados Prudenciais e Instituições Independentes - Ativo ##
path_ativo <- "C:/Users/carlo/OneDrive/Área de Trabalho/Estudos/FGC - Ativo"
files_ativo <- list.files(path_ativo, pattern = "^dados_\\d{6}\\.csv$", full.names = TRUE)

looks_like_header <- function(row_vec) {
  s <- paste(row_vec, collapse = " ")
  letters_ratio <- sum(str_detect(strsplit(s, "")[[1]], "[A-Za-zÀ-ÿ]")) / max(1, nchar(s))
  digits_ratio  <- sum(str_detect(strsplit(s, "")[[1]], "[0-9]")) / max(1, nchar(s))
  letters_ratio > 0.10 && digits_ratio < 0.20
}

read_ativo_file <- function(fp) {
  raw <- read_delim(
    fp, 
    delim = ";", 
    col_names = FALSE,
    locale = readr::locale(encoding = "UTF-8"),
    show_col_types = FALSE, 
    quote = "\""
  )

  r1 <- raw[1,] |> unlist(use.names = FALSE)
  r2 <- raw[2,] |> unlist(use.names = FALSE)
  r3 <- raw[3,] |> unlist(use.names = FALSE)

  is_h1 <- looks_like_header(r1)
  is_h2 <- looks_like_header(r2)
  is_h3 <- looks_like_header(r3)

  n_header <- if (is_h1 && is_h2 && is_h3) 3 else if (is_h1 && is_h2) 2 else 1

  if (n_header == 3) {
    new_names <- make_names_3row(r1, r2, r3)
    dat <- raw[-c(1,2,3),] |> as_tibble()
  } else if (n_header == 2) {
    new_names <- make_names_2row(r1, r2)
    dat <- raw[-c(1,2),] |> as_tibble()
  } else {
    new_names <- make.unique(str_squish(as.character(r1)), sep = " | ")
    new_names[is.na(new_names) | new_names == ""] <- paste0("V", which(is.na(new_names) | new_names == ""))
    dat <- raw[-1,] |> as_tibble()
  }

  names(dat) <- new_names
  keep <- vapply(dat, function(col) !all(is.na(col) | col == ""), logical(1))
  dat <- dat[, keep, drop = FALSE]
  dat
}

conglomerados_ativo_raw <- map_dfr(files_ativo, ~ read_ativo_file(.x) %>% add_file_meta(.x))

conglomerados_ativo <- conglomerados_ativo_raw %>%
  filter(!is.na(`Código`), `Código` != "") %>%
  transmute(
    Instituição = `Instituição`,
    Código = as.character(`Código`),
    Data = parse_data_mista(`Data`),

    ativo_total = to_num_br_safe(pick1(., "^Ativo Total \\(k\\) = \\(i\\) - \\(j\\)")),
    operacoes_credito = to_num_br_safe(pick1(., "Operações de Crédito - Operações de Crédito \\(d1\\)")),
    provisoes_credito = to_num_br_safe(pick1(., "Operações de Crédito - Provisão sobre Operações de Crédito \\(d2\\)"))
  ) %>%
  distinct() %>%
  group_by(Código, Data) %>%
  slice_max(ativo_total, n = 1, with_ties = FALSE) %>%
  ungroup()

## Dados referentes a Conglomerados Prudenciais e Instituições Independentes - Demonstração de Resultados ##
path_demo <- "C:/Users/carlo/OneDrive/Área de Trabalho/Estudos/FGC - Demonstração"
files_demo <- list.files(path_demo, pattern = "^dados_\\d{6}\\.csv$", full.names = TRUE)

read_demo_file_3header <- function(fp) {
  raw <- read_delim(
    fp, 
    delim = ";", 
    col_names = FALSE,
    col_types = readr::cols(.default = readr::col_character()),
    locale = readr::locale(encoding = "UTF-8"),
    show_col_types = FALSE, 
    quote = "\""
  )

  h1 <- raw[1,] |> unlist(use.names = FALSE)
  h2 <- raw[2,] |> unlist(use.names = FALSE)
  h3 <- raw[3,] |> unlist(use.names = FALSE)

  new_names <- make_names_3row(h1, h2, h3)
  dat <- raw[-c(1,2,3),] |> as_tibble()
  names(dat) <- new_names
  keep <- vapply(dat, function(col) !all(is.na(col) | col == ""), logical(1))
  dat <- dat[, keep, drop = FALSE]
  dat
}

conglomerados_demonstracoes_raw <- map_dfr(files_demo, ~ read_demo_file_3header(.x) %>% add_file_meta(.x))

conglomerados_demonstracoes <- conglomerados_demonstracoes_raw %>%
  filter(!is.na(`Código`), `Código` != "") %>%
  transmute(
    Instituição = `Instituição`,
    Código = as.character(`Código`),
    Data = parse_data_mista(`Data`),

    despesas_captacao = to_num_br_safe(pick1(., "Despesas de Captação \\(b1\\)")),
    resultado_intermediacao = to_num_br_safe(
      pick1(., "Resultado de Intermediação Financeira.*\\(c\\)$|Resultado de Intermediação Financeira.*\\(c\\) =")
    )
  ) %>%
  distinct() %>%
  group_by(Código, Data) %>%
  slice_max(coalesce(resultado_intermediacao, -Inf), n = 1, with_ties = FALSE) %>%
  ungroup()

## Unificação de base de dados ##
conglomerados_resumo %>%
    left_join(conglomerados_passivo %>%
                dplyr::select(-Instituição), by = c("Código","Data")) %>%
    left_join(conglomerados_ativo %>%
                dplyr::select(-Instituição),   by = c("Código","Data")) %>%
    left_join(conglomerados_demonstracoes %>%
                dplyr::select(-Instituição),    by = c("Código","Data")) -> conglomerados

## Paineis consolidados de instituições financeiras ##
painel_final %>%
  filter(TCB %in% c("b1","b2")) -> painel_bancos

painel_final %>%
  filter(TCB %in% c("b3S","b3C")) -> painel_cooperativas

painel_final %>%
  filter(TCB %in% c("n1","n4")) -> painel_fintechs

## Gráfico de Evolução do crescimento de crédito por instituição ##
cores_tcb <- c(
  "Bancos"       = "#1B4F8A",
  "Cooperativas" = "#2E8B57",
  "Fintechs"     = "#C0392B",
  "SFN"          = "#4a1e4a"
)

tema_base <- theme_minimal(base_family = "Georgia") +
  theme(
    plot.title       = element_text(size = 14, face = "bold",   color = "#1a1a2e", margin = margin(b = 6)),
    plot.subtitle    = element_text(size = 10, color = "#555577", margin = margin(b = 12)),
    plot.caption     = element_text(size = 7,  color = "#888888", hjust = 0, margin = margin(t = 10)),
    plot.background  = element_rect(fill = "#FAFAF8", color = NA),
    panel.background = element_rect(fill = "#FAFAF8", color = NA),
    panel.grid.major = element_line(color = "#E8E8E0", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(size = 9,  color = "#444444"),
    axis.title       = element_text(size = 9,  color = "#333333", face = "italic"),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 9, color = "#333333"),
    strip.text       = element_text(size = 9, face = "bold", color = "#1a1a2e")
  )

caption_padrao <- "Fonte: IF.data/BCB. Modelos TWFE com cluster por instituição. Período: 2015–2025."

altas_selic <- data.frame(
  xmin = as.Date(c("2015-01-01", "2020-03-01")),
  xmax = as.Date(c("2016-12-31", "2022-12-31")),
  label = c("Crise 2015–16", "Covid-19")
)

media_tcb <- painel_final %>%
  filter(!is.na(delta_credito_w), TCB %in% c("b1","b2","b3S","b3C","n1","n4")) %>%
  mutate(
    Grupo = case_when(
      TCB %in% c("b1","b2")   ~ "Bancos",
      TCB %in% c("b3S","b3C") ~ "Cooperativas",
      TCB %in% c("n1","n4")   ~ "Fintechs"
    )
  ) %>%
  group_by(Data, Grupo) %>%
  summarise(delta_medio = mean(delta_credito_w, na.rm = TRUE), .groups = "drop")

ggplot(media_tcb, aes(x = Data, y = delta_medio, color = Grupo)) +
  geom_rect(
    data = altas_selic,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "#FFE4B5", alpha = 0.5
  ) +
  geom_text(
    data = altas_selic,
    aes(x = xmin + (xmax - xmin) / 2, y = Inf, label = label),
    inherit.aes = FALSE,
    vjust = 1.5, size = 2.8, color = "#B8860B", fontface = "italic"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#AAAAAA", linewidth = 0.5) +
  geom_line(linewidth = 0.9, alpha = 0.85) +
  geom_smooth(se = FALSE, linewidth = 0.3, linetype = "dotted", alpha = 0.5) +
  scale_color_manual(values = cores_tcb) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title    = "Evolução do Crescimento do Crédito por Tipo de Instituição",
    subtitle = "Variação trimestral média da carteira de crédito (log-diferença, winsorizado)",
    x = NULL, y = "Δ Crédito (trimestral)",
    caption  = caption_padrao
  ) +
  tema_base +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Gráfico de Efeitos marginais da selic sobre crédito por nível de capitalização ##
slopes(
  t6,
  variables  = "d_selic",
  newdata    = datagrid(
    basileia_lag_w = quantile(painel_final$basileia_lag_w, seq(0.05, 0.95, by = 0.05), na.rm = TRUE),
    funding_c      = 0,
    tamanho_c      = 0,
    risco_c        = 0
  )
) -> slopes_basileia

as.data.frame(slopes_basileia) %>%
  dplyr::select(basileia_lag_w, estimate, conf.low, conf.high) -> df_slopes

ggplot(df_slopes, aes(x = basileia_lag_w, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#AAAAAA", linewidth = 0.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#1B4F8A", alpha = 0.15) +
  geom_line(color = "#1B4F8A", linewidth = 1.1) +
  geom_point(color = "#1B4F8A", size = 2) +
  annotate("text",
           x = max(df_slopes$basileia_lag_w) * 0.6,
           y = max(df_slopes$conf.high) * 0.85,
           label = "Basileia elevado\namortece a retração",
           size = 3, color = "#1B4F8A", fontface = "italic") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(
    title    = "Efeito Marginal da Selic sobre o Crédito\npor Nível de Capitalização (Basileia)",
    subtitle = "∂(Δ Crédito) / ∂(Selic) para diferentes percentis do Índice de Basileia — SFN",
    x = "Índice de Basileia (nível)", y = "Efeito marginal da Selic",
    caption  = caption_padrao
  ) +
  tema_base

## Coeficientes das interações com Selic por tipo de instituição ##
extrair_coefs <- function(modelo, grupo) {
  cf  <- coef(modelo)
  se  <- sqrt(diag(vcov(modelo)))
  nms <- names(cf)

  idx <- grep("d_selic[^_]|d_selic$|d_selic:", nms)
  if (length(idx) == 0) return(NULL)

  data.frame(
    variavel = nms[idx],
    estimate = cf[idx],
    se       = se[idx],
    grupo    = grupo,
    stringsAsFactors = FALSE
  )
}

df_coefs <- bind_rows(
  extrair_coefs(t6,           "SFN"),
  extrair_coefs(bancos,       "Bancos"),
  extrair_coefs(cooperativas, "Cooperativas"),
  extrair_coefs(fintechs,     "Fintechs")
) %>%
  mutate(
    conf_low  = estimate - 1.96 * se,
    conf_high = estimate + 1.96 * se,
    sig       = !(conf_low < 0 & conf_high > 0),
    label = case_when(
      grepl("basileia", variavel, ignore.case = TRUE) ~ "Selic × Basileia",
      grepl("funding",  variavel, ignore.case = TRUE) ~ "Selic × Funding",
      grepl("tamanho",  variavel, ignore.case = TRUE) ~ "Selic × Tamanho",
      grepl("risco",    variavel, ignore.case = TRUE) ~ "Selic × Risco",
      grepl("^d_selic$|^d_selic_lag1$", variavel)    ~ "Selic (efeito direto)",
      TRUE ~ variavel
    ),
    grupo = factor(grupo, levels = c("SFN", "Bancos", "Cooperativas", "Fintechs"))
  ) %>%
  filter(label != variavel)

ggplot(df_coefs, aes(x = estimate, y = label, color = grupo, shape = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#AAAAAA", linewidth = 0.5) +
  geom_errorbarh(
    aes(xmin = conf_low, xmax = conf_high),
    height = 0.2, linewidth = 0.7, alpha = 0.7,
    position = position_dodge2(preserve = "single")
  ) +
  geom_point(
    size = 3,
    position = position_dodge2(preserve = "single")
  ) +
  scale_color_manual(values = cores_tcb) +
  scale_shape_manual(values = c(`FALSE` = 1, `TRUE` = 19),
                     labels = c("Não significativo", "Significativo (p<0.05)"),
                     guide  = guide_legend(title = NULL)) +
  scale_x_continuous(labels = number_format(accuracy = 0.01)) +
  labs(
    title    = "Coeficientes das Interações com a Selic\npor Tipo de Instituição",
    subtitle = "Estimativas OLS com efeitos fixos — IC 95% | Ponto sólido = significativo a 5%",
    x = "Coeficiente estimado", y = NULL,
    caption  = caption_padrao
  ) +
  tema_base +
  theme(legend.position = "right")

## Efeito de 1 p.p. sobre crescimento do crédito ##
simular_efeito <- function(modelo, grupo, var_nome, var_col, dados) {

  p25 <- quantile(dados[[var_col]], 0.25, na.rm = TRUE)
  p75 <- quantile(dados[[var_col]], 0.75, na.rm = TRUE)

  nd_base <- datagrid(
    model          = modelo,
    newdata        = dados,
    funding_c      = 0,
    tamanho_c      = 0,
    basileia_lag_w = if ("basileia_lag_w" %in% names(dados)) median(dados$basileia_lag_w, na.rm=TRUE) else 0,
    basileia_c     = 0,
    risco_c        = 0
  )

  calcular <- function(val) {
    nd        <- nd_base
    nd[[var_col]] <- val
    avg_slopes(modelo, variables = "d_selic", newdata = nd)
  }

  r25 <- calcular(p25)
  r75 <- calcular(p75)

  bind_rows(
    data.frame(grupo = grupo, moderador = var_nome, percentil = "P25 (baixo)",
               estimate = r25$estimate, conf.low = r25$conf.low, conf.high = r25$conf.high),
    data.frame(grupo = grupo, moderador = var_nome, percentil = "P75 (alto)",
               estimate = r75$estimate, conf.low = r75$conf.low, conf.high = r75$conf.high)
  )
}

dados_lista <- list(
  SFN          = painel_final %>% filter(!is.na(delta_credito_w)),
  Bancos       = painel_bancos %>% filter(!is.na(delta_credito_w)),
  Cooperativas = painel_cooperativas %>% filter(!is.na(delta_credito_w)),
  Fintechs     = painel_fintechs %>% filter(!is.na(delta_credito_w))
)

modelos_lista <- list(
  SFN          = t6,
  Bancos       = bancos,
  Cooperativas = cooperativas,
  Fintechs     = fintechs
)

moderadores <- list(
  list(nome = "Basileia",  col = "basileia_lag_w"),
  list(nome = "Funding",   col = "funding_c"),
  list(nome = "Tamanho",   col = "tamanho_c"),
  list(nome = "Risco",     col = "risco_c")
)

df_efeitos <- purrr::map_dfr(names(modelos_lista), function(grp) {
  purrr::map_dfr(moderadores, function(mod) {
    tryCatch(
      simular_efeito(modelos_lista[[grp]], grp, mod$nome, mod$col, dados_lista[[grp]]),
      error = function(e) NULL
    )
  })
}) %>%
  mutate(
    grupo     = factor(grupo,     levels = c("SFN","Bancos","Cooperativas","Fintechs")),
    percentil = factor(percentil, levels = c("P25 (baixo)","P75 (alto)")),
    sig       = !(conf.low < 0 & conf.high > 0)
  )

ggplot(df_efeitos,
             aes(x = percentil, y = estimate, fill = grupo, alpha = sig)) +
  geom_col(position = position_dodge(0.75), width = 0.65) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(0.75), width = 0.2, linewidth = 0.6, color = "#333333"
  ) +
  geom_hline(yintercept = 0, color = "#333333", linewidth = 0.4, linetype = "dashed") +
  facet_wrap(~moderador, nrow = 1, scales = "free_y") +
  scale_fill_manual(values = cores_tcb) +
  scale_alpha_manual(values = c(`FALSE` = 0.4, `TRUE` = 0.95), guide = "none") +
  scale_y_continuous(labels = number_format(accuracy = 0.001)) +
  labs(
    title    = "Efeito de +1 p.p. na Selic sobre o Crescimento do Crédito",
    subtitle = "Efeito médio marginal para instituições no P25 vs P75 de cada moderador\n(barras translúcidas = não significativo a 5%)",
    x = NULL, y = "Δ Crédito esperado (+1 p.p. Selic)",
    caption  = caption_padrao
  ) +
  tema_base +
  theme(
    strip.background = element_rect(fill = "#E8E8E0", color = NA),
    axis.text.x      = element_text(size = 8)
  )
