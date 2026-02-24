# Bank Lending Channel
Painel de instituições financeiras (IF.data/BCB) para estimar o efeito de choques na Selic sobre o crescimento do crédito com heterogeneidade por capitalização, funding, risco e tamanho (TWFE + efeitos marginais).

---

## 📌 Objetivo
Investigar como um aumento na Selic afeta o crescimento do crédito, com foco em heterogeneidade institucional:

- Canal de capital bancário (Índice de Basileia)
- Estrutura de funding (depósitos / captações)
- Risco (proxy de qualidade)
- Tamanho (porte do conglomerado pelo log(Ativo Total)
- Comparação entre **Bancos**, **Cooperativas**, **Fintechs** e **SFN**

---

## 📦 Bases e Fontes
- **IF.data / BCB**: Conglomerados prudenciais e instituições independentes (Resumo, Ativo, Passivo, Demonstração)
- **BCB (SGS via GetBCBData)**: Selic, IPCA, IBC-Br, ICC, spreads, saldos público/privado

> Os arquivos do IF.data são lidos a partir de diretórios locais e **não são versionados** no repositório.

---

## 📦 Variáveis (principais)
Variável | Descrição
---|---
`delta_credito_w` | Crescimento do crédito (log-diferença, winsorizado)
`selic` | Selic trimestral (média)
`d_selic` | Variação trimestral da Selic
`basileia_lag_w` | Índice de Basileia (nível/defasado, winsorizado)
`funding_c` | Medida de funding (centralizada)
`tamanho_c` | Tamanho/porte (centralizado)
`risco_c` | Medida de risco (centralizada)
`TCB` | Tipo institucional (bancos/coops/fintechs)

---

## ⚙️ Como Rodar
1. **Baixe/organize os CSVs do IF.data** em pastas locais (exemplo abaixo).
2. Ajuste os caminhos no script:
   - `path_resumo`, `path_passivo`, `path_ativo`, `path_demo`
3. Abra o script no RStudio e execute.


---

## 📈 Gráficos
01: Evolução do crescimento do crédito por tipo de instituição (com janelas destacadas)  
02: Efeito marginal da Selic sobre o crédito por nível de capitalização (slopes)  
03: Coeficientes das interações com Selic por tipo de instituição (IC 95% e significância)  
04: Efeito de +1 p.p. na Selic no P25 vs P75 dos moderadores (Basileia/Funding/Risco/Tamanho)

---

## 📦 Pacotes usados
GetBCBData, tidyverse, dplyr, readr, stringr, lubridate, janitor, fixest, marginaleffects, patchwork, scales, DescTools

---

## 📊 Resultados (resumo)
- Evidência de heterogeneidade no canal de crédito: **capitalização (Basileia) tende a amortecer** o efeito contracionista
- Diferenças de sensibilidade entre **Bancos, Cooperativas e Fintechs**
- Resultados reportados com **efeitos fixos (TWFE)** e **cluster por instituição**

---

© Carlos Sena — Economista | RD Investimentos/XP Investimentos
