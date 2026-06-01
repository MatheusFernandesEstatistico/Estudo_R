library(tidyverse)
library(readxl)   # substitui xlsx (mais leve, sem dependência de Java)
library(vcd)
library(officer)  # necessário para fp_text_default() nas captions

library(flextable)
set_flextable_defaults(
  font.family  = "Calibri (Corpo)",
  font.size    = 10,
  border.color = "black",
  big.mark     = "")

n_tabela     <- 0
n_graf       <- 0
n_cruzamento <- 0
n_qq         <- 0


# Auxiliar: monta uma flextable com caption padrão 
.ft_caption <- function(ft, titulo, subtitulo = NULL, bold = TRUE) {
  partes <- list(
    as_chunk(titulo, props = fp_text_default(bold = bold, font.size = 10))
  )
  if (!is.null(subtitulo)) {
    partes <- c(partes, list(
      as_chunk("\n"),
      as_chunk(subtitulo, props = fp_text_default(italic = TRUE, font.size = 9))
    ))
  }
  set_caption(ft, caption = do.call(as_paragraph, partes))
}

# Auxiliar: monta tabela FA/FR padronizada 
.tabela_fafr <- function(variavel) {
  FA <- table(variavel)
  FR <- FA / sum(FA)
  d  <- data.frame(FA, FR) # colunas: Var1, Freq, Freq.1
  
  d <- d %>%
    add_row(
      Var1   = "Total",
      Freq   = sum(d$Freq),
      Freq.1 = sum(d$Freq.1),
      .after = nrow(.)
    ) %>%
    mutate(porcentagem_formatada = paste0(round(Freq.1 * 100, 1), "%"))
  
  d %>%
    select(Var1, Freq, porcentagem_formatada) %>%
    filter(Freq > 0) %>%
    slice(c(order(-Freq[seq_len(n() - 1)]), n()))
}


# Função Tabela de Freq. Absoluta e Relativa
fafe <- function(variavel, nome_var) {
  n_tabela <<- n_tabela + 1
  
  .tabela_fafr(variavel) %>%
    flextable() %>%
    bold(part = "header") %>%
    set_header_labels(
      Var1                  = nome_var,
      Freq                  = "Frequência Absoluta",
      porcentagem_formatada = "Frequência Relativa"
    ) %>%
    fontsize(part = "header", size = 12) %>%
    set_table_properties(layout = "autofit", width = 0) %>%
    .ft_caption(paste("Tabela", n_tabela, "-", nome_var)) %>%
    align(align = "right", part = "all") %>%
    align(j = "Var1", align = "left", part = "all") %>%
    bg(i = ~ Var1 == "Total", bg = "#83dea5", part = "body") %>%
    bold(i = ~ Var1 == "Total")
}



# Função Tabela de Cruzamentos 
cruzamentos <- function(Var1, Var2, nomev1, nomev2) {
  n_cruzamento <<- n_cruzamento + 1
  
  c1 <- xtabs(~ Var1 + Var2) %>% as.data.frame()
  
  c2 <- c1 %>%
    mutate(
      FR                    = Freq / sum(Freq),
      p                     = FR * 100,
      porcentagem_formatada = paste0(round(p, 1), "%")
    ) %>%
    add_row(
      Var1                  = "Total",
      Var2                  = "",
      Freq                  = sum(.$Freq),
      p                     = sum(.$p),
      porcentagem_formatada = paste0(round(sum(.$p), 1), "%"),
      .after                = nrow(.)
    )
  
  c3 <- c2 %>%
    select(Var1, Var2, Freq, porcentagem_formatada) %>%
    filter(Freq > 0) %>%
    slice(c(order(-Freq[seq_len(n() - 1)]), n()))
  
  c3 %>%
    flextable() %>%
    bold(part = "header") %>%
    set_header_labels(
      Var1                  = nomev1,
      Var2                  = nomev2,
      Freq                  = "Frequência Absoluta",
      porcentagem_formatada = "Frequência Relativa"
    ) %>%
    fontsize(part = "header", size = 12) %>%
    set_table_properties(layout = "autofit", width = 0) %>%
    .ft_caption(paste("Tabela de Cruzamento", n_cruzamento, "-", nomev1, "x", nomev2)) %>%
    align(align = "right", part = "all") %>%
    align(j = "Var1", align = "left", part = "all") %>%
    bg(i = ~ Var1 == "Total", bg = "#83dea5", part = "body") %>%
    bold(i = ~ Var1 == "Total")
}


# Função tabela de associações (Qui-quadrado + V de Cramér) 
associacao <- function(Var1, Var2, nomev1, nomev2) {
  n_qq <<- n_qq + 1
  
  tabela     <- table(data.frame(Var1, Var2))
  teste_qui  <- chisq.test(tabela, simulate.p.value = TRUE)
  assoc_stat <- assocstats(tabela)
  
  data.frame(
    "Estatística de Teste" = teste_qui$statistic,
    "P-Valor"              = teste_qui$p.value,
    "V de Cramér"          = ifelse(
      teste_qui$p.value > 0.05, "-",
      as.character(round(assoc_stat$cramer, 4))
    ),
    check.names = FALSE
  ) %>%
    flextable() %>%
    set_table_properties(layout = "autofit", width = 0) %>%
    .ft_caption(
      titulo    = paste("Tabela", n_qq, "de Associação"),
      subtitulo = paste("Associação entre", nomev1, "e", nomev2)
    ) %>%
    align(align = "center", part = "all")
}


# Função tabela de associações (Teste de Fisher) 
fisher_test <- function(Var1, Var2, nomev1, nomev2) {
  n_qq <<- n_qq + 1
  
  tabela <- table(data.frame(Var1, Var2))
  teste  <- fisher.test(tabela, simulate.p.value = TRUE)
  
  data.frame("P-Valor" = teste$p.value, check.names = FALSE) %>%
    flextable() %>%
    set_table_properties(layout = "autofit", width = 0.4) %>%
    .ft_caption(
      titulo    = paste("Tabela", n_qq, "de Associação (Fisher)"),
      subtitulo = paste("Associação entre", nomev1, "e", nomev2)
    ) %>%
    align(align = "center", part = "all")
}


# Função tabela de Correlação de Pearson 
correlacao <- function(Var1, Var2, nomev1, nomev2) {
  n_qq <<- n_qq + 1
  
  cor_test <- cor.test(Var1, Var2)
  
  data.frame(
    "Estatística de Teste" = cor_test$statistic,
    "P-Valor"              = cor_test$p.value,
    "Correlação de Pearson" = cor(Var1, Var2, use = "complete.obs"),
    check.names = FALSE
  ) %>%
    flextable() %>%
    set_table_properties(layout = "autofit", width = 0) %>%
    .ft_caption(
      titulo    = paste("Tabela", n_qq, "de Correlação"),
      subtitulo = paste("Correlação entre", nomev1, "e", nomev2)
    ) %>%
    align(align = "center", part = "all")
}


# Função tabela de Teste de normalidade (Shapiro-Wilk) 
normalidade <- function(variavel) {
  swt <- shapiro.test(variavel)
  
  data.frame(
    "Estatística W *" = swt$statistic,
    "P-Valor **"      = format(swt$p.value, scientific = TRUE, digits = 4),
    check.names = FALSE
  ) %>%
    flextable() %>%
    set_table_properties(layout = "autofit", width = 0) %>%
    .ft_caption(
      titulo    = "Teste de Shapiro-Wilk",
      subtitulo = "Resultado da normalidade dos dados"
    ) %>%
    align(align = "center", part = "all") %>%
    add_footer_lines(values = c(
      "* Quanto mais próximo de 1, mais normal é a distribuição.",
      "** P-Valor > 0,05: Não rejeita H₀ → dados normais"
    )) %>%
    fontsize(part = "footer", size = 8) %>%
    color(part = "footer", color = "black") %>%
    padding(part = "footer", padding = 1)
}


# Função tabela de Medidas descritivas 
medidas <- function(colunas) {
  n_tabela <<- n_tabela + 1
  
  db %>%
    select(all_of(colunas)) %>%
    mutate(across(everything(), as.numeric)) %>%
    select(where(is.numeric)) %>%
    map_dfr(~ data.frame(
      Media   = mean(., na.rm = TRUE),
      Mediana = median(., na.rm = TRUE),
      Desvio  = sd(., na.rm = TRUE),
      Q1      = quantile(., 0.25, na.rm = TRUE),
      Q3      = quantile(., 0.75, na.rm = TRUE)
    ), .id = "Coluna") %>%
    mutate(across(where(is.numeric), ~ round(., 2))) %>%
    flextable() %>%
    bold(part = "header") %>%
    fontsize(part = "header", size = 12) %>%
    set_table_properties(layout = "autofit", width = 0) %>%
    .ft_caption(paste("Tabela", n_tabela, "- Medidas Descritivas")) %>%
    align(align = "center", part = "all") %>%
    align(j = "Coluna", align = "left", part = "all")
}


# Função tabela de Teste de Wilcoxon pareado (pré × pós) 
wilcox_pareado <- function(pre, pos, nome_variavel) {
  n_qq <<- n_qq + 1
  
  teste <- wilcox.test(pre, pos, paired = TRUE, exact = FALSE)
  
  data.frame(
    "V de Wilcoxon" = teste$statistic,
    "P-Valor"       = teste$p.value,
    "Mediana Pré"   = median(pre, na.rm = TRUE),
    "Mediana Pós"   = median(pos, na.rm = TRUE),
    check.names = FALSE
  ) %>%
    flextable() %>%
    set_table_properties(layout = "autofit", width = 0) %>%
    .ft_caption(
      titulo    = paste("Tabela", n_qq, "- Teste de Wilcoxon Pareado"),
      subtitulo = nome_variavel
    ) %>%
    align(align = "center", part = "all")
}


# Auxiliar: histograma padrão 
histograma <- function(variavel, nome_var, bins = 20) {
  n_graf <<- n_graf + 1
  
  ggplot(data.frame(x = variavel), aes(x = x)) +
    geom_histogram(bins = bins, fill = "#4b9b69", color = "white", na.rm = TRUE) +
    labs(
      title = paste("Gráfico", n_graf, "- Distribuição de", nome_var),
      x     = nome_var,
      y     = "Frequência"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
}

