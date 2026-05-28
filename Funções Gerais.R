library(tidyverse)
library(flextable)
set_flextable_defaults( 
  font.family = "Calibri (Corpo)", font.size = 10, 
  border.color = "black", big.mark = "")

# Função Tabela de Freq. Absoluta e Relativa 
fafr = function(Var1, nome_var, ordenar = TRUE) {
  FA = table(Var1, useNA = "ifany") 

  d1 = data.frame(
    Categoria = names(FA),
    Freq_Abs = as.integer(FA),
    Freq_Rel = as.numeric(prop.table(FA)),
    stringsAsFactors = FALSE)

  d1 = d1 %>% 
    add_row(
      Categoria = "Total",
      Freq_Abs = sum(d1$Freq_Abs),
      Freq_Rel = sum(d1$Freq_Rel),
      .after = nrow(.))
  
  d2 = d1 %>% 
    mutate(
      Freq_Rel_Pct = Freq_Rel * 100,
      Freq_Rel_Fmt = paste0(sprintf("%.1f", Freq_Rel * 100), "%")
    ) %>% 
    select(Categoria, Freq_Abs, Freq_Rel_Fmt)

  if(ordenar) {
    d2 = d2 %>% 
      mutate(is_total = Categoria == "Total") %>%
      arrange(is_total, desc(Freq_Abs)) %>%
      select(-is_total)
  }
  
  ft = d2 %>% flextable() %>% bold(part = "header") %>% 
    set_header_labels(
      Categoria = nome_var, 
      Freq_Abs = "Frequência Absoluta", 
      Freq_Rel_Fmt = "Frequência Relativa") %>%
    fontsize(part = "header", size = 12) %>% 
    set_table_properties(layout = "autofit", width = 0) %>% 
    set_caption(
      caption = as_paragraph(
        as_chunk(
          paste("Tabela", n_tabela , "-", nome_var), props = fp_text_default(bold = TRUE)))) %>% 
    align(align = "right", part = "all") %>% 
    align(j = "Categoria", align = "left", part = "all") %>% 
    bg(i = nrow(d2), bg = "#83dea5", part = "body") %>% 
    bold(i = nrow(d2))
  
  return(ft)
}

# Função Tabela de Cruzamentos
cruzamentos = function(Var1, Var2, nomev1, nomev2, data, n_cruzamento = "") {

  c1 = data %>%
    count({{ Var1 }}, {{ Var2 }}, name = "Freq") %>%
    filter(Freq > 0)

  total = sum(c1$Freq)

  c2 = c1 %>%
    mutate(
      p                   = Freq / total * 100,
      porcentagem_formatada = paste0(round(p, 1), "%")) %>%
    arrange(desc(Freq))

  linha_total = tibble(
    {{ Var1 }} := "Total",
    {{ Var2 }} := "",
    Freq                  = total,
    p                     = 100,
    porcentagem_formatada = "100,0%")

  c3 = bind_rows(c2, linha_total) %>%
    select({{ Var1 }}, {{ Var2 }}, Freq, porcentagem_formatada)

  caption_label = if (n_cruzamento != "") {
    paste("Tabela de Cruzamento", n_cruzamento, "-", nomev1, "x", nomev2)
  } else {
    paste("Tabela de Cruzamento -", nomev1, "x", nomev2)
  }

  c3 %>%
    flextable() %>%
    bold(part = "header") %>%
    set_header_labels(
      values = setNames(
        list(nomev1, nomev2, "Frequência Absoluta", "Frequência Relativa"),
        c(as_name(enquo(Var1)), as_name(enquo(Var2)), "Freq", "porcentagem_formatada"))) %>%
    fontsize(part = "header", size = 12) %>%
    set_table_properties(layout = "autofit", width = 0) %>%
    set_caption(
      caption = as_paragraph(
        as_chunk(caption_label,
                 props = fp_text_default(bold = TRUE)))) %>%
    align(align = "right", part = "all") %>%
    align(j = 1, align = "left", part = "all") %>%
    bg(i = nrow(c3), bg = "#83dea5", part = "body") %>%
    bold(i = nrow(c3))
}

# Função tabela de associações (Teste Qui-Quadrado)
