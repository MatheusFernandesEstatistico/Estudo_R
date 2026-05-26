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

