theme_report <- function(base_size = 11,
                         strip_text_size = 12,
                         strip_text_margin = 5,
                         subtitle_size = 13,
                         subtitle_margin = 10,
                         plot_title_size = 16,
                         plot_title_margin = 10,
                         ...) {
    require(ggplot2)
    ret <- theme_minimal(base_family = "Roboto-Regular",
                         base_size = base_size, ...)
    ret$strip.text <- element_text(
        hjust = 0,
        size = strip_text_size,
        margin = margin(b = strip_text_margin),
        family = "Roboto-Bold"
    )
    ret$plot.subtitle <- element_text(
        hjust = 0,
        size = subtitle_size,
        margin = margin(b = subtitle_margin),
        family = "PT Sans"
    )
    ret$plot.title <-
        element_text(
            hjust = 0,
            size = plot_title_size,
            margin = margin(b = plot_title_margin),
            family = "Oswald"
        )
    ret
}

import_data <- function(celebridade = "tom_cruise"){
    #' Usa o nome como aparece na url do rottentomatoes.com 
    #' Ex: Tom Cruise tem a url https://www.rottentomatoes.com/celebrity/tom_cruise/ 
    #' O argumento deve ser tom_cruise
    require(tidyverse, warn.conflicts = F)
    require(rvest)
    require(janitor)
    
    url_alvo = str_glue("https://www.rottentomatoes.com/celebrity/{celebridade}")
    
    from_page <- read_html(url_alvo) %>% 
        html_node(".scroll-x") %>% # A sintaxe da expressão é de um seletor à lá JQuery: https://rdrr.io/cran/rvest/man/html_nodes.html 
        html_node("table") %>%
        html_table(fill=TRUE) %>% # Faz parse
        as_tibble()
    
    filmes = from_page %>% 
        clean_names() %>% 
        rename(rating = tomatometer_r) %>% 
        filter(rating != "No Score Yet", 
               box_office != "—", 
               !(credit %in% c("Producer", "Executive Producer"))) %>%
        mutate(rating = as.numeric(gsub("%", "", rating)),
               credit = gsub("\n *", " ", credit),
               audience_score = str_remove(audience_score, "%"),
               box_office = as.numeric(gsub("[$|M]", "", box_office))) %>% 
        filter(box_office >= 1)
    
    filmes %>% 
        write_csv(here::here(paste0("data/", celebridade, ".csv")))
}

read_imported_data <- function(celebridade){
    read_csv(here::here(paste0("data/", celebridade, ".csv")), 
             col_types = "idccdi") %>%
        rename(filme = title,
               avaliacao = rating, 
               nota_audiencia = audience_score,
               bilheteria = box_office,
               ano = year, 
               papel = credit)
}
