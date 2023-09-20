library(shiny)
library(rhandsontable)
library(bslib)
library(tidyverse)
library(gt)


# Constantes --------------------------------------------------------------

N_DADOS <- 5
MIN_DADOS <- -5
MAX_DADOS <- 5
N_PESOS <- 3
N_DEC_PESOS <- 2
N_EPOCAS <- 10


# UI helpers --------------------------------------------------------------

vspace <- function(n = 10) {
  div(style = paste0('height: ', n, 'px;'))
}


myApp <- function(...) {

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  titlePanel('PerceptRomulo'),

  div(
    style = "text-align: right",
    a(
      href = 'https://github.com/fnaufel/Perceptromulo',
      target = '_blank',
      "fnaufel",
      icon('github')
    )
  ),

  tabsetPanel(
    id = 'painel',
    type = 'hidden',

    tabPanelBody(
      value = 'entrada',

      card(
        min_height = '350px',
        card_header('Entrar dados'),
        fluidRow(
          column(4,
                 helpText(p(
                   withMathJax('Arquivos (\\(\\vec a\\))'),
                   br(),
                   '(escolha da lista)'
                 )),
                 rHandsontableOutput('dados')),
          column(4,
                 helpText(
                   p('Rótulos (\\(\\vec y\\))',
                     br(),
                     '(escolha da lista)')
                 ),
                 rHandsontableOutput('rotulos')),
          column(
            4,
            helpText(p(
              'Pesos (\\(\\vec w\\)):',
              br(),
              '(sugestão: entre -1 e 1)'
            )),
            verticalLayout(
              rHandsontableOutput('pesos'),
              vspace(20),
              actionButton('sortear_pesos', 'Sortear\npesos')
            )
          )
        )
      ),

      fluidRow(
        column(
          2,
          vspace(20),
          actionButton('simular', 'Simular'),
          offset = 5
        )
      )

    ),

    tabPanelBody(
      value = 'simulacao',

       card(
         card_header('Simulação'),
         fluidRow(
           column(
             7,
             verticalLayout(
               splitLayout(
                 cellWidths = c('24.5%'),
                 actionButton('inicio', 'Início', width = '100%'),
                 actionButton('anterior', 'Anterior', width = '100%'),
                 actionButton('proximo', 'Próximo', width = '100%'),
                 actionButton('final', 'Último', width = '100%')
               ),
               div(
                 gt_output('chines'),
                 style = 'overflow-y: scroll; height: 500px;'
               )
             )
           ),
           column(
             5,
             plotOutput('grafico')
           )
         )
       ),

       fluidRow(
         column(
           2,
           vspace(20),
           actionButton('voltar', 'Voltar'),
           offset = 5
         )
       )
     )

  ),

  lang = 'pt',
  theme = bs_theme(version = 5, bootswatch = "cerulean")

)


# Server ------------------------------------------------------------------

server <- function(input, output) {
  # Reactive values ---------------------------------------------------------

  df_chines <- reactiveVal()
  tab_chines <- reactiveVal()
  linha <- reactiveVal()
  grafico <- reactiveVal()
  tab_com_linha <- reactiveVal()
  dados <- reactiveVal()
  rotulos <- reactiveVal()
  pesos <- reactiveVal()
  n_linhas <- reactiveVal()

  # DF com dados
  output$dados <- renderRHandsontable({
    if (!is.null(input$dados)) {
      dados = hot_to_r(input$dados)
    } else {
      dados = inicializar_dados()
    }

    rhandsontable(dados) %>%
      hot_table(
        contextMenu = FALSE,
        highlightCol = FALSE,
        highlightRow = FALSE,
        overflow = 'hidden'
      ) %>%
      hot_col(
        col = c(1, 2),
        type = 'dropdown',
        source = c(NA, MIN_DADOS:MAX_DADOS),
        allowInvalid = FALSE
      )
  })

  # DF com rótulos
  output$rotulos <- renderRHandsontable({
    if (!is.null(input$rotulos)) {
      rotulos = hot_to_r(input$rotulos)
    } else {
      rotulos = inicializar_rotulos()
    }

    rhandsontable(rotulos) %>%
      hot_table(
        contextMenu = FALSE,
        highlightCol = FALSE,
        highlightRow = FALSE,
        overflow = 'hidden'
      ) %>%
      hot_col(
        col = 1,
        type = 'dropdown',
        source = c(NA, -1, 1),
        allowInvalid = FALSE
      )
  })

  # DF com pesos iniciais
  output$pesos <- renderRHandsontable({
    if (!is.null(input$pesos)) {
      pesos = hot_to_r(input$pesos)
    } else {
      pesos = inicializar_pesos()
    }
    formatar_tab_pesos(pesos)
  })

  # Sortear pesos iniciais
  observeEvent(input$sortear_pesos, {
    w <- runif(3, -1, 1)
    output$pesos <-
      renderRHandsontable(inicializar_pesos(w) %>% formatar_tab_pesos())
  })

  # Iniciar simulação -------------------------------------------------------

  observeEvent(input$simular, {
    dados_df <- hot_to_r(input$dados) %>% drop_na()
    rotulos_df <- hot_to_r(input$rotulos) %>% drop_na()
    pesos_df <- hot_to_r(input$pesos) %>% drop_na()

    if (nrow(dados_df) > 0 &&
        nrow(dados_df) == nrow(rotulos_df) &&
        nrow(pesos_df) == N_PESOS) {
      updateTabsetPanel(inputId = 'painel', selected = 'simulacao')

      dados(dados_df)
      rotulos(rotulos_df)
      pesos(pesos_df)
      df_chines(rodar(dados(), rotulos(), pesos(), N_EPOCAS))
      n_linhas(nrow(df_chines()))
      tab_chines(gerar_tabela(df_chines()))
      linha(1)
      tab_com_linha(add_linha(tab_chines(), linha()))
      grafico(gerar_grafico(df_chines(),
                            linha(),
                            dados(),
                            rotulos()))

      output$chines <- render_gt(tab_com_linha())
      output$grafico <- renderPlot(grafico())
    } else {
      showModal(
        modalDialog(
          title = 'Dados incompletos',
          p('É obrigatório preencher'),
          tags$ul(
            tags$li('Pelo menos uma linha de "Arquivos", E'),
            tags$li('Pelo menos uma linha de "Rótulos", E'),
            tags$li('Os valores de TODOS os pesos.')
          ),
          footer = modalButton('OK'),
          easyClose = TRUE
        )
      )
    }
  })


  # Observers ---------------------------------------------------------------

  observeEvent(linha(), {
    tab_com_linha(add_linha(tab_chines(), linha()))
    grafico(gerar_grafico(df_chines(),
                          linha(),
                          dados(),
                          rotulos()))
  })


  # Botões de trace ---------------------------------------------------------

  observeEvent(input$inicio, {
    linha(1)
  })

  observeEvent(input$anterior, {
    linha(max(1, linha() - 1))
  })

  observeEvent(input$proximo, {
    linha(min(linha() + 1, n_linhas()))
  })

  observeEvent(input$final, {
    linha(n_linhas())
  })


  # Sair da simulação -------------------------------------------------------

  observeEvent(input$voltar, {
    updateTabsetPanel(inputId = 'painel', selected = 'entrada')
  })


  # Fim SERVER --------------------------------------------------------------

}


# Inicializar dfs ---------------------------------------------------------

inicializar_dados <- function() {
  tibble(x1 = rep(NA_integer_, N_DADOS),
         x2 = rep(NA_integer_, N_DADOS))
}

inicializar_rotulos <- function() {
  tibble(y = rep(NA_integer_, N_DADOS))
}

inicializar_pesos <- function(pesos = rep(NA_real_, N_PESOS)) {
  tibble(w = pesos %>% round(N_DEC_PESOS))
}


# Formatar tabela pesos ---------------------------------------------------

formatar_tab_pesos <- function(pesos) {
  rhandsontable(pesos) %>%
    hot_table(
      contextMenu = FALSE,
      highlightCol = FALSE,
      highlightRow = FALSE,
      overflow = 'hidden'
    ) %>%
    hot_cols(colWidths = 80)

}



# Rodar simulação ---------------------------------------------------------

rodar <- function(dados, rotulos, pesos, n_epocas) {
  # Transformar em matriz e vetor
  dados_mat <- matrix(c(dados$x1, dados$x2),
                      ncol = 2)
  rotulos_vet <- rotulos %>% pull(y)
  pesos_vet <- pesos %>% pull(w)

  # Valores iniciais
  n <- nrow(dados_mat)
  k <- ncol(dados_mat)
  w <- pesos_vet

  # Alocar vetores e listas
  linhas <- n_epocas * n
  epocas_col <- vector('integer', linhas)
  passos_col <- vector('integer', linhas)
  pesos_col <- vector('list', linhas)
  dados_col <- vector('list', linhas)
  produtos_col <- vector('double', linhas)
  rotulos_col <- vector('integer', linhas)
  decisoes_col <- vector('character', linhas)

  # Preencher primeira linha (menos produto e decisão)
  epocas_col[1] <- 1
  passos_col[1] <- 1
  pesos_col[[1]] <- w
  dados_col[[1]] <- c(1, dados_mat[1, ])
  rotulos_col[1] <- rotulos_vet[1]

  linha <- 1
  epoca <- 1
  passo <- 1
  houve_erro <- FALSE
  epoca_mudou <- FALSE

  while (epoca <= n_epocas) {
    proxima <- linha + 1

    # Calcular produto usando pesos atuais
    produtos_col[linha] <- (w %*% dados_col[[linha]])[1, 1]

    # Calcular decisão (e novos pesos, se necessário)
    if (sign(produtos_col[linha]) == sign(rotulos_col[linha])) {
      decisoes_col[linha] <- 'Certo. Pesos continuam inalterados.'
    } else {
      decisoes_col[linha] <- 'Errado. Mudar pesos.'
      # Calcular novos pesos em w
      w <- w - sign(produtos_col[linha]) * dados_col[[linha]]
      houve_erro <- TRUE
    }
    pesos_col[[proxima]] <- w

    # Calcular número do próximo passo
    passo <- (passo + 1) %% (n + 1)

    # Iniciar nova época ou acabar
    # (se não houve mudança OU se n_epocas ultrapassado)
    if (passo == 0) {
      passo <- 1

      if (houve_erro) {
        epoca <- epoca + 1
        if (epoca > n_epocas) {
          break
        }
        epoca_mudou <- TRUE
        houve_erro <- FALSE
      } else {
        break
      }
    }

    # Preencher próxima linha (menos pesos, produto e decisão)
    passos_col[proxima] <- passo
    dados_col[[proxima]] <- c(1, dados_mat[passo, ])
    rotulos_col[proxima] <- rotulos_vet[passo]
    epocas_col[proxima] <- epoca
    if (epoca_mudou) {
      epoca_mudou <- FALSE
    }

    linha <- proxima

  }

  # Retornar tibble só com as linhas preenchidas
  tibble(
    época = epocas_col[1:linha],
    passo = passos_col[1:linha],
    pesos = pesos_col[1:linha],
    dados = dados_col[1:linha],
    produto = produtos_col[1:linha],
    rótulo = rotulos_col[1:linha],
    decisão = decisoes_col[1:linha]
  )

}


# Gerar tabela ------------------------------------------------------------

gerar_tabela <- function(df) {
  df %>%
    gt(rowname_col = 'passo',
       groupname_col = 'época') %>%
    tab_options(table.width = '100%') %>%
    # tab_stub_indent(everything(), 5) %>%
    opt_stylize(style = 2, color = "blue") %>%
    cols_label(
      pesos = md('Pesos (**w**)'),
      dados = md('Dados (**x**)'),
      produto = md('Produto (**w.x**)'),
      rótulo = md('Rótulo (**y**)'),
      decisão = ''
    ) %>%
    tab_style(
      locations = list(cells_stubhead(),
                       cells_column_labels(),
                       cells_body()),
      style = list(cell_borders(color = 'black'))
    ) %>%
    tab_style(
      locations = list(cells_stubhead(),
                       cells_column_labels()),
      style = list(cell_text(align = 'center'))
    ) %>%
    opt_table_font(stack = 'humanist',
                   weight = 400) %>%
    text_transform(
      locations = cells_row_groups(),
      fn = function(x) {
        paste('Época', x)
      }
    ) %>%
    text_transform(
      locations = cells_body(columns = c(pesos, dados)),
      fn = function(x) {
        paste0('[', x, ']')
      }
    ) %>%
    cols_align('right',
               c(pesos, dados)) %>%
    cols_width(pesos ~ pct(18),
               dados ~ pct(12),
               produto ~ pct(15),
               rótulo ~ pct(10),
               decisão ~ pct(40)) %>%
    data_color(
      decisão,
      rows = startsWith(decisão, 'Errado'),
      method = 'factor',
      palette = c('red'),
      apply_to = 'text',
      target_columns = c(decisão, produto, rótulo)
    ) %>%
    opt_all_caps(locations = 'row_group')

}


# Montar gráfico ----------------------------------------------------------

gerar_grafico <- function(df_chines, linha, dados, rotulos) {

  # df_chines:
  #
  # Columns: 7
  # $ época   <dbl> 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6
  # $ passo   <dbl> 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2
  # $ pesos   <list> <0.25, 0.49, -0.18>, <0.25, 0.49, -0.18>, ...
  # $ dados   <list> <1, 5, 4>, <1, 2, 3>, <1, 5, 4>, <1, 2, 3>, ...
  # $ produto <dbl> 1.98, 0.69, -21.02, 9.69, -2.02, 18.69, ...
  # $ rótulo  <int> 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1
  # $ decisão <chr> "Certo. Pesos continuam inalterados.", ...

  # linha: int

  # dados (entrados pelo usuário):
  #
  # 'data.frame':	2 obs. of  2 variables:
  #  $ x1: int  5 2
  #  $ x2: int  4 3

  # rotulos (entrados pelo usuário):
  #
  # 'data.frame':	2 obs. of  1 variable:
  # $ y: int  1 -1

  # Pesos da linha atual: w0, w1, w2
  pesos <- df_chines %>%
    slice(linha) %>%
    pull(pesos)

  pesos <- pesos[[1]]

  w0 <- pesos[1]
  w1 <- pesos[2]
  w2 <- pesos[3]

  # Decisão da linha atual
  decisao <- df_chines %>%
    slice(linha) %>%
    pull(decisão)

  if (startsWith(decisao, 'Certo')) {
    decisao <- 'blue'
  } else {
    decisao <- 'red'
  }

  # Preparar df para plotar
  df <- cbind(dados, rotulos) %>%
    mutate(
      rótulo = case_when(
        y == -1 ~ '-1',
        TRUE ~ '+1'
      )
    )

  # tibble com ponto da vez
  ponto_da_vez <-
    df_chines %>%
    slice(linha) %>%
    unnest_wider(dados, '') %>%
    rename(
      x1 = dados2,
      x2 = dados3
    ) %>%
    mutate(classificação = decisao)

  # plot
  ggplot() +
    # reta
    stat_function(
      fun = function(x) {
        -(w0 + w1 * x) / w2
      },
      xlim = c(MIN_DADOS, MAX_DADOS),
      show.legend = FALSE,
      geom = 'line',
      color = 'gray',
      linewidth = 3
    ) +
    # Ponto da vez
    geom_point(
      data = ponto_da_vez,
      mapping = aes(x1, x2, fill = classificação),
      shape = 21, # círculo preenchido com cor da decisão
      size = 12,  # grande
      show.legend = FALSE
    ) +
    scale_fill_identity() +
    # Todos os pontos
    geom_point(
      mapping = aes(x1, x2, shape = rótulo), # forma depende do rótulo
      data = df,
      size = 6,
      fill = 'white'
    ) +
    scale_shape_manual(values = c('-1' = 25, '+1' = 24)) +
    scale_x_continuous(
      limits = c(MIN_DADOS, MAX_DADOS),
      breaks = MIN_DADOS:MAX_DADOS
    ) +
    scale_y_continuous(
      limits = c(MIN_DADOS, MAX_DADOS),
      breaks = MIN_DADOS:MAX_DADOS
    ) +
    labs(
      x = NULL,
      y = NULL,
      shape = 'rótulo: '
    ) +
    theme_light(20) +
    theme(legend.position = 'bottom')

}



# Adicionar linha à tabela ------------------------------------------------

add_linha <- function(tab_chines, linha) {
  tab_chines %>%
    tab_style_body(
      style = list(cell_borders(
        color = 'black', weight = px(5)
      )),
      rows = linha,
      fn = function(x)
        TRUE
    )

}


# Run the application  ----------------------------------------------------

shinyApp(ui = ui, server = server, ...)

} # Fim myApp


myApp()
