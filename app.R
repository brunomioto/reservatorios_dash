#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# setup -------------------------------------------------------------------


library(shiny)
library(reservatoriosBR)
library(dplyr)
library(forcats) #unused
library(ggplot2)
library(plotly) #unused
library(scales)
library(ggthemes)
library(glue)
library(reactable)
library(lubridate)
library(DT)


tabela <- tabela_reservatorios_ONS()

options_eixo <- c(
  "Volume útil (%)" = "volume_util_percentual",
  "Cota (m)" = "cota_m",
  "Afluência (m³/s)" = "afluencia_m3_s",
  "Defluência (m³/s)" = "defluencia_m3_s",
  "Vazão turbinada (m³/s)" = "vazao_turbinada_m3_s",
  "Vazão natural (m³/s)" = "vazao_natural_m3_s"
)

# ui ----------------------------------------------------------------------

ui <- fluidPage(

    titlePanel("Reservatórios brasileiros"),

    sidebarLayout(

        sidebarPanel(

            selectInput(inputId = "reservatorio",
                        label = "Selecione um reservatório",
                        choices = sort(tabela$reservatorio)),

            selectInput(inputId = "vary",
                        label = "Selecione a variável",
                        choices = c("Volume útil (%)" = "volume_util_percentual",
                                    "Cota (m)" = "cota_m",
                                    "Afluência (m³/s)" = "afluencia_m3_s",
                                    "Defluência (m³/s)" = "defluencia_m3_s",
                                    "Vazão turbinada (m³/s)" = "vazao_turbinada_m3_s",
                                    "Vazão natural (m³/s)" = "vazao_natural_m3_s")),

            dateRangeInput(inputId = "date_range",
                           label = "Selecione um período",
                           start = "2021-01-01",
                           end = Sys.Date(),
                           format = "dd/mm/yyyy",
                           language = "pt-BR",
                           separator = "até"),
            actionButton("go", "Carregar", class = "btn-primary"),
        ),


        mainPanel(
           plotOutput("reservatoriosPlot"),   #mudar depois pra imageOutput
           DT::dataTableOutput("table")


        )
    )
)

# server ------------------------------------------------------------------

server <- function(input, output) {


# variaveis reactive ------------------------------------------------------

  codigo_res <- eventReactive(input$go, {
    tabela %>%
    dplyr::filter(reservatorio %in% input$reservatorio) %>%
    dplyr::select(codigo) %>%
    dplyr::distinct()
    })


  reservatorio_escolha <-  eventReactive(input$go, {
    reservatorio_sin(codigo_reservatorio = codigo_res(),
                     data_inicial = isolate(input$date_range[1]),
                     data_final = isolate(input$date_range[2]))
    })

  titulo_res <- eventReactive(input$go, {
    input$reservatorio
  })

  data_inicio <- eventReactive(input$go, {
    input$date_range[1]
  })

  data_fim <- eventReactive(input$go, {
    input$date_range[2]
  })

  date_n <- eventReactive(input$go, {
    if(abs(time_length(as.Date(data_fim)-as.Date(data_inicio), "months")) <= 24 ){
      "1 month"
    }else{
      "6 months"
    }
  })

#  trans_y <- eventReactive(input$go, {
#
#    function(){
#
#    if(input$vary == volume_util_percentual){
#    scale_y_continuous(
#    breaks = seq(0,100,by=10),
#    limits = c(0,100),
#    labels = percent_format(scale = 1))
#  }else{
#
#  }
#    }
#})

# output ------------------------------------------------------------------


    output$reservatoriosPlot <- renderPlot({  #mudar para imagem

      scale_y <- switch(input$vary,
                        "volume_util_percentual" = seq(0,100,by=10),
                        "cota_m" = waiver(),
                        "afluencia_m3_s" = waiver(),
                        "defluencia_m3_s" = waiver(),
                        "vazao_turbinada_m3_s" = waiver(),
                        "vazao_natural_m3_s" = waiver())

      limits_y <- switch(input$vary,
                        "volume_util_percentual" = c(0, 100),
                        "cota_m" = NULL,
                        "afluencia_m3_s" = NULL,
                        "defluencia_m3_s" = NULL,
                        "vazao_turbinada_m3_s" = NULL,
                        "vazao_natural_m3_s" = NULL)


      g <- reservatorio_escolha() %>%
        ggplot(aes(x=data))+
        geom_line(aes_string(y = input$vary),
                  color = "#377eb8",
                  size=1.2)+
        scale_x_date(labels = label_date_short(),
                     #date_breaks = date_n, #um dia mudo
                     expand = c(0, 0)
                     )+
        scale_y_continuous(breaks = scale_y, limits = limits_y)+
        labs(x = "Data da medição",
             y = names(options_eixo[which(options_eixo == input$vary)]),
             title = glue::glue("Reservatório {titulo_res()} - {format(data_inicio(), '%d/%m/%Y')} a {format(data_fim(), '%d/%m/%Y')}"),
             caption = "Bruno Mioto @BrunoHMioto - Feito com o pacote reservatoriosBR")+
        theme_fivethirtyeight()+
        theme(
          panel.grid.minor.x = element_blank(),
          axis.title = element_text(face = "bold"),
          plot.title.position = "plot"
        )

      g


    })


  output$table <- renderDataTable(server = FALSE,{datatable(reservatorio_escolha() %>%
                                    dplyr::select(
                                      data,
                                      volume_util_percentual,
                                      cota_m,
                                      afluencia_m3_s,
                                      defluencia_m3_s,
                                      vazao_turbinada_m3_s,
                                      vazao_natural_m3_s
                                    ),
                                    extensions = 'Buttons',
                                  options = list(
                                    language = list(
                                      url = "https://cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json"
                                    ),
                                    dom = 'Bfrtip',
                                    buttons = list(
                                      list(extend = "copy", text = "Copiar"
                                      ),
                                      list(extend = "csv", text = "CSV"
                                      ),
                                      list(extend = "excel", text = "Excel"
                                      ),
                                      list(extend = "print", text = "Imprimir"
                                      ))),
                                  rownames = FALSE,
                                  colnames = c("Data",
                                               "Volume útil (%)",
                                               "Cota (m)",
                                               "Afluência (m³/s)",
                                               "Defluência (m³/s)",
                                               "Vazão turbinada (m³/s)",
                                               "Vazão natural (m³/s)")) %>%
    formatRound(columns = c(2,3,4,7), dec.mark = ",") %>%
      formatDate(columns = 1,method = "toLocaleDateString")
})


}

# Run the application
shinyApp(ui = ui, server = server)

# run app -----------------------------------------------------------------

