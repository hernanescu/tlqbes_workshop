library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)

# Levantamos datasets
calidad <- read_csv('data/calidad-de-aire-2019.csv') %>% 
    mutate(co_centenario=as.numeric(co_centenario),
           no2_centenario=as.numeric(no2_centenario),
           pm10_centenario=as.numeric(pm10_centenario),
           co_cordoba=as.numeric(co_cordoba),
           no2_cordoba=as.numeric(no2_cordoba),
           pm10_cordoba=as.numeric(pm10_cordoba),
           co_la_boca=as.numeric(co_la_boca),
           no2_la_boca=as.numeric(no2_la_boca),
           pm10_la_boca=as.numeric(pm10_la_boca))

#hacemos todo el procesamiento
centenario <- calidad %>% 
    select(fecha, hora, co_centenario, no2_centenario, pm10_centenario) %>%
    rename('co'=co_centenario,
           'no2'=no2_centenario,
           'pm10'=pm10_centenario) %>% 
    gather(key='medicion', value='valor', c(3:5)) %>% 
    mutate(observatorio='Centenario')

laboca <- calidad %>% 
    select(fecha, hora, co_la_boca, no2_la_boca, pm10_la_boca) %>% 
    rename('co'=co_la_boca,
           'no2'=no2_la_boca,
           'pm10'=pm10_la_boca) %>% 
    gather(key='medicion', value='valor', c(3:5)) %>% 
    mutate(observatorio='La Boca')

cordoba <- calidad %>% 
    select(fecha, hora, co_cordoba, no2_cordoba, pm10_cordoba) %>% 
    rename('co'=co_cordoba,
           'no2'=no2_cordoba,
           'pm10'=pm10_cordoba) %>% 
    gather(key='medicion', value='valor', c(3:5)) %>% 
    mutate(observatorio='Córdoba')

#esta es la base con la que vamos a trabajar
calidad_tidy <- rbind(centenario, cordoba, laboca)

# UI
ui <- fluidPage(
    title = 'Calidad del aire en Buenos Aires',
    theme = shinytheme('flatly'),
    #Título de la página
    div(
        h1(class = 'page-header', 'Calidad del Aire en Buenos Aires', tags$small('Workshop - Todo lo que Brilla es Shiny')),
        p(class = 'lead', 'Esta aplicación cuenta con el registro de las primeras 18 semanas de 2019. La información 
          se obtuvo del', strong(a(href = 'https://data.buenosaires.gob.ar/', target = '_blank', 'Portal Buenos Aires Data.')))
        
    ),
    
    # Sidebar: slider para los bins 
    sidebarLayout(
        sidebarPanel(
            #primer parámetro: el selector de observatorio
            selectInput(inputId = "obs", #id
                        label = "Observatorio",#lo que figura
                        choices = c('La Boca', 'Centenario', 'Córdoba'), #choices y values acá son lo mismo
                        selected = 'Centenario'), #cuál es el default?
            
            hr(),
            
            #segundo parámetro: un slider
            sliderInput(inputId = "size", 
                        label = "Tamaño del punto:", 
                        min = 1, max = 3, 
                        value = 1),
            
            hr(),
            
            #tercer parámetro: rango de fechas
            dateRangeInput(inputId = "fecha",
                           label="Fecha:",
                           start = "2019-01-01",
                           end = "2019-06-01",
                           min="2019-01-01",
                           max="2019-06-01"),
            hr(),
            
            downloadButton(outputId = 'downloadplot', label = 'Descarga')
        ),

        # El panel principal: ¿qué queremos ver?
        mainPanel(
           plotOutput('grafico'), #id
           br(),
           uiOutput('texto')
        )
    )
)

# Server: donde ocurre la magia
server <- function(input, output) {
    
    subset_observatorio <- reactive({
        calidad_tidy %>% 
            filter(observatorio %in% input$obs)%>% 
            mutate(fecha=ymd(fecha)) %>%
            filter(., between(fecha, input$fecha[1], input$fecha[2])) %>% #importante
            group_by(week(fecha), observatorio, medicion) %>%
            filter(!is.na(valor)) %>% 
            summarise(valor=mean(valor, na.rm = TRUE)) %>% 
            ungroup()
    })
    
    obs_title <- reactive({unique(subset_observatorio()$observatorio)})
    plot_title <- reactive({paste0('Mediciones en Observatorio ', obs_title())})
    
    graf_plot <- reactive({
        
       ggplot(subset_observatorio())+
            geom_line(aes(x=`week(fecha)`, y=valor, group=medicion, color=medicion))+
            geom_point(aes(x=`week(fecha)`, y=valor), size=input$size)+
            labs(x='Semanas',
                 y='Valores',
                 title=plot_title(),
                 subtitle = 'Fuente: datos.buenosaires.gob.ar')+
            theme_minimal()
    }) 

    output$grafico <- renderPlot({
       print(graf_plot())
    })
    
    output$texto <- renderUI({
        
        #Vamos a armar un párrafo automático! Para eso vamos a construir cinco objetos:
        #valores máximos en el período (tres, uno por medición)
        #las fechas (dos, inicio y fin)
        
        max_co <- reactive({
            subset_observatorio() %>% 
            filter(medicion=='co') %>%
            select(valor) %>% 
            max(.$valor, na.rm=TRUE)
            })
        
        max_no2 <- reactive({
            subset_observatorio() %>% 
            filter(medicion=='no2') %>%
            select(valor) %>% 
            max(.$valor, na.rm=TRUE)
        })
        
        max_pm10 <- reactive({
            subset_observatorio() %>% 
            filter(medicion=='pm10') %>%
            select(valor) %>% 
            max(.$valor, na.rm=TRUE)
            })
        
        
        fecha_ini <- input$fecha[1]
        fecha_fin <- input$fecha[2]
        
        HTML(paste0('Para el período entre ', fecha_ini, ' y ', fecha_fin, ', los valores más altos son: <br> <br>
                    Monóxido de carbono (CO): ', round(max_co(), 2), '<br>
                    Dióxido de nitrógeno (NO2): ', round(max_no2(), 2), '<br>
                    Particulas respirables (PM10): ', round(max_pm10(), 2)))
    })
    
    output$downloadplot <- downloadHandler(
        filename=function(){
            paste('calidad-aire-', Sys.Date(),'.png', sep="")
        },
        content = function(file){ #por esto es que conviene el objeto aparte
            ggsave(file, plot = graf_plot(), width = 31, height = 17, 
                   units = 'cm', device='png', dpi=300)
        }
    )
}

# App: esta es la parte donde todo corre
shinyApp(ui = ui, server = server)
