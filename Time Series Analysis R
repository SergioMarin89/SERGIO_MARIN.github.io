#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(forecast)
library(descomponer)
library(shinythemes)
library(taRifx)
library(TSA)
library(msir)
library(fANCOVA)
library(ggplot2)
library(plotly)
packageVersion('plotly')

# Define UI for data upload app ----


ui <- fluidPage(theme = shinytheme("superhero"),shinythemes::themeSelector(),

  navbarPage("Analisis serie de tiempo",
             tabPanel("Analisis descriptivo",

              # App title ----
              titlePanel("A N A L I - C"),

              # Sidebar layout with input and output definitions ----
              sidebarLayout(

              # Sidebar panel for inputs ----
              sidebarPanel(

              fileInput('TextFile', 'C A R G U E - D A T O S',
                        accept = c(
                        'text/csv',
                        'text/comma-separated-values',
                        'text/tab-separated-values',
                        'text/plain')),

              tags$hr(),

              radioButtons('skipper', 'R E G I S T R O S - A - S A L T A R',
                   c(ONE=1,
                     ZERO=0

                   )),
              tags$hr(),

              numericInput('year','A N I O - I N I C I O',value =2017 ),
              tags$hr(),
              sliderInput('month','M E S - I N I C I O',value=01, min = 01,max = 12),
              tags$hr(),
              sliderInput('frequency','F R E C U E N C I A',value = 12,min=01,max=12)
              ),#parentesis sidebarPanel


              mainPanel(
                tabsetPanel(

                  tabPanel("Analisis basico ST",
                          h4("Grafico de la serie de tiempo"),
                          plotOutput("contents1"),

                          #Horizontal line ----
                          tags$hr(),

                          h4("Histograma de la serie"),
                          plotOutput("hist_serie"),

                          #Horizontal line ----
                          tags$hr(),

                          h4("Resumen de los datos"),
                          verbatimTextOutput("contents2"),

                          #Horizontal line ----
                          tags$hr(),

                          h4("Determinar serie Aditiva o Multiplicativa"),
                          plotOutput("logdata")
                          ),#parentesis Analisis basico de la serie

                  tabPanel("Autocorrelacion",
                          h4("Autocorrelacion ACF Datos"),
                          plotOutput("pac_Data"),

                          tags$hr(),
                          h4("Autocorrelacion parcial PACF Datos"),
                          plotOutput("pacf_Data"),

                          tags$hr(),

                          h4("Autocorrelacion ACF logData"),
                          plotOutput("pacLogData"),

                          tags$hr(),

                          h4("Autocorrelacion parcial PACF logData"),
                          plotOutput("pacfLogData")
                          ),#pare©ntesis tabpanel Autocorrelacion

                  tabPanel("Estacionalidad - Descomp",
                          h4("Estacionalidad"),
                          plotOutput("estacionalidad"),

                          tags$hr(),

                          h4("Descomposicion multiplicativa de la serie original"),
                          plotOutput("descomp_mult_data"),

                          tags$hr(),

                          h4("Descomposicion aditiva del logaritmo"),
                          plotOutput("descomp_adit_Log"),

                          tags$hr(),

                          h4("Descomposicion aditiva de la serie"),
                          plotOutput("descomp_adit_data")

                          ),#parentesis tabpanel estacionalidad y descomposiciÃÂ³n


                  tabPanel("Tendencia - Error",

                        h4("Tendencia estimada por descomposicion clasica"),
                        plotOutput("tendencia_desc_data"),

                        tags$hr(),

                        h4("Tendencia estimada por descomposicion clasica (Log)"),
                        plotOutput("tendencia_desc_logdata"),

                        tags$hr(),

                        h4("Error de los datosy logdatos ADITIVA"),
                        plotOutput("error_datos_logdatos"),

                        tags$hr(),

                        h4("Error de los datosy logdatos MULTIPLICATIVA"),
                        plotOutput("error_datos_logdatos_multi"),

                        tags$hr(),

                        h4("Periodograma sobre los logaritmos diferenciados"),
                        plotOutput("periodograma_log")

                      )#parentesis tabpanel Tendencia error
            )#parentesis del tapsetpanel
          )#parentesis main panel
        )#parentesis sidebarlayout
      ),#parentesis tabpanel

      tabPanel("MODELO 1",

               titlePanel("LOG CUADRÁTICO ESTACIONAL"),

               mainPanel(
                 tabsetPanel(
                   tabPanel("Modelo 1",
                            h4("Serie de tiempo de valores ajustados en escala original"),
                            verbatimTextOutput("st_ajustado_original"),

                            tags$hr(),
                            h4("Serie de tiempo de valores ajustados en escala original"),
                            plotOutput("ythat1_ajustado_original"),

                            tags$hr(),
                            h4("Grafico de residuales en escala LOG con limites 2 sigma"),
                            plotOutput("residuales2sigma"),

                            tags$hr(),
                            h4("Pronosticando n periodos en escala original"),
                            tableOutput("pronostico_periodos"),

                            tags$hr(),
                            h4("Serie de tiempo con pronosticos puntuales"),
                            plotOutput("pronostico_puntuales")

                   )#parentesis tabPanel interior
                 )#parentesis tabsetPanel Modelo 1
               )#parentesis mainPanel Modelo 1
      ),#parentesis tabPanel Modelo1

      tabPanel("MODELO 2",

               titlePanel("MODELO LOG CUBICO ESTACIONAL"),

               mainPanel(
                 tabsetPanel(
                   tabPanel("Modelo cubico estacional",
                            h4("Summary modelo cubico estacional"),
                            verbatimTextOutput("summary_mod_cubico"),

                            tags$hr(),
                            h4("Grafico de residuales en escale LOG con limites 2 sigma"),
                            plotOutput("residuales_2sigma"),

                            tags$hr(),
                            h4("Pronosticando periordos t=152 a 155 en escala original"),
                            tableOutput("pronostico_periodos_cubico"),

                            tags$hr(),
                            h4("Graficando serie cos ajustes y pronosticos"),
                            plotOutput("pronostico_ajuste_cubico")

                   )#parentesistabpanel
                 )#parentesis tabsetpanel modelo cubico estacional
               )#parentesis main panel modelo cubico estacional
      ),#parÃÂ©ntesis tabpanel principal Modelo log cubico estacional

      tabPanel("MODELO 3",
               titlePanel("MODELO FUNCIONES TRIGONOMETRICAS"),

               mainPanel(
                 tabsetPanel(
                   tabPanel("Modelo funciones trigonometicas",
                            h4("Summary modelo funciones trigonometricas"),
                            verbatimTextOutput("summary_mod_funciones"),

                            tags$hr(),
                            h4("Grafica de residuales con escala de ajuste"),
                            plotOutput("residuales_escala_funciones"),

                            tags$hr(),
                            h4("Pronosticando periordos t=152 a 155 en escala original"),
                            tableOutput("pronostico_periodos_funciones"),

                            tags$hr(),
                            h4("Graficando serie con ajustes y pronosticos"),
                            plotOutput("grafica_pronosticos_funciones")

                   )#modelo tabpanel interior modelo trigonometrico
                 )#parentesis tabsetpanel modelo trigonometrico
               )#parentesis mainpanel modelo trigonemetrico
               ),#parentesis tabpanel principal modelo funciones trigonometricas

      tabPanel("MODELO 4",
               titlePanel("MODELO HOLT WINTERS"),

               mainPanel(
                 tabsetPanel(
                   tabPanel("Modelo Holt Winters",
                            h4("Summary Holt Winters"),
                            verbatimTextOutput("summary_suaviz_HW"),

                            tags$hr(),
                            h4("Residuos vs. Suavizamiento Holt-Winters Multiplicativo"),
                            plotOutput("residuos_suaviz_HW"),

                            tags$hr(),
                            h4("Residuos vs. ajustados \ Suavizamiento Holt-Winters Multiplicativo"),
                            plotOutput("residuos_ajustados_HW"),

                            tags$hr(),
                            h4("Pronosticando periordos Holt Winters"),
                            tableOutput("pronostico_periodos_HW"),

                            tags$hr(),
                            h4("Serie real, ajustes y pronosticos \ Suavizamiento Holt-Winters"),
                            plotOutput("grafica_pronosticos_HW")


                   )#parentesis tabpanel interior holt winetrs
                 )#parentesis tabsetpanel hotl winters
               )#parentesis mainpanel holt winter
      ),#parentesis tabpanel principal holt-winters

      tabPanel("MODELO 5",
               titlePanel("MODELO LOESS"),

               mainPanel(
                 tabsetPanel(
                   tabPanel("Modelo ajuste LOESS",
                            h4("Summary modelo LOESS"),
                            verbatimTextOutput("summary_loess"),

                            tags$hr(),
                            h4("Serie desestacionalizada y su ajuste LOESS cuadrático óptimo"),
                            plotOutput("serie_desestacionalizada_loess"),

                            tags$hr(),
                            h4("Gráfico resuduales LOESS"),
                            plotOutput("graficos_residuales_loess"),

                            tags$hr(),
                            h4("Pronóstico periodos LOESS"),
                            tableOutput("pronostico_periodos_LOESS"),

                            tags$hr(),
                            h4("Gráficca pronóstico periodos LOESS"),
                            plotOutput("grafica_pronostico_LOESS")

                   )#parentesis tabpanel inferior LOESS
                 )#parentesis tabsetpanel LOESS
               )#parenteis mainpanel LOESS
               ),#parentesis tabpanel principal LOESS

      tabPanel("COMPARATIVO",
               titlePanel("Analisis grafico de modelos"),

               mainPanel(
                 tabsetPanel(
                   tabPanel("Comparativo grafico",
                            h4("Comparativo entre los distintos modelos"),
                            plotOutput("comparativo_primario")
                   )#parentesis tabpanel interior comparativo grafico
                 )#parentesis tabsetpanel comparativo grafico
               )#parentesis main panel comparativo grafico
      ),#parentesis tabpanel comparativo grafico

      tabPanel("PRONOSTICOS",
               titlePanel("PRONOSTICOS SELECCIONADO"),

               sidebarLayout(
                 sidebarPanel(

                   radioButtons("modelo", "Seleccione el modelo que mas se ajuste",
                                c("LOG CUADRÁTICO ESTACIONAL",
                                  "LOG CUBICO ESTACIONAL",
                                  "FUNCIONES TRIGONOMETRICAS",
                                  "HOLT WINTERS",
                                  "AJUSTE LOESS"
                                )#parentesis combine radiobuttons
                   )#parentesis radiobuttons
                 ),#parentesis sidebarpanel PRONOSTICOS

               mainPanel(
                 tabsetPanel(
                   tabPanel("LOG CUADRÁTICO ESTACIONAL",

                            h4("Ajuste Modelo Log Cuadratico Estacional"),
                            plotOutput("ajuste_log_cuadratico"),

                            tags$hr(),
                            h4("Grafica serie real Vs ajustada"),
                            plotOutput("real_vs_ajustada"),

                            tags$hr(),
                            h4("Pronóstico del Modelo"),
                            tableOutput("pronostico_del_modelo"),

                            tags$hr(),
                            h4("Gráfica pronóstico del Modelo"),
                            plotOutput("grafica_pronostico_del_modelo")
                            ),#parentesis tabpanel LOG CUADRATICO

                   tabPanel("LOG CUBICO ESTACIONAL",

                            h4("Ajuste Modelo Log cúbico Estacional"),
                            plotOutput("ajuste_log_cubico"),

                            tags$hr(),
                            h4("Grafica serie real Vs ajustada"),
                            plotOutput("real_vs_ajustada_cubico"),

                            tags$hr(),
                            h4("Pronóstico del Modelo"),
                            tableOutput("pronostico_del_modelo_cubico"),

                            tags$hr(),
                            h4("Gráfica pronóstico del Modelo"),
                            plotOutput("grafica_pronostico_del_modelo_cubico")
                   )#parentesis tabpanel LOG CUBICO
                 )#paretesis tabsetPanel PRONOSTICOS
               )#parentesis mainpanel PRONOSTICOS
              )#parentesis sidebarlayout PRONOSTICOS
            )#parentesis tabpanel principal PRONOSTICOS
    )#Pareentesis NavbarPage
  )#parentesis fuidPage

# Define server logic to read selected file ----
server <- function(input, output) {

  data_l<-reactive({
    inFile <- input$TextFile
    if (is.null(inFile))
      return(NULL)

    data_st<-ts(scan(inFile$datapath,skip=input$skipper),start=c(input$year,input$month),
             frequency = input$frequency)
    return(data_st)
  })#anterior


    #================= ANALISIS DESCRIPTIVO DE LA SERIE ======================

    #GRAFICAR LA SERIE DE TIEMPO
    output$contents1 <- renderPlot({
    data_st<-data_l()
    plot(data_st, ylim=c(min(data_st),max(data_st)), ylab = "kW Consumo", type = 'o', lwd = 2,col="darkorange")
    grid()

  })#anterior

    #ADICIONAR EL RESUMEN DE LOS DATOS
    output$contents2 <- renderPrint({
    data_st<-data_l()
    summary(data_st)})#anterior

    #ADICIONAR EL HISTOGRAMA DE LOS DATOS
    output$hist_serie <- renderPlot({
      data_st<-data_l()
      hist(data_st,freq = FALSE, col="darkorange", lwd = 2, main = "Hist consumo energía")
      })#anterior

    #GRAFICAR LOGDATA PARA DETERMINAR MOD ADITIVO - MULTIPLICATIVO
    output$logdata <- renderPlot({
      data_st <- data_l()
      logdata <- log(data_st)
      plot(logdata,ylim=c(min(logdata),max(logdata)),col="darkorange", lwd = 2)
      title(main="Log(data)")
      grid()
          })#anterior

    #GRAFICAR AUTOCORRELACION DATOS
    output$pac_Data <- renderPlot({
      data_st <- data_l()
      acf(data_st,col="darkorange", lwd = 2)
    })#anterior

    #GRAFICAR AUTOCORRELACION PARCIAL DATOS
    output$pacf_Data <- renderPlot({
      data_st <- data_l()
      pacf(data_st,col="darkorange", lwd = 2)
    })#anterior

    #GRAFICAR PAC LOGDATOS
    output$pacLogData <- renderPlot({
      data_st <- data_l()
      logdata <- log(data_st)
      acf(logdata,col="darkorange", lwd = 2)

    })#anterior

    #GRAFICAR PACF LOGDATOS
    output$pacfLogData <- renderPlot({
      data_st <- data_l()
      logdata <- log(data_st)
      pacf(logdata,col="darkorange", lwd = 2)
    })#anterior

    #GRAFICAR ESTACIONALIDAD
    output$estacionalidad <- renderPlot({
      data_st <- data_l()
      boxplot(data_st~cycle(data_st),names=c("Q1","Q2","Q3","Q4"),col="darkorange", lwd = 2)
      title(main="Estacionalidad")
      grid()

    })#anterior
    #
    #GRAFICAR DESCOMPOSICION MULTIPLICATIVA DE LA SERIE
    output$descomp_mult_data <- renderPlot({
      data_st <- data_l()
      plot(decompose(data_st, type = "multiplicative"),col="darkorange", lwd = 2)
    })#anterior

    #GRAFICAR DESCOMPOSICION ADITIVA DEL LOGARITMO
    output$descomp_adit_Log <- renderPlot({
      data_st <- data_l()
      plot(decompose(log(data_st), type="additive"),col="darkorange", lwd = 2)
    })#anterior

    #GRAFICAR DESCOMPOSICION ADITIVA DE LA SERIR
    output$descomp_adit_data <- renderPlot({
      data_st <- data_l()
      plot(decompose(data_st, type="additive"),col="darkorange", lwd = 2)
    })#anterior

    #GRAFICAR TENDENCIA POR DESCOMPOSION DE LA SERIE
    output$tendencia_desc_data <- renderPlot({
      data_st <- data_l()
      Tt=decompose(data_st,type="additive")$trend
      plot(Tt,ylim=c(min(data_st),max(data_st)),col="darkorange", lwd = 2,main="Componente tendencia estimada por descomposición
     clásica\ gráfico ajustado a rango de variacion de la serie")
      grid()
      })#anterior

    #GRAFICAR TENDENCIA POR DESCOMPOSION DEL LOGARITMO DE LA SERIE
    output$tendencia_desc_logdata <- renderPlot({
      data_st <- data_l()
      log_data <- (data_st)
      Tt_log=decompose(log_data,type="multiplicative")$trend
      plot(Tt_log,ylim=c(min(data_st),max(data_st)),col="darkorange", lwd = 2,main="Componente tendencia estimada por descomposición
           clásica\ gráfico ajustado a rango de variación del log de la serie")
      grid()
    })#anterior

      #GRAFICAR ERROR DATOS Y LOGDATOS ADITIVA
      output$error_datos_logdatos <- renderPlot({
        data_st <- data_l()
        log_data <- (data_st)
        Tt_error_datos=decompose(data_st,type="additive")$random
        plot(Tt_error_datos,main="Componente error estimada por descomposición clásica aditiva",col="darkorange", lwd = 2)
      grid()
    })#anterior

      #GRAFICAR ERROR DATOS Y LOGDATOS MULTIPLICATIVA
      output$error_datos_logdatos_multi <- renderPlot({
        data_st <- data_l()
        log_data <- log(data_st)
        error_datos_logdatos_multi=decompose(data_st,type="multiplicative")$random
        plot(error_datos_logdatos_multi,main="Componente error estimada por descomposición clásica multiplicativa",col="darkorange", lwd = 2)
        grid()
      })#anterior

      #GRAFICAR PERIODODOGRAMA
      output$periodograma_log <- renderPlot({
        data_st <- data_l()
        log_data <- (data_st)
        periodogram(diff(log(data_st)),col="darkorange") #periodograma sobre los logaritmos diferenciados
        abline(v=c(0.25,0.5),col=2,lty=2)
        grid()
      })

      #==================MODELO LOG CUADRATICO ESTACIONAL ====================

      #GRAFICAR SERIE DE TIEMPO DE VALORES AJUSTADOS EN ESCALA ORIGINAL
      output$st_ajustado_original <- renderPrint({
        data_st <- data_l()
        log_data <- log(data_st)
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        plot.ts(yt)
        ###########complemento del an?lisis descritivo
        periodogram(diff(log(yt))) #periodograma sobre los logaritmos diferenciados
        abline(v=c(0.25,0.5),col=2,lty=2)

        ################################

        #############Definiendo estacionalidad

        m=4 #N?umero de per?iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste
        t=1:n #?indice de tiempo
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones
        yt2


        #Definiendo factor para estacionalidad pero s?olo para la regresi?on lineal modelos 1 y 1b
        trimestre=season(yt2)
        trimestre=relevel(trimestre,ref="4Q")

        ######Para el pron?stico
        tnuevo=c((n+1):(n+m)) #definiendo valor de t para las m observaciones a pronosticar
        trimestre.nuevo=season(yt)[(length(yt2)+1):length(yt)] #definiendo trimestre correspondiente a tiempos de pron?ostico,
        #estos s?olo para la regresi?on lineal
        ytf=ts(yt[tnuevo],frequency=4,start=c(2016,4)) #Tomando de la serie completa los ?ultimos m valores que son pronosticados

        ###########Modelo 1
        lnyt2=log(yt2)
        lnyt2
        modelo1=lm(lnyt2~t+I(t^2)+trimestre)
        summary(modelo1)
      })#anterior

      #GRAFICAR SERIE DE TIEMPO DE VALORES AJUSTADOS EN ESCALA ORIGINAL
      output$ythat1_ajustado_original <- renderPlot({

        data_st <- data_l()
        log_data <- log(data_st)
        m = 4 #NÃÂ´umero de perÃÂ´iodos a pronosticar dentro de la muestra
        n = length(data_st)-m #tama~no de la muestra para el ajuste
        t = 1:n #ÃÂ´indice de tiempo
        yt2=ts(data_st[t],frequency=4,start=c(input$year,input$month)) #serie con las primeras n observaciones
        trimestre=seasonal(decompose(yt2))
        #trimestre=relevel(trimestre,ref="4Q")
        lnyt2=log(yt2)
        lnyt2
        modelo1=lm(lnyt2~t+I(t^2)+trimestre)
        ythat1=ts(exp(fitted(modelo1))*exp(summary(modelo1)$sigma^2/2),frequency=4,start=c(input$year,input$month))
        plot.ts(ythat1,col="darkorange", lwd = 2)
      })

      #GRAFICO DE RESIDUALES EN ESCALA LOG CON LIMITES 2sigma
      output$residuales2sigma <- renderPlot({
        data_st <- data_l()
        log_data <- log(data_st)
        m = 4 #NÃÂ´umero de perÃÂ´iodos a pronosticar dentro de la muestra
        n = length(data_st)-m #tama~no de la muestra para el ajuste
        t = 1:n #ÃÂ´indice de tiempo
        yt2=ts(data_st[t],frequency=4,start=c(input$year,input$month)) #serie con las primeras n observaciones
        trimestre=seasonal(decompose(yt2))
        #trimestre=relevel(trimestre,ref="4Q")
        lnyt2=log(yt2)
        lnyt2
        modelo1=lm(lnyt2~t+I(t^2)+trimestre)
        ythat1=ts(exp(fitted(modelo1))*exp(summary(modelo1)$sigma^2/2),frequency=4,start=c(input$year,input$month))
        plot.ts(residuals(modelo1),main="Residuos vs. t en escala Log\nModelo Log cuadratico estacional")
        abline(h=0,col=2)
        abline(h=c(-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma),col=2)
        plot(fitted(modelo1),residuals(modelo1),main="Residuos vs. ajustados en escala Log \ Modelo Log cuadratico estacional")
        abline(h=0,col=2)
        abline(h=c(-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma),col=2)
      })

      ##PRONOSTICANDO PERÃÂ´IODOS t=152 a 155 EN ESCALA ORIGINAL
      output$pronostico_periodos <- renderTable({
        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(input$year,input$month)) #serie con todas las observaciones
        ###########complemento del anÃ¡lisis descritivo
        periodogram(diff(log(yt))) #periodograma sobre los logaritmos diferenciados
        abline(v=c(0.25,0.5),col=2,lty=2)
        m=4 #NÂ´umero de perÂ´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste
        t=1:n #Â´indice de tiempo
        yt2=ts(yt[t],frequency=4,start=c(input$year,input$month)) #serie con las primeras n observaciones

        #Definiendo factor para estacionalidad pero sÂ´olo para la regresiÂ´on lineal modelos 1 y 1b
        trimestre=season(yt2)
        trimestre=relevel(trimestre,ref="4Q")

        ######Para el pronÃ³stico
        tnuevo=c((n+1):(n+m)) #definiendo valor de t para las m observaciones a pronosticar
        trimestre.nuevo=season(yt)[(length(yt2)+1):length(yt)] #definiendo trimestre correspondiente a tiempos de pronÂ´ostico,
        #estos sÂ´olo para la regresiÂ´on lineal
        ytf=ts(yt[tnuevo],frequency=4,start=c(2016,4)) #Tomando de la serie completa los Â´ultimos m valores que son pronosticados

        ###########Modelo 1
        lnyt2=log(yt2)
        lnyt2
        modelo1=lm(lnyt2~t+I(t^2)+trimestre)
        summary(modelo1)
        #Serie de tiempo de valores ajustados en escala original
        ythat1=ts(exp(fitted(modelo1))*exp(summary(modelo1)$sigma^2/2),frequency=4,start=c(1979,1))
        ythat1
        #Calculando AIC y BIC usando C*n(p)
        resmod1.orig=yt2-ythat1 #residuos en la escala original. Usados sÂ´olo para cÂ´alcular AIC y BIC
        #aic1=crit.inf.resid(resmod1.orig,n.par=6)
        #aic1
        #bic1=crit.inf.resid(resmod1.orig,n.par=6,AIC="FALSE")
        #bic1
        #GRÂ´AFICO DE RESIDUALES EN ESCALA LOG CON LÂ´IMITES 2sigma
        plot.ts(residuals(modelo1),main="Residuos vs. t en escala Log\ Modelo Log cuadrático estacional")
        abline(h=0,col=2)
        abline(h=c(-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma),col=2)
        plot(fitted(modelo1),residuals(modelo1),main="Residuos vs. ajustados en escala Log\ Modelo Log cuadrático estacional")
        abline(h=0,col=2)
        abline(h=c(-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma),col=2)
        #PRONOSTICANDO PERÂ´IODOS t=152 a 155 EN ESCALA ORIGINAL
        predicciones1=exp(predict(modelo1,data.frame(t=tnuevo,trimestre=trimestre.nuevo),
                                  interval="prediction"))*exp(summary(modelo1)$sigma^2/2)
        predicciones1=ts(predicciones1,frequency=4,start=c(2016,4))
        predicciones1
      })


      #GRAFICO DE SERIE DE TIEMPO CON PRONOSTICOS PUNTUALES
      output$pronostico_puntuales <- renderPlot({
        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones

        ###########complemento del anÃ¡lisis descritivo
        periodogram(diff(log(yt))) #periodograma sobre los logaritmos diferenciados
        abline(v=c(0.25,0.5),col=2,lty=2)

        ################################

        #############Definiendo estacionalidad

        m=4 #NÂ´umero de perÂ´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste
        t=1:n #Â´indice de tiempo
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones

        #Definiendo factor para estacionalidad pero sÂ´olo para la regresiÂ´on lineal modelos 1 y 1b
        trimestre=season(yt2)
        trimestre=relevel(trimestre,ref="4Q")

        ######Para el pronÃ³stico
        tnuevo=c((n+1):(n+m)) #definiendo valor de t para las m observaciones a pronosticar
        trimestre.nuevo=season(yt)[(length(yt2)+1):length(yt)] #definiendo trimestre correspondiente a tiempos de pronÂ´ostico,
        #estos sÂ´olo para la regresiÂ´on lineal
        ytf=ts(yt[tnuevo],frequency=4,start=c(2016,4)) #Tomando de la serie completa los Â´ultimos m valores que son pronosticados

        ###########Modelo 1
        lnyt2=log(yt2)
        lnyt2
        modelo1=lm(lnyt2~t+I(t^2)+trimestre)
        summary(modelo1)
        #Serie de tiempo de valores ajustados en escala original
        ythat1=ts(exp(fitted(modelo1))*exp(summary(modelo1)$sigma^2/2),frequency=4,start=c(1979,1))
        ythat1
        #Calculando AIC y BIC usando C*n(p)
        resmod1.orig=yt2-ythat1 #residuos en la escala original. Usados sÂ´olo para cÂ´alcular AIC y BIC
        #aic1=crit.inf.resid(resmod1.orig,n.par=6)
        #aic1
        #bic1=crit.inf.resid(resmod1.orig,n.par=6,AIC="FALSE")
        #bic1
        #GRÂ´AFICO DE RESIDUALES EN ESCALA LOG CON LÂ´IMITES 2sigma
        plot.ts(residuals(modelo1),main="Residuos vs. t en escala Log\nModelo Log cuadrÂ´atico estacional")
        abline(h=0,col=2)
        abline(h=c(-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma),col=2)
        plot(fitted(modelo1),residuals(modelo1),main="Residuos vs. ajustados en escala Log\nModelo Log cuadrÂ´atico estacional")
        abline(h=0,col=2)
        abline(h=c(-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma),col=2)
        #PRONOSTICANDO PERÂ´IODOS t=152 a 155 EN ESCALA ORIGINAL
        predicciones1=exp(predict(modelo1,data.frame(t=tnuevo,trimestre=trimestre.nuevo),
                                  interval="prediction"))*exp(summary(modelo1)$sigma^2/2)
        predicciones1=ts(predicciones1,frequency=4,start=c(2016,4))
        #serie de tiempo de los pronÂ´osticos puntuales
        ytpron1=ts(predicciones1[,1],frequency=4,start=c(2016,4)) #los pronÂ´osticos comienzan desde 1993-Q4
        accuracy(ytpron1,ytf) #Calculando exactitud de los pronÂ´osticos
        #GRAFICANDO LA SERIE, SUS AJUSTES Y PRONÂ´OSTICOS
        plot(yt,main="Serie real, ajustes y pronÂ´osticos\nModelo Log cuadrÂ´atico estacional")
        lines(ythat1,col=2, lwd = 2)
        lines(ytpron1,col=4, lwd = 2)
        legend("topleft",legend=c("Original","Ajustada","Pronósticos"),col=c(1,2,4),lty=1)
      })

      #===========MODELO CÚBICO ESTACIONAL =======================

      #SUMMARY MODELO CUBICO ESTACIONAL
      output$summary_mod_cubico <- renderPrint({
        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        m=4 #NÂ´umero de perÂ´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste
        t=1:n #Â´indice de tiempo
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones
        trimestre=season(yt2)
        lnyt2=log(yt2)
        modelo1b=lm(lnyt2~t+I(t^2)+I(t^3)+trimestre)
        summary(modelo1b)
      })#anterior

      #GRAFICA RESIDUALES LIMITES 2 SIGMA
      output$residuales_2sigma <- renderPlot({
        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        m=4 #NÂ´umero de perÂ´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste
        t=1:n #Â´indice de tiempo
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones
        trimestre=season(yt2)
        lnyt2=log(yt2)
        modelo1b=lm(lnyt2~t+I(t^2)+I(t^3)+trimestre)
        plot.ts(residuals(modelo1b),main="Residuos vs. t en escala Log\ Modelo Log cúbico estacional")
        abline(h=0,col=2)
        abline(h=c(-2*summary(modelo1b)$sigma,2*summary(modelo1b)$sigma),col=2)
        plot(fitted(modelo1b),residuals(modelo1b),main="Residuos vs. ajustados en escala Log\ Modelo Log cúbico estacional")
        abline(h=0,col=2)
        abline(h=c(-2*summary(modelo1b)$sigma,2*summary(modelo1b)$sigma),col=2)
      })#anterior

      #TABLA PRONOSTICANDO PERIOROS t=152 A 155 EN ESCALA ORIGINAL
      output$pronostico_periodos_cubico <- renderTable({
        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        m=4 #NÂ´umero de perÂ´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste
        t=1:n #Â´indice de tiempo
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones
        #Definiendo factor para estacionalidad pero sÂ´olo para la regresiÂ´on lineal modelos 1 y 1b
        trimestre=season(yt2)
        trimestre=relevel(trimestre,ref="4Q")

        ######Para el pronÃ³stico
        tnuevo=c((n+1):(n+m)) #definiendo valor de t para las m observaciones a pronosticar
        trimestre.nuevo=season(yt)[(length(yt2)+1):length(yt)] #definiendo trimestre correspondiente a tiempos de pronÂ´ostico,
        #estos sÂ´olo para la regresiÂ´on lineal
        ytf=ts(yt[tnuevo],frequency=4,start=c(2016,4)) #Tomando de la serie completa los Â´ultimos m valores que son pronosticados
        lnyt2=log(yt2)
        modelo1b=lm(lnyt2~t+I(t^2)+I(t^3)+trimestre)
        predicciones1b=exp(predict(modelo1b,data.frame(t=tnuevo,trimestre=trimestre.nuevo),
                                   interval="prediction"))*exp(summary(modelo1b)$sigma^2/2)
        predicciones1b=ts(predicciones1b,frequency=4,start=c(2016,4))
        predicciones1b
      })#anterior

      #GRAFICANDO LA SERIE, SUS AJUSTES Y PRONÂ´OSTICOS
      output$pronostico_ajuste_cubico <- renderPlot({
        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        m=4 #NÂ´umero de perÂ´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste
        t=1:n #Â´indice de tiempo
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones
        #Definiendo factor para estacionalidad pero sÂ´olo para la regresiÂ´on lineal modelos 1 y 1b
        trimestre=season(yt2)
        trimestre=relevel(trimestre,ref="4Q")

        ######Para el pronÃ³stico
        tnuevo=c((n+1):(n+m)) #definiendo valor de t para las m observaciones a pronosticar
        trimestre.nuevo=season(yt)[(length(yt2)+1):length(yt)] #definiendo trimestre correspondiente a tiempos de pronÂ´ostico,
        #estos sÂ´olo para la regresiÂ´on lineal
        ytf=ts(yt[tnuevo],frequency=4,start=c(2016,4)) #Tomando de la serie completa los Â´ultimos m valores que son pronosticados
        lnyt2=log(yt2)
        modelo1b=lm(lnyt2~t+I(t^2)+I(t^3)+trimestre)
        ythat1b=ts(exp(fitted(modelo1b))*exp(summary(modelo1b)$sigma^2/2),frequency=4,start=c(1979,1))
        predicciones1b=exp(predict(modelo1b,data.frame(t=tnuevo,trimestre=trimestre.nuevo),
                                   interval="prediction"))*exp(summary(modelo1b)$sigma^2/2)
        predicciones1b=ts(predicciones1b,frequency=4,start=c(2016,4))
        #Convirtiendo en serie de tiempo a los pronÂ´osticos puntuales
        ytpron1b=ts(predicciones1b[,1],frequency=4,start=c(2016,4)) #los pronÂ´osticos comienzan desde 1993-Q4
        accuracy(ytpron1b,ytf) #Calculando exactitud de los pronÂ´osticos
        #GRAFICANDO LA SERIE, SUS AJUSTES Y PRONOSTICOS
        plot(yt,main="Serie real, ajustes y pronósticos\ Modelo Log cúbico estacional")
        lines(ythat1b,col=2,lwd = 2)
        lines(ytpron1b,col=4,lwd = 2)
        legend("topleft",legend=c("Original","Ajustada","Pronósticos"),col=c(1,2,4),lty=1)
      })#anterior

      #=========MODELO FUNCIONES TRIGONOMETRICAS =============================

      #ADICIONAR EL RESUMEN DE LOS DATOS MODELO FUNCIONES TRIGONOMETRICAS
      output$summary_mod_funciones <- renderPrint({

        Var_modelo1c <- reactive({data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        m=4 #NÂ´umero de perÂ´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste
        t=1:n #Â´indice de tiempo
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones
        lnyt2=log(yt2)
        sen1=sin(pi*t/2); cos1=cos(pi*t/2); cos2=cos(pi*t)
        modelo1c=lm(lnyt2~t+I(t^2)+I(t^3)+sen1+cos1+cos2)})

        summary(Var_modelo1c())
        })#anterior

      output$residuales_escala_funciones <- renderPlot({
        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        m=4 #NÂ´umero de perÂ´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste
        t=1:n #Â´indice de tiempo
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones
        lnyt2=log(yt2)
        sen1=sin(pi*t/2); cos1=cos(pi*t/2); cos2=cos(pi*t)
        modelo1c=lm(lnyt2~t+I(t^2)+I(t^3)+sen1+cos1+cos2)
        plot.ts(residuals(modelo1c),main="Residuos vs. t en escala Log\ Modelo Log cúbico estacional con trigonométricas")
        abline(h=0)
        abline(h=c(-2*summary(modelo1c)$sigma,2*summary(modelo1c)$sigma),col=2)
        plot(fitted(modelo1c),residuals(modelo1c),
             main="Residuos vs. ajustados en escala Log\ Modelo Log cúbico estacional con trigonométricas")
        abline(h=0)
        abline(h=c(-2*summary(modelo1c)$sigma,2*summary(modelo1c)$sigma),col=2)

      })

      output$pronostico_periodos_funciones <- renderTable({

        #Creando funci´on usuario crit.inf.resid() para calcular C*n(p)
        crit.inf.resid=function(residuales,n.par,AIC="TRUE"){
          if(AIC=="TRUE"){
            #Calcula AIC
            CI=log(mean(residuales^2))+2*n.par/length(residuales)
          }
          if(AIC=="FALSE"){
            #Calcula BIC
            CI=log(mean(residuales^2))+n.par*log(length(residuales))/length(residuales)
          }
          CI
        }
        #Funci´on para obtener las estimaciones de los factores estacionales deltai en la descomposici´on cl´asica.
        #el argumento descom es un objeto que supone guard´o el resultado de la funci´on decompose();
        #el argumento s es la longitud del periodo estacional y estacionini es el No. de la estaci´on en t=1.
        factoresdeltai=function(descom,s,estacionini){
          if(estacionini==1){
            deltasi=descom$figure
          }
          if(estacionini!=1){
            j=estacionini;deltasi=c(descom$figure[(s-j+2):s],descom$figure[1:(s-j+1)])
          }
          deltasi
        }
        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        m=4 #NÂ´umero de perÂ´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste
        t=1:n #Â´indice de tiempo
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones
        lnyt2=log(yt2)
        trimestre=seasonal(decompose(yt2))#======OK
        #trimestre=relevel(trimestre,ref="4Q")#======OK

        ######Para el pronóstico
        tnuevo=c((n+1):(n+m)) #definiendo valor de t para las m observaciones a pronosticar
        trimestre.nuevo=season(yt)[(length(yt2)+1):length(yt)] #definiendo trimestre correspondiente a tiempos de pron´ostico,
        trimestre.nuevo1 <- as.numeric(substr(trimestre.nuevo[1], start = 1,stop = 1))
        trimestre.nuevo1
        #estos s´olo para la regresi´on lineal
        ytf=ts(yt[tnuevo],frequency=4,start=c(2016,4)) #Tomando de la serie completa los ´ultimos m valores que son pronosticados

        sen1=sin(pi*t/2); cos1=cos(pi*t/2); cos2=cos(pi*t)
        modelo1c=lm(lnyt2~t+I(t^2)+I(t^3)+sen1+cos1+cos2)
        plot.ts(residuals(modelo1c),main="Residuos vs. t en escala Log\ Modelo Log cúbico estacional con trigonométricas")
        abline(h=0)
        abline(h=c(-2*summary(modelo1c)$sigma,2*summary(modelo1c)$sigma),col=2)
        plot(fitted(modelo1c),residuals(modelo1c),
             main="Residuos vs. ajustados en escala Log\ Modelo Log cúbico estacional con trigonométricas")
        abline(h=0)
        abline(h=c(-2*summary(modelo1c)$sigma,2*summary(modelo1c)$sigma),col=2)
        #Serie de tiempo de valores ajustados en escala original. Se usa factor de correcci´on lognormal
        ythat1c=ts(exp(fitted(modelo1c))*exp(summary(modelo1c)$sigma^2/2),frequency=4,start=c(1979,1))
        #Calculando AIC y BIC usando C*n(p)
        resmod1c.orig=yt2-ythat1c #Residuos en escala original
        aic1c=crit.inf.resid(resmod1c.orig,n.par=7); aic1c
        bic1c=crit.inf.resid(resmod1c.orig,n.par=7,AIC="FALSE"); bic1c

        #PRONOSTICANDO PER´IODOS t=152 a 155 EN ESCALA ORIGINAL, E IP DEL 95%
        sen1nuevo <- sin(pi*tnuevo/2); cos1nuevo <- cos(pi*tnuevo/2); cos2nuevo <- cos(pi*tnuevo)
        predicciones1c=exp(predict(modelo1c,data.frame(t=tnuevo,sen1=sen1nuevo,cos1=cos1nuevo,
                                                       cos2=cos2nuevo),interval="prediction"))*exp(summary(modelo1c)$sigma^2/2)
        predicciones1c=ts(predicciones1c,freq=4,start=c(2016,4))
        predicciones1c
      })

      output$grafica_pronosticos_funciones <- renderPlot({
        #Creando funci´on usuario crit.inf.resid() para calcular C*n(p)
        crit.inf.resid=function(residuales,n.par,AIC="TRUE"){
          if(AIC=="TRUE"){
            #Calcula AIC
            CI=log(mean(residuales^2))+2*n.par/length(residuales)
          }
          if(AIC=="FALSE"){
            #Calcula BIC
            CI=log(mean(residuales^2))+n.par*log(length(residuales))/length(residuales)
          }
          CI
        }
        #Funci´on para obtener las estimaciones de los factores estacionales deltai en la descomposici´on cl´asica.
        #el argumento descom es un objeto que supone guard´o el resultado de la funci´on decompose();
        #el argumento s es la longitud del periodo estacional y estacionini es el No. de la estaci´on en t=1.
        factoresdeltai=function(descom,s,estacionini){
          if(estacionini==1){
            deltasi=descom$figure
          }
          if(estacionini!=1){
            j=estacionini;deltasi=c(descom$figure[(s-j+2):s],descom$figure[1:(s-j+1)])
          }
          deltasi
        }
        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        m=4 #NÂ´umero de perÂ´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste
        t=1:n #Â´indice de tiempo
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones
        lnyt2=log(yt2)
        trimestre=seasonal(decompose(yt2))#======OK
        #trimestre=relevel(trimestre,ref="4Q")#======OK

        ######Para el pronóstico
        tnuevo=c((n+1):(n+m)) #definiendo valor de t para las m observaciones a pronosticar
        trimestre.nuevo=season(yt)[(length(yt2)+1):length(yt)] #definiendo trimestre correspondiente a tiempos de pron´ostico,
        trimestre.nuevo1 <- as.numeric(substr(trimestre.nuevo[1], start = 1,stop = 1))
        trimestre.nuevo1
        #estos s´olo para la regresi´on lineal
        ytf=ts(yt[tnuevo],frequency=4,start=c(2016,4)) #Tomando de la serie completa los ´ultimos m valores que son pronosticados

        sen1=sin(pi*t/2); cos1=cos(pi*t/2); cos2=cos(pi*t)
        modelo1c=lm(lnyt2~t+I(t^2)+I(t^3)+sen1+cos1+cos2)
        plot.ts(residuals(modelo1c),main="Residuos vs. t en escala Log\nModelo Log c´ubico estacional con trigonom´etricas")
        abline(h=0)
        abline(h=c(-2*summary(modelo1c)$sigma,2*summary(modelo1c)$sigma),col=2)
        plot(fitted(modelo1c),residuals(modelo1c),
             main="Residuos vs. ajustados en escala Log\nModelo Log c´ubico estacional con trigonom´etricas")
        abline(h=0)
        abline(h=c(-2*summary(modelo1c)$sigma,2*summary(modelo1c)$sigma),col=2)
        #Serie de tiempo de valores ajustados en escala original. Se usa factor de correcci´on lognormal
        ythat1c=ts(exp(fitted(modelo1c))*exp(summary(modelo1c)$sigma^2/2),frequency=4,start=c(1979,1))
        #Calculando AIC y BIC usando C*n(p)
        resmod1c.orig=yt2-ythat1c #Residuos en escala original
        aic1c=crit.inf.resid(resmod1c.orig,n.par=7); aic1c
        bic1c=crit.inf.resid(resmod1c.orig,n.par=7,AIC="FALSE"); bic1c

        #PRONOSTICANDO PER´IODOS t=152 a 155 EN ESCALA ORIGINAL, E IP DEL 95%
        sen1nuevo <- sin(pi*tnuevo/2); cos1nuevo <- cos(pi*tnuevo/2); cos2nuevo <- cos(pi*tnuevo)
        predicciones1c=exp(predict(modelo1c,data.frame(t=tnuevo,sen1=sen1nuevo,cos1=cos1nuevo,
                                                       cos2=cos2nuevo),interval="prediction"))*exp(summary(modelo1c)$sigma^2/2)
        predicciones1c=ts(predicciones1c,freq=4,start=c(2016,4))
        predicciones1c
        #Serie de los pron´osticos puntuales
        ytpron1c=predicciones1c[,1] #s´olo los pron´osticos puntuales
        accuracy(ytpron1c,ytf) #Calculando exactitud de los pron´osticos
        plot(yt,main="Serie real, ajustes y pron´osticos\nModelo Log c´ubico estacional con trigonom´etricas")
        lines(ythat1c,col=2,lwd = 2); lines(ytpron1c,col=4,lwd = 2)
        legend("topleft",legend=c("Original","Ajustada","Pron´osticos"),col=c(1,2,4),lty=1)

      })

      #==========MODELO HOLT WINTER =========================================

      output$summary_suaviz_HW <- renderPrint({
        #Creando funci´on usuario crit.inf.resid() para calcular C*n(p)
        crit.inf.resid=function(residuales,n.par,AIC="TRUE"){
          if(AIC=="TRUE"){
            #Calcula AIC
            CI=log(mean(residuales^2))+2*n.par/length(residuales)
          }
          if(AIC=="FALSE"){
            #Calcula BIC
            CI=log(mean(residuales^2))+n.par*log(length(residuales))/length(residuales)
          }
          CI
        }
        #Funci´on para obtener las estimaciones de los factores estacionales deltai en la descomposici´on cl´asica.
        #el argumento descom es un objeto que supone guard´o el resultado de la funci´on decompose();
        #el argumento s es la longitud del periodo estacional y estacionini es el No. de la estaci´on en t=1.
        factoresdeltai=function(descom,s,estacionini){
          if(estacionini==1){
            deltasi=descom$figure
          }
          if(estacionini!=1){
            j=estacionini;deltasi=c(descom$figure[(s-j+2):s],descom$figure[1:(s-j+1)])
          }
          deltasi
        }
        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        m=4 #NÂ´umero de perÂ´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste
        t=1:n #Â´indice de tiempo
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones
        lnyt2=log(yt2)

        suaviza=HoltWinters(yt2,seasonal="multiplicative")
        suaviza


      })

      output$residuos_suaviz_HW <- renderPlot({

        #Creando funci´on usuario crit.inf.resid() para calcular C*n(p)
        crit.inf.resid=function(residuales,n.par,AIC="TRUE"){
          if(AIC=="TRUE"){
            #Calcula AIC
            CI=log(mean(residuales^2))+2*n.par/length(residuales)
          }
          if(AIC=="FALSE"){
            #Calcula BIC
            CI=log(mean(residuales^2))+n.par*log(length(residuales))/length(residuales)
          }
          CI
        }
        #Funci´on para obtener las estimaciones de los factores estacionales deltai en la descomposici´on cl´asica.
        #el argumento descom es un objeto que supone guard´o el resultado de la funci´on decompose();
        #el argumento s es la longitud del periodo estacional y estacionini es el No. de la estaci´on en t=1.
        factoresdeltai=function(descom,s,estacionini){
          if(estacionini==1){
            deltasi=descom$figure
          }
          if(estacionini!=1){
            j=estacionini;deltasi=c(descom$figure[(s-j+2):s],descom$figure[1:(s-j+1)])
          }
          deltasi
        }
        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        m=4 #NÂ´umero de perÂ´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste
        t=1:n #Â´indice de tiempo
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones
        lnyt2=log(yt2)

        suaviza=HoltWinters(yt2,seasonal="multiplicative")
        suaviza

        ythat3=fitted(suaviza)[,1] #valores ajustados. Ya tienen formato de serie de tiempo
        et3=residuals(suaviza) #residuales. Ya tienen formato de serie de tiempo
        aic3=crit.inf.resid(residuals(suaviza),n.par=6) #n´umero de par´ametros es s+2=6
        aic3
        bic3=crit.inf.resid(residuals(suaviza),n.par=6,AIC="FALSE")
        bic3
        plot(et3,main="Residuos vs. Suavizamiento Holt-Winters Multiplicativo")
        abline(h=0,col=2,lwd = 2)
      })

      output$residuos_ajustados_HW <- renderPlot({

        #Creando funci´on usuario crit.inf.resid() para calcular C*n(p)
        crit.inf.resid=function(residuales,n.par,AIC="TRUE"){
          if(AIC=="TRUE"){
            #Calcula AIC
            CI=log(mean(residuales^2))+2*n.par/length(residuales)
          }
          if(AIC=="FALSE"){
            #Calcula BIC
            CI=log(mean(residuales^2))+n.par*log(length(residuales))/length(residuales)
          }
          CI
        }
        #Funci´on para obtener las estimaciones de los factores estacionales deltai en la descomposici´on cl´asica.
        #el argumento descom es un objeto que supone guard´o el resultado de la funci´on decompose();
        #el argumento s es la longitud del periodo estacional y estacionini es el No. de la estaci´on en t=1.
        factoresdeltai=function(descom,s,estacionini){
          if(estacionini==1){
            deltasi=descom$figure
          }
          if(estacionini!=1){
            j=estacionini;deltasi=c(descom$figure[(s-j+2):s],descom$figure[1:(s-j+1)])
          }
          deltasi
        }
        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        m=4 #NÂ´umero de perÂ´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste
        t=1:n #Â´indice de tiempo
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones
        lnyt2=log(yt2)

        suaviza=HoltWinters(yt2,seasonal="multiplicative")
        suaviza

        ythat3=fitted(suaviza)[,1] #valores ajustados. Ya tienen formato de serie de tiempo
        et3=residuals(suaviza) #residuales. Ya tienen formato de serie de tiempo
        aic3=crit.inf.resid(residuals(suaviza),n.par=6) #n´umero de par´ametros es s+2=6
        aic3
        bic3=crit.inf.resid(residuals(suaviza),n.par=6,AIC="FALSE")
        bic3
        plot(ythat3,et3,xy.labels=F,main="Residuos vs. ajustados\ Suavizamiento Holt-Winters Multiplicativo")
        abline(h=0,col=2,lwd = 2)
      })

      output$pronostico_periodos_HW <- renderTable({

        #Creando funci´on usuario crit.inf.resid() para calcular C*n(p)
        crit.inf.resid=function(residuales,n.par,AIC="TRUE"){
          if(AIC=="TRUE"){
            #Calcula AIC
            CI=log(mean(residuales^2))+2*n.par/length(residuales)
          }
          if(AIC=="FALSE"){
            #Calcula BIC
            CI=log(mean(residuales^2))+n.par*log(length(residuales))/length(residuales)
          }
          CI
        }
        #Funci´on para obtener las estimaciones de los factores estacionales deltai en la descomposici´on cl´asica.
        #el argumento descom es un objeto que supone guard´o el resultado de la funci´on decompose();
        #el argumento s es la longitud del periodo estacional y estacionini es el No. de la estaci´on en t=1.
        factoresdeltai=function(descom,s,estacionini){
          if(estacionini==1){
            deltasi=descom$figure
          }
          if(estacionini!=1){
            j=estacionini;deltasi=c(descom$figure[(s-j+2):s],descom$figure[1:(s-j+1)])
          }
          deltasi
        }
        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        m=4 #NÂ´umero de perÂ´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste
        t=1:n #Â´indice de tiempo
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones

        trimestre=seasonal(decompose(yt2))#======OK
        #trimestre=relevel(trimestre,ref="4Q")#======OK

        ######Para el pronóstico
        tnuevo=c((n+1):(n+m)) #definiendo valor de t para las m observaciones a pronosticar
        trimestre.nuevo=season(yt)[(length(yt2)+1):length(yt)] #definiendo trimestre correspondiente a tiempos de pron´ostico,
        trimestre.nuevo1 <- as.numeric(substr(trimestre.nuevo[1], start = 1,stop = 1))
        trimestre.nuevo1
        #estos s´olo para la regresi´on lineal
        ytf=ts(yt[tnuevo],frequency=4,start=c(2016,4)) #Tomando de la serie completa los ´ultimos m valores que son pronosticados

        lnyt2=log(yt2)

        suaviza=HoltWinters(yt2,seasonal="multiplicative")
        suaviza

        ythat3=fitted(suaviza)[,1] #valores ajustados. Ya tienen formato de serie de tiempo
        et3=residuals(suaviza) #residuales. Ya tienen formato de serie de tiempo
        aic3=crit.inf.resid(residuals(suaviza),n.par=6) #n´umero de par´ametros es s+2=6
        aic3
        bic3=crit.inf.resid(residuals(suaviza),n.par=6,AIC="FALSE")
        bic3
        #PREDICCIONES Y INTERVALOS DE PRON´OSTICO
        predicciones3=predict(suaviza,n.ahead=4,prediction.interval=TRUE)
        predicciones3
      })

      output$grafica_pronosticos_HW <- renderPlot({

        #Creando funci´on usuario crit.inf.resid() para calcular C*n(p)
        crit.inf.resid=function(residuales,n.par,AIC="TRUE"){
          if(AIC=="TRUE"){
            #Calcula AIC
            CI=log(mean(residuales^2))+2*n.par/length(residuales)
          }
          if(AIC=="FALSE"){
            #Calcula BIC
            CI=log(mean(residuales^2))+n.par*log(length(residuales))/length(residuales)
          }
          CI
        }
        #Funci´on para obtener las estimaciones de los factores estacionales deltai en la descomposici´on cl´asica.
        #el argumento descom es un objeto que supone guard´o el resultado de la funci´on decompose();
        #el argumento s es la longitud del periodo estacional y estacionini es el No. de la estaci´on en t=1.
        factoresdeltai=function(descom,s,estacionini){
          if(estacionini==1){
            deltasi=descom$figure
          }
          if(estacionini!=1){
            j=estacionini;deltasi=c(descom$figure[(s-j+2):s],descom$figure[1:(s-j+1)])
          }
          deltasi
        }
        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        m=4 #NÂ´umero de perÂ´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste
        t=1:n #Â´indice de tiempo
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones

        trimestre=seasonal(decompose(yt2))#======OK
        #trimestre=relevel(trimestre,ref="4Q")#======OK

        ######Para el pronóstico
        tnuevo=c((n+1):(n+m)) #definiendo valor de t para las m observaciones a pronosticar
        trimestre.nuevo=season(yt)[(length(yt2)+1):length(yt)] #definiendo trimestre correspondiente a tiempos de pron´ostico,
        trimestre.nuevo1 <- as.numeric(substr(trimestre.nuevo[1], start = 1,stop = 1))
        trimestre.nuevo1
        #estos s´olo para la regresi´on lineal
        ytf=ts(yt[tnuevo],frequency=4,start=c(2016,4)) #Tomando de la serie completa los ´ultimos m valores que son pronosticados

        lnyt2=log(yt2)

        suaviza=HoltWinters(yt2,seasonal="multiplicative")
        suaviza

        ythat3=fitted(suaviza)[,1] #valores ajustados. Ya tienen formato de serie de tiempo
        et3=residuals(suaviza) #residuales. Ya tienen formato de serie de tiempo
        aic3=crit.inf.resid(residuals(suaviza),n.par=6) #n´umero de par´ametros es s+2=6
        aic3
        bic3=crit.inf.resid(residuals(suaviza),n.par=6,AIC="FALSE")
        bic3
        #PREDICCIONES Y INTERVALOS DE PRON´OSTICO
        predicciones3=predict(suaviza,n.ahead=4,prediction.interval=TRUE)
        predicciones3
        ytpron3=predicciones3[,1] #Separando los pronosticos puntuales
        accuracy(ytpron3,ytf) #Calculando exactitud de los pron´osticos
        #GRAFICANDO LA SERIE, SUS AJUSTES Y PRON´OSTICOS
        plot(suaviza,main="Serie real, ajustes y pronósticos con Suavizamiento Holt-Winters",lwd = 2)
        lines(ytpron3,col=4,lwd = 2)
        legend("topleft",legend=c("Original","Ajustada","Pronósticos"),col=c(1,2,4),lty=1)

      })

      #========= MODELO LOESS ========================================

      output$summary_loess <- renderPrint({
        data_l<-reactive({
          inFile <- input$TextFile
          if (is.null(inFile))
            return(NULL)

          data_st<-ts(scan(inFile$datapath,skip=input$skipper),start=c(input$year,input$month),
                      frequency = input$frequency)
          return(data_st)})

        data_st<-data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        m=4 #N´umero de per´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste#======OK
        t=1:n #´indice de tiempo#======OK
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones

        des=decompose(yt2,type="multiplicative") #Descomposici´on multiplicativa
        St=des$seasonal #Extrayendo la serie de componente estacional
        yt3=yt2/St #Serie desestacionalizada en forma multiplicativa
        #AJUSTE LOESS ´OPTIMO GRADO 2 DE LA TENDENCIA
        ajusteLoess=loess.as(t,yt3,degree=2,criterion="aicc",family="gaussian",plot=F)
        summary(ajusteLoess)
      })

      output$serie_desestacionalizada_loess <- renderPlot({
        data_l<-reactive({
          inFile <- input$TextFile
          if (is.null(inFile))
            return(NULL)

          data_st<-ts(scan(inFile$datapath,skip=input$skipper),start=c(input$year,input$month),
                      frequency = input$frequency)
          return(data_st)})

        data_st<-data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        m=4 #N´umero de per´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste#======OK
        t=1:n #´indice de tiempo#======OK
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones

        des=decompose(yt2,type="multiplicative") #Descomposici´on multiplicativa
        St=des$seasonal #Extrayendo la serie de componente estacional
        yt3=yt2/St #Serie desestacionalizada en forma multiplicativa
        #AJUSTE LOESS ´OPTIMO GRADO 2 DE LA TENDENCIA
        ajusteLoess=loess.as(t,yt3,degree=2,criterion="aicc",family="gaussian",plot=F)
        summary(ajusteLoess)

        Tt=ts(fitted(ajusteLoess),frequency=4,start=c(1979,1))
        alfa.optim=ajusteLoess$pars$span #guardando el valor ´optimo del par´ametro alfa
        plot(yt3,main="Serie desestacionalizada y su ajuste LOESS cuadrático óptimo")
        lines(Tt,col=2,lwd = 2)
        legend("topleft",legend=c("Serie ajustada estacionalmente","Tendencia LOESS ajustada"),col=c(1,2),lty=1)
      })

      output$graficos_residuales_loess <- renderPlot({

        #Creando funci´on usuario crit.inf.resid() para calcular C*n(p)
        crit.inf.resid=function(residuales,n.par,AIC="TRUE"){
          if(AIC=="TRUE"){
            #Calcula AIC
            CI=log(mean(residuales^2))+2*n.par/length(residuales)
          }
          if(AIC=="FALSE"){
            #Calcula BIC
            CI=log(mean(residuales^2))+n.par*log(length(residuales))/length(residuales)
          }
          CI
        }

        data_st<-data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        m=4 #N´umero de per´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste#======OK
        t=1:n #´indice de tiempo#======OK
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones

        des=decompose(yt2,type="multiplicative") #Descomposici´on multiplicativa
        St=des$seasonal #Extrayendo la serie de componente estacional
        yt3=yt2/St #Serie desestacionalizada en forma multiplicativa
        #AJUSTE LOESS ´OPTIMO GRADO 2 DE LA TENDENCIA
        ajusteLoess=loess.as(t,yt3,degree=2,criterion="aicc",family="gaussian",plot=F)
        summary(ajusteLoess)

        Tt=ts(fitted(ajusteLoess),frequency=4,start=c(1979,1))
        alfa.optim=ajusteLoess$pars$span #guardando el valor ´optimo del par´ametro alfa
        plot(yt3,main="Serie desestacionalizada y su ajuste LOESS cuadrático óptimo")
        lines(Tt,col=2)
        legend("topleft",legend=c("Serie ajustada estacionalmente","Tendencia LOESS ajustada"),col=c(1,2),lty=1)

        Tt=ts(fitted(ajusteLoess),frequency=4,start=c(1979,1))
        #AJUSTE DE LA SERIE
        ythat4=Tt*St
        #RESIDUALES
        et4=yt2-ythat4
        #Calculando AIC y BIC
        aic4=crit.inf.resid(et4,n.par=28) #n´umero de par´ametros aproximados loess 24 + 4 factores estacionales
        aic4
        bic4=crit.inf.resid(et4,n.par=28,AIC="FALSE")
        bic4
        #GR´AFICOS DE RESIDUALES
        plot(et4,main="Residuos vs. t\nAjuste por descomposici´on & LOESS")
        abline(h=0,col=2,lwd = 2)
        plot(ythat4,et4,main="Residuos vs. ajustados\nAjuste por descomposici´on & LOESS")
        abline(h=0,col=2,lwd = 2)
      })

      output$pronostico_periodos_LOESS <- renderTable({

        #Creando funci´on usuario crit.inf.resid() para calcular C*n(p)
        crit.inf.resid=function(residuales,n.par,AIC="TRUE"){
          if(AIC=="TRUE"){
            #Calcula AIC
            CI=log(mean(residuales^2))+2*n.par/length(residuales)
          }
          if(AIC=="FALSE"){
            #Calcula BIC
            CI=log(mean(residuales^2))+n.par*log(length(residuales))/length(residuales)
          }
          CI
        }
        #Funci´on para obtener las estimaciones de los factores estacionales deltai en la descomposici´on cl´asica.
        #el argumento descom es un objeto que supone guard´o el resultado de la funci´on decompose();
        #el argumento s es la longitud del periodo estacional y estacionini es el No. de la estaci´on en t=1.
        factoresdeltai=function(descom,s,estacionini){
          if(estacionini==1){
            deltasi=descom$figure
          }
          if(estacionini!=1){
            j=estacionini;deltasi=c(descom$figure[(s-j+2):s],descom$figure[1:(s-j+1)])
          }
          deltasi
        }


        data_st<-data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        m=4 #N´umero de per´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste#======OK
        t=1:n #´indice de tiempo#======OK
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones
        trimestre=seasonal(decompose(yt2))#======OK
        #trimestre=relevel(trimestre,ref="4Q")#======OK

        ######Para el pronóstico
        tnuevo=c((n+1):(n+m)) #definiendo valor de t para las m observaciones a pronosticar
        trimestre.nuevo=season(yt)[(length(yt2)+1):length(yt)] #definiendo trimestre correspondiente a tiempos de pron´ostico,
        trimestre.nuevo1 <- as.numeric(substr(trimestre.nuevo[1], start = 1,stop = 1))
        trimestre.nuevo1
        #estos s´olo para la regresi´on lineal
        ytf=ts(yt[tnuevo],frequency=4,start=c(2016,4)) #Tomando de la serie completa los ´ultimos m valores que son pronosticados

        des=decompose(yt2,type="multiplicative") #Descomposici´on multiplicativa
        St=des$seasonal #Extrayendo la serie de componente estacional
        yt3=yt2/St #Serie desestacionalizada en forma multiplicativa
        #AJUSTE LOESS ´OPTIMO GRADO 2 DE LA TENDENCIA
        ajusteLoess=loess.as(t,yt3,degree=2,criterion="aicc",family="gaussian",plot=F)
        summary(ajusteLoess)
        Tt=ts(fitted(ajusteLoess),frequency=4,start=c(1979,1))
        alfa.optim=ajusteLoess$pars$span #guardando el valor ´optimo del par´ametro alfa
        plot(yt3,main="Serie desestacionalizada y su ajuste LOESS cuadr´atico ´optimo")
        lines(Tt,col=2)
        legend("topleft",legend=c("Serie ajustada estacionalmente","Tendencia LOESS ajustada"),col=c(1,2),lty=1)
        #AJUSTE DE LA SERIE
        ythat4=Tt*St
        #RESIDUALES
        et4=yt2-ythat4
        #Calculando AIC y BIC
        aic4=crit.inf.resid(et4,n.par=28) #n´umero de par´ametros aproximados loess 24 + 4 factores estacionales
        aic4
        bic4=crit.inf.resid(et4,n.par=28,AIC="FALSE")
        bic4
        #GR´AFICOS DE RESIDUALES
        plot(et4,main="Residuos vs. Ajuste por descomposición & LOESS")
        abline(h=0,col=2)
        plot(ythat4,et4,main="Residuos vs. ajustados por descomposición & LOESS")
        abline(h=0,col=2)
        #PRON´OSTICOS
        #Pron´osticos para la componente estacional
        deltas_i=factoresdeltai(descom=des,s=4,estacionini=1) #Obteniendo valor de los s factores estacionales estimados
        #el per´iodo es s=4 y la serie arranca en estaci´on 1
        i=c(4,1,2,3) #identificando la estaci´on correspondiente a los m=4 per´iodos de pron´ostico
        Stnuevo=deltas_i[i] #Asignando el valor de St a los per´iodos a pronosticar
        Stnuevo=ts(Stnuevo,frequency=4,start=c(2016,4)) #convirtiendo en serie de tiempo al pron´ostico de St
        Stnuevo
        #Pron´osticos de s´olo tendencia por loess cuadr´atico ´optimo
        Ttnuevo=predict(loess(yt3~t,span=alfa.optim,degree=2,control=loess.control(surface="direct")),data.frame(t=tnuevo),se=FALSE)
        Ttnuevo=ts(Ttnuevo,freq=4,start=c(2016,4))#convirtiendo en serie de tiempo al pron´ostico de Tt
        #Pron´ostico de la serie seg´un modelo multiplicativo
        ytpron4=Ttnuevo*Stnuevo
        #Calculando medidas de precisi´on de pron´osticos
        accuracy(ytpron4,ytf)
        ytpron4

      })

      output$grafica_pronostico_LOESS <- renderPlot({
        #Creando funci´on usuario crit.inf.resid() para calcular C*n(p)
        crit.inf.resid=function(residuales,n.par,AIC="TRUE"){
          if(AIC=="TRUE"){
            #Calcula AIC
            CI=log(mean(residuales^2))+2*n.par/length(residuales)
          }
          if(AIC=="FALSE"){
            #Calcula BIC
            CI=log(mean(residuales^2))+n.par*log(length(residuales))/length(residuales)
          }
          CI
        }

        #Funci´on para obtener las estimaciones de los factores estacionales deltai en la descomposici´on cl´asica.
        #el argumento descom es un objeto que supone guard´o el resultado de la funci´on decompose();
        #el argumento s es la longitud del periodo estacional y estacionini es el No. de la estaci´on en t=1.
        factoresdeltai=function(descom,s,estacionini){
          if(estacionini==1){
            deltasi=descom$figure
          }
          if(estacionini!=1){
            j=estacionini;deltasi=c(descom$figure[(s-j+2):s],descom$figure[1:(s-j+1)])
          }
          deltasi
        }



        data_st<-data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        m=4 #N´umero de per´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste#======OK
        t=1:n #´indice de tiempo#======OK
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones

        trimestre=seasonal(decompose(yt2))#======OK
        #trimestre=relevel(trimestre,ref="4Q")#======OK

        ######Para el pronóstico
        tnuevo=c((n+1):(n+m)) #definiendo valor de t para las m observaciones a pronosticar
        trimestre.nuevo=season(yt)[(length(yt2)+1):length(yt)] #definiendo trimestre correspondiente a tiempos de pron´ostico,
        trimestre.nuevo1 <- as.numeric(substr(trimestre.nuevo[1], start = 1,stop = 1))
        trimestre.nuevo1
        #estos s´olo para la regresi´on lineal
        ytf=ts(yt[tnuevo],frequency=4,start=c(2016,4)) #Tomando de la serie completa los ´ultimos m valores que son pronosticados

        des=decompose(yt2,type="multiplicative") #Descomposici´on multiplicativa
        St=des$seasonal #Extrayendo la serie de componente estacional
        yt3=yt2/St #Serie desestacionalizada en forma multiplicativa
        #AJUSTE LOESS ´OPTIMO GRADO 2 DE LA TENDENCIA
        ajusteLoess=loess.as(t,yt3,degree=2,criterion="aicc",family="gaussian",plot=F)
        summary(ajusteLoess)

        Tt=ts(fitted(ajusteLoess),frequency=4,start=c(1979,1))
        alfa.optim=ajusteLoess$pars$span #guardando el valor ´optimo del par´ametro alfa
        plot(yt3,main="Serie desestacionalizada y su ajuste LOESS cuadrático óptimo")
        lines(Tt,col=2)
        legend("topleft",legend=c("Serie ajustada estacionalmente","Tendencia LOESS ajustada"),col=c(1,2),lty=1)

        Tt=ts(fitted(ajusteLoess),frequency=4,start=c(1979,1))
        #AJUSTE DE LA SERIE
        ythat4=Tt*St
        #RESIDUALES
        et4=yt2-ythat4
        #Calculando AIC y BIC
        aic4=crit.inf.resid(et4,n.par=28) #n´umero de par´ametros aproximados loess 24 + 4 factores estacionales
        aic4
        bic4=crit.inf.resid(et4,n.par=28,AIC="FALSE")
        bic4
        #PRON´OSTICOS
        #Pron´osticos para la componente estacional
        deltas_i=factoresdeltai(descom=des,s=4,estacionini=1) #Obteniendo valor de los s factores estacionales estimados
        #el per´iodo es s=4 y la serie arranca en estaci´on 1
        i=c(4,1,2,3) #identificando la estaci´on correspondiente a los m=4 per´iodos de pron´ostico
        Stnuevo=deltas_i[i] #Asignando el valor de St a los per´iodos a pronosticar
        Stnuevo=ts(Stnuevo,frequency=4,start=c(2016,4)) #convirtiendo en serie de tiempo al pron´ostico de St
        Stnuevo
        #Pron´osticos de s´olo tendencia por loess cuadr´atico ´optimo
        Ttnuevo=predict(loess(yt3~t,span=alfa.optim,degree=2,control=loess.control(surface="direct")),data.frame(t=tnuevo),se=FALSE)
        Ttnuevo=ts(Ttnuevo,freq=4,start=c(2016,4))#convirtiendo en serie de tiempo al pron´ostico de Tt
        #Pron´ostico de la serie seg´un modelo multiplicativo
        ytpron4=Ttnuevo*Stnuevo
        #Calculando medidas de precisi´on de pron´osticos
        accuracy(ytpron4,ytf)
        ytpron4
        #GRAFICANDO LA SERIE, SUS AJUSTES Y PRON´OSTICOS
        plot(yt,main="Serie real, ajustes y pronósticos\ Ajuste por descomposición & LOESS")
        lines(ythat4,col=2,lwd = 2)
        lines(ytpron4,col=4,lwd = 2)
        legend("topleft",legend=c("Original","Ajustada","Pronósticos"),col=c(1,2,4),lty=1)

      })

      #=========GRAFICA COMPARATIVA ENTRE LOS DIFERENTES MODELOS==========

      output$comparativo_primario <- renderPlot({

        #Creando funci´on usuario crit.inf.resid() para calcular C*n(p)
        crit.inf.resid=function(residuales,n.par,AIC="TRUE"){
          if(AIC=="TRUE"){
            #Calcula AIC
            CI=log(mean(residuales^2))+2*n.par/length(residuales)
          }
          if(AIC=="FALSE"){
            #Calcula BIC
            CI=log(mean(residuales^2))+n.par*log(length(residuales))/length(residuales)
          }
          CI
        }
        #Funci´on para obtener las estimaciones de los factores estacionales deltai en la descomposici´on cl´asica.
        #el argumento descom es un objeto que supone guard´o el resultado de la funci´on decompose();
        #el argumento s es la longitud del periodo estacional y estacionini es el No. de la estaci´on en t=1.
        factoresdeltai=function(descom,s,estacionini){
          if(estacionini==1){
            deltasi=descom$figure
          }
          if(estacionini!=1){
            j=estacionini;deltasi=c(descom$figure[(s-j+2):s],descom$figure[1:(s-j+1)])
          }
          deltasi
        }

        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        m=4 #NÂ´umero de perÂ´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste
        t=1:n #Â´indice de tiempo
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones
        #Definiendo factor para estacionalidad pero sÂ´olo para la regresiÂ´on lineal modelos 1 y 1b
        trimestre=season(yt2)
        trimestre=relevel(trimestre,ref="4Q")

        ######Para el pronÃ³stico
        tnuevo=c((n+1):(n+m)) #definiendo valor de t para las m observaciones a pronosticar
        trimestre.nuevo=season(yt)[(length(yt2)+1):length(yt)] #definiendo trimestre correspondiente a tiempos de pronÂ´ostico,
        #estos sÂ´olo para la regresiÂ´on lineal
        ytf=ts(yt[tnuevo],frequency=4,start=c(2016,4)) #Tomando de la serie completa los Â´ultimos m valores que son pronosticados
        lnyt2=log(yt2)
        modelo1=lm(lnyt2~t+I(t^2)+trimestre)
        modelo1b=lm(lnyt2~t+I(t^2)+I(t^3)+trimestre)
        sen1=sin(pi*t/2); cos1=cos(pi*t/2); cos2=cos(pi*t)
        modelo1c=lm(lnyt2~t+I(t^2)+I(t^3)+sen1+cos1+cos2)
        sen1nuevo <- sin(pi*tnuevo/2); cos1nuevo <- cos(pi*tnuevo/2); cos2nuevo <- cos(pi*tnuevo)

        ythat1b=ts(exp(fitted(modelo1b))*exp(summary(modelo1b)$sigma^2/2),frequency=4,start=c(1979,1))

        predicciones1=exp(predict(modelo1,data.frame(t=tnuevo,trimestre=trimestre.nuevo),
                                  interval="prediction"))*exp(summary(modelo1)$sigma^2/2)
        predicciones1=ts(predicciones1,frequency=4,start=c(2016,4))
        ytpron1=ts(predicciones1[,1],frequency=4,start=c(2016,4)) #los pronÂ´osticos comienzan desde 1993-Q4

        predicciones1b=exp(predict(modelo1b,data.frame(t=tnuevo,trimestre=trimestre.nuevo),
                                   interval="prediction"))*exp(summary(modelo1b)$sigma^2/2)
        predicciones1b=ts(predicciones1b,frequency=4,start=c(2016,4))
        #Convirtiendo en serie de tiempo a los pronÂ´osticos puntuales
        ytpron1b=ts(predicciones1b[,1],frequency=4,start=c(2016,4)) #los pronÂ´osticos comienzan desde 1993-Q4

        #PRONOSTICANDO PER´IODOS t=152 a 155 EN ESCALA ORIGINAL, E IP DEL 95%
        predicciones1c=exp(predict(modelo1c,data.frame(t=tnuevo,sen1=sen1nuevo,cos1=cos1nuevo,
                                                       cos2=cos2nuevo),interval="prediction"))*exp(summary(modelo1c)$sigma^2/2)
        predicciones1c=ts(predicciones1c,freq=4,start=c(2016,4))
        predicciones1c
        #Serie de los pron´osticos puntuales
        suaviza=HoltWinters(yt2,seasonal="multiplicative")
        ytpron1c=predicciones1c[,1] #s´olo los pron´osticos puntuales
        accuracy(ytpron1c,ytf) #Calculando exactitud de los pron´osticos

        predicciones3=predict(suaviza,n.ahead=4,prediction.interval=TRUE)
        predicciones3
        ytpron3=predicciones3[,1] #Separando los pronosticos puntuales
        accuracy(ytpron3,ytf) #Calculando exactitud de los pron´osticos

        #PRON´OSTICOS
        des=decompose(yt2,type="multiplicative") #Descomposici´on multiplicativa
        St=des$seasonal #Extrayendo la serie de componente estacional
        yt3=yt2/St #Serie desestacionalizada en forma multiplicativa
        #AJUSTE LOESS ´OPTIMO GRADO 2 DE LA TENDENCIA
        ajusteLoess=loess.as(t,yt3,degree=2,criterion="aicc",family="gaussian",plot=F)
        summary(ajusteLoess)
        Tt=ts(fitted(ajusteLoess),frequency=4,start=c(1979,1))
        alfa.optim=ajusteLoess$pars$span #guardando el valor ´optimo del par´ametro alfa
        plot(yt3,main="Serie desestacionalizada y su ajuste LOESS cuadr´atico ´optimo")
        lines(Tt,col=2)
        legend("topleft",legend=c("Serie ajustada estacionalmente","Tendencia LOESS ajustada"),col=c(1,2),lty=1)
        #AJUSTE DE LA SERIE
        ythat4=Tt*St
        #RESIDUALES
        et4=yt2-ythat4
        #Pron´osticos para la componente estacional
        deltas_i=factoresdeltai(descom=des,s=4,estacionini=1) #Obteniendo valor de los s factores estacionales estimados
        #el per´iodo es s=4 y la serie arranca en estaci´on 1
        i=c(4,1,2,3) #identificando la estaci´on correspondiente a los m=4 per´iodos de pron´ostico
        Stnuevo=deltas_i[i] #Asignando el valor de St a los per´iodos a pronosticar
        Stnuevo=ts(Stnuevo,frequency=4,start=c(2016,4)) #convirtiendo en serie de tiempo al pron´ostico de St
        Stnuevo
        #Pron´osticos de s´olo tendencia por loess cuadr´atico ´optimo
        Ttnuevo=predict(loess(yt3~t,span=alfa.optim,degree=2,control=loess.control(surface="direct")),data.frame(t=tnuevo),se=FALSE)
        Ttnuevo=ts(Ttnuevo,freq=4,start=c(2016,4))#convirtiendo en serie de tiempo al pron´ostico de Tt
        #Pron´ostico de la serie seg´un modelo multiplicativo
        ytpron4=Ttnuevo*Stnuevo
        #Calculando medidas de precisi´on de pron´osticos
        accuracy(ytpron4,ytf)
        ytpron4


        plot(ytf,ylim=c(130000,200000),lty=1,col="black",type="b",pch=19,lwd = 2,
             main="Gráfico comparativo serie de tiempo analizada - Diferentes modelos",xaxt="n")
        axis(1,at=time(ytf),labels=c("Q4","Q1","Q2","Q3")) #REAL
        lines(ytpron1,lty=2,col="red",type="b",pch=2,lwd = 2) #CUADR
        lines(ytpron1b,lty=3,col="blue",type="b",pch=3,lwd = 2) #CUBI
        lines(ytpron1c,lty=4,col="orange",type="b",pch=4,lwd = 2) #TRIGON
        lines(ytpron3,lty=6,col="darkgreen",type="b",pch=5,lwd = 2) #HW
        lines(ytpron4,lty=7,col="darkred",type="b",pch=6,lwd = 2) #LOESS
        legend("topleft",legend=c("Real","Log-cuadrático estacional","Log-cúbico estacional",
                                  "log-cuadr´atico con trigonométricas","Holt-Winters","Descomposición & LOESS"),
               lty=c(1:7),pch=c(19,2:7),col=c("black","red","blue","orange","brown","darkgreen","darkred"))
      })

      #=========PRONOSTICOS========================================

      #=========PRONOSTICO LOG CUADRATICO ESTACIONAL ==============

      output$ajuste_log_cuadratico <- renderPlot({

        ##############Pron?stico modelo 1 cuadr?tico estacional
        #Ajuste con modelo seleccionado
        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones

        lnyt=log(yt)
        tc=1:length(yt)
        stc=season(yt)
        stc=relevel(stc,ref="4Q")

        modelo1ante=lm(lnyt~tc+I(tc^2)+stc)
        summary(modelo1ante)
        ajustec=ts(exp(fitted(modelo1ante))*exp(summary(modelo1ante)$sigma^2/2),frequency=4,start=c(1979,1))
        plot(ajustec,lwd = 2)

      })

      output$real_vs_ajustada <- renderPlot({
        ##############Pron?stico modelo 1 cuadr?tico estacional
        #Ajuste con modelo seleccionado
        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        lnyt=log(yt)
        tc=1:length(yt)
        stc=season(yt)
        stc=relevel(stc,ref="4Q")

        modelo1ante=lm(lnyt~tc+I(tc^2)+stc)
        summary(modelo1ante)
        ajustec=ts(exp(fitted(modelo1ante))*exp(summary(modelo1ante)$sigma^2/2),frequency=4,start=c(1979,1))
        plot(ajustec)


        #win.graph(width=5,height=4,pointsize=8)
        plot(yt,main="Serie real vs Serie ajustada\n Modelo cuadrático estacional", ylab="yt")
        lines(ajustec,col=2,lwd = 2)
        legend("topleft",legend=c("Original","Ajustado"),col=c(1,2),lty=1)


      })

      output$pronostico_del_modelo <- renderTable({
        ##############Pron?stico modelo 1 cuadr?tico estacional
        #Ajuste con modelo seleccionado
        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        lnyt=log(yt)
        tc=1:length(yt)
        stc=season(yt)
        stc=relevel(stc,ref="4Q")

        modelo1ante=lm(lnyt~tc+I(tc^2)+stc)
        summary(modelo1ante)
        ajustec=ts(exp(fitted(modelo1ante))*exp(summary(modelo1ante)$sigma^2/2),frequency=4,start=c(1979,1))
        l=6
        tn=c((length(yt)+1):(length(yt)+l))

        a=ts(rep(l,l),freq=4,start=c(2017,4))

        stn=relevel(season(a),ref="4Q")
        stn


        pronf=exp(predict(modelo1ante,data.frame(tc=tn,stc=stn),interval="prediction",
                          level=0.95))*exp(summary(modelo1ante)$sigma^2/2)
        pronf=ts(pronf,freq=4,start=c(2017,3))
        plot(pronf)
        ytpronf=ts(pronf[,1],frequency=4,start=c(2017,3))
        ytpronf

      })

      output$grafica_pronostico_del_modelo <- renderPlot({

        ##############Pron?stico modelo 1 cuadr?tico estacional
        #Ajuste con modelo seleccionado
        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        lnyt=log(yt)
        tc=1:length(yt)
        stc=season(yt)
        stc=relevel(stc,ref="4Q")

        modelo1ante=lm(lnyt~tc+I(tc^2)+stc)
        summary(modelo1ante)
        ajustec=ts(exp(fitted(modelo1ante))*exp(summary(modelo1ante)$sigma^2/2),frequency=4,start=c(1979,1))
        l=6
        tn=c((length(yt)+1):(length(yt)+l))

        a=ts(rep(l,l),freq=4,start=c(2017,4))

        stn=relevel(season(a),ref="4Q")
        stn


        pronf=exp(predict(modelo1ante,data.frame(tc=tn,stc=stn),interval="prediction",
                          level=0.95))*exp(summary(modelo1ante)$sigma^2/2)
        pronf=ts(pronf,freq=4,start=c(2017,3))
        plot(pronf)
        ytpronf=ts(pronf[,1],frequency=4,start=c(2017,3))
        ytpronf

        plot(yt,main="Serie real, ajustes y pronósticos\ Modelo cuadrático estacional",xlab="Tiempo",xlim=c(1979,2018), ylab="yt")
        lines(ajustec,col="2",lwd = 2)
        lines(ytpronf,col="3",lwd = 2)
        legend("topleft",legend=c("Original","Ajuste y pronóstico"),col=c(1,"red"),lty=1)

      })

      #=======PRONOSTICO LOG CUBICO ==============================

      output$ajuste_log_cubico <- renderPlot({

        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        #Ajuste con modelo seleccionado
        lnyt=log(yt)
        tc=1:length(yt)
        stc=season(yt)
        stc=relevel(stc,ref="4Q")

        modelo2ante=lm(lnyt~tc+I(tc^2)+I(tc^3)+stc)
        summary(modelo2ante)
        ajustece=ts(exp(fitted(modelo2ante))*exp(summary(modelo2ante)$sigma^2/2),frequency=4,start=c(1979,1))
        plot(ajustece,lwd = 2)

      })

      output$real_vs_ajustada_cubico <- renderPlot({

        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        #Ajuste con modelo seleccionado
        lnyt=log(yt)
        tc=1:length(yt)
        stc=season(yt)
        stc=relevel(stc,ref="4Q")

        modelo2ante=lm(lnyt~tc+I(tc^2)+I(tc^3)+stc)
        summary(modelo2ante)
        ajustece=ts(exp(fitted(modelo2ante))*exp(summary(modelo2ante)$sigma^2/2),frequency=4,start=c(1979,1))
        plot(ajustece)

        plot(yt,main="Serie real vs Serie ajustada \ Modelo cúbico estacional", ylab="yt")
        lines(ajustece,col=2,lwd = 2)
        legend("topleft",legend=c("Original","Ajustado"),col=c(1,2),lty=1)

      })

      output$pronostico_del_modelo_cubico <- renderTable({

        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        #Ajuste con modelo seleccionado
        lnyt=log(yt)
        tc=1:length(yt)
        stc=season(yt)
        stc=relevel(stc,ref="4Q")

        modelo2ante=lm(lnyt~tc+I(tc^2)+I(tc^3)+stc)
        summary(modelo2ante)
        ajustece=ts(exp(fitted(modelo2ante))*exp(summary(modelo2ante)$sigma^2/2),frequency=4,start=c(1979,1))
        plot(ajustece)

        plot(yt,main="Serie real vs Serie ajustada\n Modelo cúbico estacional", ylab="yt")
        lines(ajustece,col=2)
        legend("topleft",legend=c("Original","Ajustado"),col=c(1,2),lty=1)

        l=6
        tn=c((length(yt)+1):(length(yt)+l))

        a=ts(rep(l,l),freq=4,start=c(2017,4))

        stn=relevel(season(a),ref="4Q")
        stn


        pronfe=exp(predict(modelo2ante,data.frame(tc=tn,stc=stn),interval="prediction",
                           level=0.95))*exp(summary(modelo2ante)$sigma^2/2)
        pronf=ts(pronfe,freq=4,start=c(2017,3))

        ytpronfe=ts(pronfe[,1],frequency=4,start=c(2017,3))

        ytpronfe
      })

      output$grafica_pronostico_del_modelo_cubico <- renderPlot({

        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        #Ajuste con modelo seleccionado
        lnyt=log(yt)
        tc=1:length(yt)
        stc=season(yt)
        stc=relevel(stc,ref="4Q")

        modelo2ante=lm(lnyt~tc+I(tc^2)+I(tc^3)+stc)
        summary(modelo2ante)
        ajustece=ts(exp(fitted(modelo2ante))*exp(summary(modelo2ante)$sigma^2/2),frequency=4,start=c(1979,1))
        plot(ajustece)

        plot(yt,main="Serie real vs Serie ajustada\n Modelo cúbico estacional", ylab="yt")
        lines(ajustece,col=2)
        legend("topleft",legend=c("Original","Ajustado"),col=c(1,2),lty=1)

        l=6
        tn=c((length(yt)+1):(length(yt)+l))

        a=ts(rep(l,l),freq=4,start=c(2017,4))

        stn=relevel(season(a),ref="4Q")
        stn


        pronfe=exp(predict(modelo2ante,data.frame(tc=tn,stc=stn),interval="prediction",
                           level=0.95))*exp(summary(modelo2ante)$sigma^2/2)
        pronf=ts(pronfe,freq=4,start=c(2017,3))

        ytpronfe=ts(pronfe[,1],frequency=4,start=c(2017,3))

        ytpronfe

        plot(yt,main="Serie real, ajustes y pronósticos\ Modelo cúbico estacional",xlab="Tiempo",xlim=c(1979,2018), ylab="yt")
        lines(ajustece,col="2",lwd = 2)
        lines(ytpronfe,col="3",lwd = 2)
        legend("topleft",legend=c("Original","Ajuste y pronóstico"),col=c(1,"red"),lty=1)
      })

}#parentesis para cerrar el server


# Create Shiny app ----
shinyApp(ui, server)
