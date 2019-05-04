library(shiny)
library(plotly)
library(readr)
library(ggplot2)
library(synchrony)

normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

x <- read.csv("dataSaul2018.csv")

x2<-lapply(x[,3:ncol(x)],normalize)
d<-lapply(x2,ts,frequency=12)
d<-lapply(d,stl,s.window="periodic")
d<-data.frame(d$precip$time.series[,2],d$rad$time.series[,2],d$temp$time.series[,2],d$heat$time.series[,2],d$s_mois0_10$time.series[,2],d$s_mois10_200$time.series[,2],d$hum$time.series[,2],d$NOI$time.series[,2],d$A.fendleri$time.series[,2],d$P.tovarense$time.series[,2],d$H.moritziana$time.series[,2],d$R.grandis$time.series[,2],d$C.tremadena$time.series[,2],
              d$B.reticulata$time.series[,2],d$V..ferrugia$time.series[,2],d$P.fendleri$time.series[,2],d$G.olfenciana$time.series[,2],d$E.amazonicum$time.series[,2],d$M.dodecandra$time.series[,2],d$G.latifolia$time.series[,2],d$T.rubriverbium$time.series[,2],d$E.americanus$time.series[,2],d$Viburnum.timoides.var.venezuelense$time.series[,2],d$Aspidosperma.fendleri$time.series[,2],d$Protium..tovarense$time.series[,2],
              d$Hyronima..moritziana$time.series[,2],d$Richeria..grandis$time.series[,2],d$Caracasia.tremadena$time.series[,2],d$Byrsonima.reticulata$time.series[,2],
              d$Vismia.ferruginea$time.series[,2],d$Palicurea.fendleri$time.series[,2],d$Guapira.olfersiana$time.series[,2],d$Erytroxylum.amazonicum$time.series[,2],
              d$Miconea.dodecandra$time.series[,2],d$Gaffenridia.latifolia$time.series[,2],d$Tretrorchidium.rubriverbium$time.series[,2],d$Eliocarpus.americanus$time.series[,2],
              d$Viburnum.timoides.var.venezuelense.1$time.series[,2])
colnames(d)<-colnames(x[,3:ncol(x)])



ui <- fluidPage(
  
  # Application title
  tags$h3("Fenología del bosque nublado y variables climáticas en 34 años de estudio",align="center"),
  h5("App Creada por Rodrigo Díaz Lupanow, Investigador encargado: Saúl Flores, Profesor: Otto Rendón "),
  h5("Centro de Física, Materia de Postgrado: Topología de la data"),
  h5("Instituto Venezolano de Investigaciones Científicas (IVIC), Dic. 2017 "),
  br(),

  sidebarLayout(
    sidebarPanel(
      checkboxInput("data","Usar el componente interanual (uncheck para data cruda)",value = TRUE),
      sliderInput("range", "Meses desde 1983 a 2016:",min = 1, max = nrow(d),value = c(1,nrow(d))),
      selectInput("var","Variable del clima (Negras)",colnames(d[,1:8])),
      selectInput("sp","Flores (Naranjas)", colnames(d[,9:23]),selected = "P.fendleri"),
      selectInput("var2","Frutos (Moradas)",colnames(d[,24:ncol(d)]),selected = "Palicurea.fendleri"),
      tags$b("Picos comunes entre variable climatica y"),
      tags$b("flores"),
      textOutput("text"),
      tags$b("frutos"),
      textOutput("text2"),
      br(),
      tags$hr(),
      tags$b("Sincronía de la comunidad"),
      selectizeInput("com","Variables a incluir",colnames(d[,1:ncol(d)]),multiple=T,selected=colnames(d[,9:23]) ),
      
      tags$hr(),
      tags$b("Histograma"),
      selectInput("sel1","Serie 1",colnames(d[,1:ncol(d)]),selected = "P.fendleri"),
      selectInput("sel2","Serie 2",colnames(d[,1:ncol(d)]),selected = "precip")
      

    ),
    

    mainPanel(
     
      tags$b("Plot 1. Series temporales de la tendencia interanual fenológica en diferentes plantas del bosque y variable climática. Triángulos representan picos máximos, círculos los picos mínimos."),
      plotOutput("distPlot"),
      tags$hr(),
      
      tags$b("Tabla 1. Sincronía:"),
      tableOutput("tab"),
      
     
    
      tags$hr(),
      tags$b("Plot 2. ",align="center"),
      plotOutput("distplot2"),
     
       tags$footer( code("App por: rodlupanow"),align="right")
     
    )
  )
)


server <- function(input, output) {
  

  
  output$distPlot <- renderPlot({
    a=input$range[1];b=input$range[2]
    if (input$data==T){d=d}
    if (input$data==F){d=x}
    t1<-d[a:b,input$sp]
    t2<-d[a:b,input$var]
    t3<-d[a:b,input$var2]
    
    mx1<-find.minmax(t1)
    mx2<-find.minmax(t2)
    mx3<-find.minmax(t3)
   
    
    x<-a:b
    plot (x,t3, t="l",col="purple",ylim=c(0,max(c(t1,t2,t3))),xlab="Meses",main = input$sp,
          ylab="Tendencia interanual escalada", panel.first = grid((b-a)/10,4))
    lines(x,t1,col="orange")
    lines(x,t2)
    points(x[mx2$maxs$index],t2[mx2$maxs$index],col="black",pch=2)
    points(x[mx1$maxs$index],t1[mx1$maxs$index],col="orange",pch=2)
    points(x[mx3$maxs$index],t3[mx3$maxs$index],col="purple",pch=2)
    points(x[mx2$mins$index],t2[mx2$mins$index],col="black")
    points(x[mx1$mins$index],t1[mx1$mins$index],col="orange")
    points(x[mx3$mins$index],t3[mx3$mins$index],col="purple")

      
  })

 
 output$text<-renderText({
   a=input$range[1];b=input$range[2]
   
   if (input$data==T){d=d}
   if (input$data==F){d=x}
   t1<-d[a:b,input$sp]
   t2<-d[a:b,input$var]

 
  pflmx<- peaks(t2,t1)
  pflmx$obs
   
 })
 
 output$text2<-renderText({
   a=input$range[1];b=input$range[2]
   if (input$data==T){d=d}
   if (input$data==F){d=x}
   t2<-d[a:b,input$var]
   t3<-d[a:b,input$var2]
   
   pfrmx<- peaks(t2,t3)
   pfrmx$obs
   
 })
 
 output$distplot2<-renderPlot({
   a=input$range[1];b=input$range[2]
   if (input$data==T){d=d}
   if (input$data==F){d=x}
   sync=phase.sync (d[a:b,input$sel1], d[a:b,input$sel2],  nrands = 0, mod = 1, method = c("markov", "fft"), 
                    nbreaks = 10, mins = FALSE, quiet = FALSE)
   
      hist(sync$deltaphase$mod_phase_diff_2pi)
 })
 
 output$tab<-renderTable({
   a=input$range[1];b=input$range[2]
   if (input$data==T){d=d}
   if (input$data==F){d=x}
   comm.corr<- d[a:b,c(input$com)]
   
   s<-community.sync(comm.corr, nrands=10)
   sa<-data.frame(s$obs,s$meancorr,s$pval)
   colnames(sa)<-c("Sincronía","Media de la correlación","valor p")
   sa
 })
 
}

# Run the application 
shinyApp(ui = ui, server = server)
