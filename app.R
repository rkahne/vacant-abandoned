#.libPaths('C:/R-lib')
library(shiny)
library(xtable)
library(scales)
library(choroplethrZip)
options(stringsAsFactors = F)


data<-read.csv('./TaxDue_TextFormat.csv')
LouisvilleZips <- c(40212,40211,40216,40258,40272,40203,40210,40208,40215,40214,40118,40209,40202,40206,40204,40217,
                               40213,40219,40229,40205,40218,40228,40291,40220,40299,40023,40245,40059,40241,40223,40243,
                               40242,40025,40222,40207,40177,40231,40041,40280)
data<-subset(data, TAXYEAR>2006 & STATE=='KY')
data$ZIP<-substr(data$ZIP,1,5)
data<-subset(data,ZIP %in% LouisvilleZips)
leinFlag<-subset(data, LIENFLAG != '')
unpaid<-subset(data, FULLPAID != 'Y')


ui <- fluidPage(theme="readable.css",
  
  #Application Title
  headerPanel("Vacant And Abandoned Properties"),
  #Sidebar
  sidebarPanel(
    checkboxGroupInput('zip', 'Which Zip Codes?', sort(LouisvilleZips),selected = sort(LouisvilleZips), inline=T),
    checkboxGroupInput('year', 'For Which Years?', sort(unique(data$TAXYEAR)), selected = max(sort(unique(data$TAXYEAR))))
  ),
  mainPanel(
    div(class="col-sm-6",
        div(style="text-align: center; font-size: 250%; font-weight: bold;",
          textOutput('totalDue')
        ),
        tableOutput('dueToWho')
    ),
    div(class="col-sm-6",
      plotOutput('zipChoro')
    )
  )
)
server <- function(input, output){
  output$totalDue<-renderText({
    outData<-subset(data, ZIP %in% input$zip & TAXYEAR %in% input$year)
    paste('Total Due in Area Selected: ',dollar_format()(sum(outData$BILLTOT)),sep='')
    
  })
  
  output$dueToWho<-renderTable({
    outData<-subset(data, ZIP %in% input$zip & TAXYEAR %in% input$year)
    newData<-data.frame(unique(outData$PDBYSOLDTO))
    colnames(newData)<-'PaidBySoldTo'
    newData$Total<-sapply(newData$PaidBySoldTo,function(i){
      sum(outData$BILLTOT[which(outData$PDBYSOLDTO == i)])
    })
    newData$NumberOfProperties<-sapply(newData$PaidBySoldTo,function(i){
      length(which(outData$PDBYSOLDTO == i))
    })
    newData$PaidBySoldTo[which(newData$PaidBySoldTo=='')]<-'Metro Louisville'
    return(newData[order(-newData$Total),])
  })
  
  output$zipChoro<-renderPlot({
    newData<-subset(data, ZIP %in% input$zip & TAXYEAR %in% input$year, select=c('ZIP', 'BILLTOT'))
    outData<-data.frame(input$zip)
    colnames(outData)<-'region'
    outData$value<-sapply(outData$region,function(i){
        sum(newData$BILLTOT[which(newData$ZIP == i)])
    })
    zip_choropleth(outData,
                   county_zoom = '21111',
                   title = 'Owed Per Zip',
                   legend = '$ per Zip',
                   num_colors = 1)
  })
}

shinyApp(ui = ui, server = server)