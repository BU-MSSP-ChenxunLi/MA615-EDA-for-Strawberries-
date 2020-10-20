library(shiny)
library(tidyverse)
library(readr)
library(magrittr)
library(kableExtra)
library(plyr)
library(stringr)
library(srvyr)

original_berries <- read_csv("berries(3).csv", col_names = TRUE)
## look at number of unique values in each column
original_berries %>% summarize_all(n_distinct) -> aa

## make a list of the columns with only one unique value
bb <- which(aa[1,]==1)

## list the 1-unique value column names 
cn <- colnames(original_berries)[bb]

## remove the 1-unique columns from the data set
original_berries  <-  original_berries[,-all_of(bb)]
aa <-  aa[,-all_of(bb)]

## State name and the State ANSI code are (sort of) redundant
## Just keep the name
original_berries <-  original_berries[,-4]
aa <- aa[,-4] 
#choose STRAWBERRIES
original_STRAWBERRIES <- filter(original_berries, Commodity=="STRAWBERRIES")

#discard useless values
original_STRAWBERRIES <- filter(original_STRAWBERRIES, Value != "(D)")
original_STRAWBERRIES <- filter(original_STRAWBERRIES, Value != "(NA)")

#read the 'Data Item'
dt_item <- original_STRAWBERRIES$`Data Item`

#replace the '-' with ',' to prepare for spliting
dt_item_with_comma <- gsub(" - ",",",dt_item)

#extract 'MEASURED IN'
original_STRAWBERRIES$unit_desc <- str_extract_all(dt_item_with_comma,"MEASURED.*[^./AVG]|ACRES.*")

original_STRAWBERRIES <- separate(data=original_STRAWBERRIES, col =6,into=c("Domain", "Domain.Detail"), sep=",")

for(i in 1:length(original_STRAWBERRIES$Domain)){
  if(is.na(original_STRAWBERRIES$Domain.Detail[i]) == T){
    original_STRAWBERRIES$Domain.Detail[i] = original_STRAWBERRIES$Domain[i]
  }
}

original_STRAWBERRIES <- original_STRAWBERRIES[,-8]
original_STRAWBERRIES <- original_STRAWBERRIES[,-5]
original_STRAWBERRIES <- original_STRAWBERRIES[,-4]

#delete the comma symbols in Values
for(i in 1:length(original_STRAWBERRIES$Value)){
  original_STRAWBERRIES$Value[i] <- gsub(pattern = ",", replacement = "",
                                         original_STRAWBERRIES$Value[i])
}

#change the type
original_STRAWBERRIES$Year <- as.integer(original_STRAWBERRIES$Year)
original_STRAWBERRIES$Value <- as.numeric(original_STRAWBERRIES$Value)

#get the new data set
STRAWBERRIES <- original_STRAWBERRIES

dt <- unique(STRAWBERRIES$unit_desc)
dt <- do.call(rbind,dt)

ui <- fluidPage(
  mainPanel(
    headerPanel('EDA for Strawberries'),
    tabsetPanel(
      tabPanel("Filterred Data Set",
               br(),
               sidebarPanel(
                 selectInput(
                   inputId = 'period',
                   label = 'Period:',
                   choices = sort(unique(STRAWBERRIES$Period)),
                   selected = sort(unique(STRAWBERRIES$Period))[2]
                 ),
                 selectInput(
                   inputId = 'domain',
                   label = 'Domain:',
                   choices = sort(unique(STRAWBERRIES$Domain)),
                   selected = sort(unique(STRAWBERRIES$Domain))[3]
                 ),
                 selectInput(
                   inputId = 'unit',
                   label = 'MEASURED IN:',
                   choices = sort(dt),
                   selected = sort(dt)[1]
                 )
                 
                 ),
                 mainPanel(
                   h3('Data Frame After Filterring'),
                   dataTableOutput('t')
                 )
               ),
      tabPanel("Plcanted and Harvested",
               br(),
               sidebarPanel(
                 selectInput(
                   inputId = "state",
                   label = "State",
                   choices = sort(unique(STRAWBERRIES$State))
                 ),
                 uiOutput('ui_year')
               ),
               mainPanel(
                 h3('Planted and Harvested in different states'),
                 plotOutput('p')
               )
               )
    )
  )
)


server <- function(input,output){
  s_year = reactive({
    sort(unique(STRAWBERRIES$Year[STRAWBERRIES$State%in%input$state]))
  })
  output$ui_year=renderUI({
    checkboxGroupInput('year','Year',s_year(),s_year())
  })
  
  
  p = reactive({
    filter(STRAWBERRIES, unit_desc%in%input$unit, Domain%in%input$domain, Period%in%input$period)%>%
      select(Year,Period,State,Value,Domain,unit_desc)
  })
  output$t=renderDataTable(p())
  
  plot_unit=reactive({
    
    x1 <- filter(STRAWBERRIES, State%in%input$state, unit_desc==dt[2], Year%in%input$year)
    p1 <- ggplot(x1)+geom_bar(aes(x=Year, weight = Value), fill="#75AADB", color= "black")+
      labs(x='Year', y =paste(dt[2]))+ggtitle(paste("Sum of",dt[2],"in",input$state))
    
    x2 <- filter(STRAWBERRIES, State%in%input$state, unit_desc==dt[3], Year%in%input$year)
    p2 <- ggplot(x2)+geom_bar(aes(x=Year, weight = Value), fill="#FF6666", color= "black")+
      labs(x='Year', y =paste(dt[3]))+ggtitle(paste('Sum of',dt[3],"in",input$state))
    
    ggpubr::ggarrange(p2,p1,ncol = 1)
  })
  output$p=renderPlot(plot_unit())
}


shinyApp(ui = ui ,server = server)
