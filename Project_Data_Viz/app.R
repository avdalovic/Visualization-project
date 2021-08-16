#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(readr)
library(tidyverse)
library(leaflet.extras)
library(randomcoloR)



# Reading the data file
gtd <- read_csv("globalterrorism.csv")
gtd<-gtd%>%select(-nperps)
gtd_top_targets <- gtd %>% 
    filter(targtype1_txt %in% c('Private Citizens & Property', 'Military', 'Police', 
                                'Government (General)','Government (Diplomatic)', 'Business', 'Religious Figures/Institutions', 'Airports & Aircraft','Journalists & Media')
    )

gtd_top_targets<-gtd_top_targets%>%mutate(target_type=ifelse(grepl('Citizens',targtype1_txt),'Citizens',
                                                             ifelse(grepl('Government',targtype1_txt),'Goverment',
                                                                    ifelse(grepl('Religious',targtype1_txt),'Religion related',
                                                                           ifelse(grepl('Airports',targtype1_txt),'Airports',
                                                                                  ifelse(grepl('Journalists',targtype1_txt),'Journalists',targtype1_txt)))))) %>% 
    mutate(decades=cut(iyear,c(1970,1980,1990,2000,2010,2018),labels = c("70's","80's","90's","2000's","2010's"), include.lowest=TRUE,right =FALSE ,dig.lab=4))

gtd_top_targets$decades<-as.character(gtd_top_targets$decades)

gtd_top_targets<-gtd_top_targets%>%filter(!is.na(nkill))

gtd_top_targets<-gtd_top_targets%>%mutate(attack_type=ifelse(grepl('Facility',attacktype1_txt),'Facility attack',
                                                             ifelse(grepl('Hostage',attacktype1_txt),'Hostage taking',
                                                                    ifelse(grepl('Armed',attacktype1_txt),'Armed assault',
                                                                           ifelse(grepl('Unarmed ',attacktype1_txt),'Unarmed assault',
                                                                                  ifelse(grepl('Bombing',attacktype1_txt),'Explosion',attacktype1_txt))))))
gtd_years<- gtd %>% 
    filter(iyear %in% c(1970, 2017))


gtd_leaflet<- gtd %>% filter(!is.na(latitude) && !is.na(longitude))
#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Global Terrorism ")  


#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Visit kaggle for data", icon = icon("send",lib='glyphicon'), 
                 href = "https://www.kaggle.com/START-UMD/gtd")))    



frow1 <- fluidRow(
    valueBoxOutput("value1")
    ,valueBoxOutput("value2")
    ,valueBoxOutput("value3"))
 frow2 <- fluidRow(title = "Comparison of Attacks overtime"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE
        ,leafletOutput("mymap", height = "500px")
        ,sliderInput("slider", "Select year:", 1970, 2017, 2000,width = "100%",ticks = FALSE,sep = ""))
 



 frow3 <- fluidRow( 
     box(
         title = "Who were the main targets"
         ,status = "primary"
         ,solidHeader = TRUE 
         ,collapsible = TRUE
         ,plotOutput("targetkilled", height = "300px")
         ,radioButtons("radio", h3("Select Decade"),
                       choices = list("70's" = "70's","80's" = "80's", 
                                      "90's" = "90's","2000's" = "2000's", 
                                      "2010's" = "2010's"),selected = "2000's", inline = T))
     ,box(
         title = "Number of kills per Region"
         ,status = "primary"
         ,solidHeader = TRUE 
         ,collapsible = TRUE
         ,plotOutput("killbyregion", height = "300px")
         ,radioButtons("yearsradio", h3("Select Decade"), choices = list("70's" = "70's","80's" = "80's", 
                                                                       "90's" = "90's","2000's" = "2000's", 
                                                                       "2010's" = "2010's"),selected = "2000's", inline = T))) 
 
 
frow4<-fluidRow(box(
    title = "Number of Kills by Attack Type"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("Killbyattacktype", height = "300px"),
    selectInput("selector", h3("Select Region"), 
                choices = list("Australia & Oceania" = "Australasia & Oceania", "Central America" = "Central America & Caribbean", 
                               "Central Asia" = "Central Asia", "East Asia" = "East Asia", "Eastern Europe" = "Eastern Europe", 
                               "Middle East" = "Middle East & North Africa", "North America" = "North America",
                               "South America" = "South America", "South Asia" = "South Asia", "Southeast Asia" = "Southeast Asia", 
                               "Sub-Saharan Africa" = "Sub-Saharan Africa", "Western Europe" = "Western Europe"), selected = 1)),
    box(
        title = "Type of terrorist attacks by years",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("Attackbyyear",height = "300px"),
        checkboxGroupInput("checkbox",h3("Select attack type"),
                           choices = list("Assassination"="Assassination", "Hostage taking"= "Hostage taking",
                                          "Armed assault"="Armed assault","Facility attack"="Facility attack",
                                          "Explosion"="Explosion","Hijacking"="Hijacking","Unknown"="Unknown",
                                          "Unarmed assault"="Unarmed assault"),selected = 1)
    ))

# combine the 4 fluid rows to make the body
body <- dashboardBody(frow1, frow2,frow3,frow4)    


#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'Global Terrorism Dashboard', header, sidebar, body, skin='red')    



# create the server functions for the dashboard  
server <- function(input, output) { 
    
    #some data manipulation to derive the values
    total_kills <- sum(gtd$nkill[!is.na(gtd$nkill)])
    countries <- gtd %>%filter(!is.na(nkill)) %>%  group_by(country) %>% summarise(value = sum(nkill)) %>% filter(value==max(value))
    weapon.type <- gtd %>%filter(!is.na(nkill)) %>% filter(!is.na(attacktype1_txt))%>%group_by(attacktype1_txt) %>% summarise(value = sum(nkill)) %>% filter(value==max(value))
    attacker <- gtd %>% filter(!is.na(nkill)) %>% group_by(nationality) %>% summarise(value = sum(nkill)) %>% filter(value==max(value))
    
    
    
    
    #creating the valueBoxOutput content
    output$value1 <- renderValueBox({
        valueBox(
            formatC(countries$value, format="d", big.mark=',')
            ,paste('Most fatalities:',countries$country)
            ,icon = icon("stats",lib='glyphicon')
            ,color = "purple")
        
        
    })
    
    
    
    output$value2 <- renderValueBox({
        
        valueBox(
            formatC(total_kills, format="d", big.mark=',')
            ,paste('Most common victim nationality:',attacker$nationality)
            ,icon = icon("stats",lib='glyphicon')
            ,color = "green")
        
    })
    
    
    
    output$value3 <- renderValueBox({
        
        valueBox(
            formatC(weapon.type$value, format="d", big.mark=',')
            ,paste('Most common attack type:',weapon.type$attacktype1_txt)
            ,icon = icon("stats",lib='glyphicon')
            ,color = "yellow")
        
    })
    pallete=distinctColorPalette(12)
    pal <- colorFactor(
        palette = pallete,
        domain = gtd_leaflet$region
    )
    
    datasetyears <- reactive({
        
        filter(gtd_leaflet, gtd_leaflet$iyear == input$slider)
    })
    #create the map
    output$mymap <- renderLeaflet({
        data_years <- datasetyears()
        leaflet(data_years) %>% 
            setView(lng = 43.6793, lat = 33.2232, zoom = 2.5) %>% 
            addTiles() %>% 
            addCircles(data = data_years, lat = ~ latitude, lng = ~ longitude, weight = 1, 
                       radius = ~sqrt(nkill)*25000, popup = ~paste0("Region: ",as.character(region),"<br>Attack Type: ",as.character(attacktype1_txt)),
                       label = ~as.character(paste0("Number of deaths:", sep = " ", nkill)),
                       color = ~pal(region) ,
                       fillOpacity = 0.5) %>% 
            addLegend(position = "bottomright",pal=pal,values = ~region,title = "Regions")
    })
    


#creating the plotOutput content
datasetInput <- reactive({
    
    filter(gtd_top_targets, gtd_top_targets$decades <= input$yearsradio) #%>% arrange(desc(Sales)
    
})

output$killbyregion <- renderPlot({
    data_filter <- datasetInput()
    
    ggplot(data = data_filter, 
           aes(x=region, y=nkill,fill=region)) + 
        geom_bar(position = "dodge", stat = "identity") + ylab("Number of fatalities") + 
        xlab("Region") +
        scale_fill_manual(values = pallete)+
    theme(legend.position="top",
          legend.title = element_blank(),
          axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

       
})

datasettarget <- reactive({
    
    filter(gtd_top_targets, gtd_top_targets$decades <= input$radio)
})

output$targetkilled <- renderPlot({
    data_target <- datasettarget()
    
    
    ggplot(data = data_target, 
           aes(x=target_type, y=nkill)) + 
        geom_bar(position = "dodge", stat = "identity", fill="steelblue") + ylab("Number of Fatalities") + 
        xlab("") +
        coord_flip()+
        theme(legend.position="bottom",
              plot.title = element_text(size=15, face="bold"),
              axis.ticks.x = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        ggtitle("Type of Target")
})


datasetregion <- reactive({
    
    filter(gtd_top_targets, gtd_top_targets$region <= input$selector)
})
output$Killbyattacktype <- renderPlot({
    data_region <- datasetregion()
    
    ggplot(data = data_region, 
           aes(x=attack_type, y=nkill,fill=attack_type)) + 
        geom_bar(position = "dodge", stat = "identity") + ylab("Number of Fatalities") + 
        xlab("Attack Type") + theme(legend.position="right" 
                                    ,plot.title = element_text(size=15, face="bold"),
                                    axis.text.x = element_blank(),
                                    axis.ticks.x = element_blank(),
                                    panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank())+ 
        scale_fill_viridis_d()+
        ggtitle("Fatalities by Type of Attack")
})

datasetattack<-reactive({
    filter(gtd_top_targets,gtd_top_targets$attack_type %in% input$checkbox)
})

output$Attackbyyear<-renderPlot({
    data_attack<-datasetattack()
    data_attack<-data_attack%>%group_by(iyear,attack_type)%>%summarise(kill=sum(nkill))
    
    ggplot(data_attack,aes(x=iyear,y=kill,color=attack_type))+
        geom_point(size=1)+
        geom_line()+
        xlab("Year")+
        ylab("Number of fatalities")+
        theme(legend.position="right",
              legend.title = element_blank()
              ,plot.title = element_text(size=15, face="bold"))+
        scale_color_viridis_d()+
            ggtitle("Fatalities by Year")
})

}

shinyApp(ui, server)

