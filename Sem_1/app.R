#----------------------------
#-------- library -----------
#----------------------------

library(shiny)
library(shinydashboard)
library(dplyr)

#----------------------------
#------- import data --------
#----------------------------

# COVID
# Število umrlih glede na starostno skupino
# Stats
# Source: https://github.com/sledilnik/data/blob/master/csv/stats.csv
data_all = read.csv("https://raw.githubusercontent.com/sledilnik/data/master/csv/stats.csv")

data_age = data_all %>% 
    select('deceased.0.4.todate', 'deceased.5.14.todate', 'deceased.15.24.todate',
           'deceased.25.34.todate', 'deceased.35.44.todate', 'deceased.45.54.todate',
           'deceased.55.64.todate', 'deceased.65.74.todate', 'deceased.75.84.todate',
           'deceased.85..todate', 'deceased.todate', 'date')
data_age$`0-14` <- data_age$deceased.0.4.todate + data_age$deceased.5.14.todate
data_age = data_age %>% 
    select('0-14', 'deceased.15.24.todate',
           'deceased.25.34.todate', 'deceased.35.44.todate', 'deceased.45.54.todate',
           'deceased.55.64.todate', 'deceased.65.74.todate', 'deceased.75.84.todate',
           'deceased.85..todate', 'deceased.todate', 'date')
colnames(data_age) <- c('0-14',
                        '15-24',
                        '25-34',
                        '35-44',
                        '45-54',
                        '55-64',
                        '65-74',
                        '75-84',
                        '85+')


# By region
# Active: https://github.com/sledilnik/data/blob/master/csv/region-active.csv
# Confirmed: https://github.com/sledilnik/data/blob/master/csv/region-confirmed.csv
# Deceased: https://github.com/sledilnik/data/blob/master/csv/region-deceased.csv
data_regions = read.csv("https://raw.githubusercontent.com/sledilnik/data/master/csv/region-cases.csv")
data_regions$date <- as.Date(data_regions$date)

# Population
# Absolutno število ljudi v starostni skupini
# SURS https://www.stat.si/StatWeb/Field/Index/17/104
data_pop_dist = read.csv("Sem_1/data_pop_dist.csv")
data_pop$`0-14` <- 316657
data_pop$`15-24` <- 197116
data_pop$`25-34` <- 247791
data_pop$`35-44` <- 312259
data_pop$`45-54` <- 301052
data_pop$`55-64` <- 295396
data_pop$`65-74` <- 236812
data_pop$`75-84` <- 138009
data_pop$`85+` <- 55034
								


#----------------------------
#------  sidebar  -----------
#----------------------------

dashboardSidebar <- dashboardSidebar(
    sidebarMenu(
        id = "tabs",
        menuItem("Uvod", tabName = "uvod", selected = TRUE),
        menuItem("Starostne skupine", tabName = "star"),
        menuItem("Regije", tabName = "reg")
    )
)

#----------------------------
#---------  body ------------
#----------------------------


body <- dashboardBody(
    tabItems(
        
        # UVOD
        tabItem(tabName = "uvod",
                fluidRow(
                    h2(align="center", "Uvod")
                    
                ),
                fluidRow(
                    box(
                        width = 12, status = "primary",
                        p("Temp. text")
                    )
                ),
                # fluidRow(
                #     box(width = 4, status = "primary",
                #         img(src="incidenca.jpg", align = "center", width="100%")),
                #     box(width = 4, status = "primary",
                #         img(src="prezivetje.jpg", align = "center", width="100%")),
                #     box(width = 4, status = "primary",
                #         img(src="umrljivost.jpg", align = "center", width="100%"))
                # 
                # ),

        ),
        
        # STAROST
        tabItem(tabName = "star",
                fluidRow(
                    h2(align="center", "Umrljivost po starostnih skupinah relativno na populacijo")
                    
                ),
                # fluidRow(
                #     box(
                #         status = "primary", width=4,
                #         selectInput(
                #             "starVar", 
                #             "Izbira prikazane spremenljivke:",
                #             c("Incidenca" = "inc",
                #               "Umrljivost" = "umr")
                #         ),
                #         sliderInput(
                #             inputId = "num",
                #             label = "Leto",
                #             value = 1985,
                #             min = 1985,
                #             max = 2016,
                #             animate=TRUE,
                #             sep ="" 
                #         )
                #     ),
                #     box(
                #         status = "primary", width=8,
                #         plotOutput(outputId = "grafStar"))
                # ),
                fluidRow(
                    box(
                        width = 12, status = "primary",
                        p("temp text")
                    )
                )
        ),
        
        # REGIJE
        tabItem(tabName = "reg",
                fluidRow(
                    h2(align="center", "po regijah")
                    
                ),
                # fluidRow(   
                #     box(status = "primary", width=4,  
                #         selectInput("choices", "Incidenca ali umrljivost ?", choices = c("Groba incidencna stopnja","Groba umrljivostna stopnja"), selected = "Groba incidencna stopnja"),
                #         uiOutput("Slider"),
                #         helpText("Podrobnosti za vsako regijo se izpisejo ob kliku na izbrano regijo."), 
                #         uiOutput("selection"))  ,
                #     box(status = "primary", width=8, leafletOutput("map_1", width = "100%", height = "600px")  ) ),
                fluidRow(
                    box(
                        width = 12, status = "primary",
                        p("Prvi odstavek"),
                        p("Drugi odstavek")
                    )
                )
        )
        
    )
)

#----------------------------
#----------- ui -------------
#----------------------------

ui <- dashboardPage(
    dashboardHeader(title = "Napredni R: Seminar 1"),
    dashboardSidebar,
    body
)

#----------------------------
#------- shiny app ----------
#----------------------------

shinyApp(ui = ui, 
         server = function(input, output, session) {
             
             # # Zemljevid
             # dataPays<- reactive({
             #     if(!is.null(input$choices)){
             #         if(input$choices == "Groba incidencna stopnja"){
             #             return(dataIncidenca)
             #             
             #         }else{
             #             return(dataUmrljivost)
             #         }}
             # })
             # 
             # jourDate<- reactive({
             #     if(!is.null(input$choices)){
             #         if(input$choices == "Groba incidencna stopnja"){
             #             return(jourDate1)
             #             
             #         }else{
             #             return(jourDate2)
             #         }}
             #     else{
             #         return(jourDate1)
             #     }
             # })
             # 
             # #jour <- reactive({return(names(dataPays())[-c(1)])})
             # #jourDate <- reactive(as.numeric(jour()))
             # 
             # maxTotal<- reactive(max(dataPays()%>%select_if(is.numeric), na.rm = T)
             # ) #pogleda vrednosti v tabeli Incidence/Umrljivosti, ki niso Regija in spravi najvecjo
             # minTotal<- reactive(min(dataPays()%>%select_if(is.numeric), na.rm = T)
             # ) #se minimum za legendo
             # 
             # output$map_1 <- renderLeaflet({
             #     leaflet(data = gadm) %>%
             #         # Nastavitev zacetnega prikaza (pozicija, velikost):
             #         setView(15, 46.07, zoom = 8)
             # })
             # 
             # pal <- reactive(colorNumeric(palette = "YlGnBu", domain = dataPays()%>%select_if(is.numeric)))
             # 
             # observe({
             #     casesDeath<- ifelse(input$choices == "Groba incidencna stopnja","Groba incidencna stopnja","Groba umrljivostna stopnja")
             #     
             #     if (!is.null(input$leto2)) {
             #         if (as.numeric(input$leto2[1]) < min(jourDate())){
             #             indicator2<-as.character(c(max(jourDate()),max(jourDate())))
             #         }
             #         else{
             #             indicator2<-as.character(c(input$leto2,input$leto2)) #vrne dve letnici
             #         }
             #     }else{
             #         indicator2 = as.character(c(min(jourDate()),max(jourDate())))
             #     }
             #     
             #     variable<- input$variable
             #     dataPaysSel<-dataPays()%>%select(Regija,)
             #     # Naredili bomo nov stolpec ncases
             #     st.let = as.numeric(indicator2[2])-as.numeric(indicator2[1])+1
             #     #vektor = (as.numeric(indicator2[1])):as.numeric(indicator2[2])
             #     #vsota = apply(dataPays()[,as.character(vektor)],1,sum)
             #     vsota=0
             #     for (k in ((as.numeric(indicator2[1])):as.numeric(indicator2[2]))){
             #         vsota = vsota + dataPays()[,as.character(k)]}
             #     
             #     dataPaysSel$ncases<-round(vsota/st.let,1)
             #     
             #     regije2 <- merge(gadm,
             #                      dataPaysSel,
             #                      by.x = "NAME_1",
             #                      by.y = "Regija",
             #                      sort = FALSE)
             #     regije_popup <- paste0("<strong>Regija: </strong>",
             #                            regije2$NAME_1,
             #                            "<br><strong>",
             #                            casesDeath," v obdobju :",
             #                            " </strong>",
             #                            format(regije2$ncases, big.mark = ".", decimal.mark = ","), " /100.000") #ncases z dataPaysSel
             #     
             #     
             #     leafletProxy("map_1", data = regije2)%>%
             #         addPolygons(fillColor = pal()(regije2$ncases),
             #                     fillOpacity = 1,
             #                     color = "#BDBDC3",
             #                     layerId = ~NAME_1,
             #                     weight = 1,
             #                     popup = regije_popup)
             # }
             # )
             # 
             # observe({
             #     proxy <- leafletProxy("map_1", data = gadm)
             #     # Legenda
             #     proxy %>% clearControls()
             #     
             #     proxy %>% addLegend(position = "bottomright",
             #                         pal = pal(), opacity = 1,
             #                         bins = seq(100,1000,100),
             #                         value = dataPays()%>%select_if(is.numeric),
             #                         data = dataPays()%>%select_if(is.numeric),
             #                         labFormat = labelFormat(prefix = " ", suffix = " /100.000")
             #                         # data je enako kot value. Ni toliko vazno, ce zacnemo pri 1 ali npr.3.
             #     )
             # })
             # 
             # output$Slider<-renderUI({
             #     
             #     sliderInput("leto2", "Leto ali obdobje", min(jourDate()), max(jourDate()), sep="",
             #                 ticks = TRUE,
             #                 value =  c(max(jourDate()),max(jourDate())),animate = T, step = 1
             #                 #kje zacnemo, ko odpremo aplikacijo
             #     )
             # })
             
})