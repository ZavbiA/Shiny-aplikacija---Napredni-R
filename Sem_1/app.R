#----------------------------
#-------- library -----------
#----------------------------

library(shiny)
library(shinydashboard)

#----------------------------
#------- import data --------
#----------------------------

# COVID

# Stats
# Source: https://github.com/sledilnik/data/blob/master/csv/stats.csv
data_age = read.csv("https://raw.githubusercontent.com/sledilnik/data/master/csv/stats.csv")

# By region
# Active: https://github.com/sledilnik/data/blob/master/csv/region-active.csv
# Confirmed: https://github.com/sledilnik/data/blob/master/csv/region-confirmed.csv
# Deceased: https://github.com/sledilnik/data/blob/master/csv/region-deceased.csv
data_regions = read.csv("https://raw.githubusercontent.com/sledilnik/data/master/csv/region-cases.csv")
data_regions$date <- as.Date(data_regions$date)

# Population
# SURS https://www.stat.si/StatWeb/Field/Index/17/104
data_pop_dist = read.csv("Sem_1/data_pop_dist.csv")
# Ohranim samo zanimive stolpce, spremenim imena & odštejem, da dobim zaprto skupino
data_pop_dist <- data_pop_dist[,c(2,4)]
colnames(data_pop_dist) <- c("Starost", "Delez")
data_pop_dist$Starost <- c("0-14", "15-64", "65-79", "80+")
data_pop_dist[3,2] <- data_pop_dist[3,2] - data_pop_dist[4,2]

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