#----------------------------
#-------- library -----------
#----------------------------

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(reshape) # za funkcijo melt
library(sp) # za zemljevid
# Naslednji paketi so za preverit, katere se res rabi (so za zemljevid, mogoče ne rabiva vseh)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(RCurl)
library(plotly)
library(viridis)
library(tidyverse)
library(stringr)
library(data.table) # za pretvorbo imen vrstic v prvi stolpec

#----------------------------
#------- import data --------
#----------------------------

# -----------------------------------------------------------------------------------------------------------------------------------------

# STAROSTNE SKUPINE

# Covid
# Število umrlih glede na starostno skupino
# Source: https://github.com/sledilnik/data/blob/master/csv/stats.csv
data_all = read.csv("https://raw.githubusercontent.com/sledilnik/data/master/csv/stats.csv")
data_all$date <- as.Date(data_all$date)

# Poberem ven smrti glede na starostno skupino
data_smrt = data_all %>% 
    select('deceased.0.4.todate', 'deceased.5.14.todate', 'deceased.15.24.todate',
           'deceased.25.34.todate', 'deceased.35.44.todate', 'deceased.45.54.todate',
           'deceased.55.64.todate', 'deceased.65.74.todate', 'deceased.75.84.todate',
           'deceased.85..todate', 'deceased.todate', 'date')
data_smrt$`0-14` <- data_smrt$deceased.0.4.todate + data_smrt$deceased.5.14.todate
data_smrt = data_smrt %>% 
    select('0-14', 'deceased.15.24.todate',
           'deceased.25.34.todate', 'deceased.35.44.todate', 'deceased.45.54.todate',
           'deceased.55.64.todate', 'deceased.65.74.todate', 'deceased.75.84.todate',
           'deceased.85..todate', 'deceased.todate', 'date')
colnames(data_smrt) <- c('0-14',
                        '15-24',
                        '25-34',
                        '35-44',
                        '45-54',
                        '55-64',
                        '65-74',
                        '75-84',
                        '85+',
                        'Cumulative',
                        'Date')
data_smrt[is.na(data_smrt)] <- 0

# Poberem ven potrjene primere glede na starostno skupino
data_incidenca = data_all %>%
    select('age.0.4.todate', 'age.5.14.todate', 'age.15.24.todate',
           'age.25.34.todate', 'age.35.44.todate', 'age.45.54.todate',
           'age.55.64.todate', 'age.65.74.todate', 'age.75.84.todate',
           'age.85..todate', 'age.todate', 'date')
data_incidenca$`0-14` <- data_incidenca$age.0.4.todate + data_incidenca$age.5.14.todate
data_incidenca = data_incidenca %>% 
    select('0-14', 'age.15.24.todate',
           'age.25.34.todate', 'age.35.44.todate', 'age.45.54.todate',
           'age.55.64.todate', 'age.65.74.todate', 'age.75.84.todate',
           'age.85..todate', 'age.todate', 'date')
colnames(data_incidenca) <- c('0-14',
                         '15-24',
                         '25-34',
                         '35-44',
                         '45-54',
                         '55-64',
                         '65-74',
                         '75-84',
                         '85+',
                         'Cumulative',
                         'Date')
data_incidenca[is.na(data_incidenca)] <- 0

# Population
# Absolutno število ljudi v starostni skupini
# SURS https://www.stat.si/StatWeb/Field/Index/17/104
#data_pop_dist = read.csv("Sem_1/data_pop_dist.csv")
#data_pop_dist = read.csv("data_pop_dist.csv")
# Ročno pregledani (hitreje)
data_pop <- data.frame(matrix())
data_pop$`0-14` <- 316657
data_pop$`15-24` <- 197116
data_pop$`25-34` <- 247791
data_pop$`35-44` <- 312259
data_pop$`45-54` <- 301052
data_pop$`55-64` <- 295396
data_pop$`65-74` <- 236812
data_pop$`75-84` <- 138009
data_pop$`85+` <- 55034
data_pop <- data_pop[,-1]
								
# Podatki po starostnih skupinah relativno na populacijo
data_incidenca_rel <- data.frame(matrix(nrow = dim(data_incidenca)[1], ncol = 1))
data_incidenca_rel$`0-14`  <- data_incidenca$`0-14`  / data_pop$`0-14`
data_incidenca_rel$`15-24` <- data_incidenca$`15-24` / data_pop$`15-24`
data_incidenca_rel$`25-34` <- data_incidenca$`25-34` / data_pop$`25-34`
data_incidenca_rel$`35-44` <- data_incidenca$`35-44` / data_pop$`35-44`
data_incidenca_rel$`45-54` <- data_incidenca$`45-54` / data_pop$`45-54`
data_incidenca_rel$`55-64` <- data_incidenca$`55-64` / data_pop$`55-64`
data_incidenca_rel$`65-74` <- data_incidenca$`65-74` / data_pop$`65-74`
data_incidenca_rel$`75-84` <- data_incidenca$`75-84` / data_pop$`75-84`
data_incidenca_rel$`85+`   <- data_incidenca$`85+`   / data_pop$`85+`
data_incidenca_rel$Date <- data_incidenca$Date
data_incidenca_rel = data_incidenca_rel %>% 
    select('0-14',  '15-24', '25-34',
           '35-44', '45-54', '55-64',
           '65-74', '75-84', '85+',
           'Date')

data_smrt_rel <- data.frame(matrix(nrow = dim(data_smrt)[1], ncol = 1))
data_smrt_rel$`0-14`  <- data_smrt$`0-14`  / data_pop$`0-14`
data_smrt_rel$`15-24` <- data_smrt$`15-24` / data_pop$`15-24`
data_smrt_rel$`25-34` <- data_smrt$`25-34` / data_pop$`25-34`
data_smrt_rel$`35-44` <- data_smrt$`35-44` / data_pop$`35-44`
data_smrt_rel$`45-54` <- data_smrt$`45-54` / data_pop$`45-54`
data_smrt_rel$`55-64` <- data_smrt$`55-64` / data_pop$`55-64`
data_smrt_rel$`65-74` <- data_smrt$`65-74` / data_pop$`65-74`
data_smrt_rel$`75-84` <- data_smrt$`75-84` / data_pop$`75-84`
data_smrt_rel$`85+`   <- data_smrt$`85+`   / data_pop$`85+`
data_smrt_rel$Date <- data_smrt$Date
data_smrt_rel = data_smrt_rel %>% 
    select('0-14',  '15-24', '25-34',
           '35-44', '45-54', '55-64',
           '65-74', '75-84', '85+',
           
           
           'Date')


# ---------------------------------------------------------------------------------------------------------------------------------------

# REGIJE

# Active: https://github.com/sledilnik/data/blob/master/csv/region-active.csv
# Confirmed: https://github.com/sledilnik/data/blob/master/csv/region-confirmed.csv
# Deceased: https://github.com/sledilnik/data/blob/master/csv/region-deceased.csv
data_regions = read.csv("https://raw.githubusercontent.com/sledilnik/data/master/csv/region-cases.csv") # vsi podatki skupaj
data_regions$date <- as.Date(data_regions$date)

# Uvozimo število prebivalcev po regijah
data_pop_regions <- read.csv("https://raw.githubusercontent.com/sledilnik/data/master/csv/dict-region.csv")
data_pop_regions <- data_pop_regions[-c(14,15),] # odstranimo zadnji dve vrstici, ker ju ne rabimo

# Podatki za izris zemljevida:
gadm <- readRDS("Data/SVN.rds")
# Da se znebimo šumnikov:
regije <- c("Gorenjska", "Goriska", "Jugovzhodna Slovenija", "Koroska",
            "Primorsko-notranjska", "Obalno-kraska", "Osrednjeslovenska",
            "Podravska", "Pomurska", "Savinjska", "Posavska", "Zasavska")
gadm$NAME_1 <- regije

# Urejanje tabel za zemljevid:
df2 <- data.frame(t(data_regions[-1])) # obrnemo tabelo (zamenjamo stolpce in vrstice)
colnames(df2) <- data_regions[, 1] # imena stolpcev so datumi
df2 <- df2[-c(seq(from=1, to=nrow(df2), by=3)), ] # za vsako regijo je prva vrstica neuporabna za našo aplikacijo (št.aktivnih primerov)
df2 <- df2[-c(3,4,25,26), ] # odstranimo še podatke o tujcih in "unknown"

razdeljeno <- str_extract_all(rownames(df2), "[a-z]+", simplify = TRUE) # naredimo matriko posameznih besed iz imen vrstic (regija, okuženi/umrli,...)
razdeljeno <- razdeljeno[, c(2:3)] # rabimo samo te stolpce in samo te vrstice 
imena <- paste(razdeljeno[,1], razdeljeno[,2], sep="_") # zdaj imamo shranjena imena vrstic za našo skupno tabelo
rownames(df2) <- imena

dataIncidenca <- df2[c(seq(from=1, to=nrow(df2), by=2)), ]
dataUmrljivost <- df2[c(seq(from=2, to=nrow(df2), by=2)), ]
rownames(dataIncidenca) <- regije[c(10, 11, 6, 1, 7, 8, 9, 2, 3, 5, 4, 12)] # uredimo imena vrstic (regij) in spotoma popravimo vrstni red
dataIncidenca <- setDT(dataIncidenca, keep.rownames = TRUE)[] # imena vrstic pretvorimo v prvi stolpec
colnames(dataIncidenca)[1] <- "Regija"
rownames(dataUmrljivost) <- regije[c(10, 11, 6, 1, 7, 8, 9, 2, 3, 5, 4, 12)]
dataUmrljivost <- setDT(dataUmrljivost, keep.rownames = TRUE)[]
colnames(dataUmrljivost)[1] <- "Regija"

dataIncidenca[is.na(dataIncidenca)] <- 0 # NA-je pretvorimo v ničle
dataUmrljivost[is.na(dataUmrljivost)] <- 0
dataIncidenca <- as.data.frame(dataIncidenca)
dataIncidenca[2:ncol(dataIncidenca)] <- lapply(dataIncidenca[2:ncol(dataIncidenca)], as.numeric)
dataUmrljivost <- as.data.frame(dataUmrljivost)
dataUmrljivost[2:ncol(dataUmrljivost)] <- lapply(dataUmrljivost[2:ncol(dataUmrljivost)], as.numeric)

dataPays <- function(data=dataIncidenca) return(data) # za kasneje
jour <- names(dataIncidenca)[2:ncol(dataIncidenca)] # datumi so v obeh tabelah enaki
jourDate <- as.Date(jour)
names(dataIncidenca)[2:ncol(dataIncidenca)] <- format.Date(jourDate)
names(dataUmrljivost)[2:ncol(dataUmrljivost)] <- format.Date(jourDate)

arrondi <- function(x) 10^(ceiling(log10(x))) #za racunanje na populacijo

# ---------------------------------------------------------------------------------------------------------------------------------------

#----------------------------
#------  sidebar  -----------
#----------------------------

dashboardSidebar <- dashboardSidebar(
    sidebarMenu(
        id = "tabs",
        menuItem("Uvod", tabName = "uvod", selected = TRUE), # aplikacija se odpre v tem zavihku
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
                        # znak <br/> je za skok v novo vrstico, znak <p> pa za prazno vrstico
                        p(HTML("<p>V sklopu predmeta 'Napredni pristopi v programskem okolju R' sva pripravila dve različni vizualizaciji podatkov, povezanih z boleznijo Covid-19 v Sloveniji.  
                          Podatki se berejo neposredno iz podatkovnega repozitorija projekta <a href='https://github.com/sledilnik/data'>Sledilnik</a>.
                          <p>
                          Prvi graf predstavlja delež okuženega prebivalstva (ali delež umrlega prebivalstva) po starostnih skupinah.
                          Podatki o številu prebivalcev po starostnih skupinah so dostopni na spletni strani <a href='https://pxweb.stat.si/SiStatData/pxweb/sl/Data/-/05C4002S.px'>SURS</a>.
                          <p>
                          V zavihku 'Regije' je izrisan zemljevid, ki prikazuje število vseh potrjenih primerov okužbe (ali število vseh smrti) glede na število prebivalcev slovenskih statističnih regij.
                          Podatki o številu prebivalcev po regijah so prav tako dostopni na repozitoriju projekta <a href='https://github.com/sledilnik/data'>Sledilnik</a>.
                          </p>"))
                    )
                ),
                
                # Če želimo dodati slike na uvodno stran:
                
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
        
        # STAROSTNE SKUPINE
        tabItem(tabName = "star",
                fluidRow(
                    h2(align="center", "Umrljivost po starostnih skupinah relativno na populacijo")
                    
                ),
                fluidRow(
                    box(status = "primary", width=4,
                        selectInput(
                            "starVar",
                            "Izbira relativne statistike:",
                            c("Relativna incidenca" = "inc",
                              "Relativna umrljivost" = "umr")),
                        sliderInput(
                            inputId = "num",
                            label = "Datum:",
                            value = as.Date('2020-02-24',"%Y-%m-%d"),
                            min = as.Date('2020-02-24',"%Y-%m-%d"),
                            max = as.Date(Sys.Date()-7), # Alternativa: Sys.Date()
                            timeFormat = "%Y-%m-%d",
                            animate = animationOptions(interval = 200))
                    ),
                    box(
                        status = "primary", width=8,
                        plotOutput(outputId = "grafStar"))
                ),
                fluidRow(
                    box(
                        width = 12, status = "primary",
                        p("Graf prikazuje delež okuženih oziroma delež umrlih v starostni skupini glede na velikost starostne skupine v populaciji.")
                    )
                )
        ),
        
        # REGIJE
        tabItem(tabName = "reg",
                fluidRow(
                    h2(align="center", "Prikaz podatkov po regijah")
                    
                ),
                fluidRow(   
                    box(status = "primary", width=4,  
                        selectInput("choices", "Incidenca ali umrljivost ?", choices = c("Relativno število potrjenih primerov","Relativno število smrti"), selected = "Relativno število potrjenih primerov"),
                        uiOutput("Slider"),
                        helpText("Podrobnosti za vsako regijo se izpišejo ob kliku na izbrano regijo."), 
                        uiOutput("selection"))  ,
                    box(status = "primary", width=8, leafletOutput("map_1", width = "100%", height = "600px")  ) ),
                fluidRow(
                    box(
                        width = 12, status = "primary",
                        p("Tukaj lahko dodam kratek opis/razlago grafa.")
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
             
             # Zemljevid
             dataPays <- reactive({
                 if(!is.null(input$choices)){
                     if(input$choices == "Relativno število potrjenih primerov"){
                         return(dataIncidenca)

                     }else{
                         return(dataUmrljivost)
                     }}
             })

             # jourDate <- reactive({
             #     if(!is.null(input$choices)){
             #         if(input$choices == "Relativno število potrjenih primerov"){
             #             return(jourDate)
             # 
             #         }else{
             #             return(jourDate)
             #         }}
             #     else{ # če nič ni izbrano, je število potrjenih primerov, zato jourDate1
             #         return(jourDate)
             #     }
             # })

             #jour <- reactive({return(names(dataPays())[-c(1)])})
             #jourDate <- reactive(jour())

             maxTotal<- reactive(max(dataPays()%>%select_if(is.numeric), na.rm = T)
             ) #pogleda vrednosti v tabeli Incidence/Umrljivosti, ki niso Regija in spravi najvecjo
             minTotal<- reactive(min(dataPays()%>%select_if(is.numeric), na.rm = T)
             ) #se minimum za legendo

             output$map_1 <- renderLeaflet({
                 leaflet(data = gadm) %>%
                     # Nastavitev zacetnega prikaza (pozicija, velikost):
                     setView(15, 46.07, zoom = 8)
             })

             pal <- reactive(colorNumeric(palette = "YlGnBu", domain = dataPays()%>%select_if(is.numeric)))

             observe({
                 casesDeath <- ifelse(input$choices == "Groba incidencna stopnja","Groba incidencna stopnja","Groba umrljivostna stopnja")

                 if (!is.null(input$day1)) {
                     indicator <- format.Date(input$day1)
                     # if (as.numeric(input$leto2[1]) < min(jourDate())){
                     #     indicator2<-as.character(c(max(jourDate()),max(jourDate())))
                     # }
                     # else{
                     #     indicator2<-as.character(c(input$leto2,input$leto2)) #vrne dve letnici
                     # }
                 }
                 else{
                     indicator = format.Date(max(jourDate))
                     # indicator2 = as.character(c(min(jourDate()),max(jourDate())))
                 }
                 
                 if (!is.null(input$day2)) {
                     indicator2 <- format.Date(input$day2-c(1,0))
                     
                 }else{
                     indicator2 <- format.Date(c(min(jourDate)-1, max(jourDate)))
                 }
                 
                 # if(is.null(input$variable)){ # TOLE NE VEM ČE JE POTREBNO, MOGOČE PA LAHKO REŠI PROBLEM ZAČETNEGA PRIKAZA ZEMLJEVIDA....NI PA NUJNO
                 #     
                 # }else{

                 variable <- input$variable
                 dataPaysSel <- dataPays()%>%select(Regija,)
                 # Naredili bomo nov stolpec ncases
                 # st.let = (indicator2[2])-(indicator2[1])+1 # v resnici zdaj gledamo dneve, ne več let
                 # vsota=0
                 # for (k in 1:(indicator2[2]-indicator2[1])){
                 #     vsota = vsota + dataPays()[,as.character(k)]} # k pretvorimo v character zato, da dobimo indeks

                 dataPaysSel$ncases <- dataPays()[,3] # to je v resnici treba naredit tako, da gre po vseh izbranih stolpcih oz. samo po zadnjem izbranem...

                 regije2 <- merge(gadm,
                                  dataPaysSel,
                                  by.x = "NAME_1",
                                  by.y = "Regija",
                                  sort = FALSE)
                 regije_popup <- paste0("<strong>Regija: </strong>",
                                        regije2$NAME_1,
                                        "<br><strong>",
                                        casesDeath," v obdobju :",
                                        " </strong>",
                                        format(regije2$ncases, big.mark = ".", decimal.mark = ","), " /100.000") #ncases z dataPaysSel


                 leafletProxy("map_1", data = regije2)%>%
                     addPolygons(fillColor = pal()(regije2$ncases),
                                 fillOpacity = 1,
                                 color = "#BDBDC3",
                                 layerId = ~NAME_1,
                                 weight = 1,
                                 popup = regije_popup)
             }# }
             )

             observe({
                 proxy <- leafletProxy("map_1", data = gadm)
                 # Legenda
                 proxy %>% clearControls()

                 proxy %>% addLegend(position = "bottomright",
                                     pal = pal(), opacity = 1,
                                     bins = seq(100,1000,100),
                                     value = dataPays()%>%select_if(is.numeric),
                                     data = dataPays()%>%select_if(is.numeric),
                                     labFormat = labelFormat(prefix = " ", suffix = " /100.000")
                                     # data je enako kot value. Ni toliko vazno, ce zacnemo pri 1 ali npr.3.
                 )
             })

             output$Slider<-renderUI({

                 sliderInput("day2", "Datum ali obdobje", min(jourDate), max(jourDate), sep="",
                             ticks = TRUE,
                             value =  c(max(jourDate), max(jourDate)), animate = T, step = 1
                             #kje zacnemo, ko odpremo aplikacijo
                 )
             })
             
             
             # ----------------------------------------------------------------------------------------------------------------------
             
             
             # Porazdelitev po starosti
             data_starost <- reactive({
                 if(!is.null(input$starVar)){
                     if(input$starVar == "inc"){
                         return(data_incidenca_rel)

                     }else{
                         return(data_smrt_rel)
                     }}
             })

             label_y <- reactive({
                 if(!is.null(input$starVar)){
                     if(input$starVar == "inc"){
                         return("Incidenca relativna na populacijo")

                     }else{
                         return("Umrljivost relativna na populacijo")
                     }}
             })
             
             lim_y <- reactive({
                 if(!is.null(input$starVar)){
                     if(input$starVar == "inc"){
                         return(0.2)
                         
                     }else{
                         return(0.05)
                     }}
             })

             observeEvent(input$starVar,{
                 print(input$starVar)
             }
             )

             output$grafStar<-renderPlot({
                 input = as.Date(input$num)
                 Data = data_starost() # Izbira pravega df
                 Data = Data[Data$Date == input,] # Pravi datum
                 Data = select(Data, -Date) # Samo še številke, brez datuma
                 Data <- melt(Data, id=c()) # Preoblikovanje za ggplot
                 
                 ggplot(data = Data, aes(x = variable, y = value)) +
                     geom_col(fill='#2c7fb8') +
                     xlab("Starostna skupina") +
                     ylab(label_y()) +
                     ylim(0, lim_y()) +
                     coord_flip()+
                     theme_minimal()
             })

             
})

