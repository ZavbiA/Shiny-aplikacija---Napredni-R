
library(shiny)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(readxl)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(RCurl)
library(plotly)
library(viridis)
library(tidyverse)
library(sp)
library(stringr)
library(shinydashboard)
library(googleCharts)
library(crayon)
library(shinyWidgets)


# uvod

# Stadij
stadij <- read_csv2("Data/SLORA_stadij.csv", col_names = TRUE, cols(
  Leto = col_integer(),
  Stevilo = col_integer(),
  Stadij = col_character(),
  Vsi = col_integer()
))
stadij$Stadij <- as.factor(stadij$Stadij)
stadij$Vsi <- NULL
stadij$Leto <- as.factor(stadij$Leto)

# Spol
inc_spol_1 <- read_csv2("Data/SLORA_spol_1.csv", col_names = TRUE, cols(
  Leto = col_integer(),
  Stevilo = col_integer(),
  Spol = col_character(),
  Skupaj = col_integer()
))
inc_spol_1$rel = inc_spol_1$Stevilo/inc_spol_1$Skupaj
inc_spol_1$Leto <- as.factor(as.character(inc_spol_1$Leto))
ia = ggplot(inc_spol_1, aes(x=Leto, y=Stevilo, group=Spol,fill =Spol)) + 
  geom_area(show.legend = FALSE)+
  theme_minimal()+
  ylab("Incidenca")+
  scale_x_discrete(breaks = c(1961, 1966, 1971, 1976, 1981, 1986, 1991, 1996, 2001, 2006, 2011, 2016))+
  scale_fill_manual(values=c("#2c7fb8", "#69cce6")) 

ir = ggplot(inc_spol_1, aes(x=Leto, y=rel, group=Spol,fill =Spol)) + 
  geom_area()+ 
  theme_minimal()+
  ylab("Incidenca")+ 
  geom_hline(yintercept=0.5, linetype="dashed", 
             color = "black", size=1)+
  scale_x_discrete(breaks = c(1961, 1966, 1971, 1976, 1981, 1986, 1991, 1996, 2001, 2006, 2011, 2016))+
  scale_fill_manual(values=c("#2c7fb8", "#69cce6")) 

umrl_spol_1 <- read_csv2("Data/umrljivost_spol_1.csv", col_names = TRUE, cols(
  Leto = col_integer(),
  Stevilo = col_integer(),
  Spol = col_character(),
  Skupaj = col_integer()
))
umrl_spol_1$rel = umrl_spol_1$Stevilo/umrl_spol_1$Skupaj
umrl_spol_1$Leto <- as.factor(as.character(umrl_spol_1$Leto)) #mora biti factor, ne numeric, ce zelim, da izpise letnice na x-os
ua = ggplot(umrl_spol_1, aes(x=Leto, y=Stevilo, group=Spol,fill =Spol)) + 
  geom_area()+ 
  theme_minimal()+
  ylab("Umrljivost")+
  scale_x_discrete(breaks = c(1986, 1991, 1996, 2001, 2006, 2011, 2016))+
  scale_fill_manual(values=c("#2c7fb8", "#69cce6")) 

ur = ggplot(umrl_spol_1, aes(x=Leto, y=rel, group=Spol,fill =Spol)) + 
  geom_area()+ 
  theme_minimal()+
  ylab("Umrljivost")+ 
  geom_hline(yintercept=0.5, linetype="dashed", 
             color = "black", size=1) +
  scale_x_discrete(breaks = c(1986, 1991, 1996, 2001, 2006, 2011, 2016))+
  scale_fill_manual(values=c("#2c7fb8", "#69cce6"))  

prev_spol_1 <- read_csv2("Data/prevalenca_spol_1.csv", col_names = TRUE, cols(
  Leto = col_integer(),
  Stevilo = col_integer(),
  Spol = col_character(),
  Skupaj = col_integer()
))
prev_spol_1$Leto <- as.factor(as.character(prev_spol_1$Leto))
prev_spol_1$rel = prev_spol_1$Stevilo/prev_spol_1$Skupaj
prev_spol_1$Leto <- as.factor(as.character(prev_spol_1$Leto))
pa = ggplot(prev_spol_1, aes(x=Leto, y=Stevilo, group=Spol,fill =Spol)) + 
  geom_area()+ 
  theme_minimal()+
  ylab("Prevalenca")+
  scale_x_discrete(breaks = c(1961, 1966, 1971, 1976, 1981, 1986, 1991, 1996, 2001, 2006, 2011, 2016))+
  scale_fill_manual(values=c("#2c7fb8", "#69cce6")) 


pr = ggplot(prev_spol_1, aes(x=Leto, y=rel, group=Spol,fill =Spol)) + 
  geom_area()+ 
  theme_minimal()+
  ylab("Prevalenca")+ 
  geom_hline(yintercept=0.5, linetype="dashed", 
             color = "black", size=1)+
  scale_x_discrete(breaks = c(1961, 1966, 1971, 1976, 1981, 1986, 1991, 1996, 2001, 2006, 2011, 2016))+
  scale_fill_manual(values=c("#2c7fb8", "#69cce6")) 
# Starostna skupina
inc_starost <- read_csv2("Data/SLORA-incidencna_mera_starost.csv", col_names = TRUE, row.names(1))
#vrstice = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")
#stolpci = seq(from = 1961, to = 2016, by = 1)
#colnames(inc_starost) = c("Starost",stolpci)
#(inc_starost) = vrstice
#barplot(inc_starost$`1961`, names.arg = vrstice, horiz=TRUE,cex.names=0.7, las = 1)
umr_starost <- read_csv2("Data/SLORA_umrljivost.csv", col_names = TRUE, row.names(1))
#colnames(umr_starost) = c("Starost",seq(from = 1985, to = 2016, by = 1))
#rownames(umr_starost) = vrstice


# Krogci
source("skupna_tabela.R")
data <- skupna_tabela

# Use global max/min for axes so the view window stays
# constant as the user moves between years
xlim <- list(
  min = min(data$incidenca) - 20, #s tem podaljsas x os
  max = max(data$incidenca) + 100 #s tem podaljsas x os
) # TU SEM PODALJSALA x-OS, DA NE ODREZE KROGCEV
ylim <- list(
  min = min(data$umrljivost) - 100,
  max = max(data$umrljivost) + 100 #po potrebi spremeni stevilke (da podaljsas y os)
)


# Regije

load("Data/gadm.RData")
gadm$NAME_1 <- c("Gorenjska", "Goriska", "Jugovzhodna Slovenija", "Koroska",
                 "Notranjsko-kraska", "Obalno-kraska", "Osrednjeslovenska",
                 "Podravska", "Pomurska", "Savinjska", "Spodnjeposavska", "Zasavska")
#fix.encoding(gadm)

regije <- c("Gorenjska", "Goriska", "Jugovzhodna Slovenija", "Koroska",
            "Notranjsko-kraska", "Obalno-kraska", "Osrednjeslovenska",
            "Podravska", "Pomurska", "Savinjska", "Spodnjeposavska", "Zasavska")
source("incidencna_stopnja.R")
incidencna_stopnja$Regija <- regije[c(9,8,4,10,12,11,3,7,1,5,2,6)] #prilagodimo vrstni red
dataIncidenca <- incidencna_stopnja

# Umrljivost:
source("umrljivostna_stopnja.R")
umrljivostna_stopnja$Regija <- regije[c(9,8,4,10,12,11,3,7,1,5,2,6)] #prilagodimo vrstni red
dataUmrljivost <- umrljivostna_stopnja

dataPays<-function(data=dataIncidenca) return(data)
jour1<-names(dataIncidenca[2:57])
jour2<-names(dataUmrljivost[2:33])
jourDate1 <- as.numeric(jour1)
jourDate2 <- as.numeric(jour2)
names(dataIncidenca)[2:57]<-jourDate1
names(dataUmrljivost)[2:33]<-jourDate2

dataIncidenca<-left_join(data.frame(Regija = gadm$NAME_1%>%as.character()),dataIncidenca)
#dataIncidenca<-dataIncidenca%>%filter(!is.na(Regija))
dataUmrljivost<-left_join(data.frame(Regija = gadm$NAME_1%>%as.character()),dataUmrljivost)
#dataUmrljivost<-dataUmrljivost%>%filter(!is.na(Regija))
arrondi<- function(x) 10^(ceiling(log10(x))) #za racunanje na populacijo


dashboardSidebar <- dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Rak v Sloveniji", tabName = "uvod", selected = TRUE),
      menuItem("Spol", tabName = "spol"),
      menuItem("Starostne skupine", tabName = "star"),
      menuItem("Regije", tabName = "reg"),
      menuItem("Stadij", tabName = "sta"),
      menuItem("Tip raka po spolu", tabName = "krog")
    )
  )

body <- dashboardBody(
  tabItems(

    tabItem(tabName = "uvod",
            fluidRow(
              h2(align="center", "Rak v Sloveniji")
              
            ),
            fluidRow(
              box(
                width = 12, status = "primary",
                p("Rak (malignom/maligni tumor) je skupina bolezni, za katere je znacilna nenadzorovana celicna delitev in sposobnost teh celic, da se razsirijo v sosednja tkiva po telesu. Vzrokov za nastanek rakavih celic je vec, najpogostejsa pa je genetska mutacija zaradi okoljskih faktorjev ali zivljenjskega stila bolnika. V Sloveniji na Onkoloskem institutu Ljubljana v sklopu Registra raka Republike Slovenije zbirajo podatke o incidenci, prevalenci in prezivetju rakavih bolnikov ze od leta 1950, kar jih na tem podrocju postavlja med vodilne v Evropi. Podatke o umrljivosti za rakom v Sloveniji od leta 1985 zbira Nacionalni institut za javno zdravje. Podatki so javno dostopni na spletni strani Slora. Vsi ti podatki so izhodisce za ocenjevanje uspesnosti onkoloske primarne in sekundarne preventive, diagnostike, zdravljenja, rehabilitacije in paliativne oskrbe ter za nacrtovanje zmogljivosti in sredstev (osebja, medicinske opreme, posteljnih zmogljivosti,...), ki so potrebni za obvladovanje rakavih bolezni na vseh omenjenih podrocjih.")
              )
            ),
            fluidRow(
              box(width = 4, status = "primary",
                  img(src="incidenca.jpg", align = "center", width="100%")),
              box(width = 4, status = "primary",
                  img(src="prezivetje.jpg", align = "center", width="100%")),
              box(width = 4, status = "primary",
                  img(src="umrljivost.jpg", align = "center", width="100%"))
              
            ),
            fluidRow(
              box(
                width = 12, status = "primary",
                p("V obdobju od leta 2011 do 2015 je bilo diagnosticiranih 69.957 novih primerov raka v Sloveniji, umrlo pa je 29.869 bolnikov. Opazovano 3-letno prezivetje je bilo v povprecju 64,2 odstotka.")
              )
            )
    ),
    
    tabItem(tabName = "spol",
            fluidRow(
              h2(align="center", HTML("Incidenca/umrljivost/prevalenca glede na spol (<b style='color:darkblue'>M</b>/<b style='color:deepskyblue'>Z</b>)"))
              
            ),
      fluidRow(
        box(
          status = "primary", width=4,
          selectInput(
     "mode", 
     "Izbira prikazane spremenljivke:",
     c("Incidenca" = "inc",
       "Umrljivost" = "umr",
       "Prevalenca" = "prev")
     ),
     selectInput(
     "type", 
     "Izbira tipa:",
     c("Absolutna" = "abs",
       "Relativna" = "rel")
     )
        ),
   box(
     status = "primary", width=8,
   plotOutput(outputId = "grafSpol")
   )
   
   
   ),
   fluidRow(
     box(
       width = 12, status = "primary",
       p("Incidenca ali pojavnost je absolutno stevilo novih primerov bolezni v dolocenem casovnem intervalu oziroma obdobju na dolocenem obmocju v doloceni populaciji. V nasem primeru smo za obdobje vzeli eno koledarsko leto."),
       p("Prevalenca je stevilo vseh bolnikov z doloceno boleznijo, ki so bili zivi na izbrani datum (ponavadi zadnji dan v letu)."),
       p("Z umrljivostjo izrazimo absolutno stevilo vseh umrlih za doloceno boleznijo v tocno doloceni populaciji v dolocenem obdobju; v nasem primeru v enem koledarskem letu."),
       p("Na zgornjem grafu lahko izbiramo med absolutnim ali relativnim prikazom incidence, umrljivosti ali prevalence glede na spol. Opazimo lahko, da je umrljivost v letih od 1985 do 2016 pri moskih nekoliko vecja kot pri zenskah. Vseh primerov raka je pri zenskah vec, se pa relativni delez med spoloma v zadnjih letih vedno bolj izenacuje. Incidenca med spoloma je bolj izenacena.")
     )
   )
    ),
    
    tabItem(tabName = "star",
            fluidRow(
              h2(align="center", "Incidenca/umrljivost glede na starostne skupine")
              
            ),
      fluidRow(
       box(
         status = "primary", width=4,
       selectInput(
         "starVar", 
         "Izbira prikazane spremenljivke:",
         c("Incidenca" = "inc",
           "Umrljivost" = "umr")
         ),
       sliderInput(
         inputId = "num",
         label = "Leto",
         value = 1985,
         min = 1985,
         max = 2016,
         animate=TRUE,
         sep ="" 
         )
       ),
       box(
         status = "primary", width=8,
         plotOutput(outputId = "grafStar"))
       ),
      fluidRow(
        box(
          width = 12, status = "primary",
          p("Pri stolpicnem grafikonu lahko izbiramo med prikazom absolutnega stevila incidence ali umrljivosti po starostnih skupinah. Vecjo incidenco in umrljivost v najvisjem starostnem razredu v zadnjih letih gre pripisati predvsem visji pricakovani zivljenjski dobi. Ob spremljanju skozi cas moramo upostevati tudi spremembe starostne strukture prebivalstva. 
")
        )
      )
    ),

    
    tabItem(tabName = "krog",
            googleChartsInit(),
            tags$link(
              href=paste0("http://fonts.googleapis.com/css?",
                          "family=Source+Sans+Pro:300,600,300italic"),
              rel="stylesheet", type="text/css"),
            tags$style(type="text/css",
                       "body {font-family: 'Source Sans Pro'}"
            ),
            fluidRow(
              h2(align="center", HTML("Razmerje med incidenco in umrljivostjo ter prevalenca glede na tip raka in spol (<b style='color:darkblue'>M</b>/<b style='color:deepskyblue'>Z</b>)"))
              
            ),
            fluidRow(
              box(
                width = 4, status = "primary",
                sliderInput("leto", "leto", sep="",
                            min = min(data$leto), max = max(data$leto),
                            value = max(data$leto), animate = TRUE)
                
              ),
              box(
                width = 8, status = "primary",
                googleBubbleChart("chart",
                                  width="100%", height = "475px",
                                  # Set the default options for this chart; they can be
                                  # overridden in server.R on a per-update basis. See
                                  # https://developers.google.com/chart/interactive/docs/gallery/bubblechart
                                  # for option documentation.
                                  options = list(
                                    legend="none",
                                    fontName = "Source Sans Pro",
                                    fontSize = 13,
                                    # Set axis labels and ranges
                                    hAxis = list(
                                      title = "Incidenca (absolutno stevilo)",
                                      viewWindow = xlim
                                    ),
                                    vAxis = list(
                                      title = "Umrljivost (absolutno stevilo)",
                                      viewWindow = ylim
                                    ),
                                    # The default padding is a little too spaced out
                                    chartArea = list(
                                      top = 60, left = 75,
                                      height = "75%", width = "75%"
                                    ),
                                    # Allow pan/zoom
                                    # explorer = list(
                                    #   'keepInBounds' = TRUE,
                                    #   'maxZoomIn' = 1,
                                    #   'maxZoomOut' = 1),
                                    # Set bubble visual props
                                    bubble = list(
                                      opacity = 0.4, shape=21, stroke = "none", 
                                      # Hide bubble label
                                      textStyle = list(
                                        color = "none"
                                      )
                                    ),
                                    # Set fonts
                                    titleTextStyle = list(
                                      fontSize = 13
                                    ),
                                    tooltip = list(
                                      textStyle = list(
                                        fontSize = 12
                                        #format = format(big.mark=".", decimal.mark = ",")
                                        # Tu se doloci pisavo izpisa ob pomiku na krogec.
                                      )
                                    )
                                  )
                ))
            ),
            fluidRow(
              box(
                width = 12, status = "primary",
                p("Graf prikazuje razmerje med incidenco in umrljivostjo glede na tip raka, razdeljen po spolu. Velikost krogca ponazarja prevalenco. S pomikom na izbrani krogec se izpisejo podatki o spolu, incidenci, prevalenci, umrljivosti in tipu raka. Skozi celotno opazovano obdobje najvecjo umrljivost pri moskih povzroca rak na sapniku, sapnici in pljucih, pri zenskah pa rak na dojki. Slednji ima pri zenskah najvecjo prevalenco in incidenco skozi vsa leta od 1985 naprej. Pri moskih je v zadnjih letih opaziti vse vecjo incidenco pri raku prostate. Raki drugih malignih neoplazem koze se pogosto pojavljajo pri obeh spolih, umrljivost pa je relativno nizka.")
              )
            )
    ),
    tabItem(tabName = "sta",
            fluidRow(
              h2(align="center", "Stadij raka ob diagnozi")
              
            ),
      fluidRow(
        box(status = "primary",
          width = 12,
          plotOutput(outputId = "grafSta")
        )
        
       
       ),
      fluidRow(
        box(
          width = 12, status = "primary",
          p("Rakave bolezni razvrscamo glede na stadij, s katerim dolocimo lokacijo rakavega tkiva in/ali se je rak razsiril tudi na druge dele telesa. Ocena stadija obolenja mora biti izvrsena pred zacetkom zdravljenja, saj se na podlagi stadija zdravnik pri bolniku odloci za najustreznejso terapijo in deloma predvidi obnasanje raka ter napove, kaksne so moznosti ozdravljenja. Razvrscanje v stadije je pomembno tudi za primerjavo uspesnosti zdravljenja med razlicnimi ustanovami, regijami ali drzavami.")
        )
      )
    ),

    tabItem(tabName = "reg",
            fluidRow(
              h2(align="center", "Groba incidencna/umrljivostna stopnja po regijah")
              
            ),
            fluidRow(   
              box(status = "primary", width=4,  
                  selectInput("choices", "Incidenca ali umrljivost ?", choices = c("Groba incidencna stopnja","Groba umrljivostna stopnja"), selected = "Groba incidencna stopnja"),
                  uiOutput("Slider"),
                  helpText("Podrobnosti za vsako regijo se izpisejo ob kliku na izbrano regijo."), 
                  uiOutput("selection"))  ,
              box(status = "primary", width=8, leafletOutput("map_1", width = "100%", height = "600px")  ) ),
            fluidRow(
              box(
                width = 12, status = "primary",
                p("Groba stopnja je podatek o stevilu novih primerov bolezni ali stevilu umrlih, preracunana na 100.000 oseb opazovane populacije."),
                p("Graf prikazuje grobo incidencno/umrljivostno stopnjo po regijah. Opazimo lahko, da incidencna stopnja narasca hitreje kot umrljivostna. V zadnjih letih izstopa visoka umrljivostna stopnja v Zasavski regiji.")
              )
            )
            
            
  )
  )
)



ui <- dashboardPage(
  dashboardHeader(title = "Register raka"),
  dashboardSidebar,
  body
)


shinyApp(ui = ui, 
         server = function(input, output, session) {
  
  # Stadij
  
  output$grafSta<-renderPlot({
    ggplot(stadij, aes(x=Leto, y=Stevilo, group=Stadij,fill =Stadij)) +
      geom_area()+
      theme_minimal()+
      ylab("stevilo")+
      scale_x_discrete(breaks = c(1961, 1966, 1971, 1976, 1981, 1986, 1991, 1996, 2001, 2006, 2011, 2016))+ 
      scale_fill_viridis(discrete = TRUE, option = "D", direction = -1)
  })
  
  
  
    
    
      data_1 <- reactive({
        if(!is.null(input$starVar)){
          if(input$starVar == "inc"){
            updateSliderInput(session,
                              inputId = "num",
                              value = 2016,
                              min = 1961, 
            )
            return(inc_starost)
            
          }else{
            updateSliderInput(session,
                              inputId = "num",
                              value = 2016,
                              min = 1985, 
            )
            return(umr_starost)
          }}
      })
      
      label_y <- reactive({
        if(!is.null(input$starVar)){
          if(input$starVar == "inc"){
            return("Incidenca")
            
          }else{
            return("Umrljivost")
          }}
      })
      
      observeEvent(input$starVar,{
        print(input$starVar)
      }
      )
      
      output$grafStar<-renderPlot({
        if (input$num < min(as.numeric(colnames(data_1()[,-c(1)])))){
          a = as.character(min(as.numeric(colnames(data_1()[,-c(1)]))))
        }
        else{
          a = as.character(input$num)
        }
        ggplot(data = data_1(), aes(x=data_1()$Starost, y =unlist(data_1()[a])))+ 
          geom_col(fill='#2c7fb8') +
          xlab("Starostna skupina") +
          ylab(label_y()) +
          ylim(0,max(data_1()[,-c(1)])) +
          coord_flip()+ 
          theme_minimal()
      })
      
      #Spol
      podatki <- reactive({
        if(!is.null(input$mode)){
          if(input$mode == "inc"){
            if(!is.null(input$type)){
              if(input$type == "abs"){
                print("incabs")
                return(ia)
              }else{
                print("increl")
                return(ir)
              }
            }
          }else if(input$mode == "umr"){
            if(!is.null(input$type)){
              if(input$type == "abs"){
                print("umrabs")
                return(ua)
              }else{
                print("umrrel")
                return(ur)
              }
            }
          }else{
            if(!is.null(input$type)){
              if(input$type == "abs"){
                print("prevabs")
                return(pa)
              }else{
                print("prevrel")
                return(pr)
              }
            }
          }
        }
      })
      
      output$grafSpol<-renderPlot({
        podatki()
      })
      
      
    
# Zemljevid
      dataPays<- reactive({
        if(!is.null(input$choices)){
          if(input$choices == "Groba incidencna stopnja"){
            return(dataIncidenca)
            
          }else{
            return(dataUmrljivost)
          }}
      })
      
      jourDate<- reactive({
        if(!is.null(input$choices)){
          if(input$choices == "Groba incidencna stopnja"){
            return(jourDate1)
            
          }else{
            return(jourDate2)
          }}
        else{
          return(jourDate1)
        }
      })
      
      #jour <- reactive({return(names(dataPays())[-c(1)])})
      #jourDate <- reactive(as.numeric(jour()))
      
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
        casesDeath<- ifelse(input$choices == "Groba incidencna stopnja","Groba incidencna stopnja","Groba umrljivostna stopnja")
        
        if (!is.null(input$leto2)) {
          if (as.numeric(input$leto2[1]) < min(jourDate())){
            indicator2<-as.character(c(max(jourDate()),max(jourDate())))
          }
          else{
            indicator2<-as.character(c(input$leto2,input$leto2)) #vrne dve letnici
          }
        }else{
          indicator2 = as.character(c(min(jourDate()),max(jourDate())))
        }
        
        variable<- input$variable
        dataPaysSel<-dataPays()%>%select(Regija,)
        # Naredili bomo nov stolpec ncases
        st.let = as.numeric(indicator2[2])-as.numeric(indicator2[1])+1
        #vektor = (as.numeric(indicator2[1])):as.numeric(indicator2[2])
        #vsota = apply(dataPays()[,as.character(vektor)],1,sum)
        vsota=0
        for (k in ((as.numeric(indicator2[1])):as.numeric(indicator2[2]))){
          vsota = vsota + dataPays()[,as.character(k)]}
        
        dataPaysSel$ncases<-round(vsota/st.let,1)
        
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
      }
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
        
        sliderInput("leto2", "Leto ali obdobje", min(jourDate()), max(jourDate()), sep="",
                    ticks = TRUE,
                    value =  c(max(jourDate()),max(jourDate())),animate = T, step = 1
                    #kje zacnemo, ko odpremo aplikacijo
        )
      })
  
  #krogci
  defaultColors <- c("darkblue", "deepskyblue")
  series <- structure(
    lapply(defaultColors, function(color) { list(color=color) }),
    names = levels(data$spol)
  )
  
  yearData <- reactive({
    # Filter to the desired year, and put the columns
    # in the order that Google's Bubble Chart expects
    # them (name, x, y, color, size). Also sort by region
    # so that Google Charts orders and colors the regions
    # consistently.
    df <- data %>%
      filter(leto == input$leto) %>%
      select(
        tip_raka, 
        incidenca, 
        umrljivost,
        spol, 
        prevalenca
        ) %>%
      arrange(spol)
    #df <- as.numeric(format(df, big.mark='.', decimal.mark = ",")) 
  })
  #format(yearData()%>%select_if(is.numeric), big.mark='.', decimal.mark = ",")
  # observe({
  #   updateNumericInput(session, "inp1", #label = ("Total"),
  #                      paste(value = prettyNum(input$inp1, big.mark=".", scientific=FALSE)))
  # })
  
  output$chart <- reactive({
    # Return the data and options
    list(
      data = googleDataTable(yearData()),
      options = list(
        # title = sprintf(
        #   "Izpis podatkov je mozen s pomikom miske na krogec.",
        #   input$leto),
        series = series
      )
    )
  })
  
  isolate({updateTabItems(session, "tabs", "uvod")})
  
  
    
})


