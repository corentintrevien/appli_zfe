library(data.table)
library(sf)
library(htmltools)
library(rsconnect)
library(mapview)
library(leaflet)
library(Rfast)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyBS)
library(scales)
library(ggplot2)
library(stringr)
library(plyr)
library(plotly)
library(RColorBrewer)
library(shinyjs)
library(writexl)
library(utils)
library(R.utils)

options(scipen=15)
#setwd("/Users/corentintrevien/Dropbox/Travail/ZFE")
#current_directory <- dirname(rstudioapi::getSourceEditorContext()$path)
#setwd(current_directory)

#Chargement des données
map_epci <- st_read("Map_EPCI/map_epci.shp")
contour_zfe <- st_read("Map_ZFE/contour_zfe.shp")
map_commune <- st_read("Map_com/map_commune.shp")
data_zfe <- read.csv("Data/data_zfe.csv",encoding = "UTF-8") 
epci_zfe <- read.csv("Data/epci_zfe.csv",encoding = "UTF-8") 
parc <- read.csv("Data/data_parc.csv",colClasses = c("codgeo"="character"),encoding = "UTF-8")

#Couleur associée à chaque vignette Crit'Air
parc$vignette <- factor(parc$vignette, levels = c("Crit'Air E","Crit'Air 1","Crit'Air 2","Crit'Air 3","Crit'Air 4","Crit'Air 5","Non classés ou inconnus"))
color_critair<-c("Crit'Air E"="#009650","Crit'Air 1"="#794685","Crit'Air 2"="#edbe31","Crit'Air 3"="#de8235",
                 "Crit'Air 4"="#462021","Crit'Air 5"="#4f4b48","Non classés ou inconnus"="lightgrey")

#Liste des ZFE, rangées par ordre alphabétique
list_zfe <- data_zfe$codzfe
names(list_zfe) <- data_zfe$libzfe
list_zfe <- list_zfe[!duplicated(list_zfe)]
list_zfe <- list_zfe[order(names(list_zfe))]
  
#Liste des vignettes Crit'Air
list_vignette <- levels(parc$vignette)
list_critair <- list("Crit'Air E"="e","Crit'Air 1"="1","Crit'Air 2"="2","Crit'Air 3"="3",
                   "Crit'Air 4"="4","Crit'Air 5"="5","Non classés ou inconnus"="n")

#Liste des véhicules
list_veh <- c('Véhicule particulier'="VP",'Véhicule utilitaire léger'="VUL")

#Programmes annexes
#Fonctions (texte "retractable" pour la barre latérale droite et mise en forme des pourcentages pour la carte)
source("App_fonctions.R")
#Permet l'utilisation des ShinyWidgets (http://shinyapps.dreamrs.fr/shinyWidgets/)
source("App_widgets.R")
#Page de style CSS
source("App_CSS.R")
#Mentions légales de l'application
source("App_mentions_legales.R")
#Contenus des menus définitions, méthodes et données
source("App_definitions.R",encoding = "UTF-8")

#####################
#####APPLICATION#####
#####################

#Items défintions et méthodes chargés partir du fichier App_definitions.R
critcontenu_char <- lapply(critcontenu,as.character)
itemlabel <- list("Méthodologie et données","Définitions")
itemcontenu <- list(methode,CleverItemsUi("defin",deflabel,defcontenu))

#Larre latérale gauche
ui_sidebar <- sidebarPanel(
                          #Affichage hors mentions légales 
                          conditionalPanel(condition = "input.onglets != 'Mentions'",
                                           #Définitions et données
                                            div(class="form-group",CleverItemsUi("methdef",itemlabel,itemcontenu)),
                                            hr(),
                                           #Menu déroulant de choix de la ZFE
                                            selectInput(inputId = 'selected_zfe',
                                       label='Zone à faibles émissions',
                                       choices = list_zfe,selected="ZFE010"),
                          #awesomeCheckboxGroup "Personalisés"
                          #Choix du type de véhicule
                          attachShinyWidgetsDep(HTML('<div id="selected_vehicule" class="form-group shiny-input-container shiny-input-checkboxgroup awesome-bootstrap-checkbox">
                            <label for="selected_vehicule" style="margin-bottom: 10px;">Type de véhicule</label>
                            <div class="shiny-options-group">
                            <div class="awesome-checkbox checkbox-primary">
                              <input type="checkbox" name="selected_vehicule" value="VP" id="selected_vehiculeVP" checked="checked"/>
                              <label for="selected_vehiculeVP"><a id="vehinfoVP" class="action-button">
                                Véhicule particulier&nbsp;&nbsp;&nbsp;<i class="fa fa-info-circle"></i>
                              </a></label>
                            </div>
                            <div class="awesome-checkbox checkbox-primary">
                              <input type="checkbox" name="selected_vehicule" value="VUL" id="selected_vehiculeVUL"/>
                              <label for="selected_vehiculeVUL"><a id="vehinfoVUL" class="action-button">
                                Véhicule utilitaire léger&nbsp;&nbsp;&nbsp;<i class="fa fa-info-circle"></i>
                              </a></label>
                            </div>
                            </div>
                            </div>'),"awesome"),
                          #Choix de la vignette
                           attachShinyWidgetsDep(HTML(
                             "<div id='selected_vignette' class='form-group shiny-input-container shiny-input-checkboxgroup awesome-bootstrap-checkbox'>
            <label for='selected_vignette' style='margin-bottom: 10px;'>Vignette Crit&#39;Air</label>
            <div class='shiny-options-group'>
              <div class='awesome-checkbox checkbox-primary'>
                <input type='checkbox' name='selected_vignette' value='Crit&#39;Air E' id='selected_vignettee'/>
                <label for='selected_vignettee'> 
                  <a id='critinfoe' class='action-button'>Crit'Air E&nbsp;&nbsp;&nbsp;<i class='fa fa-info-circle' ></i></a>
              </label></div>
              <div class='awesome-checkbox checkbox-primary' style='display:inline-block;'>
                <input type='checkbox' name='selected_vignette' value='Crit&#39;Air 1' id='selected_vignette1' checked='checked'/>
                <label for='selected_vignette1'>
                  <a id='critinfo1' class='action-button'>Crit'Air 1&nbsp;&nbsp;&nbsp;<i class='fa fa-info-circle' ></i></a>
              </label></div>
              <div class='awesome-checkbox checkbox-primary'>
                <input type='checkbox' name='selected_vignette' value='Crit&#39;Air 2' id='selected_vignette2'/>
                <label for='selected_vignette2'>
                <a id='critinfo2' class='action-button'>Crit'Air 2&nbsp;&nbsp;&nbsp;<i class='fa fa-info-circle' ></i></a>
              </label></div>
              <div class='awesome-checkbox checkbox-primary'>
                <input type='checkbox' name='selected_vignette' value='Crit&#39;Air 3' id='selected_vignette3'/>
                <label for='selected_vignette3'>
                  <a id='critinfo3' class='action-button'>Crit'Air 3&nbsp;&nbsp;&nbsp;<i class='fa fa-info-circle' ></i></a>
              </label></div>
              <div class='awesome-checkbox checkbox-primary'>
                <input type='checkbox' name='selected_vignette' value='Crit&#39;Air 4' id='selected_vignette4'/>
                <label for='selected_vignette4'>
                  <a id='critinfo4' class='action-button'>Crit'Air 4&nbsp;&nbsp;&nbsp;<i class='fa fa-info-circle' ></i></a>
              </label></div>
              <div class='awesome-checkbox checkbox-primary'>
                <input type='checkbox' name='selected_vignette' value='Crit&#39;Air 5' id='selected_vignette5'/>
                <label for='selected_vignette5'>
                <a id='critinfo5' class='action-button'>Crit'Air 5&nbsp;&nbsp;&nbsp;<i class='fa fa-info-circle' ></i></a>
             </label> </div>
              <div class='awesome-checkbox checkbox-primary'>
                <input type='checkbox' name='selected_vignette' value='Non classés ou inconnus' id='selected_vignetten'/>
                <label for='selected_vignetten'>Non classés ou inconnus</label>
              </div>
            </div>
          </div>"),"awesome"),
                          #Bouton de téléchargement
            downloadButton("downloadData", "Télécharger les données de ma sélection")),
            #Rétractation de la barre lattérale en cas d'affichage des mentions légales 
            conditionalPanel(condition = "input.onglets == 'Mentions'",
                             bsButton("retourapp","Retour à l'application")))

#Module d'affichage du graphique 
tab_graphique <- list(h3(textOutput("title_graph")),
                           h5(textOutput("etat_zfe")),
                           plotlyOutput("barplot1"),
                           #h6(style="display:inline-block;",htmlOutput("note_graph"), style="font-size: 100%"),
                           uiOutput("note_graph"),
                           p("Source : RSVERO, 2021, SDES"))

#Module d'affichage de la carte
tab_carte <- list(h3(textOutput("title_map")),
                  leafletOutput("mapzfe"),
                  br(),
                  p("Note : données à l'échelle communale à l'intérieur de l'intercommunalité concernée par la ZFE et à l'échelle intercommunale à l'extérieur"),
                  p("Source : RSVERO, 2021, SDES"))

#Main Panel
ui_mainpanel <- mainPanel(tabsetPanel(type = "tabs",id = "onglets",
                                      tabPanel("Graphique",tab_graphique),
                                      tabPanel("Carte",tab_carte)))


#UI (affichage)
ui <- fluidPage(theme = shinytheme("flatly"),useShinyjs(), css_style,
                #Titre et contenu du titre
                fluidRow(style="margin-bottom:10px",
                         #Ajout d'images
                  column(2, img(src = "SDES.png",height=90)), 
            column(9, titlePanel(HTML("Parc automobile des zones à faibles émissions 2021<div class='submaintitle'>Application de visualisation des données</div>"))), 
            column(1, img(src = "logo-ssp.jpg",height=90,class = "pull-right"))),

                          sidebarLayout(ui_sidebar,ui_mainpanel),
            #Bas de page 
            fluidRow(style='background-color: #dce4e6;padding-top:10px;padding-bottom:10px;margin-bottom:10px;margin-top:15px',
                     column(4,div(style='padding-left:15px;',
                       actionLink("gotoml",HTML("<strong>Mentions légales</strong>")))),
                     column(4, HTML("<strong> Service des données et études statistiques (SDES)</strong>
                     <i><br> Commissariat général au développement durable </i>")),
                     #Lien vers le site du SDES
                     column(4,
                            tags$a(href="https://www.statistiques.developpement-durable.gouv.fr/",
                                   target="_blank",
                                   tags$b("www.statistiques.developpement-durable.gouv.fr")))))
 

# column(4,div(style='margin-bottom:10px;padding:15px;background-color: #ecf0f1;border: 1px solid transparent;border-radius: 4px;',
#              ,
#              hr(),
#             
#             Site web : <a href='https://www.statistiques.developpement-durable.gouv.fr/' target = '_blank'> www.statistiques.developpement-durable.gouv.fr</a> ")))


#################
#####SERVEUR#####
#################

server <- function(input, output, session){
  
    #########Gestion des mentions légales###########
    observeEvent(input$onglets, {
      #Cacher les définitions
      for(item in c(paste0("defin",1:4),paste0("methdef",1:2))){
        hide(paste0(item,"-contents"))
        removeClass(item,"active-item")}
      
      #Retirer l'onglet "fantôme"
      if(input$onglets %in% c("Graphique","Carte")){
        removeTab(inputId = "onglets", target = "Mentions")
        #removeClass("col-sm-4","mentions-legales-sidebar")
      }
    })
  
  #Afficher les onglets carte et graphique quand on quitte les mentions légales 
    observeEvent(input$retourapp, {
      insertTab(inputId = "onglets",tabPanel("Graphique", tab_graphique),target = "Mentions")
      insertTab(inputId = "onglets",tabPanel("Carte", tab_carte),target = "Mentions")
      removeTab(inputId = "onglets", target = "Mentions")
    })
  
    #Cacher les onglets carte et graphique pour afficher les mentions légales
    observeEvent(input$gotoml, {
      insertTab(inputId = "onglets",
                tabPanel("Mentions", mentions_legales),
                target = "Carte",position = "after",select=T)
      #removeTab(inputId = "onglets", target = "Carte")
      removeTab(inputId = "onglets", target = "Graphique")
      removeTab(inputId = "onglets", target = "Carte")
    })
 
    ######Gestion de Items Définition (permet de rétracter le texte)######
    CleverItemsServer("defin",2)
    CleverItemsServer("methdef",2)

    
    #####Correction de la catégorie de véhicule quand rien n'est sélectionné#####
    input_selected_vehicule<- reactive({
        if(is.null(input$selected_vehicule)){c('VP','VUL')}else{input$selected_vehicule}
    })
      
    ######Libélé de catégorie Crit'Air######
    lib_selected_vignette <- reactive({
        
      vignette <- input$selected_vignette
    
      #Simplification
      vignette <- gsub("Non classés ou inconnus","non classés",vignette)
    
      #Suppression de la répétition "Crit'Air"
      if(length(vignette)>=2){
        vignette[2:length(vignette)] <- gsub("Crit'Air ","",vignette[2:length(vignette)])
      }
      #Virgules de séparation
      if(length(vignette)>=2){
        vignette <- c(paste(vignette[1:(length(vignette)-1)],collapse = ", "),vignette[length(vignette)])
      }
      #Et de séparation
      if(length(vignette)==2){
        vignette <- paste(vignette,collapse = " et ")
      }
      #Ajout du terme classé
      if(vignette!="non classés"){
        vignette <- paste("classés",vignette)
      }
    
      list("singulier"=gsub("et","ou",gsub("classés","classé",vignette)),"pluriel"=vignette)
    })
    
    ########Libélé des catégorie de véhicules#######
    lib_selected_vehicule <- reactive({
      catveh <- ifelse(length(input_selected_vehicule())>1,
                       "Véhicules particuliers et utilitaires légers",
                       ifelse(input_selected_vehicule()=="VUL","Véhicules utilitaires légers","Véhicules particuliers"))
      list("singulier"=gsub("et","ou",gsub("s","",catveh)),"pluriel"=catveh)
    })
    
    
    ######Libélés des EPCI voisines (POPUP en bas du graphique)#######
    output$note_graph <- renderUI({
      epci_zvo_lib <- epci_zfe[epci_zfe[,input$selected_zfe]==1,]$libgeo
      epci_zvo_lib <- paste(epci_zvo_lib,collapse = " ; ")
      epci_zvo_lib <- gsub("'","\\\\'",epci_zvo_lib)
    
      note_graph <- p("Note : les intercommunalités voisines sont celles situées à moins de 20 kilomètres de la ZFE  ",
         popify(actionLink("pB3",label="", icon=icon("info-circle")),
                title = "<b> Liste des intercommunalités voisines </b>",content= epci_zvo_lib,placement="top"))
      
      #Liste des intercommunalités de la vallée de l'Arve
      if(input$selected_zfe=="ZFEARV"){
        epci_zfe_lib <- paste(data_zfe[data_zfe$codzfe == input$selected_zfe,]$libgeo,collapse = " ; ")
        epci_zfe_lib <- p(paste0("Intercommunalités de la vallée de l'Arve : ",epci_zfe_lib))
        note_graph <- list(epci_zfe_lib,note_graph)
      }
      note_graph
     })
    
    #######Gestion des checkboxs véhicule et vignette selon l'onglet affiché (carte ou graphique)######
    observeEvent(input$onglets, {
       if(input$onglets=="Carte"){
         shinyjs::enable("selected_vignette")
       }
       if(input$onglets=="Graphique"){
         shinyjs::disable("selected_vignette")
       }
     })
    
     ####Popup de défintion des différentes vignettes Crit'Air####
     lapply(1:6, function(i){addPopover(session,id=paste0("critinfo",list_critair[[i]]),
                #title=paste0("<b>Véhicules classés ",names(list_critair)[[i]],"</b>"),
                title=NULL,
                content=critcontenu_char[[i]],placement = "right",trigger="hover")})
    
    ####Popup de défintion des différents véhicules####
    lapply(c("VP","VUL"), function(v){addPopover(session,id=paste0("vehinfo",v),title=NULL,
                                                 content=defveh[[v]] ,placement = "right",trigger="hover")})
    
    
    #####Etat d'avancement de la ZFE (texte)#######
    output$etat_zfe <- renderText({
      #Affichage seulement si une catégorie de véhicule a été sélectionnée
      etat <- data_zfe[data_zfe$codzfe==input$selected_zfe,c("libzfed","EtatZFEd")]
      etat <- unique(etat)
      etat <- paste(etat,collapse = " ")
      if(input$selected_zfe == "ZFEARV"){etat <- gsub("envisage","envisagent",etat)}
      etat
    })
    
    ######Titre du graphique#####
    output$title_graph <- renderText({
      paste(lib_selected_vehicule()["pluriel"],"par vignette Crit'Air")
    })
    
    #####Titre de la carte#####
    output$title_map <- renderText({
      if(length(input$selected_vignette)>0){
        paste(lib_selected_vehicule()["pluriel"],lib_selected_vignette()["pluriel"])
      }else{"Aucune vignette Crit'Air selectionnée"}
    })
    
    ######Préparation des données du graphique selon la sélection######
    dataplot <- reactive({
      #Sélection des données du graphique
      epci_selected <- epci_zfe[epci_zfe[,input$selected_zfe]>0,]
    
      parc_graph <- parc[parc$codgeo %in% c(epci_selected$codgeo,"FRANCE") & parc$vehicule %in% input_selected_vehicule(),]
      parc_graph$zfe <- epci_selected[match(parc_graph$codgeo,epci_selected$codgeo),input$selected_zfe]
      parc_graph$zfe <- ifelse(parc_graph$codgeo=="FRANCE",0,parc_graph$zfe)
    
      #Agregation
      dataplot <- data.table(aggregate(data=parc_graph,value~zfe+vignette,FUN=sum))
      dataplot$zfe <- factor(dataplot$zfe,levels = c("2","1","0"))
      dataplot
    })
   
   ######Graphique######
    output$barplot1 <-renderPlotly({
      parc_graph <- dataplot()
  
      #Indicateurs
      parc_graph[,sum:=sum(value),by=zfe]
      parc_graph[,pct:=value/sum,by=zfe]
      parc_graph$part <- label_percent(accuracy = 0.1, suffix = " %",decimal.mark = ",")(parc_graph$pct)
      parc_graph$nbveh <- trimws(format(round(parc_graph$value),big.mark=" "))
  
      #Création d'un nouveau label pour les abcisses (en ordre)
      parc_graph$new_label <- as.character(revalue(parc_graph$zfe,
                                                   c("2"="<b>Intercommunalité <br>concernée par la ZFE</b>",
                                                     "1"="<b>Intercommunalités <br>voisines</b>",
                                                     "0"="<b>France <br></b>")))
      parc_graph$new_label <- paste0(parc_graph$new_label,"<br><br><i>",trimws(format(round(parc_graph$sum), big.mark=" ")),
                                     " véhicules</i>")
      new_label <- parc_graph$new_label
      names(new_label) <- parc_graph$zfe
      new_label <- new_label[!duplicated(new_label)]
      parc_graph$zfe <- revalue(parc_graph$zfe,new_label)
  
      #Graphique
      bars <- ggplot(parc_graph, aes(x = zfe, y = pct,
                     text = paste(vignette,'<br>Pourcentage :', part,'<br>Nombre de véhicules :', nbveh)))+
        scale_y_continuous(labels=scales::percent_format(suffix = " %"))+
        geom_col(aes(fill = vignette),width = 0.5,show.legend = FALSE)+
        scale_fill_manual(values=color_critair)+
        theme_minimal() +
        theme(axis.title=element_blank(),
              axis.ticks=element_blank(),
              legend.position='none',
              axis.text.x=element_text(colour="#2c3e50",size=10.5))
  
      #Interactivité du graphique
      ggplotly(bars,tooltip = "text")%>%
        style(hoverlabel = list(bordercolor = "white",  font = list(color = "white",family = "Lato",size=13))) %>%
        config(displayModeBar = FALSE) %>%
        layout(xaxis=list(fixedrange=TRUE),font = list(family = "Lato"),yaxis=list(fixedrange=TRUE))
      })
  
    #Polygonnes des communes et des EPCI affichés sur la carte
    polyzfe <- reactive({
      #Sélection des EPCI
      codgeo_epci_zfe <- epci_zfe[epci_zfe[,input$selected_zfe]==2,]$codgeo
      codgeo_epci_zvo <- epci_zfe[epci_zfe[,input$selected_zfe]==1,]$codgeo
      polyzfe_epci <- map_epci[map_epci$codgeo %in% c(codgeo_epci_zfe,codgeo_epci_zvo,data_zfe$codgeo),c("codgeo","libgeo","typgeo")]
  
      polyzfe_epci[polyzfe_epci$codgeo %in% data_zfe$codgeo,"zfe"] <- 3
      polyzfe_epci[polyzfe_epci$codgeo %in% codgeo_epci_zvo,"zfe"] <- 1
      polyzfe_epci[polyzfe_epci$codgeo %in% codgeo_epci_zfe,"zfe"] <- 2
  
      #Communes
      polyzfe_com <- map_commune[map_commune$codzfe==input$selected_zfe,c("codgeo","libgeo","typgeo")]
      polyzfe_com$zfe <- 2
  
      polyzfe <- rbind(polyzfe_epci,polyzfe_com)
  
      polyzfe
    })
  
    ######Données pour la carte######
    datamap <- reactive({
      datamap <- parc[parc$vehicule %in% input_selected_vehicule() & parc$codgeo %in% polyzfe()$codgeo,]
      datamap <- data.table(aggregate(data=datamap,value~codgeo+vignette,FUN=sum))
      datamap[,total:=sum(value),by=codgeo]
      datamap$percent <- with(datamap,value/total*100)
      datamap
  
      if(!is.null(input$selected_vignette)){
        datamap <- datamap[datamap$vignette %in% input$selected_vignette,]
        datamap <- data.table(aggregate(data=datamap,cbind(value,percent)~codgeo+total,FUN=sum))
      }
  
    })
  
    #Téléchargement des données correspondant à la séletion
    output$downloadData <- downloadHandler(
  
      filename = function() {"data-app-zfe.xlsx"},
      content = function(file) {
        if(input$onglets=="Carte"){
          data <- datamap()
          data$percent <- round(data$percent,digits=1)
          data$value <- round(data$value)
          data$total <- round(data$total)
          data$libgeo <- map_commune[match(data$codgeo,map_commune$codgeo),]$libgeo
          data$libgeo_epci <- epci_zfe[match(data$codgeo,epci_zfe$codgeo),]$libgeo
          data$`Territoire` <- ifelse(is.na(data$libgeo),data$libgeo_epci,data$libgeo)
          data[,paste("Nombre total de",tolower(lib_selected_vehicule()["pluriel"]))] = data$total
          data[,paste(lib_selected_vehicule()["pluriel"],lib_selected_vignette()["pluriel"])] = data$value
          data[,paste("Part des",tolower(lib_selected_vehicule ()["pluriel"]),lib_selected_vignette()["pluriel"])] = data$percent
          data <- subset(data,select=-c(libgeo,libgeo_epci,codgeo,total,value,percent))
        }
        if(input$onglets=="Graphique"){
          data <- dataplot()
          data <- data[,c("zfe","vignette","value")]
          data[data$zfe=="2","zfe"] <- names(list_zfe)[which(list_zfe==input$selected_zfe)]
          data[data$zfe=="1","zfe"] <- "Intercommunalités voisines"
          data[data$zfe=="0","zfe"] <- "France"
          data$value <- round(data$value)
          data <- plyr::rename(data,replace=c("vignette"="Classement","zfe"="Territoire",
                                              "value"=paste("Nombre total de",tolower(lib_selected_vehicule()["pluriel"]))))
        }
        write_xlsx(list("Données ZFE"=data), path = file)
      }
    )
    
    ######Pas de téléchargement quand aucune vignette n'est sélectionnée#######
    observe({
      if(is.null(input$selected_vignette)  & input$onglets=="Carte"){disable("downloadData")}
      else{enable("downloadData")}
    })
    
    ######Carte######
    output$mapzfe <- renderLeaflet({
    
      polyzfe <- polyzfe()
      datamap <- datamap()
    
      mz <- leaflet()
      mz <- addTiles(mz)
    
      if(!is.null(input$selected_vignette)){
    
        #Windsorisation des valeurs pour l'affichage des couleurs (permet d'avoir un affichage plus harmonieux)
        p10_90 <- quantile(datamap$percent,probs = c(1,9)/10, na.rm=T)
        datamap$windpct <- ifelse(datamap$percent<p10_90[1],p10_90[1],datamap$percent)
        datamap$windpct <- ifelse(datamap$windpct>p10_90[2],p10_90[2],datamap$windpct)
    
        #Ajout des données aux polygons
        polyzfe[,c("value","percent","windpct")] <- datamap[match(polyzfe$codgeo,datamap$codgeo),c("value","percent","windpct")]
        #Suppression des données pour l'EPCI ZFE (pour laisser la place aux données communales)
        polyzfe[polyzfe$typgeo=="epci"&polyzfe$zfe==2,"windpct"] <- NA
    
        #Bulle de texte
        #Passage à la ligne si le texte est trop long
        bullebr <- ifelse(nchar(lib_selected_vehicule()["pluriel"])>=30,"<br>","")
        polyzfe$bulle <- with(polyzfe,paste0("<strong>",libgeo,'</strong><br>',
                                             ifelse(is.na(value),paste("Pas de",tolower(lib_selected_vehicule()["singulier"]),lib_selected_vignette()["singulier"]),
                                                    paste(trimws(format(round(value), big.mark=" "))," ",
                                                          ifelse(round(value)>=2,
                                                                 #Au pluriel
                                                                 paste(tolower(lib_selected_vehicule()["pluriel"]),bullebr,lib_selected_vignette()["pluriel"]),
                                                                 paste(tolower(lib_selected_vehicule()["singulier"]),bullebr,lib_selected_vignette()["singulier"])),
                                                          "<br><i>",trimws(format(round(percent,digits = 1), decimal.mark=","))," % du parc</i>"))))
        #Remplacement des valeurs manquantzs

        #Palette de couleur
        color_map <- ifelse(length(input$selected_vignette)==1 & input$selected_vignette != "Non classés ou inconnus",
                             color_critair[input$selected_vignette],"#0080ff")
        color_map <- unique(color_map)
        pal_colors <- colorNumeric(c("white",unique(color_map)),p10_90,na.color = "#00000000")
    
        #Ajout des polygons
        mz <- addPolygons(mz,data=polyzfe,layerId= ~codgeo,
                           color="white",  fillColor= ~pal_colors(windpct),
                           opacity = 0.8, fillOpacity = 0.8,weight = 1.5, smoothFactor = 1,
                           highlightOptions = highlightOptions(color = "white", weight = 2,fillOpacity = 0.5,opacity=1),
                           label=lapply(polyzfe$bulle,HTML),labelOptions = labelOptions(textsize = "12px",style = list(
                             "color" = "#2c3e50","font-family"="Lato","font"="Lato",'padding'="6px",'border'='0px')))
    
        #Ajout d'une échelle de couleur, sauf si toutes les vignettes sont sélectionnées
        if(length(input$selected_vignette)<7){
           mz <- addLegend(mz,"topright",pal =  pal_colors,values = p10_90,opacity = 0.8,title = NULL,
                           labFormat = labelFormat_decimal(decimal.mark = ",",prefix="",suffix=" %",big.mark=" "))
        }
      }
    
      #Contour des ZFE
      mz <- addPolylines(mz,data=contour_zfe,color="red",opacity = 0.8,weight = 2,layerId= ~codzfe,
                         label=lapply(paste("<strong>",contour_zfe$libzfe,'</strong><br>',contour_zfe$EtatZFEd),HTML),
                         highlightOptions = highlightOptions(weight = 3),labelOptions = labelOptions(textsize = "12px",style = list(
                           "color" = "#2c3e50","font-family"="Lato","font"="Lato",'padding'="6px",'border'='0px')))
    
      square <- paste0("white", "; width:", "30", "px; height:", "30", "px; border:2px solid ", "red", "; border-radius:", "square")
    
      #Légende des contours ZFE
      mz <- addLegend(mz,"bottomright",colors = square, title = "<text style='font-size: 15px'> Intercommunalités <br>concernées par <br>une ZFE </text> " ,
                      labels = "",opacity = 1)
    
      #Centrage de la carte sur la ZFE sélectionnée
      bbox <- st_bbox(polyzfe[polyzfe$zfe != 3,])
      mz <- fitBounds(mz,bbox[[1]],bbox[[2]],bbox[[3]],bbox[[4]])
    
      mz
    })
    
    ########Changement de ZFE en cliquant sur la carte#######
    observe({
       event <- input$mapzfe_shape_click
       if(!is.null(event$id)){
         codzfe <- NULL
         if(nchar(event$id)==6){codzfe <- event$id}
         if(nchar(event$id)==9){codzfe <- data_zfe[data_zfe$codgeo==event$id,]$codzfe}
    
       if(length(codzfe)>0){
         output$test_observer <- renderText(codzfe)
         updateSelectInput(session, 'selected_zfe', label='Zone à faibles émissions',choices = list_zfe, selected = codzfe)
       }}})
}

shinyApp(ui, server)




