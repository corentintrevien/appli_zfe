library(readxl)
library(writexl)
library(data.table)
library(plyr)
library(sf)
library(archive)
library(Rfast)
library(stringr)
library(purrr)
library(curl)

options(encoding = 'UTF-8')
options(scipen=15)
options(timeout=1800)


#Emplacement du fichier de travail 
current_directory <- dirname(rstudioapi::getSourceEditorContext()$path)
#Espace de travail 
setwd(current_directory)

#Liste des types de véhicules
list_vehicule <- c("vp","vul","pl","tcp")

#Chemin d'accès aux fichiers IGN ADMIN EXPRESS
path_map <- "ADMIN-EXPRESS-COG_3-0__SHP__FRA_2021-05-19/ADMIN-EXPRESS-COG/1_DONNEES_LIVRAISON_2021-05-19/ADECOG_3-0_SHP_LAMB93_FR/"

################################
###TELECHARGEMENT DES DONNEES###
################################
#Création de dossiers pour stocker les données 
dir.create(paste0(current_directory,"/IGN"))
dir.create(paste0(current_directory,"/SDES"))


#Téléchargement et décompression des données de parc de véhicules 2021

for(vehicule in list_vehicule){
  print(vehicule)
  url <- paste0('https://www.statistiques.developpement-durable.gouv.fr/sites/default/files/2021-08/parc_',vehicule,'_communes.zip')
  #Téléchargement du fichier
  download.file(url,paste0("SDES/parc_",vehicule,"_communes.zip"), mode="wb")
  #Décompression du fichier 2021
  unzip(paste0("SDES/parc_",vehicule,"_communes.zip"),file= paste0("Parc_",toupper(vehicule),"_Communes_2021.xlsx"),exdir="SDES")
}
 
#Téléchargement des cartes des communes sur le site de l'IGN
curl_download("ftp://Admin_Express_ext:Dahnoh0eigheeFok@ftp3.ign.fr/ADMIN-EXPRESS-COG_3-0__SHP__FRA_L93_2021-05-19.7z",
              "IGN/ADMIN-EXPRESS-COG_3-0__SHP__FRA_L93_2021-05-19.7z", mode="wb")

#Contenu de l'archive
archive_content <- archive("IGN/ADMIN-EXPRESS-COG_3-0__SHP__FRA_L93_2021-05-19.7z")
#Emplacement des cartes communes et EPCI dans l'archive
files_map <- c("cpg","dbf","prj","shp","shx")
files_map <- c(paste("COMMUNE",files_map,sep="."),paste("EPCI",files_map,sep="."))
files_map <- paste0(path_map,files_map)
#Extraction des fichiers 
archive_extract("IGN/ADMIN-EXPRESS-COG_3-0__SHP__FRA_L93_2021-05-19.7z",
                dir="IGN",
                files = files_map)

#Lecture de l'ensemble des fichiers de parc
parc_veh <- lapply(list_vehicule,function(vehicule){
  print(vehicule)
  #Lecture du fichier excel
  parc_zfe <- read_xlsx(paste0("SDES/Parc_",toupper(vehicule),"_Communes_2021.xlsx"))
  #Nouveaux noms de variables
  parc_zfe <- plyr::rename(parc_zfe,c("Code Epci"="codepci","Code commune"="depcom","Libellé EPCI"="libepci",
                                "Parc au 01/01/2021"="parc2021","Vignette Crit'air"="critair",
                                "Energie"="energie","Libellé commune"="libcom"))
  #Selection des variables d'intéret 
  parc_zfe <- subset(parc_zfe,select = c("codepci","depcom","parc2021","critair","energie","libcom","libepci"))
  parc_zfe$vehicule <- vehicule
  return(parc_zfe)
})

parc_veh <- rbindlist(parc_veh)
#Correction d'erreur 
parc_veh$energie <- revalue(as.factor(parc_veh$energie),c("Electrique"="Electrique et hydrogène",
                                                          "Inconnu"="Carburant inconnu"))
parc_veh$critair <- gsub("Crit'air","Crit'Air",parc_veh$critair)
parc_veh$critair <- revalue(as.factor(parc_veh$critair),c("Inconnu"="Vignette inconnue"))

##############################
###CARTES ET STATUT DES ZFE###
##############################

#Carte des EPCI ZFE
map_epci <- st_read(paste0("IGN/",path_map,"EPCI.shp"))
map_epci <- plyr::rename(map_epci,replace=c("CODE_SIREN"="codgeo","NOM"="libgeo"))
map_epci <- map_epci[,c("codgeo","libgeo")]

#Etat et code EPCI de chaque ZFE 
etat_zfe <- read_xlsx("etat_zfe_2021.xlsx")
#Création d'un code d'identification des ZFE 
etat_zfe$codzfe <- paste0("ZFE",str_pad(1:nrow(etat_zfe),3,pad="0"))
etat_zfe$libzfe <- etat_zfe$libgeo

#Libellés détaillés des EPCI
etat_zfe$libzfed <- gsub("CA","La Communauté d'agglomération",etat_zfe$libzfe)
etat_zfe$libzfed <- gsub("CU","La Communauté urbaine",etat_zfe$libzfed)
etat_zfe$libzfed <- gsub("CC","La Communauté de communes",etat_zfe$libzfed)
etat_zfe$libzfed <- gsub("Eurométropôle","L'Eurométropôle",etat_zfe$libzfed)
etat_zfe$libzfed <- ifelse(substr(etat_zfe$libzfed,1,9)=="Métropole",gsub("Métropole","La Métropole",etat_zfe$libzfed),etat_zfe$libzfed)

#Reformulation de l'état de la ZFE
etat_zfe$EtatZFEd <- gsub("ZFE en réflexion",
                          "envisage la mise en place d'une ZFE",etat_zfe$EtatZFE)
etat_zfe$EtatZFEd <- gsub("ZFE mise en place prochainement",
                          "mettra prochainement en place une ZFE",etat_zfe$EtatZFEd)    
etat_zfe$EtatZFEd <- gsub("ZFE existante",
                          "a mis en place une ZFE",etat_zfe$EtatZFEd) 

z <- "ZFE001"
#EPCI voisines de chaque ZFE
for(z in etat_zfe$codzfe){
  print(z)
  #Sélection d'une ZFE
  codgeo_zfe <- etat_zfe[etat_zfe$codzfe==z,]$codgeo
  epci_zfe <- st_simplify(map_epci[map_epci$codgeo==codgeo_zfe,], dTolerance = 1000)
  #Identification de la ZFE dans la carte des EPCI
  map_epci[map_epci$codgeo==codgeo_zfe,z] <- 2
  #Fenetre de 20 km autour de l'EPCI
  proxy_epci_zfe <- st_crop(st_simplify(map_epci,dTolerance = 1000),st_bbox(epci_zfe)+20000*c(-1,-1,1,1))
  #Distance entre la ZFE et les EPCI voisines 
  proxy_epci_zfe$dist_zfe <- as.numeric(st_distance(proxy_epci_zfe,epci_zfe))
  #EPCI à moins de 20 km de la ZFE
  proxy_epci_zfe <- proxy_epci_zfe[proxy_epci_zfe$dist_zfe<=20000,]$codgeo
  #Enregistrement des EPCI voisines de la ZFE dans la carte des EPCI
  map_epci[,z] <- as.numeric(map_epci$codgeo %in% proxy_epci_zfe) 
  map_epci[map_epci$codgeo==codgeo_zfe,z] <- 2
}


#Traitement du cas spécifique de la vallée de l'Arve (Plusieurs EPCI pour une même ZFE)
list_codzfe_arve <- etat_zfe[etat_zfe$codgeo %in% c("200000172","200023372","200033116","200034882"),]$codzfe
etat_zfe[etat_zfe$codgeo %in% c("200000172","200023372","200033116","200034882"),"codzfe"] <- "ZFEARV"
etat_zfe[etat_zfe$codgeo %in% c("200000172","200023372","200033116","200034882"),"libzfe"] <- "Vallée de l'Arve"
etat_zfe[etat_zfe$codgeo %in% c("200000172","200023372","200033116","200034882"),"libzfed"] <- "Plusieurs intercommunalités de la vallée de l'Arve"

#EPCI voisines en fonction du code d'identification ZFE
map_epci[,'ZFEARV'] <- rowMaxs(as.matrix(st_drop_geometry(map_epci[,c(list_codzfe_arve)])),value=T)
map_epci <- subset(map_epci,select = colnames(map_epci) [!colnames(map_epci) %in% list_codzfe_arve] )

#On retient uniquement les ZFE et les EPCI voisines dans la carte finale 
map_epci <- map_epci[rowMaxs(as.matrix(st_drop_geometry(map_epci[,unique(etat_zfe$codzfe)])),value=T)>0,]
map_epci$typgeo <- "epci"

#Carte du contour des ZFE
contour_zfe <- map_epci[map_epci$codgeo %in% etat_zfe$codgeo,c("codgeo")] 
contour_avr <-  st_union(contour_zfe[contour_zfe$codgeo %in%  etat_zfe[etat_zfe$codzfe == "ZFEARV",]$codgeo ,])
contour_zfe[,c("codzfe","libzfe","EtatZFEd")] <- etat_zfe[match(contour_zfe$codgeo,etat_zfe$codgeo),c("codzfe","libzfe","EtatZFEd")]
contour_zfe <- contour_zfe[!duplicated(contour_zfe$codzfe),]
st_geometry(contour_zfe[contour_zfe$libzfe == "Vallée de l'Arve",]) <- contour_avr
contour_zfe <- st_transform(contour_zfe,crs=4326)
contour_zfe <- st_cast(contour_zfe, "MULTILINESTRING")

#Enregistrement des données et cartes sur les EPCI et les ZFE
dir.create(paste0(current_directory,"/Map_EPCI"))
dir.create(paste0(current_directory,"/Map_ZFE"))
dir.create(paste0(current_directory,"/Data"))

map_epci <- st_transform(map_epci,crs=4326)
st_write(map_epci[,c("codgeo","libgeo","typgeo")],layer_options = "ENCODING=UTF-8","Map_EPCI/map_epci.shp",delete_dsn = T)
st_write(contour_zfe,layer_options = "ENCODING=UTF-8","Map_ZFE/contour_zfe.shp",delete_dsn = T)
write.csv(etat_zfe,"Data/data_zfe.csv",fileEncoding = "UTF-8") 
write.csv(st_drop_geometry(map_epci)[,c("codgeo","libgeo",unique(etat_zfe$codzfe))],"Data/epci_zfe.csv",fileEncoding = "UTF-8") 

#Communes des ZFE 
map_commune <- st_read(paste0("IGN/",path_map,"COMMUNE.shp"))
map_commune <- plyr::rename(map_commune,replace=c("INSEE_COM"="codgeo","NOM"="libgeo","SIREN_EPCI"="EPCI"))
map_commune <- map_commune[,c("codgeo","libgeo","EPCI")]
map_commune$EPCI <- substr(map_commune$EPCI,1,9)
map_commune[,"codzfe"] <- etat_zfe[match(map_commune$EPCI,etat_zfe$codgeo),]$codzfe
map_commune <- map_commune[!is.na(map_commune$codzfe),]
map_commune <- st_transform(map_commune,crs=4326)
map_commune$typgeo <- "com"
#map_commune[map_commune$EPCI %in% c(200054781,200023430),]

dir.create(paste0(current_directory,"/Map_com"))
st_write(map_commune,"Map_com/map_commune.shp", layer_options = "ENCODING=UTF-8",delete_dsn = T)

######################################
###MISE EN FORME DE DONNEES DE PARC###
######################################

#Mise en forme des données de parc pour l'application RShiny
#Données France entière
parc_france_rshiny <- aggregate(data=parc_veh[vehicule %in% c("vp","vul"),],
                                parc2021~vehicule+critair,FUN=sum)
parc_france_rshiny$codgeo <- "FRANCE"
parc_france_rshiny$typgeo <- "france"

#Données EPCI
#Liste de l'ensemble EPCI ZFE et de leurs voisines
liste_epci_zfe_vois <- st_drop_geometry(map_epci)$codgeo
parc_epci_rshiny <- aggregate(data=parc_veh[vehicule %in% c("vp","vul") & codepci %in% liste_epci_zfe_vois,],
                              parc2021~vehicule+critair+codepci,FUN=sum)
parc_epci_rshiny$typgeo <- "epci"
parc_epci_rshiny <- plyr::rename(parc_epci_rshiny,c("codepci"="codgeo"))

#Données Communales
liste_epci_zfe <- etat_zfe$codgeo
parc_com_rshiny <- aggregate(data=parc_veh[vehicule %in% c("vp","vul") & codepci %in% liste_epci_zfe,],
                              parc2021~vehicule+critair+depcom,FUN=sum)
parc_com_rshiny$typgeo <- "com"
parc_com_rshiny <- plyr::rename(parc_com_rshiny,c("depcom"="codgeo"))

#Réunion des données 
parc_rshiny <- rbind.fill(list(parc_com_rshiny,parc_epci_rshiny,parc_france_rshiny))
parc_rshiny$vehicule <- toupper(parc_rshiny$vehicule)
parc_rshiny <- plyr::rename(parc_rshiny,c("critair"="vignette","parc2021"="value"))
parc_rshiny <- as.data.table(parc_rshiny)
parc_rshiny[,total := sum(value),by=c("vehicule","codgeo","typgeo")]

write.csv(parc_rshiny,"Data/data_parc.csv",row.names=F,fileEncoding = "UTF-8")

######################################
#MISE EN FORME DES DONNEES POUR EXCEL#
######################################

#Sélection des EPCI ZFE 
parc_veh_zfe <- parc_veh[codepci %in% etat_zfe$codgeo,]
parc_veh_zfe$EtatZFE <- etat_zfe[match(parc_veh_zfe$codepci,etat_zfe$codgeo),]$EtatZFE

#Fonction de somme arrondie à une décimale
veh <- "vp"
dico_veh_det <- c("vul"="VUL","vp"="voitures","pl"="poids lourds",'tcp'="bus et cars")

parc_zfe_excel <- function(geo,veh){
  
  #Intitulé détaillé du véhicule 
  veh_det <- dico_veh_det[names(dico_veh_det)==veh]
  
  round0_sum <- function(x){round(sum(x,na.rm = T))}
  
  #Variable géographiques du fichier final selon 
  if(geo=="epci"){var_geo="libepci+codepci+EtatZFE"}
  if(geo=="com"){var_geo="libepci+codepci+libcom+depcom"}
  
  #Statistiques pour l'ensemble, les vignettes Crit'Air, le carburant et le croisement des deux 
  stat_zfe <- lapply(c("1","critair","energie","paste(critair,energie,sep=' - ')"),function(var_veh){
    dcast(data=parc_veh_zfe[vehicule==veh,],as.formula(paste(var_geo,var_veh,sep="~")),value.var = "parc2021",fun.aggregate = round0_sum)
  })
  
  #Réunion des quatre tableaux 
  stat_zfe <- do.call(cbind,stat_zfe)
  stat_zfe <- subset(stat_zfe,select=unique(colnames(stat_zfe)))

  #Nom des variables 
  stat_zfe <- plyr::rename(stat_zfe,c("libepci"="Nom de l'EPCI","codepci"="Code de l'EPCI",
                                "libcom"="Nom de la commune","depcom"="Code de la commune",
                                "EtatZFE"="Etat de la ZFE","."=paste("Total",veh_det)))
  
  #Ligne de nom des variables
  nom_var <- as.data.table(t(colnames(stat_zfe)))
  colnames(nom_var) <- colnames(stat_zfe)
  #Lignes d'entête
  entete <- data.table("Nom de l'EPCI"=c(
    paste0(if(geo=="com"){"Parcs communaux des "}else{"Parc des "},veh_det,", au 1er janvier 2021, selon la vignette ",
           "Crit'Air et le carburant pour les EPCI qui ont mis en place une ZFE ou qui l'envisagent"),
    "Source : Sdes",""))
  stat_zfe <- rbind.fill(list(entete,nom_var,stat_zfe))
  #Décile virgule
  stat_zfe <- apply(stat_zfe,2,function(x) str_replace(x,"\\.",","))
  stat_zfe <- as.data.table(stat_zfe)
  
  return(stat_zfe)
  
}

#Application de la fonction aux 4 types de véhicules et 2 échelles géographiques 
croissements_geo_veh <- cross2(c("epci","com"),c("vp","vul","tcp","pl"))
croissements_geo_veh <- lapply(croissements_geo_veh,unlist)

stat_geo_veh <- lapply(croissements_geo_veh,function(c) parc_zfe_excel(geo=c[1],veh=c[2]))

#Nom des onglets 
names_geo_veh <- lapply(croissements_geo_veh,function(c) revalue(c,dico_veh_det)) 
names_geo_veh <- lapply(names_geo_veh,function(c) revalue(c,c("com"="communal","epci"="EPCI"))) 
names_geo_veh <- lapply(names_geo_veh,paste,collapse=" 2021 ") 
names_geo_veh <- paste("Parc",names_geo_veh)
names(stat_geo_veh) <- names_geo_veh

write_xlsx(stat_geo_veh,"Data/Parc ZFE 2021.xlsx",col_names=F)

unlink("IGN",recursive=TRUE)
unlink("SDES",recursive=TRUE)
