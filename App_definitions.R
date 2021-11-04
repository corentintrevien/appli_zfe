#Définitions, liens et méthodes

#Classement Crit Air
critlabel <-c("Crit'Air E",
              "Crit'Air 1",
              "Crit'Air 2",
              "Crit'Air 3",
              "Crit'Air 4",
              "Crit'Air 5")

#Bulles de définition de chaque type de véhicule Crit'Air
critcontenu <-list(tags$ul(class='textlist',tags$li("Tout véhicule électrique ou à hydrogène")),
                   tags$ul(class='textlist',tags$li("Véhicule essence Euro 5 et 6",br(),
                                                    tags$i("immatriculé après le 01/01/2011")),
                           tags$li("Tout véhicule au gaz ou hybride rechargeable")),
                   tags$ul(class='textlist',tags$li("Véhicule essence Euro 4",br(),
                                                    tags$i("immatriculé du 01/01/2006 au 31/12/2010")),
                           tags$li("Véhicule diesel Euro 5 et 6",br(), 
                                   tags$i("immatriculé après le 01/01/2011"))),
                   tags$ul(class='textlist',tags$li("Véhicule essence Euro 2 et 3", br(),
                                                    tags$i("immatriculé du 01/10/1997 au 31/12/2005")),
                           tags$li("Véhicule diesel Euro 4", br(),
                                   tags$i("immatriculé du 01/01/2006 au 31/12/2010"))),
                   tags$ul(class='textlist',tags$li("Véhicule diesel Euro 3", br(),
                                                    tags$i("immatriculé du 01/01/2001 au 31/12/2005"))),
                   tags$ul(class='textlist',tags$li("Véhicule diesel Euro 2",br(), 
                                                    tags$i("immatriculé du 01/10/1997 au 31/12/2000"))))

#Définition des types de véhicules
defveh <- c("VUL"="Véhicule de moins de 3,5 tonnes conçu et construit pour le transport de marchandises.",
            "VP"="Véhicule conçu pour le transport de personnes et comportant, outre le siège du conducteur, 8 places assises au maximum. ")
  
#Définitions des ZFE et du certificat Crit'Air
deflabel <- c("Zone à faibles émissions","Certificat qualité de l'air Crit'Air")
defcontenu <- list(div("Dispositif, soutenu par l’Etat, destiné à améliorer la qualité de l’air, notamment dans les grandes agglomérations. Il consiste à limiter la circulation des véhicules les plus polluants dans un périmètre défini, par décision du ou des maires concernés.",
                       helpText(icon("caret-right"),HTML("Pour en savoir plus sur les <a href = 'https://www.ecologie.gouv.fr/zones-faibles-emissions-19-collectivites-sengagent-qualite-lair' target='_blank'>zones à faibles émissions</a>"))),
                   div("Indique le niveau de pollution du véhicule. Plus le numéro de certificat est élevé, plus le véhicule pollue. Il est obligatoire pour circuler lors d'un pic de pollution en cas de circulation différenciée et dans une zone à faibles émissions.",
                       helpText(icon("caret-right"),HTML("Pour en savoir plus sur les <a href = 'https://www.ecologie.gouv.fr/certificats-qualite-lair-critair' target='_blank'>certificats qualité de l'air  Crit'Air</a>"))))

deflabel_vb <- c("Zone à faibles émissions","Certificat qualité de l'air Crit'Air","Véhicule utilitaire léger","Véhicule particulier")
defcontenu_vb <- list(div("Dispositif, soutenu par l’État, destiné à améliorer la qualité de l’air, notamment dans les grandes agglomérations. Il consiste à limiter la circulation des véhicules les plus polluants dans un périmètre défini, par décision du ou des maires concernés.",
                       helpText(icon("caret-right"),HTML("Pour en savoir plus sur les <a href = 'https://www.ecologie.gouv.fr/zones-faibles-emissions-19-collectivites-sengagent-qualite-lair' target='_blank'>zones à faibles émissions</a>"))),
                   div("Indique le niveau de pollution du véhicule. Plus le numéro de certificat est élevé, plus le véhicule pollue. Il est obligatoire pour circuler lors d'un pic de pollution en cas de circulation différenciée et dans une zone à faibles émissions.",
                       helpText(icon("caret-right"),HTML("Pour en savoir plus sur les <a href = 'https://www.ecologie.gouv.fr/certificats-qualite-lair-critair' target='_blank'>certificats qualité de l'air  Crit'Air</a>"))),
                   "Véhicule de moins de 3,5 tonnes conçu et construit pour le transport de marchandises.",
                   "Véhicule conçu pour le transport de personnes et comportant, outre le siège du conducteur, 8 places assises au maximum. ")

#Méthode 
methode <- div(div("Le répertoire statistique des véhicules routiers (RSVERO) est issu du système d’immatriculation des véhicules (SIV), enrichi avec les données issues des contrôles techniques.
Un véhicule est réputé en circulation s’il est en règle vis-à-vis du contrôle technique et si aucune opération indiquant une sortie de parc n’a été enregistrée par le SIV. Il est localisé à l'adresse de l'utilisateur, renseignée sur le certificat d'immatriculation. Les données présentées dans cette application correspondent à l'état du parc automobile au 01/01/2021."),
               helpText(icon("caret-right"),HTML("Pour en savoir plus sur la <a href = 'https://www.statistiques.developpement-durable.gouv.fr/le-parc-de-vehicules-selon-leur-categorie-critair-dans-les-zones-faibles-emissions-zfe' target='_blank'>méthodologie et télécharger les données complètes</a>")),
               helpText(icon("caret-right"),HTML("Le code source de cette application est disponible <a href = 'https://github.com/corentintrevien/appli_zfe/' target='_blank'>ici</a>")))
