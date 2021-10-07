
#Mentions légales
mentions_legales <- div(class="mentions-legales",
                        h3("Mentions légales"),
                        h4("Objet"),
                        p("Cet outil de datavisualisation a pour objet d'assurer la diffusion, sur Internet, des données sur la structure du parc automobile dans les zones à faibles émissions produites en 2020 par le Service des données et études statistiques (SDES) du ministère de la Transition écologique."),
                        p("Tout utilisateur connecté à ce site est réputé avoir pris connaissance des mentions légales et conditions d'utilisation ci-après et les accepter sans réserve."),
                        p("Les publications et données disponibles sur ce site sont des informations publiques dont la réutilisation est gratuite et n'est soumise à aucune autre condition que celles de la loi du 17 juillet 1978. "),
                        
                        h4("Service gestionnaire"),
                        div("Ministère de la Transition écologique"),
                        div("Commissariat général au développement durable"),
                        div("Service des données et études statistiques (SDES)"),
                        div("Tour Séquoia"),
                        div("92055 La Défense cedex"),
                        div("Téléphone : 01 40 81 21 22"),
                        br(),
                        div(tags$strong("Directrice de publication")," : Béatrice Sédillot, cheffe du service des données et études statistiques (SDES)"),
                        div("Contact :",a(href='mailto:diffusion.sdes.cgdd@developpement-durable.gouv.fr',"diffusion.sdes.cgdd@developpement-durable.gouv.fr")),
                        
                        h4("Développement"),
                        p("L'application « Parc automobile des zones à faibles émissions » a été développée par la sous-direction des statistiques des transports du SDES. "),
                        p("Cet outil a été élaboré à partir du logiciel R version 4.0.3 (2020-10-10)"),
                        h4("Réutilisation des informations publiques"),
                        p("Les conditions de libre réutilisation fixées ci-après s'appliquent uniquement aux contenus et données diffusés par cet outil de datavisualisation et constituant des informations publiques au sens du livre III du code des relations entre le public et l'administration."),
                        p("Les publications et bases de données disponibles sur ce site sont des œuvres originales dont le SDES 
      concède des droits de reproduction et de diffusion dans les conditions de la licence ouverte version 
      2.0 accessible sur le site : ",a(target='_blank',href='https://www.data.gouv.fr/fr/licences',"www.data.gouv.fr/fr/licences"),"."), 
                        p("Pour toute demande de dérogation à ces conditions, veuillez vous adresser à ",
                          a(href="mailto:diffusion.sdes.cgdd@developpement-durable.gouv.fr",
                            "diffusion.sdes.cgdd@developpement-durable.gouv.fr"),"."), 
                        
                        h4("Données personnelles"),
                        p("Ce site enregistre votre adresse IP et l'associe à des cookies enregistrés par votre navigateur Internet. Ces données sont utilisées à des fins statistiques, pour une durée limitée et ne sont en aucun cas transmises à des tiers."),
                        
                        h4("Liens vers ce site"),
                        p("L'administration est favorable à la création de liens hypertextes vers les pages de son site. De façon générale, tout lien établi doit indiquer de façon claire à l'internaute qu'il est dirigé vers cette application, en faisant notamment mention intégrale et visible de son adresse."),
                        p("Les liens directs et profonds vers les fichiers téléchargeables diffusés sur ce site sont susceptibles de changer à tout moment : il est suggéré de créer principalement des liens vers les pages décrivant ces publications et données et donnant accès à ces fichiers."),
                        p("Le SDES se réserve le droit de demander la dissolution des liens dont elle estimera qu'ils sont de nature à porter préjudice à son image ou à ses droits."),
                        
                        h4("Disponibilité du service"),
                        p("L'administration s'efforce d'ouvrir l'accès à ce site 24 heures sur 24, 7 jours sur 7, sauf en cas de force majeure et sous réserve d'éventuelles pannes et d'interventions de maintenance. L'administration peut être amenée à interrompre ce site ou une partie des services, à tout moment et sans préavis."),
                        p("La responsabilité de l'administration ne saurait être engagée en cas d'impossibilité d'accès à ce site ou d'utilisation de ces services."),
                        
                        h4("Informations contenues sur ce site"),
                        p("Les informations figurant sur ce site proviennent de sources considérées comme étant fiables. Toutefois, elles sont susceptibles de contenir des erreurs que l'administration se réserve le droit de corriger dès qu'elles sont portées à sa connaissance. Les informations disponibles sur ce site sont susceptibles d'être modifiées à tout moment, et peuvent avoir fait l'objet de mises à jour depuis leur dernière consultation."),
                        p("Les sites internet ou informations tierces référencées sur ce site par des liens ont été sélectionnés et mis à disposition des utilisateurs à titre d'information. Malgré le soin apporté à leur sélection, ceux-ci peuvent contenir des informations inexactes ou sujettes à interprétation."),
                        p("L'administration ne pourra en aucun cas être tenue responsable de tout dommage de quelque nature qu'il soit résultant de l'interprétation ou de l'utilisation des informations disponibles sur ce site ou sur les sites tiers."))
