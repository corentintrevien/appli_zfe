

list_critair <- list("Crit'air E"="e","Crit'air 1"="1","Crit'air 2"="2","Crit'air 3"="3",
                     "Crit'air 4"="4","Crit'air 5"="5","Non classés ou inconnus"="n")
color_critair<-c("Crit'air E"="#009650","Crit'air 1"="#794685","Crit'air 2"="#edbe31","Crit'air 3"="#de8235",
                 "Crit'air 4"="#462021","Crit'air 5"="#4f4b48","Non classés ou inconnus"="lightgrey")



#Vignette Checkbox CritAir
css_vignette_checkbox <- lapply(1:7,function(i) paste0(
  '#selected_vignette',list_critair[i],':checked+label::before {background-color: ',color_critair[i],
  '; border-color: ',color_critair[i],'} #selected_vignette',list_critair[i],
  ':disabled+label::before {background-color: ',color_critair[i],'; border-color: ',color_critair[i],
  ';} #selected_vignette',list_critair[i],':disabled+label::after {color:',color_critair[i],';}')
)

css_style <- list(
  #Style général
  tags$head(tags$style(HTML("a:hover,a:active,a:visited,a:link {text-decoration: none;}
                            .help-block{color: #2c3e50}
                            h2{font-weight:bold;}
                            .well{ background-color: #dce4e6}
                            h2, h3, h5{margin-bottom:10px;margin-top:10px;}
                            .submaintitle{font-style:italic;font-weight:normal;font-size:70%;}
                            hr {border-top: 1px solid #2c3e50;margin-top: 15px;margin-bottom: 15px;}
                            .action-button,.help-block{margin-bottom:6px;margin-top:3px;}"))),
  
#Style des popups
tags$head(tags$style(HTML("
                a.action-button{color: #2c3e50;}
                a.action-button:hover{color: #18bc9c;}
                a.action-button:active{color: #18bc9c;}
                .popover-content {padding: 9px ;font-size:13px; background-color: white;}
                label >.popover {color: #2c3e50;  border-radius: 4px; width: 290px; text-align:left;}
                label >.popover-title{height:0;padding:0}
                p> .popover {color: #2c3e50;  border-radius: 4px; max-width: 370px; text-align:left;}
                p> .popover-title {background-color: #ecf0f1;padding: 9px ;font-size:15px;}
                .textlist {list-style: none; padding: 0; margin:0;line-height: 1.42857143;}
                .textlist li {margin-left:9px; text-indent:-9px;}
                .textlist li::before {display:inline-block;width:9px;text-indent:0;vertical-align: middle;
                font-family:'Font Awesome 5 Free'; font-weight: 900; content: '\\f0da';font-style:normal; }"))),

                  
                  
#Style mentions légales
tags$head(tags$style(HTML(".nav-tabs>li.active>a[data-value = 'Mentions']{color:transparent; border: 0;border-bottom: 1px solid #ecf0f1;}
                           .nav-tabs>li>a[data-value = 'Mentions']{color:transparent; border: 0;border-bottom: 1px solid #ecf0f1; }
                           
                           #retourapp{padding : 0 1px;  display: block; margin: -9.5px 0;
                                      width: 100%;white-space: normal;font-weight:bold;font-style:italic;
                                      font-size:15px;background-color:#ecf0f1;border:0;color:#2c3e50;}
                           .mentions-legales {font-size:85%;}"))),

#Style des checkbox awesome
tags$head(tags$style(HTML(paste(css_vignette_checkbox,
                                '.awesome-checkbox>label{line-height: 1.2;font-size:14px;}
                                   .awesome-checkbox input[type="checkbox"]:disabled+label{opacity:1;}
                                          .awesome-checkbox input[type="checkbox"]:disabled {cursor:default;}
                                          .awesome-checkbox input[type="checkbox"]:disabled+label {cursor:default;}
                                          .awesome-checkbox input[type="checkbox"]:disabled+label::after {cursor:default;}
                                          .awesome-checkbox input[type="checkbox"]:disabled+label::before{cursor:default;}
                                          .awesome-checkbox,. {display: block;}
              .shiny-input-checkboxgroup label~.shiny-options-group,.shiny-options-group{margin-top:-5px;}')))),

#Bouton de téléchargement des données 
tags$head(tags$style(HTML("#downloadData {background-color: #2c3e50; border-color: #2c3e50;padding:5px 10px;font-weight:bold;}
                             #downloadData:hover {background-color: #18bc9c; border-color: #18bc9c}
                             #downloadData{display: block; max-width: 100%;white-space: normal;}"))),

#Style des clever-items 
tags$head(tags$style(HTML(".block-clever-items {margin-bottom:6px;margin-top:3px;}
                             .clever-item{margin-bottom:6px; margin-top:3px; margin-left:18px; text-indent:-18px;
                                          font-weight:bold; color:#2c3e50;display:block;}
                             .clever-item.active-item{color: #18bc9c ;}
                             .clever-item:before{font-family:'Font Awesome 5 Free'; font-weight:900;text-indent:0px;
                                                  content:'\\f107'; font-style:normal; display:inline-block;width:18px;}
                             .clever-item.active-item:before{font-family:'Font Awesome 5 Free'; content:'\\f105'; }
                             .clever-item-contents{margin-left:18px; font-style:italic; font-size: 14px;}"))))

