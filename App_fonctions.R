labelFormat_decimal <- function (prefix = "", suffix = "", between = " &ndash; ", digits = 3, 
                                 big.mark = ",", transform = identity, decimal.mark = "."){
  formatNum <- function(x) {
    format(round(transform(x), digits), trim = TRUE, scientific = FALSE, 
           big.mark = big.mark, decimal.mark = decimal.mark)
  }
  function(type, ...) {
    switch(type, numeric = (function(cuts) {
      paste0(prefix, formatNum(cuts), suffix)
    })(...), bin = (function(cuts) {
      n <- length(cuts)
      paste0(prefix, formatNum(cuts[-n]), between, formatNum(cuts[-1]), 
             suffix)
    })(...), quantile = (function(cuts, p) {
      n <- length(cuts)
      p <- paste0(round(p * 100), "%")
      cuts <- paste0(formatNum(cuts[-n]), between, formatNum(cuts[-1]))
      paste0("<span title=\"", cuts, "\">", prefix, p[-n], 
             between, p[-1], suffix, "</span>")
    })(...), factor = (function(cuts) {
      paste0(prefix, as.character(transform(cuts)), suffix)
    })(...))
  }
}



#"Clever Items" - FONCTIONS
#Serveur
CleverItemsServer <- function(iditems,nbitems){
  lapply(1:nbitems,function(i){
    onclick(paste0(iditems,i),{
      for(j in 1:nbitems){
        if(i!=j){
          hide(paste0(iditems,j,"-contents"))
          removeClass(paste0(iditems,j),"active-item")
          }else{
          toggle(paste0(iditems,j,"-contents"))
          toggleClass(paste0(iditems,j),"active-item")
          }
      }
    }) 
  })
} 

#UI
CleverItemsUi <- function(iditems,labelitems,contentitems){
  div(lapply(1:length(labelitems),function(i){
    div(class='block-clever-items',
        HTML(paste0('<a id="',iditems,i,'" href="#" class="action-button clever-item">',labelitems[[i]],'</a>')),
        hidden(div(id=paste0(iditems,i,"-contents"),class="clever-item-contents",contentitems[[i]])))
  }))
}



