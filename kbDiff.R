library(magrittr)
library(dplyr)

diff_kb <- function(kb1, kb2, listeFormat="Ekspandert"){
    stopifnot(length(kb1) == length(kb2) )
    
    "%wo%" <- function(x, y) x[!x %in% y]
    
    names <- c("skjemanavn", "navn_i_rapporteket", "ledetekst", "obligatorisk", "type", "listeverdier", "listetekst", 
               "normalintervall_start_numerisk", "normalintervall_slutt_numerisk", "maksintervall_start_numerisk", 
               "maksintervall_slutt_numerisk", "normalintervall_start_dato", "normalintervall_slutt_dato", "maksintervall_start_dato",
               "maksintervall_slutt_dato", "antall_tegn", "lovlige_tegn", "desimaler", "aktiveringsspoersmaal", "underspoersmaal", 
               "innfoert_dato", "utfaset_dato", "tabell", "fysisk_feltnavn", "kommentar", "variabel_id", "hjelpetekst")     
    
    names(kb1) <- names  
    names(kb2) <- names
    
    #variabler som er med i kn12 men ikke i kb2 
    fjernet <- unique(kb1$variabel_id %wo% kb2$variabel_id)
    
    #variabler som er 
    lagttil <- unique(kb2$variabel_id %wo% kb1$variabel_id)
    
    #uendret var
    uend <- unique(kb1$variabel_id[kb1$variabel_id %in%  kb2$variabel_id] )
    
    #sjekker endring i variabel definisjonen 
    diff <- function(x, y, KB1 = kb1, KB2 = kb2){
        
        f1 <- kb1 %>%  dplyr::filter( variabel_id == x) %>%
            dplyr::select(y) 
        
        f2 <- kb2 %>%  dplyr::filter(variabel_id == x) %>%
            dplyr::select(y)
        
        f3 <- f2[(!f1 %in% f2 | !f2 %in% f1)]
        if ( length(f3) == 0) f3 <- NULL
        
        return(f3)
    }
    
    
    if (length(uend) > 0){
    #endring i listetekst
    listeTekst <- uend %>% lapply(FUN =diff, y = "listetekst") 
    #endring i listeverdi
    listeVerdier <- uend %>% lapply(FUN =diff, y = "listeverdier") 
    # endring i variabel type
    type <- uend %>% lapply(FUN =diff, y = "type")
    
    indTex <- listeTekst  %>% sapply(FUN = function(x) (!is.null(x)) ) %>% which()
    indVer <- listeVerdier  %>% sapply(FUN = function(x) (!is.null(x)) ) %>% which()
    indType <- type  %>% sapply(FUN = function(x) (!is.null(x)) ) %>% which()
    
    #bruker navn i Rapporteket til output
    varTekst <- kb1$navn_i_rapporteket[kb1$variabel_id %in% uend[indTex]]   
    varType <- kb1$navn_i_rapporteket[kb1$variabel_id %in% uend[indType]]  
    varVerdier <- kb1$navn_i_rapporteket[kb1$variabel_id %in% uend[indVer]]
    
   
    endTekst <- listeTekst %>% unlist(use.names = F)
    endType <- type %>% unlist(use.names = F)
    endVerdier <- listeVerdier %>% unlist(use.names = F)
    
    #endring i verdiene til variablene
    VerdiEndret <- list ( "type" = list("variabel" = varType, "verdi" = endType), 
                         "listeTekst" = list("variabel" = varTekst, "verdi" = endTekst),
                         "listeVariabler" = list("variabel" = varVerdier, "verdi" = endVerdier))
    #endring i variabler
    variabelEndret <- list ("fjernet" = unique(kb1$navn_i_rapporteket[(kb1$variabel_id %in% fjernet)]),
                             "lagt_til" = unique(kb2$navn_i_rapporteket[(kb2$variabel_id %in% lagttil)])) 
    
    return(list("variabelEndret" = variabelEndret ,"verdiEndret" = VerdiEndret))    
    
    }else {
        variabelEndret <- list ("fjernet" = unique(kb1$navn_i_rapporteket[(kb1$variabel_id %in% fjernet)]),
                                "lagt_til" = unique(kb2$navn_i_rapporteket[(kb2$variabel_id %in% lagttil)])) 
        return (list("variabelEndret" = variabelEndret ))
    
    }
    
}