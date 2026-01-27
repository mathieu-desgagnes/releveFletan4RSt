
# origine=file.path('S:','Flétan','Retour de marquage','BD_retourDeTag','BD_RetourTag')
# origine=file.path('S:','Flétan','Retour de marquage','BD_retourDeTag','retour des tags_ALL')
# destination=file.path('S:','Flétan','Retour de marquage','Analyses','input','recap_vTrav')

# le travail de recherche des tags originaux est intégré dans la base de données des retours de tag et
# il ne devrait pas être nécessaire de combiner la base de données du relevé palangre.
#
# format de la sortie:

lecture_recaptures <- function(origine, destination){
    #
    # recap.init.temp <- readxl::read_excel(path=paste0(origine, '.xlsx'), sheet='Feuil1', na='NA', col_types=c('guess','date',rep('guess',23)))
    recap.init.temp <- read.csv2(file=paste0(origine, '.csv'))
    table(recap.init.temp$an_recap, useNA='ifany')
    table(recap.init.temp['séries'], useNA='ifany')
    recap.init <- as.data.frame(recap.init.temp[which(recap.init.temp['séries'] == 'HB'),])
    names(recap.init)[which(names(recap.init)=='séries')] <- 'serie'
    names(recap.init)[which(names(recap.init)=='numéro1')] <- 'no1'
    names(recap.init)[which(names(recap.init)=='numéro2')] <- 'no2'
    ## names(recap.init)[which(names(recap.init)=='date_recapture')] <- 'dateRecap'
    names(recap.init)[which(names(recap.init)=='temps.en.mer')] <- 'tEnMer'
    ## table(recap.init$serie, useNA='ifany')
    ## table(recap.init$no1, useNA='ifany')
    recap.init$no1 <- as.numeric(recap.init$no1)
    ## table(recap.init$no2, useNA='ifany')
    recap.init$no2 <- as.numeric(recap.init$no2)
    recap.init$nbTagRecap <- as.numeric(!is.na(recap.init$no1)) +  as.numeric(!is.na(recap.init$no2))
    ##
    recap.init$dateRecap <- as.Date(paste(recap.init$an_recap, recap.init$mois_recap, recap.init$jour_recap, sep='-') , format='%Y-%m-%j')
    recap.init$dateMarq <- as.Date(recap.init$date_marq, format='%Y-%m-%j')
    recap.init$anMarq <- lubridate::year(recap.init$dateMarq)
    ## recap.init$clePoisson
    ##
    ## par(mfrow=c(1,2))
    ## hist(recap.init$tEnMer, breaks=seq(0,3000,by=365.25/12)); abline(v=365*1:6, col=2)
    recap.init$tEnMer  <- as.numeric(recap.init$dateRecap - recap.init$dateMarq)
    recap.init[is.na(recap.init$tEnMer),'tEnMer'] <- as.numeric(as.Date(paste(recap.init$an_recap, recap.init$moisRecap, 15, sep='-') , format='%Y-%m-%j') -
                                                                  recap.init$dateMarq)[is.na(recap.init$tEnMer)]
    recap.init[is.na(recap.init$tEnMer),'tEnMer'] <- as.numeric(as.Date(paste(recap.init$an_recap, 1, 1, sep='-') , format='%Y-%m-%j') -
                                                                  recap.init$dateMarq + mean(lubridate::yday(recap.init$dateRecap), na.rm=TRUE))[is.na(recap.init$tEnMer)]
    ##
    recap.init$tEnMerCut <- cut(recap.init$tEnMer, breaks=c(0,100,200,300,400,600,800,1200,1600,2200,2800,3600))
    recap.init$tEnMer.an <- cut(recap.init$tEnMer, breaks=seq(-250,4000,by=365))
    recap.init$tEnMer.demiAn <- cut(recap.init$tEnMer, breaks=seq(-250,4000,by=365/2))
    recap.init$tEnMer.deuxAns <- cut(recap.init$tEnMer, breaks=seq(-250,4000,by=365*2))
    ##
    recap.init[is.na(recap.init$tailleCm), 'tailleCm'] <- recap.init[is.na(recap.init$tailleCm), 'taillePo'] * 2.54
    ##
    recap.init$ffaw <- FALSE
    recap.init[which(recap.init['prénom']=='FFAW'),'ffaw'] <- TRUE
    ##
    if(FALSE){
      table(recap.init['prénom'])
      test <- recap.init[recap.init['prénom']=='FFAW',]
      table(test$an_recap)
    }
    ## recap <- subset(recap.init, substr(recap.init$clePoisson,1,1)%in%c('s','d'))
    ## plot(recap$tEnMer.an); hist(recap$tEnMer, breaks=seq(0,3000,by=30)); abline(v=seq(-250,10000,by=365))
    write.csv2(recap.init, file=paste0(destination,'.csv'), row.names=FALSE)
    ##






    ## ###
    ## Associer le données de marquage, issus de la base de données du relevé à la palangre 4RST
    ## ###
    anneeCourante <- max(recap.init$an_recap, na.rm=TRUE)
    load(file.path('S:','Flétan','Relevé 4RST', 'Analyses','input',anneeCourante,'longueursPlusMeta.RData'), verbose=1)
    nrow(longueurs.temp)
    ##
    tag.temp <- subset(longueurs.temp, !is.na(tag_1) | !is.na(tag_2)); nrow(tag.temp)
    tag.temp$nbTagPose <- as.numeric(!is.na(tag.temp$tag_1)) +  as.numeric(!is.na(tag.temp$tag_2))
    tag <- tag.temp[,c('cleStation','clePoisson','annee','dateDescente','noStation','longueur_cm','blessure','condition','nbTagPose','observateur')]
    ##
    ## recap sans longueur associé
    if(FALSE){
      test.temp <- test[which(!test$clePoisson%in%longueurs.temp$clePoisson),]
      ## test <- recap.init[which(!recap.init$clePoisson%in%longueurs.temp$clePoisson),]
      nrow(test.temp)
      test2 <- recap.init[which(recap.init['prénom']=='FFAW' &
                                  recap.init$clePoisson%in%longueurs.temp$clePoisson),]
      table(test2$an_recap)
      sum(table(test2$an_recap)[-c(1:3)])
      ##
      table(test2$anMarq)
      sum(table(test2$anMarq)[-c(1:3)])
    }
    ##
    if(TRUE){ #identifie les stations à considérer comme "représentatives" de la population, aller chercher dans le fichier des stations
      tag$pourCalculF <- FALSE
      length(which(tag$annee>=2017 & tag$noStation<=120 & tag$longueur_cm>=77))/length(which(tag$annee>=2017 & tag$noStation<=120))
      length(which(tag$annee>=2017 & tag$noStation>120 & tag$longueur_cm>=77))/
        length(which(tag$annee>=2017 & tag$noStation>120))
      tag[which(tag$annee>=2017 & as.numeric(tag$noStation)<=120 & tag$longueur_cm>=77),'pourCalculF'] <- TRUE
      tag$pourCalculF.prime <- FALSE
      tag[which(tag$annee>=2017 & tag$longueur_cm>=77),'pourCalculF.prime'] <- TRUE
    }
    ##
    ## ajouter recaptures
    ## table(recap.init$clePoisson, useNA='ifany')
    lesquels <- match(tag$clePoisson, recap.init$clePoisson)
    tag[which(!is.na(lesquels)),'anneeRecap'] <- recap.init[na.omit(lesquels),'an_recap']
    ##
    tag[which(!is.na(lesquels)),'longueur_cmRecap'] <- recap.init[na.omit(lesquels),'taille_cm']
    tag[which(!is.na(lesquels)),'nbTagRecap'] <- recap.init[na.omit(lesquels),'nbTagRecap']
    ## tag$nbTagPerdu <- recap.init[match(tag$clePoisson, recap.init$clePoisson),'nbPerdu']
    tag$nbTagPerdu <- tag$nbTagPose - tag$nbTagRecap
    tag[which(!is.na(lesquels)),'tEnMer'] <- recap.init[na.omit(lesquels),'tEnMer']
    tag$nbAnEnMer <- tag$anneeRecap-tag$annee
    tag[which(!is.na(lesquels)),'tEnMer.an'] <- recap.init[na.omit(lesquels),'tEnMer.an']
    ##
    tag[which(!is.na(lesquels)),'ffaw'] <- recap.init[na.omit(lesquels),'ffaw']

    if(FALSE){ #validation visuelle des la séparation des temps en mer en années
      par(mfrow=c(1,2))
      hist(tag$tEnMer, breaks=seq(0,4100, by=365/12)); abline(v=-240+365*0:12, col=2); text(x=-60+365*0:12, y=par('usr')[4]*0.95, labels=0:12)
      hist(subset(tag, nbTagPerdu==1)$tEnMer, breaks=seq(0,4100, by=365/12), col=2, add=TRUE)
      hist(subset(tag, tag$pourCalculF)$tEnMer, breaks=seq(0,4100, by=365/12)); abline(v=-240+365*0:10, col=2); text(x=-60+365*0:10, y=par('usr')[4]*0.95, labels=0:10)
      hist(subset(tag, tag$pourCalculF & nbTagPerdu==1)$tEnMer, breaks=seq(0,4100, by=365/12), col=2, add=TRUE)
      ## hist(subset(tag, tag$pourCalculF.prime)$tEnMer, breaks=seq(0,4000, by=365/12)); abline(v=-240+365*0:10, col=2); text(x=-60+365*0:10, y=par('usr')[4]*0.95, labels=0:10)
      ## hist(subset(tag, tag$pourCalculF.prime & nbTagPerdu==1)$tEnMer, breaks=seq(0,4000, by=365/12), col=2, add=TRUE)
      ## plot(tag$annee, tag$tEnMer)
    }
    ##
    ## ## évaluer les taux de retour
    ## table(tag$observateur, tag$annee)

    tagRelPal <- tag[order(tag$date),]
    write.csv2(tagRelPal, paste0(destination,'_bgTag.csv'), row.names=FALSE)
    save(tagRelPal, file=paste0(destination,'_bdTag.RData'))

    ## pour calcul de perte de tag
    perteTag <- na.omit(tagRelPal[tagRelPal$nbTagPose>1, c('nbTagPose','nbTagRecap','nbTagPerdu','nbAnEnMer')]); nrow(perteTag)
    write.csv2(perteTag, file=paste(destination, '_perteTag.csv'), row.names=FALSE)
    tagF <- subset(tag[order(tag$date),], pourCalculF==TRUE)
    write.csv2(tagF, file=paste0(destination, '_bdTagPourF.csv'), row.names=FALSE)


    return(tag)
}
