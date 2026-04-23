# du code pour aider à débugger ou vérifier les sorties

devtools::load_all()
anneeCourante <- 2025
# temp <- lecture_stations(origine=file.path('S:','Flétan','Relevé 4RST','BD','donneesReleveFletan_stations.csv'),
#                          destination=file.path('S:','Flétan','Relevé 4RST','BD','versionTravailTemporaire',anneeCourante,'stations_vTrav'))
#
# temp$dateHeureRemontee
# tail(temp,30)
#
# head(temp)
# temp[temp$cleStation=='s1-2017',]

temp <- lecture_versionTravail(annee = anneeCourante) # assez long...
###

temp <- lecture_recaptures(
  origine = file.path(
    'S:',
    'Flétan',
    'Retour de marquage',
    'BD_retourDeTag',
    'retour des tags_ALL'
  ),
  destination = file.path(
    'S:',
    'Flétan',
    'Retour de marquage',
    'Analyses',
    'input',
    'recap_vTrav'
  )
)

# tableau des années de recapture selon année de capture
x <- table(temp[,c('annee','anneeRecap')])
dimnames(x)[[2]] <- paste0('retour_',dimnames(x)[[2]])
table(temp$annee)


## détermination de la contribution de la FFAW au remboursement des retours d'étiquettes
test <- temp[which(temp$ffaw), ]
nrow(test)
table(test$anneeRecap)
sum(table(test$anneeRecap)[as.character(2017:2025)])
sum(table(test$anneeRecap)[as.character(2018:2025)])

table(test$annee)
sum(table(test$annee)[as.character(2017:2024)])
sum(table(test$annee)[as.character(2018:2024)])

table(test$annee, test$anneeRecap)
table(test$annee[test$anneeRecap >= 2018])
sum(table(test$annee[test$anneeRecap >= 2018]))


test <- temp[which(temp$ffaw & temp$pourCalculF), ]
nrow(test)
table(test$anneeRecap)
table(test$annee)
# sum(table(test$anneeRecap)[as.character(2017:2025)])
