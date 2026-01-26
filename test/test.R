# du code pour aider à débugger ou vérifier les sorties

devtools::load_all()
anneeCourante <- 2025
temp <- lecture_stations(origine=file.path('S:','Flétan','Relevé 4RST','BD','donneesReleveFletan_stations.csv'),
                         destination=file.path('S:','Flétan','Relevé 4RST','BD','versionTravailTemporaire',anneeCourante,'stations_vTrav'))

temp$dateHeureRemontee
tail(temp,30)

head(temp)
temp[temp$cleStation=='s1-2017',]
