#' Production de la carte à joindre au protocole du relevé à la palangre
#'
#' La fonction lit les données de cartographie, les stations.
#' La fonction enregistre un tableau des stations pour inclure dans le protocole.
#' La fonction enregistre des cartes pour inclure dans le protocole, soit une générale et plusieurs aggrandissements.
#'
#' @param annee anne pour laquelle la carte est calculée
#' @param dir_releve chemin pour le dossier ou se trouve les protocoles, pour lire et écrire
#' @param dir_pbs chemin pour le dossier ou se trouve les outils pré-formatés en format PBSmapping
#'
#' @import PBSmapping
#' @importFrom openxlsx write.xlsx
#' @returns NULL
#' @export
#'
#' @examples ## à venir
produire_carte_protocole <- function(
  annee,
  dir_releve,
  dir_pbs,
  dir_shapefile
) {
  ## 1) lire les données
  dir_stations <- file.path(dir_releve, annee, 'coordonnees')
  load(
    file = file.path(dir_pbs, 'output', 'vieux stock', 'traitCote.RData'),
    verbose = 1
  )
  file.info(file.path(
    dir_pbs,
    'output',
    'vieux stock',
    'traitCote.RData'
  ))$mtime
  load(
    file = file.path(dir_pbs, 'output', 'vieux stock', 'fondBleu.RData'),
    verbose = 1
  ) #fonction pour ajouter un dégradé en bleu suivant la profondeur
  # load(
  #   file = file.path(dir_pbs, 'output', 'vieux stock', 'ssZone_fletan.RData'),
  #   verbose = 1
  # )
  # file.info(file.path(
  #   dir_pbs,
  #   'output',
  #   'vieux stock',
  #   'ssZone_fletan.RData'
  # ))$mtime #ssZone

  ## 1a) lire les shapefiles de la zone d'étude
  cote.qcl <- sf::st_read(file.path(
    dir_shapefile,
    '20_50m',
    'habitat_20_50m_nettoye.shp'
  ))
  cote <- sf::st_transform(cote.qcl, sf::st_crs("epsg:4326")) #wgs84
  prof.qcl <- sf::st_read(file.path(
    dir_shapefile,
    '100_300m',
    'habitat_100_300m_nettoye.shp'
  ))
  prof <- sf::st_transform(prof.qcl, sf::st_crs("epsg:4326")) #wgs84

  ## 2) lire les stations
  stationsTt <- read.csv2(
    file.path(dir_stations, 'stationsProposees.csv'),
    stringsAsFactors = FALSE
  )
  stationsFinales <- read.csv2(
    file.path(dir_stations, 'stationsFinales_DMS.csv'),
    stringsAsFactors = FALSE
  )
  # stationsFinales <- read.csv2(
  #   file.path(dir_stations, 'stationsFinales.csv'),
  #   stringsAsFactors = FALSE
  # )
  ## table(stationsTt$priorite); table(stationsTt$strateOpano)
  ## stations4RST <- subset(stationsTt, strateOpano!='3PN')
  stations <- subset(stationsFinales, choix %in% c('base', 'exploratoireSud'))
  stations4Rab <- subset(stationsFinales, choix %in% c('supplementaire4Ra'))

  ## 3) écrire les stations sous forme de tableau pour protocole
  ## le format est de 42 lignes, avec 3x 3 colonnes ('id','latitude','longitude')
  temp <- array(
    dim = c(42, 9),
    dimnames = list(NULL, rep(c('id', 'Latitude DD', 'Longitude DD'), 3))
  )
  temp[1:42, 1:3] <- as.matrix(stations[
    match(1:42, stations$id),
    c('id', 'Latitude.DD', 'Longitude.DD')
  ])
  temp[1:42, 4:6] <- as.matrix(stations[
    match(43:84, stations$id),
    c('id', 'Latitude.DD', 'Longitude.DD')
  ])
  temp[1:41, 7:9] <- as.matrix(stations[
    match(85:125, stations$id),
    c('id', 'Latitude.DD', 'Longitude.DD')
  ])
  write.csv2(
    temp,
    file = file.path(dir_stations, 'posStation_protocole_DD.csv'),
    row.names = FALSE,
    fileEncoding = 'latin1'
  )
  openxlsx::write.xlsx(
    temp,
    file.path(dir_stations, 'posStation_protocole_DD.xlsx')
  )
  #
  temp <- array(
    dim = c(42, 9),
    dimnames = list(NULL, rep(c('id', 'Latitude DMM', 'Longitude DMM'), 3))
  )
  temp[1:42, 1:3] <- as.matrix(stations[
    match(1:42, stations$id),
    c('id', 'Latitude.DMM', 'Longitude.DMM')
  ])
  temp[1:42, 4:6] <- as.matrix(stations[
    match(43:84, stations$id),
    c('id', 'Latitude.DMM', 'Longitude.DMM')
  ])
  temp[1:41, 7:9] <- as.matrix(stations[
    match(85:125, stations$id),
    c('id', 'Latitude.DMM', 'Longitude.DMM')
  ])
  write.csv2(
    temp,
    file = file.path(dir_stations, 'posStation_protocole_DMM.csv'),
    row.names = FALSE,
    fileEncoding = 'latin1'
  )
  openxlsx::write.xlsx(
    temp,
    file.path(dir_stations, 'posStation_protocole_DMM.xlsx')
  )
  ##
  temp <- array(
    dim = c(42, 9),
    dimnames = list(NULL, rep(c('id', 'Latitude DMS', 'Longitude DMS'), 3))
  )
  temp[1:42, 1:3] <- as.matrix(stations[
    match(1:42, stations$id),
    c('id', 'Latitude.DMS', 'Longitude.DMS')
  ])
  temp[1:42, 4:6] <- as.matrix(stations[
    match(43:84, stations$id),
    c('id', 'Latitude.DMS', 'Longitude.DMS')
  ])
  temp[1:41, 7:9] <- as.matrix(stations[
    match(85:125, stations$id),
    c('id', 'Latitude.DMS', 'Longitude.DMS')
  ])
  write.csv2(
    temp,
    file = file.path(dir_stations, 'posStation_protocole_DMS.csv'),
    row.names = FALSE,
    fileEncoding = 'latin1'
  )
  openxlsx::write.xlsx(
    temp,
    file.path(dir_stations, 'posStation_protocole_DMS.xlsx')
  )
  ##
  ##
  ## 3a) écrire toutes les stations dans un .csv avec les différents formats
  temp <- stations[, c(
    'id',
    'ssZoneOpano',
    'strateProfondeur',
    'Latitude.DMS',
    'Longitude.DMS',
    'Latitude.DMM',
    'Longitude.DMM',
    'Latitude.DD',
    'Longitude.DD'
  )]
  write.csv2(
    temp,
    file = file.path(dir_stations, paste0('coordStation.csv')),
    row.names = FALSE,
    fileEncoding = 'latin1',
    quote = FALSE
  )
  openxlsx::write.xlsx(temp, file.path(dir_stations, 'coordStation.xlsx'))
  ##

  ## 4) carte pour protocole
  carteProtocole <- function(
    tabPos,
    anneeCourante = '',
    ecran = FALSE,
    xlim = c(-70, -55),
    ylim = c(45.5, 52.4),
    couleur = 'white',
    explo = NULL,
    traitCote = NWApolys_i,
    legende = NULL,
    legende.couleur = NULL,
    courbeNiveau = NULL,
    inclureAireEtude = TRUE,
    nomPng = NULL,
    pdf = FALSE,
    cercle = FALSE,
    Field1 = FALSE,
    inclureFondBleu = TRUE,
    inclureAnneeCourante = TRUE,
    lignesPoissonFond = FALSE,
    widthPourMap = 7.3,
    dirOutput = ''
  ) {
    ## Figure 1: carte des stations d'échantillonnage
    if (length(couleur) == 1) {
      couleur <- rep(couleur, 500)
    }
    if (is.null(nomPng)) {
      nomPng <- 'carteProtocole'
    }
    if (!ecran) {
      if (pdf) {
        pdf(
          file.path(dirOutput, paste0(nomPng, '.pdf')),
          height = 11,
          width = 8.5
        )
      } else {
        png(
          file.path(dirOutput, paste0(nomPng, '.png')),
          height = widthPourMap * 0.73,
          width = widthPourMap,
          units = 'in',
          res = 300
        )
      }
    }
    par(mfrow = c(1, 1), mar = c(3, 3, 0, 0) + 0.1)
    plotGSL(
      traitCote,
      main = '',
      ylim = ylim,
      xlim = xlim,
      plt = NULL,
      bg = 'white'
    )
    if (inclureFondBleu) {
      fondBleu(niveau = c(37, 91, 183, 274, 366))
    }
    if (inclureAireEtude) {
      addPolys(aireEtude$cote, col = 'lightblue', border = NA) #rgb(0.1,0.1,0.1,0.2)
      addPolys(aireEtude$prof, col = 'lightblue', border = NA) #rgb(0.1,0.1,0.1,0.2)
    }
    # if (!is.null(courbeNiveau)) {
    #   ## Avant juillet 2020, on utilisait l'objet pré-calculé "courbeNiveau.thin"
    #   ## Maintenant, on refait à chaque fois la bathy à partir du raster
    #   ## OUPS... problème de projection (lambert vs wgs84)
    #   require('raster')
    #   temp <- raster(file.path(
    #     dir.pbs,
    #     'input',
    #     'bathy_DAISS',
    #     'gulf20m_2020May26_ascii',
    #     'gulf500m_mathieuDesgagnes.grd'
    #   ))
    #   extent.obj <- as.data.frame(cbind(
    #     xlim + c(-0.2, 0.2) * diff(xlim),
    #     ylim + c(-0.2, 0.2) * diff(ylim)
    #   ))
    #   names(extent.obj) <- c('X', 'Y')
    #   coordinates(extent.obj) <- c('X', 'Y')
    #   proj4string(extent.obj) <- CRS("+init=epsg:4326") #wgs84
    #   extent <- spTransform(extent.obj, CRS(proj4string(temp))) #supposé qcLambert
    #   extent <- extent(extent)
    #   temp2 <- crop(temp, extent)
    #   cn.range <- quantile(values(temp2), probs = c(0.001, 0.999), na.rm = TRUE)
    #   cn <- courbeNiveau[which(
    #     courbeNiveau <= cn.range[2] & courbeNiveau >= cn.range[1]
    #   )]
    #   for (i.niveau in cn) {
    #     test <- rasterToContour(temp2, levels = i.niveau) #retourne un 'contourLines'
    #     unNiveau <- coordinates(test)[[1]]
    #     unNiveau <- unNiveau[which(sapply(unNiveau, nrow) > 3)]
    #     for (i.segment in seq_along(unNiveau)) {
    #       unSegment.init <- as.data.frame(unNiveau[[i.segment]])
    #       names(unSegment.init) <- c('X', 'Y')
    #       coordinates(unSegment.init) <- c('X', 'Y')
    #       proj4string(unSegment.init) <- CRS(proj4string(temp)) #supposé qcLambert
    #       unSegment <- spTransform(unSegment.init, CRS("+init=epsg:4326")) #WGS84
    #       lines(coordinates(unSegment), col = 'brown', lwd = 0.5)
    #     }
    #   }
    #   ## for(i in courbeNiveau){
    #   ##     addLines(courbeNiveau.thin[[paste0('prof',i,'m')]], col='grey70')
    #   ## }
    # }
    if (!lignesPoissonFond) {
      addLines(ssZone$opanoSsZone$lignes, lwd = 1)
    } else {
      addLines(ssZone$poissonFond$lignes, lwd = 1)
    }
    ## addLines(zpmBA, col=2)
    ## addLines(zpmBA1, col=2)
    ## addLines(posLFA25, col=2)
    ## points(tabPos[,'X'], tabPos[,'Y'], pch=22, bg=couleur[as.numeric(tabPos$id)], cex=1.5)
    ## text(as.numeric(tabPos[,'X']), as.numeric(tabPos[,'Y']), labels=tabPos[,'id'], cex=0.3)
    ## points(tabPos[,'X'], tabPos[,'Y'], pch=22, bg=couleur[as.numeric(tabPos$id)], cex=1.5*7.3/widthPourMap)
    ## text(as.numeric(tabPos[,'X']), as.numeric(tabPos[,'Y']), labels=tabPos[,'id'], cex=0.32*7.3/widthPourMap)
    points(
      tabPos[, 'X'],
      tabPos[, 'Y'],
      pch = 22,
      bg = couleur[as.numeric(tabPos$id)],
      cex = 1.5
    )
    text(
      as.numeric(tabPos[, 'X']),
      as.numeric(tabPos[, 'Y']),
      labels = tabPos[, 'id'],
      cex = 0.32
    )
    ## points(tabPos[,'X'], tabPos[,'Y'], pch=22, bg=couleur[as.numeric(tabPos$id)], cex=2)
    ## text(as.numeric(tabPos[,'X']), as.numeric(tabPos[,'Y']), labels=tabPos[,'id'], cex=0.5)
    if (Field1) {
      text(
        as.numeric(tabPos[, 'X']),
        as.numeric(tabPos[, 'Y']),
        labels = tabPos[, 'Field1'],
        cex = 0.4,
        pos = 3
      )
    }
    lesquels <- which(tabPos$priorite2 %in% c('choisie', 'supplementaire4Ra'))
    for (i.pos in lesquels) {
      if (cercle) {
        lines(cercleRayon(
          x = as.numeric(tabPos[i.pos, 'X']),
          y = as.numeric(tabPos[i.pos, 'Y']),
          rayon = 1.852 / 2
        ))
      }
    }
    if (!is.null(explo)) {
      points(explo[, 1], explo[, 2], pch = 22, bg = 'white', cex = 1.8)
      text(explo[, 1], explo[, 2], labels = letters[1:nrow(explo)], cex = 0.4)
    }
    if (inclureAnneeCourante) {
      text(
        mean(par('usr')[c(1, 2)]),
        par('usr')[c(3)] + (par('usr')[c(4)] - par('usr')[c(3)]) * 0.9,
        anneeCourante,
        cex = 3
      )
    }
    if (!is.null(legende)) {
      legend(
        'topleft',
        inset = 0.03,
        legend = legende,
        fill = legende.couleur,
        bg = 'white'
      )
    }
    if (!ecran) dev.off()
  }

  carteProtocole(
    tabPos = subset(stationsFinales, id <= 125),
    anneeCourante = annee,
    couleur = 'white',
    inclureAireEtude = FALSE,
    dirOutput = dir_stations
  )
  carteProtocole(
    tabPos = subset(stationsFinales, id <= 125),
    anneeCourante = annee,
    couleur = 'white',
    inclureAireEtude = FALSE,
    dirOutput = dir_stations,
    nomPng = 'carteProtocoleGrosse',
    widthPourMap = 9.3
  )

  ## so
  carteProtocole(
    tabPos = stations,
    xlim = c(-70, -62),
    ylim = c(45.5, 49.5),
    anneeCourante = annee,
    couleur = 'white',
    inclureAireEtude = FALSE,
    nomPng = 'carteProtocole_so',
    courbeNiveau = -seq(0, 550, by = 25),
    inclureFondBleu = FALSE,
    widthPourMap = 9.3
  )
  ## carteProtocole(tabPos=stationsFinales, xlim=c(-70,-62), ylim=c(45.5,49.5),
  ##                anneeCourante=annee.courante, couleur='white', inclureAireEtude=FALSE, nomPng='carteProtocole_so', courbeNiveau=-seq(0,550,by=25), inclureFondBleu=FALSE,
  ##                widthPourMap=9.3)
  ## carteProtocole(tabPos=subset(stationsTt, choix%in%c('base','exploratoireSud', 'supplementaire4Ra')), xlim=c(-70,-62), ylim=c(45.5,49.5),
  ##                anneeCourante=annee.courante, couleur=st, inclureAireEtude=FALSE, nomPng='carteProtocole_soCol', courbeNiveau=-seq(0,550,by=25), inclureFondBleu=FALSE,
  ##                widthPourMap=9.3)
  ## se
  ## carteProtocole(tabPos=subset(stationsTt, choix%in%c('base','exploratoireSud', 'supplementaire4Ra')), xlim=c(-64,-55), ylim=c(45.5,49.5),
  carteProtocole(
    tabPos = stationsFinales,
    xlim = c(-64, -55),
    ylim = c(45.5, 49.5),
    anneeCourante = annee.courante,
    couleur = 'white',
    inclureAireEtude = FALSE,
    nomPng = 'carteProtocole_se',
    courbeNiveau = -seq(0, 550, by = 25),
    inclureFondBleu = FALSE,
    widthPourMap = 9.3
  )
  carteProtocole(
    tabPos = subset(
      stationsTt,
      choix %in% c('base', 'exploratoireSud', 'supplementaire4Ra')
    ),
    xlim = c(-64, -55),
    ylim = c(45.5, 49.5),
    anneeCourante = annee.courante,
    couleur = st,
    inclureAireEtude = FALSE,
    nomPng = 'carteProtocole_seCol',
    courbeNiveau = -seq(0, 550, by = 25),
    inclureFondBleu = FALSE,
    widthPourMap = 9.3
  )
  ## no
  carteProtocole(
    tabPos = subset(
      stationsTt,
      choix %in% c('base', 'exploratoireSud', 'supplementaire4Ra')
    ),
    xlim = c(-70, -62),
    ylim = c(48.5, 52.4),
    anneeCourante = annee.courante,
    couleur = 'white',
    inclureAireEtude = FALSE,
    nomPng = 'carteProtocole_no',
    courbeNiveau = -seq(0, 550, by = 25),
    inclureFondBleu = FALSE,
    widthPourMap = 9.3
  )
  carteProtocole(
    tabPos = stationsFinales,
    xlim = c(-70, -62),
    ylim = c(48.5, 52.4),
    anneeCourante = annee.courante,
    couleur = st,
    inclureAireEtude = FALSE,
    nomPng = 'carteProtocole_noCol',
    courbeNiveau = -seq(0, 550, by = 25),
    inclureFondBleu = FALSE,
    widthPourMap = 9.3
  )
  ## ne
  carteProtocole(
    tabPos = subset(
      stationsTt,
      choix %in% c('base', 'exploratoireSud', 'supplementaire4Ra')
    ),
    xlim = c(-64, -55),
    ylim = c(48.5, 52.4),
    anneeCourante = annee.courante,
    couleur = 'white',
    inclureAireEtude = FALSE,
    nomPng = 'carteProtocole_ne',
    courbeNiveau = -seq(0, 550, by = 25),
    inclureFondBleu = FALSE,
    widthPourMap = 9.3
  )
  carteProtocole(
    tabPos = stationsFinales,
    xlim = c(-64, -55),
    ylim = c(48.5, 52.4),
    anneeCourante = annee.courante,
    couleur = st,
    inclureAireEtude = FALSE,
    nomPng = 'carteProtocole_neCol',
    courbeNiveau = -seq(0, 550, by = 25),
    inclureFondBleu = FALSE,
    widthPourMap = 9.3
  )
}
