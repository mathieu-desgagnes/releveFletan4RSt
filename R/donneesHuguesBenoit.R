# sourceRecap <- file.path('S:','Flétan','Retour de marquage','BD_retourDeTag','retour des tags_ALL'),
# destination <- file.path('S:','Flétan','Retour de marquage','Analyses','input','recap_vTrav')

#' Produit un fichier données utilisées par Hugues Benoit pour ses travaux sur la mortalité naturelle et survie suite au rejet par la pêche
#'
#' @param source chemin d'accès de la base de données originale
#' @param destination chemin d'accès où enregistrée les données formattées
#'
#' @returns le dataframe enregistré
#' @export
#'
#' @examples
donneesHuguesBenoit <- function(sourceRecap, destination) {
  # lecture des données de marquage

  # lecture des données de recaptures
  temp <- lecture_recaptures(
    origine = sourceRecap,
    destination = file.path(
      'S:',
      'Flétan',
      'Retour de marquage',
      'Analyses',
      'input',
      'recap_vTrav'
    ),
    anneeCourante = 2025
  )
  # combinaison des deux jeux de données
  # mise en format
}
