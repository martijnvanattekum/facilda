#' Detects annotation identifiers and converts them to the requested format
#'
#' Uses the org.Hs.eg.db database to detect the keytype of the input vector
#' and converts it to the keytype defined by return_keytype.
#' When multiple target ids are found for a source id, the first will be
#' returned.
#'
#' @param ids vector containing the original ids, all of the same keytype
#' @param return_keytype format which to convert ids to. Options can be found
#' by running library(org.Hs.eg.db); keytypes(org.Hs.eg.db)
#' @return named vector containing the converted ids, named with the original
#' ids
#' @examples
#' ensembl_ids <- c("ENSG00000132475.8", "ENSG00000185339.8")
#' symbols <- automap_ids(ensembl_ids)

#' @import org.Hs.eg.db
#' @importFrom purrr map set_names
#' @importFrom AnnotationDbi keys mapIds keytypes
#' @export
automap_ids <- function (ids, return_keytype = "SYMBOL", species = "human") {

  stopifnot(species %in% c("human", "mouse"))

  db <- switch(species,
               human = org.Hs.eg.db::org.Hs.eg.db,
               mouse = org.Mm.eg.db::org.Mm.eg.db)

  db_ids_by_type <- purrr::map(AnnotationDbi::keytypes(db) %>%
                                 purrr::set_names(), ~AnnotationDbi::keys(db, .x))
  original_ids <- ids
  ids_for_conversion <- ids %>% as.character() %>% stringr::str_replace("((^ENS[GPTM])[^\\.]*)\\.[0-9]+",
                                                                        "\\1") %>% stringr::str_replace("((^[NXY][MPR]_)[^\\.]*)\\.[0-9]+",
                                                                                                        "\\1")
  perc_ids_found_per_keytype <- purrr::map_dbl(db_ids_by_type,
                                               ~mean(ids_for_conversion %in% .x))
  if (all(perc_ids_found_per_keytype < 0.2))
    stop(paste0("Type of ids not recognized. Ensure that the ids are valid and ",
                "you selected the correct species. Valid types are: ",
                paste(names(db_ids_by_type), collapse = ", ")))
  detected_keytype <- names(perc_ids_found_per_keytype)[which.max(perc_ids_found_per_keytype)]
  AnnotationDbi::mapIds(db, keys = ids_for_conversion,
                        column = return_keytype, keytype = detected_keytype,
                        multiVals = "first") %>% set_names(original_ids)
}

