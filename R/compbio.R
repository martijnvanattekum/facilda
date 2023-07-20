#
# #input: vector of same-type IDs, function will detect the type and map to
# #return_type using the map Ids function of org.Hs.eg.db
# automapIDs <- function(IDs, return_type = "SYMBOL") {
#   all_keys <- sapply(keytypes(org.Hs.eg.db), function(keytype) keys(org.Hs.eg.db, keytype), USE.NAMES = TRUE)
#
#   IDs <- as.character(IDs)
#   IDs_copy <- IDs
#
#   #remove potential trailing .x version number from ensembl and refseq IDs
#   IDs <- gsub("((^ENS[GPT])[^\\.]*)\\.[0-9]+", "\\1", IDs)
#   IDs <- gsub("((^[NXY][MPR]_)[^\\.]*)\\.[0-9]+", "\\1", IDs)
#
#   #determine which keytype (search key) was provided by user
#   IDs_in_keys <- sapply(all_keys, function(keys) mean(IDs %in% keys)) #occurence of IDs in key sets
#   if (all(IDs_in_keys < .2)) stop(paste0("Type of IDs not recognized. Valid types are: ", paste(names(all_keys), collapse = ", ")))
#   keytype <- names(all_keys)[which.max(IDs_in_keys)]  #determine the most likely provided type
#   out <- mapIds(org.Hs.eg.db, keys = IDs, column = return_type, keytype = keytype, multiVals = "first")
#   names(out) <- IDs_copy  #restore original names
#   out
# }
