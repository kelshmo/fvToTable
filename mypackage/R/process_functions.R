# Test - PEC data tracking, track data available to the consortia, note modified, new and deleted files.
# Author: Kelsey Montgomery
# Date: 2019.08.01
#'
#' Get Synapse fileview or table
#'
#' @param fileview_id A synId c().
#' @return A tibble.
#' @examples
#' fv <- get_fv("syn18691012")
#' table <- get_fv("syn20555115")
get_fv <- function(fileview_id){
  fileview <- synapser::synTableQuery(paste0("SELECT * FROM ", fileview_id))
  readr::read_csv(fileview$filepath, col_types = readr::cols(.default = "c"))
}
#' Bind new files to the existing table, notes deleted files and file/annotation modifications or checks only for file/annotation modifications
#'
#' @param fv A tibble. A fileview imported with get_fv.
#' @param table A tibble. A table imported with get_fv.
#' @return A tibble with notation added to the notes column. ROW_ID and ROW_VERSION remain from existing table to enable storage to Synapse.
#' @example
#' mod_table(fv = get_fv("synId"), table = get_fv("synId"))
mod_table <- function(fv, table){
  verIds <- check_version(fv,table)
  annotIds <- check_etag(fv, table)
  excludeCols <- c("ROW_ID", "ROW_ETAG", "ROW_VERSION")
  includeCols <- names(fv)[!(names(fv) %in% excludeCols)]

  if (!setequal(fv$id,table$id[!(table$notes %in% c("File removed"))])) {
    new_entries <- setdiff(fv$id, table$id)
    del_entries <- setdiff(table$id, fv$id)
    current_delIds <- table$id[table$notes %in% c("File removed")]
    bound <- dplyr::bind_rows(table, fv[fv$id %in% new_entries,includeCols]) %>%
      mutate(notes = ifelse(id %in% del_entries[!(del_entries %in% current_delIds)],
                            c("File removed"),
                            notes)) %>%
      mutate(modifiedOn = ifelse(id %in% del_entries[!(del_entries %in% current_delIds)],
                                 round(as.numeric(base::Sys.time())),
                                 modifiedOn))
    coalesce_join(fv[,includeCols], bound, by = "id") %>%
      mutate(notes = ifelse(id %in% annotIds, "Annotations modified", notes)) %>%
      mutate(notes = ifelse(id %in% verIds, "File modified ", notes)) %>%
      mutate(notes = ifelse(id %in% new_entries, "File added", notes))
  } else {
    coalesce_join(fv[,includeCols], table, by = "id") %>%
      mutate(notes = ifelse(id %in% annotIds, "Annotations modified", notes)) %>%
      mutate(notes = ifelse(id %in% verIds, "File modified ", notes))
  }
}
#' Compare the fileview and table etag to note changes to the annotations or file metadata itself. This would indicate a user needs to update the metadata associated with the file.
#'
#' @param fv A tibble. A fileview imported with get_fv.
#' @param table A tibble. A table imported with get_fv.
#' @return A string of synIds where file metadata has changed, including modifications to the annotations.
#' @example
#' check_etag(fv = get_fv("synId"), table = get_fv("synId"))
check_etag <- function(fv,
                       table){
  new_key <- paste0(fv$etag, "_", fv$id)
  key <- paste0(table$etag, "_", table$id)
  if (!setequal(new_key, key)) {
    toUpdate <- setdiff(new_key, key)
    annotIds <- sapply(stringr::str_split(toUpdate, "_"), "[[", 2 )
    annotIds
     } else {
    c("No changes to existing annotations")
  }
}
#' Compare the fileview and table etag to note file metadata or file itself has been modified. This would indicate a user needs to download a new version.
#' @param fv A tibble. A fileview imported with get_fv.
#' @param table A tibble. A table imported with get_fv.
#' @return A string of synIds where file has changed.
#' @example
#' check_version(fv = get_fv("synId"), table = get_fv("synId"))
check_version <- function(fv,
                          table){
  new_key <- paste0(fv$currentVersion, "_", fv$id)
  key <- paste0(table$currentVersion, "_", table$id)
  if (!setequal(new_key, key)) {
    toUpdate <- setdiff(new_key, key)
    verIds <- sapply(stringr::str_split(toUpdate, "_"), "[[", 2 )
    verIds
  } else {
    ("No changes to existing files")
  }
}
#' Fileview is joined to the existing table by synId, prioritizing the fileview annotations which are assummed to be the most updated information. dplyr::coalesce() prioritizes the first non-missing value at each position thus prioritizing the fileview annotations.
#'
#'@param x A tibble. The data is prioritized during coalesce() thus replacing data in y.
#'@param y A tibble.
#'@return A tibble with the most relevant metadata and notation.
#'@example
#'coalesce_join(x = fv, y = table)
coalesce_join <- function(x,
                          y,
                          by = NULL, suffix = c(".x", ".y"),
                          join = dplyr::full_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
  cols <- union(names(x), names(y))
  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  to_coalesce <- unique(substr(
    to_coalesce,
    1,
    nchar(to_coalesce) - nchar(suffix_used)
  ))

  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]],
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce

  dplyr::bind_cols(joined, coalesced)[cols]
}
#' Store updated table to Synapse
#'
#'@param table_to_store A tibble. Output from mod_table()
#'@param fileview_id A synId c().
#'@return A Synapse client object with storage metadata.
store <- function(table_to_store, fileview_id){
  synapser::synStore(synapser::Table(fileview_id, table_to_store))
}
