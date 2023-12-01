utils::globalVariables(c(".", "dot_color", "dot_label", "has_changed", "is_deg",
                         "is_relevant", "log2FoldChange", "padj", "symbol"))
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
#' @param species either "human" or "mouse" to indicate from which species the
#' identifiers are
#' @return named vector containing the converted ids, named with the original
#' ids
#' @examples
#' ensembl_ids <- c("ENSG00000132475.8", "ENSG00000185339.8")
#' symbols <- automap_ids(ensembl_ids)

#' @import org.Hs.eg.db org.Mm.eg.db
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

#' Annotates deseq result with metadata from a SummarizedExperiment
#'
#' The joining will be done on the ensg column
#'
#' @param deseq_results A DESeq results table returned from DESeq2::results()
#' @param se The SummarizedExperiment from which to extract the additional data.
#' Must contain a column named ensg in its rowdata
#' @param row_data_cols_to_add a character vector containing the colnames from
#' the se's rowdata
#' @return a tibble with the information added and incomplete cases removed
#'
#' @importFrom cleanse get_row_data
#' @importFrom tibble rownames_to_column tibble
#' @importFrom dplyr filter arrange left_join
annotate_results_table <- function(deseq_results, se, row_data_cols_to_add) {

  stopifnot(all(row_data_cols_to_add %in% colnames(cleanse::get_row_data(se))))

  # Extract this info to add it back after the contrasts are calculated
  additional_rowdata <- cleanse::get_row_data(se) %>%
    dplyr::select(!!c("ensg", row_data_cols_to_add))

  deseq_results %>%
    data.frame() %>%
    tibble::rownames_to_column("ensg") %>%
    tibble::as_tibble() %>%
    dplyr::filter(!is.na(padj)) %>%
    dplyr::arrange(padj) %>%
    dplyr::left_join(additional_rowdata, by = "ensg") %>%
    dplyr::filter(!is.na(symbol))

}

#' Creates a contrasts table
#'
#' The joining will be done on the ensg column
#'
#' @param se A SummarizedExperiment containing the expression data and the
#' metadata required in the defined contrast
#' @param contrast The variables to contrast. See ?DESeq2::results
#' @param design_char a character that will be converted to the design formula.
#' See ?DESeq2::DESeqDataSet
#' @param ... additional arguments passed to DESeq2::results
#' @return A tibble with the requested contrast table
#'
#' @importFrom cleanse get_row_data options_from_coldata filter
#' @importFrom stringr str_split str_remove_all str_subset str_detect
#' @importFrom DESeq2 DESeqDataSet DESeq results
#' @importFrom purrr reduce
#' @importFrom stats as.formula
#' @export
calculate_contrasts_table <- function(se, contrast, design_char, ...) {

  # Check design_char argument
  design_parameters <- stringr::str_split(design_char, "\\+")[[1]] %>%
    stringr::str_remove_all("[ ~]") %>%
    stringr::str_subset(":", negate = TRUE)  # removes interaction terms

  stopifnot(all(design_parameters %in% colnames(cleanse::get_col_data(se))))

  # Check contrast argument
  if (!is(contrast, "list")) {
    contrast_var <- contrast[1]
    target_contrast_var_values <- contrast[c(2, 3)]
    actual_contrast_var_values <- cleanse::options_from_coldata(se, contrast_var)
    if(!all(target_contrast_var_values %in% actual_contrast_var_values)){
      warning(paste("The supplied se does not contain all values requested to",
                    "create contrasts for in the resultname argument. Contrasts",
                    "cannot be calculated. Requested values: ",
                    paste(target_contrast_var_values, collapse = ", "), "Actual values: ",
                    paste(actual_contrast_var_values, collapse = ", "), "Returning NA."))
      return(NA)}

    stopifnot(stringr::str_detect(design_char, contrast_var))
  }

  # filters only the columns of se that are not NA and finite
  filter_valid_values <- function(se, colname) {

    se_filtered <- cleanse::filter(se, col, !is.na(!!rlang::sym(colname)))
    if (is.numeric(se$colname)){
      se_filtered <- cleanse::filter(se_filtered, col, is.finite(!!rlang::sym(colname)))
    }
    if (ncol(se) != ncol(se_filtered)) {
      warning(paste("se contains samples with NA or infinite values for design parameter ",
                    colname, ". Removing these samples as they cannot be used to ",
                    "calculate contrasts."
      ))}
    se_filtered

  }

  se %>%
    # DESeq cannot handle NA values in design variables - remove those observations
    {purrr::reduce(design_parameters, \(this_se, param) filter_valid_values(this_se, param), .init = .)} %>%
    DESeq2::DESeqDataSet(design = stats::as.formula(design_char)) %>%
    DESeq2::DESeq() %>%
    DESeq2::results(contrast=contrast, ...) %>%
    annotate_results_table(se, row_data_cols_to_add = c("symbol", "biotype"))

}

#' Filters genes with padj <= max_padj and fc >= min_fc from a DESeq results table
#'
#' @param contrasts_tb A contrast table from DESeq2
#' @param max_padj Rows with padj under this value are filtered
#' @param min_abs_log2fc Rows with absolute log2FoldChanage over this value are filtered
#' @return A tibble after filtering
#'
#' @importFrom dplyr filter
#' @export
filter_relevant_genes <- function(contrasts_tb, max_padj, min_abs_log2fc) {

  contrasts_tb %>%
    dplyr::filter(padj <= max_padj,
                  abs(log2FoldChange) >= min_abs_log2fc)

}


#' Creates a volcano plot from a contrasts table
#'
#' Genes for which the absolute fold change is larger than indidated and have
#' have an adjusted p value below the significance_cutoff will be highlighted
#' and labeled (when requested).
#'
#' @param contrast_tb A tibble produced by facilda::calculate_contrasts_table
#' @param ttl The title of the plot
#' @param relevant_fc the absolute fold change above which genes are considered
#' relevant
#' @param significance_cutoff the adjusted p-value below which genes are
#' considered significant
#' @param add_labels whether to label the relevant points
#' @return A ggplot2 object of the volcano plot
#'
#' @importFrom dplyr filter mutate pull
#' @importFrom ggplot2 ggplot aes geom_point scale_x_continuous scale_color_identity
#' @importFrom ggplot2 theme_bw ggtitle geom_hline geom_vline theme element_text
#' @importFrom ggrepel geom_text_repel
#' @export
volcano_plot <- function(contrast_tb, ttl=NULL, relevant_fc=8, significance_cutoff=.01, add_labels = TRUE) {

  plot_data <- contrast_tb %>%
    dplyr::filter(!is.na(padj) & !is.na(log2FoldChange)) %>%
    dplyr::mutate(is_deg = padj < significance_cutoff,
                  has_changed = abs(log2FoldChange) > log2(relevant_fc),
                  is_relevant = is_deg & has_changed) %>%
    mutate(dot_color = ifelse(is_relevant, "orange", "grey")) %>%
    mutate(dot_label = ifelse(is_relevant, symbol, ""))

  x_span <- max(abs(dplyr::pull(plot_data, log2FoldChange)), na.rm=TRUE) * 1.05

  ggplot2::ggplot(plot_data,
                  ggplot2::aes(x=log2FoldChange, y=-log10(padj), color=dot_color, label=dot_label)) +
    ggplot2::geom_point(size=1) +
    ggplot2::scale_x_continuous(limits = c(-x_span, x_span)) +
    ggplot2::scale_color_identity() +
    ggplot2::theme_bw() +
    {if (!is.null(ttl)) ggplot2::ggtitle(ttl)} +
    {if (add_labels) ggrepel::geom_text_repel(seed = 42, color="grey30", max.overlaps=100, segment.ncp = 3, force = 20)} +
    ggplot2::geom_hline(yintercept=-log10(significance_cutoff), linetype="dotted", color = "grey30") +
    ggplot2::geom_vline(xintercept=c(-log2(relevant_fc),log2(relevant_fc)), linetype="dotted", color = "grey30") +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 20))

}
