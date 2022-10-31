xml <- XML::xmlParse("~/Desktop/uc6-wound-plate.PlateMap")
row <- XML::xpathSApply(xml, "//well", xmlGetAttr, "row")
col <- XML::xpathSApply(xml, "//well", xmlGetAttr, "col")
wells <- dplyr::tibble(row, col)

tidy_well <- function(xml, row, col) {
  q <- paste0("//well[@row='", row, "' and @col='", col, "']/items/wellItem")
  q_res <- XML::xpathSApply(xml, q, tidy_attrs, simplify = F)
  r <- paste0(q, "/referenceItem")
  r_res <- XML::xpathSApply(xml, r, tidy_attrs, simplify = F)

  mapply(rbind, q_res, r_res, SIMPLIFY = F) |>
    lapply(tidy_df) |>
    dplyr::bind_rows() |>
    dplyr::mutate(.row = as.numeric(row) + 1, .col = as.numeric(col) + 1) |>
    relocate(.row, .col) |>
    group_by(.row, .col) |>
    fill(everything(), .direction = "downup") |>
    distinct() |>
    ungroup()
}

tidy_attrs <- function(x) {
  x |>
    XML::xmlAttrs() |>
    dplyr::as_tibble(rownames = "desc")
}

tidy_df <- function(x) {
  filtered_x <- x |>
    dplyr::distinct() |>
    dplyr::filter(desc != "colorArgb") # Color doesn't mean much here

  name <- filtered_x[which(filtered_x$desc == "type"), 2][[1]] |>
    stringr::str_replace_all(" ", "_")

  filtered_x |>
    dplyr::filter(desc != "type") |>
    dplyr::mutate(desc = paste0(name, "_", desc)) |>
    tidyr::pivot_wider(names_from = desc, values_from = value)
}

out <- purrr::map2(row, col, tidy_well, xml = ml) |>
  bind_rows()
