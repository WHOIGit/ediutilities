#' @importFrom magrittr `%>%`

table_to_tsv <- function(table, output.path) {
  utils::write.table(table, output.path, quote=FALSE, na="", sep="\t", row.names=FALSE)
}

read_sheet <- function(excel.path, sheet.name) {
  return(readxl::read_excel(path=excel.path, sheet=sheet.name, na="NA"))
}

sheet_to_tsv <- function(excel.path, sheet.name, output.path) {
  if (sheet.name %in% excel_sheets(path=excel.path)) {
    table <- read_sheet(excel.path, sheet.name)
    table_to_tsv(table, output.path)
  }
}

#' takes user-provided Excel template and converts to formats needed by
#' the EML assembly line
excel_to_template <- function(metadata_path, edi_filename, rights, bbox=FALSE, other_info=FALSE) {
  # FIXME use an output directory rather than writing everything to the root directory

  excel_path = glue::glue('{metadata_path}.xlsx')

  sheet_to_tsv(excel_path, 'ColumnHeaders', glue::glue('attributes_{edi_filename}.txt'))
  sheet_to_tsv(excel_path, 'Personnel', 'personnel.txt')
  sheet_to_tsv(excel_path, 'Keywords', 'keywords.txt')
  sheet_to_tsv(excel_path, 'CategoricalVariables', glue::glue('catvars_{edi_filename}.txt'))
  sheet_to_tsv(excel_path, 'CustomUnits', 'custom_units.txt')

  # if there is no additional information (default), eliminate the template
  if(isFALSE(other_info)) {
    unlink(here("additional_info.txt"))
  }
}

merge_csv_directory <- function(dir) {
  return(list.files(path=dir, full.names = TRUE) %>%
    lapply(readr::read_csv) %>%
    dplyr::bind_rows)
}

