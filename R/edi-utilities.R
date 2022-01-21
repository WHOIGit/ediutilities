#' @importFrom magrittr `%>%`
#' @import readr dplyr readxl xml2 plyr
#' @import EML EMLassemblyline
#' @import rjson

table_to_tsv <- function(table, output.path) {
  utils::write.table(table, output.path, quote=FALSE, na="", sep="\t", row.names=FALSE)
}

read_sheet <- function(excel.path, sheet.name) {
  return(readxl::read_excel(path=excel.path, sheet=sheet.name, na="NA"))
}

sheet_to_tsv <- function(excel.path, sheet.name, output.path) {
  if (sheet.name %in% readxl::excel_sheets(path=excel.path)) {
    table <- read_sheet(excel.path, sheet.name)
    table_to_tsv(table, output.path)
  }
}

#' takes user-provided Excel template and converts to formats needed by
#' the EML assembly line
#' @export
excel_to_template <- function(metadata_path, edi_filename, rights, bbox=FALSE, other_info=FALSE) {
  # FIXME use an output directory rather than writing everything to the root directory

  excel_path = glue::glue('{metadata_path}.xlsx')

  sheet_to_tsv(excel_path, 'ColumnHeaders', glue::glue('attributes_{edi_filename}.txt'))
  sheet_to_tsv(excel_path, 'Personnel', 'personnel.txt')
  sheet_to_tsv(excel_path, 'Keywords', 'keywords.txt')
  sheet_to_tsv(excel_path, 'CategoricalVariables', glue::glue('catvars_{edi_filename}.txt'))
  sheet_to_tsv(excel_path, 'CustomUnits', 'custom_units.txt')

  # Import abstract and methods
  EMLassemblyline::template_core_metadata(path = here::here(), license = rights)
  # this will not overwrite existing files

  # if there is no additional information (default), eliminate the template
  # TODO determine if this is necessary
  if(isFALSE(other_info)) {
    unlink(here::here("additional_info.txt"))
  }
}

# Define Coverage for make_eml ----------

# date, lat, and lon columns must be identified as input for this function
# Compiles a list of geographic and temporal coverage
#' @export
data_coverage <- function(dates, lat, lon) {
  # Temporal coverage 
  # Will need this in make_eml YYYY-MM-DD
  startdate <- min(dates, na.rm = TRUE)
  enddate <- max(dates, na.rm = TRUE)
  # temporal.coverage argument is expecting objects of 'character' class, not 'Date'
  startdate_as_character <- as.character(startdate)
  enddate_as_character <- as.character(enddate)
  
  # Geographic coverage
  # Will need this order in make_eml: North, East, South, West
  North <- round(max(lat, na.rm = TRUE), 5)
  East <- round(max(lon, na.rm = TRUE), 5)
  South <- round(min(lat, na.rm = TRUE), 5)
  West <- round(min(lon, na.rm = TRUE), 5)
  
  return(list("startdate" = startdate_as_character, "enddate" = enddate_as_character,
              "North" = North, "East" = East, "South" = South, "West" = West))
}
# Insert Custom Project Node ------------
# required packages: xml2

# Function inserts project node after the methods node of an xml document
# requires the existance of a parent_project.txt
# input path to xml file

#' @export
project_insert <- function(edi_pkg) {
  if (!file.exists("parent_project.txt")) {
    stop("parent_project.txt does not exist")
  }
  # read in parent project and xml file to be modified
  newnode <- xml2::read_xml("parent_project.txt", from = "xml")
  xml_file <- xml2::read_xml(paste0(here::here(), "/", edi_pkg, ".xml"), from = "xml")
  
  # replace existant project node
  if (is.na(xml2::xml_find_first(xml_file, ".//project")) == FALSE) {
    # find old project node
    oldnode <- xml2::xml_find_first(xml_file, ".//project") # find project node
    # replace with new project node
    xml2::xml_replace(oldnode, newnode)
    warning("<project> node already existed but was overwritten")
  }
  # insert new project node
  if (is.na(xml2::xml_find_first(xml_file, ".//project")) == TRUE) {
    # find methods node
    methodsnode <- xml2::xml_find_first(xml_file, ".//methods")
    # add project node after methods and before dataTable
    xml2::xml_add_sibling(methodsnode, newnode, where = "after")
  }
  # validate script
  if (EML::eml_validate(xml_file) == FALSE) {
    warning("XML document not valid")
  }
  # return(xml_file)
  xml2::write_xml(xml_file, paste0(here::here(), "/", edi_pkg, ".xml"))
}

#' @export
merge_csv_directory <- function(dir) {
  return(list.files(path=dir, full.names = TRUE) %>%
    lapply(readr::read_csv) %>%
    dplyr::bind_rows)
}

# interacting with the NES-LTER REST API

#' @export
api_list_cruises <- function() {
  return(rjson::fromJSON(file="https://nes-lter-data.whoi.edu/api/cruises")$cruises)
}

#' @export
read_from_api <- function(type, cruises) {
  # expand the cruises into a dataframe (avoids nested for loops)
  z <- expand.grid(cruises)
  
  # read in data based on the specified source
  if (type == "metadata") {
    urls <- paste0("https://nes-lter-data.whoi.edu/api/ctd/", z$Var1, "/metadata.csv")
    urls <- unlist(urls)
  }
  if (type == "summary") {
    urls <- paste0("https://nes-lter-data.whoi.edu/api/ctd/", z$Var1, "/bottle_summary.csv")
    urls <- unlist(urls)
  }
  if (type == "nutrient") {
    urls <- paste0("https://nes-lter-data.whoi.edu/api/nut/", z$Var1, ".csv")
    urls <- unlist(urls)
  }
  if (type == "chl") {
    urls <- paste0("https://nes-lter-data.whoi.edu/api/chl/", z$Var1, ".csv")
    urls <- unlist(urls)
  }
  if (type == "bottles") {
    urls <- paste0("https://nes-lter-data.whoi.edu/api/ctd/", z$Var1, "/bottles.csv")
    urls <- unlist(urls)
  }
  if (type == "stations") {
    urls <- paste0("https://nes-lter-data.whoi.edu/api/stations/", z$Var1, ".csv")
    urls <- unlist(urls)
  }
  
  ## Cruise Compilation ##
  # case: more than one cruise given
  if (length(cruises) > 1) {
    # begin compilation  
    prev_cruise <- readr::read_csv(urls[1])
    
    if (isFALSE("cruise" %in% names(prev_cruise))) {
      prev_cruise$cruise <- toupper(cruises[1])
    }
    
    # loop through urls to compile cruise data into one file
    for (k in 2:length(urls)){
      # read in data per cruise
      next_cruise <- readr::read_csv(urls[k])
      
      if (isFALSE("cruise" %in% names(next_cruise))) {
        next_cruise$cruise <- toupper(cruises[k])
      }
      
      # bind the next cruise to the compiled cruise dataset
      all <- plyr::rbind.fill(prev_cruise, next_cruise)
      
      # if statment to reset the previous cruises until all cruises are read in
      if(k < length(urls)) {
        prev_cruise <- all
      }
    }
    return(all)
    
    # case: only one cruise is given
  } else {
    all <- readr::read_csv(urls)
    return(all)
  }
}
