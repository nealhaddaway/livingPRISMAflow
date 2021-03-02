#' Read in Data for LSR Prisma Flow Diagram
#' 
#' @description Function to read in data for 'LSRPrisma_flow()' from 
#' the template file. The data are stored in columns, with each column 
#' being an additional data point in a LSR incremental update.
#' @param data Input data frame based on 'inst/extdata/template.csv'.
#' @return List of objects needed by 'LSRPRISMA_flow()'.
#' @importFrom stringr str_count
#' @examples 
#' \dontrun{
#' data <- read.csv('inst/extdata/approach4.csv')
#' flowdata <- LSRPrisma_data(data)
#' }
#' @export
LSRPrisma_data <- function (data){
  
  #Subset relevant columns from data
  cols <- ncol(data)
  dataset <- data.frame(data[8:cols])
    names(dataset) <- gsub('.',' ',colnames(dataset), fixed = TRUE)
    names(dataset) <- gsub('  ',', ',colnames(dataset), fixed = TRUE)
    colsnew <- ncol(dataset)
  
  #Create objects for each flow diagram input
  previous_studies <- dataset[1,1]
  previous_reports <- dataset[2,1]
  database_results <- data.frame(dataset[3,])
    colnames(database_results) <- names(dataset)
    rownames(database_results) <- NULL
  register_results <- data.frame(dataset[4,])
    colnames(register_results) <- names(dataset)
    rownames(register_results) <- NULL
  other_ident_web <- dataset[5,]
  other_ident_org <- dataset[6,]
  other_ident_cit <- dataset[7,]
  db_excl_dup <- dataset[8,]
  db_excl_aut <- dataset[9,]
  db_excl_oth <- dataset[10,]
  records_screened <- data.frame(dataset[11,])
    colnames(records_screened) <- names(dataset)
    rownames(records_screened) <- NULL
  records_excluded <- data.frame(dataset[12,])
    colnames(records_excluded) <- names(dataset)
    rownames(records_excluded) <- NULL
  dbr_sought_reports <- data.frame(dataset[13,])
    colnames(dbr_sought_reports) <- names(dataset)
    rownames(dbr_sought_reports) <- NULL
  dbr_notretrieved_reports <- data.frame(dataset[14,])
    colnames(dbr_notretrieved_reports) <- names(dataset)
    rownames(dbr_notretrieved_reports) <- NULL
  other_sought_reports <- data.frame(dataset[15,])
    colnames(other_sought_reports) <- names(dataset)
    rownames(other_sought_reports) <- NULL
  other_notretrieved_reports <- data.frame(dataset[16,])
    colnames(other_notretrieved_reports) <- names(dataset)
    rownames(other_notretrieved_reports) <- NULL
  dbr_assessed <- data.frame(dataset[17,])
    colnames(dbr_assessed) <- names(dataset)
    rownames(dbr_assessed) <- NULL
  dbr_excluded <- stringr::str_split_fixed(dataset[18,], '; ', 2)
    rownames(dbr_excluded) <- names(dataset)
    dbr_excluded <- t(data.frame(subset(dbr_excluded, dbr_excluded[,1] != "0")))
    if (colsnew > 1) {
      dbr_excluded <- sort(paste0(paste(dbr_excluded, colnames(dbr_excluded), sep = " ("), ")"))
    } 
    dbr_excluded <- paste(subset(dbr_excluded, substring(dbr_excluded, 1, 1) != " "), collapse = '\n')
  dbr_excluded_data <- trimws(gsub("[^0-9 ]", "", dataset[18,])) # cannot get it in the right format
    dbr_excluded_data <- t(data.frame(sapply(strsplit(dbr_excluded_data, ' '), function(x) sum(as.numeric(x, na.rm = TRUE), na.rm = TRUE))))
    rownames(dbr_excluded_data) <- NULL
    colnames(dbr_excluded_data) <- names(dataset)
  excl_lines <- stringr::str_count(dbr_excluded, '\n') + 1
  other_assessed <- data.frame(dataset[19,])
    colnames(other_assessed) <- names(dataset)
    rownames(other_assessed) <- NULL
  other_excluded <- stringr::str_split_fixed(dataset[20,], '; ', 2)
    rownames(other_excluded) <- names(dataset)
    other_excluded <- t(data.frame(subset(other_excluded, other_excluded[,1] != "0")))
    if (colsnew > 1) {
      other_excluded <- sort(paste0(paste(other_excluded, colnames(other_excluded), sep = " ("), ")"))
    }
    other_excluded <- paste(subset(other_excluded, substring(other_excluded, 1, 1) != " "), collapse = '\n')
  other_excluded_data <- trimws(gsub("[^0-9 ]", "", dataset[20,])) # cannot get it in the right format
    other_excluded_data <- t(data.frame(sapply(strsplit(other_excluded_data, ' '), function(x) sum(as.numeric(x, na.rm = TRUE), na.rm = TRUE))))
    rownames(other_excluded_data) <- NULL
    colnames(other_excluded_data) <- names(dataset)
  excl_lines_other <- stringr::str_count(other_excluded, '\n') + 1
  new_studies <- data.frame(dataset[21,])
    colnames(new_studies) <- names(dataset)
    rownames(new_studies) <- NULL
  new_reports <- data.frame(dataset[22,])
    colnames(new_reports) <- names(dataset)
    rownames(new_reports) <- NULL
  total_studies <- data.frame(dataset[23,])
    colnames(total_studies) <- names(dataset)
    rownames(total_studies) <- NULL
  total_reports <- data.frame(dataset[24,])
    colnames(total_reports) <- names(dataset)
    rownames(total_reports) <- NULL
  
  #Build the list of objects to be returned
  data <- list(previous_studies = previous_studies,
               previous_reports = previous_reports,
               database_results = database_results,
               register_results = register_results,
               other_ident_web = other_ident_web,
               other_ident_org = other_ident_org,
               other_ident_cit = other_ident_cit,
               db_excl_dup = db_excl_dup,
               db_excl_aut = db_excl_aut,
               db_excl_oth = db_excl_oth,
               records_screened = records_screened,
               records_excluded = records_excluded,
               dbr_sought_reports = dbr_sought_reports,
               dbr_notretrieved_reports = dbr_notretrieved_reports,
               other_sought_reports = other_sought_reports,
               other_notretrieved_reports = other_notretrieved_reports,
               dbr_assessed = dbr_assessed,
               dbr_excluded = dbr_excluded,
               dbr_excluded_data = dbr_excluded_data,
               excl_lines = excl_lines,
               other_assessed = other_assessed,
               other_excluded = other_excluded,
               other_excluded_data = other_excluded_data,
               excl_lines_other = excl_lines_other,
               new_studies = new_studies,
               new_reports = new_reports,
               total_studies = total_studies,
               total_reports = total_reports)
  
  return(data)
}
