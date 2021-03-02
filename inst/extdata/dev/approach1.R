

library(ggplot2)
library(gridExtra)
library(ggpmisc)

#' @param data A dataset ready for plotting, produced as a list 
#' of items using `LSRPrisma_data()`.
#' @param 
#' @return A LSR Prisma plot of the data plotted onto a flow 
#' diagram.
#' @examples
#' data <- read.csv('inst/extdata/approach1.csv')
#' data <- LSRPrisma_data(data)
#' plot <- LSRPrisma_flow(data)
#' plot
#' @export
LSRPrisma_flow <- function(data,
                           format = 'PRISMA1'){
  
  #Set font size
  font_size <- 2
  
  #Box dimensions
  #set box sizes
  box_width <- 13
  box_height1 <- 2.5
  box_height2 <- 4
  box_height3 <- 5.9
  basey <- 0
  basex <- 0
  #excluded box heights
  dbrexclheight <- box_height1 + (data$excl_lines * 0.5) + 0.5
  otherexclheight <- box_height1 + (data$excl_lines_other * 0.5) + 0.5
  
  #Table data positions
  #number of columns
  cols <- ncol(data$database_results)
  #x positions for each column
  col_width <- box_width / cols
  col_mid_x <- (seq(1:cols) * col_width) - (col_width / 2)
  
  #Column names
  columns <- gsub(' ', '\n', colnames(flowdata[[3]]))
  
  #Box labels and positions
  #Vertical boxes
  #included_box
  incl_lab <- "Included"
  incl_xmin <- basex #box left limit
  incl_xmax <- incl_xmin + 1 #box right limit
  incl_ymin <- basey #box bottom limit
  incl_ymax <- incl_ymin + (2 * box_height2) + 1 #box top limit
  incl_lab_x <- incl_xmin + ((incl_xmax - incl_xmin) / 2) #x label location
  incl_lab_y <- incl_ymin + ((incl_ymax - incl_ymin) / 2) #y label location
  #screening_box
  screen_lab <- "Screening"
  screen_xmin <- incl_xmin #box left limit
  screen_xmax <- incl_xmax #box right limit
  screen_ymin <- incl_ymax + 1 #box bottom limit
  screen_ymax <- screen_ymin + (3 * box_height1) + 2 #box top limit
  screen_lab_x <- screen_xmin + ((screen_xmax - screen_xmin) / 2) #x label location
  screen_lab_y <- screen_ymin + ((screen_ymax - screen_ymin) / 2) #y label location
  #identification_box
  ident_lab <- "Identification"
  ident_xmin <- incl_xmin #box left limit
  ident_xmax <- incl_xmax #box right limit
  ident_ymin <- screen_ymax + 1 #box bottom limit
  ident_ymax <- ident_ymin + box_height3 #box top limit
  ident_lab_x <- ident_xmin + ((ident_xmax - ident_xmin) / 2) #x label location
  ident_lab_y <- ident_ymin + ((ident_ymax - ident_ymin) / 2) #y label location
  
  #Title boxes
  #previous_box
  prev_lab <- "Previous studies"
  prev_xmin <- incl_xmax + 1 #box left limit
  prev_xmax <- prev_xmin + box_width #box right limit
  prev_ymin <- ident_ymax + 1 #box bottom limit
  prev_ymax <- prev_ymin + 1 #box top limit
  prev_lab_x <- prev_xmin + ((prev_xmax - prev_xmin) / 2) #x label location
  prev_lab_y <- prev_ymin + ((prev_ymax - prev_ymin) / 2) #y label location
  #dbr_box
  dbr_lab <- "Identification of new studies via databases and registers"
  dbr_xmin <- prev_xmax + 1 #box left limit
  dbr_xmax <- dbr_xmin + 1 + (box_width * 2) #box right limit
  dbr_ymin <- prev_ymin #box bottom limit
  dbr_ymax <- prev_ymax #box top limit
  dbr_lab_x <- dbr_xmin + ((dbr_xmax - dbr_xmin) / 2) #x label location
  dbr_lab_y <- dbr_ymin + ((dbr_ymax - dbr_ymin) / 2) #y label location
  #other_box
  other_lab <- "Identification of new studies via other methods"
  other_xmin <- dbr_xmax + 1 #box left limit
  other_xmax <- other_xmin + 1 + (box_width * 2) #box right limit
  other_ymin <- dbr_ymin #box bottom limit
  other_ymax <- dbr_ymax #box top limit
  other_lab_x <- other_xmin + ((other_xmax - other_xmin) / 2) #x label location
  other_lab_y <- other_ymin + ((other_ymax - other_ymin) / 2) #y label location
  
  #Data boxes
  #Total box
  tot_lab1 <- "Total studies included in review"
  tot_lab2 <- "Total reports included in review"
  tot_xmin <- dbr_xmin #box left limit (set by position of box1 right limit)
  tot_xmax <- tot_xmin + box_width #box right limit
  tot_ymin <- basey #start point vertically
  tot_ymax <- tot_ymin + box_height2 #as above
  tot_lab_x1 <- tot_xmin + ((tot_xmax - tot_xmin) / 2) #x label 1 location
  tot_lab_y1 <- tot_ymax - 0.3 #y label 1 location
  tot_data_x1 <- tot_lab_x1 #x data 1 location
  tot_data_y1 <- tot_lab_y1 - 1.4 #y data 1 location
  tot_lab_x2 <- tot_lab_x1 #x label 2 location
  tot_lab_y2 <- tot_data_y1 - 1.2 #y label 2 location
  tot_data_x2 <- tot_lab_x1 #x data 2 location
  tot_data_y2 <- tot_lab_y2 - 0.7 #y data 2 location
  
  #New box
  new_lab1 <- "New studies included in review"
  new_lab2 <- "New reports included in review"
  new_xmin <- dbr_xmin #box left limit (set by position of box1 right limit)
  new_xmax <- new_xmin + box_width #box right limit
  new_ymin <- tot_ymax + 1 #as above
  new_ymax <- new_ymin + box_height2 #as above
  new_lab_x1 <- new_xmin + ((new_xmax - new_xmin) / 2) #x label 1 location
  new_lab_y1 <- new_ymax - 0.3 #y label 1 location
  new_data_x1 <- new_lab_x1 #x data 1 location
  new_data_y1 <- new_lab_y1 - 1.4 #y data 1 location
  new_lab_x2 <- new_lab_x1 #x label 2 location
  new_lab_y2 <- new_data_y1 - 1.2 #y label 2 location
  new_data_x2 <- new_lab_x1 #x data 2 location
  new_data_y2 <- new_lab_y2 - 0.7 #y data 2 location
  
  #DB Reports assessed
  dbrrep_lab <- "Reports assessed for eligibility"
  dbrrep_xmin <- dbr_xmin #as above
  dbrrep_xmax <- dbr_xmin + box_width #as above
  dbrrep_ymin <- new_ymax + 1 #box bottom limit
  dbrrep_ymax <- dbrrep_ymin + box_height1 #box top limit
  dbrrep_lab_x <- dbrrep_xmin + ((dbrrep_xmax - dbrrep_xmin) / 2) #x label location
  dbrrep_lab_y <- dbrrep_ymax - 0.3 #y label location
  dbrrep_data_x <- dbrrep_lab_x #x data location
  dbrrep_data_y <- dbrrep_lab_y - 1.4 #y data location
  
  #DB Reports excluded
  dbrrepexcl_lab <- "Reports excluded"
  dbrrepexcl_xmin <- dbrrep_xmax + 1 #as above
  dbrrepexcl_xmax <- dbrrepexcl_xmin + box_width #as above
  dbrrepexcl_ymax <- dbrrep_ymax #box top limit
  dbrrepexcl_ymin <- dbrrepexcl_ymax - dbrexclheight #box bottom limit
  dbrrepexcl_lab_x <- dbrrepexcl_xmin + ((dbrrepexcl_xmax - dbrrepexcl_xmin) / 2) #x label location
  dbrrepexcl_lab_y <- dbrrepexcl_ymax - 0.3 #y label location
  dbrrepexcl_data_x <- dbrrepexcl_lab_x #x data location
  dbrrepexcl_data_y <- dbrrepexcl_lab_y - 1.4 #y data location
  dbrrepexclreasons_lab_y <- dbrrepexcl_ymax - 2.75 #y reasons location
  
  #other Reports assessed
  otherrep_lab <- "Reports assessed for eligibility"
  otherrep_xmin <- dbrrepexcl_xmax + 1 #as above
  otherrep_xmax <- otherrep_xmin + box_width #as above
  otherrep_ymin <- dbrrep_ymin #box bottom limit
  otherrep_ymax <- otherrep_ymin + box_height1 #box top limit
  otherrep_lab_x <- otherrep_xmin + ((otherrep_xmax - otherrep_xmin) / 2) #x label location
  otherrep_lab_y <- otherrep_ymax - 0.3 #y label location
  otherrep_data_x <- otherrep_lab_x #x data location
  otherrep_data_y <- otherrep_lab_y - 1.4 #y data location
  
  #other Reports excluded
  otherrepexcl_lab <- "Reports excluded"
  otherrepexcl_xmin <- otherrep_xmax + 1 #as above
  otherrepexcl_xmax <- otherrepexcl_xmin + box_width #as above
  otherrepexcl_ymax <- otherrep_ymax #box top limit
  otherrepexcl_ymin <- otherrepexcl_ymax - otherexclheight #box bottom limit
  otherrepexcl_lab_x <- otherrepexcl_xmin + ((otherrepexcl_xmax - otherrepexcl_xmin) / 2) #x label location
  otherrepexcl_lab_y <- otherrepexcl_ymax - 0.3 #y label location
  otherrepexcl_data_x <- otherrepexcl_lab_x #x data location
  otherrepexcl_data_y <- otherrepexcl_lab_y - 1.4 #y data location
  otherrepexclreasons_lab_y <- otherrepexcl_ymax - 2.75 #y reasons location
  
  #DB Reports sought
  dbrrepsought_lab <- "Reports sought for retrieval"
  dbrrepsought_xmin <- dbr_xmin #as above
  dbrrepsought_xmax <- dbr_xmin + box_width #as above
  dbrrepsought_ymin <- dbrrep_ymax + 1 #box bottom limit
  dbrrepsought_ymax <- dbrrepsought_ymin + box_height1 #box top limit
  dbrrepsought_lab_x <- dbrrepsought_xmin + ((dbrrepsought_xmax - dbrrepsought_xmin) / 2) #x label location
  dbrrepsought_lab_y <- dbrrepsought_ymax - 0.3 #y label location
  dbrrepsought_data_x <- dbrrepsought_lab_x #x data location
  dbrrepsought_data_y <- dbrrepsought_lab_y - 1.4 #y data location
  
  #DB Reports not retrieved
  dbrrepnotret_lab <- "Reports not retrieved"
  dbrrepnotret_xmin <- dbrrepexcl_xmin #as above
  dbrrepnotret_xmax <- dbrrepnotret_xmin + box_width #as above
  dbrrepnotret_ymin <- dbrrep_ymax + 1 #box bottom limit
  dbrrepnotret_ymax <- dbrrepnotret_ymin + box_height1 #box top limit
  dbrrepnotret_lab_x <- dbrrepnotret_xmin + ((dbrrepnotret_xmax - dbrrepnotret_xmin) / 2) #x label location
  dbrrepnotret_lab_y <- dbrrepnotret_ymax - 0.3 #y label location
  dbrrepnotret_data_x <- dbrrepnotret_lab_x #x data location
  dbrrepnotret_data_y <- dbrrepnotret_lab_y - 1.4 #y data location
  
  #other Reports sought
  otherrepsought_lab <- "Reports sought for retrieval"
  otherrepsought_xmin <- otherrep_xmin #as above
  otherrepsought_xmax <- otherrepsought_xmin + box_width #as above
  otherrepsought_ymin <- otherrep_ymax + 1 #box bottom limit
  otherrepsought_ymax <- otherrepsought_ymin + box_height1 #box top limit
  otherrepsought_lab_x <- otherrepsought_xmin + ((otherrepsought_xmax - otherrepsought_xmin) / 2) #x label location
  otherrepsought_lab_y <- otherrepsought_ymax - 0.3 #y label location
  otherrepsought_data_x <- otherrepsought_lab_x #x data location
  otherrepsought_data_y <- otherrepsought_lab_y - 1.4 #y data location
  
  #other Reports not retrieved
  otherrepnotret_lab <- "Reports not retrieved"
  otherrepnotret_xmin <- otherrepexcl_xmin #as above
  otherrepnotret_xmax <- otherrepnotret_xmin + box_width #as above
  otherrepnotret_ymin <- otherrepexcl_ymax + 1 #box bottom limit
  otherrepnotret_ymax <- otherrepnotret_ymin + box_height1 #box top limit
  otherrepnotret_lab_x <- otherrepnotret_xmin + ((otherrepnotret_xmax - otherrepnotret_xmin) / 2) #x label location
  otherrepnotret_lab_y <- otherrepnotret_ymax - 0.3 #y label location
  otherrepnotret_data_x <- otherrepnotret_lab_x #x data location
  otherrepnotret_data_y <- otherrepnotret_lab_y - 1.4 #y data location
  
  #DB Records screened
  dbrrecscr_lab <- "Records screened"
  dbrrecscr_xmin <- dbr_xmin #as above
  dbrrecscr_xmax <- dbr_xmin + box_width #as above
  dbrrecscr_ymin <- dbrrepsought_ymax + 1 #box bottom limit
  dbrrecscr_ymax <- dbrrecscr_ymin + box_height1 #box top limit
  dbrrecscr_lab_x <- dbrrecscr_xmin + ((dbrrecscr_xmax - dbrrecscr_xmin) / 2) #x label location
  dbrrecscr_lab_y <- dbrrecscr_ymax - 0.3 #y label location
  dbrrecscr_data_x <- dbrrecscr_lab_x #x data location
  dbrrecscr_data_y <- dbrrecscr_lab_y - 1.4 #y data location
  
  #DB Records excluded
  dbrrecexcl_lab <- "Records excluded"
  dbrrecexcl_xmin <- dbrrepexcl_xmin #as above
  dbrrecexcl_xmax <- dbrrecexcl_xmin + box_width #as above
  dbrrecexcl_ymin <- dbrrepnotret_ymax + 1 #box bottom limit
  dbrrecexcl_ymax <- dbrrecexcl_ymin + box_height1 #box top limit
  dbrrecexcl_lab_x <- dbrrecexcl_xmin + ((dbrrecexcl_xmax - dbrrecexcl_xmin) / 2) #x label location
  dbrrecexcl_lab_y <- dbrrecexcl_ymax - 0.3 #y label location
  dbrrecexcl_data_x <- dbrrecexcl_lab_x #x data location
  dbrrecexcl_data_y <- dbrrecexcl_lab_y - 1.4 #y data location
  
  #DB results identified
  dbident_lab <- "Records identified from:"
  dbident_lab1 <- "Databases"
  dbident_lab2 <- "Registers"
  dbident_xmin <- dbr_xmin #box left limit (set by position of box1 right limit)
  dbident_xmax <- dbident_xmin + box_width #box right limit
  dbident_ymax <- dbrrecexcl_ymax + box_height3 + 1 #as above
  dbident_ymin <- dbident_ymax - box_height2 - 0.5 #as above
  dbident_lab_x <- dbident_xmin + ((dbident_xmax - dbident_xmin) / 2) #x main label location
  dbident_lab_y <- dbident_ymax - 0.3 #as above
  dbident_lab_x1 <- dbident_xmin + ((dbident_xmax - dbident_xmin) / 2) #x label 1 location
  dbident_lab_y1 <- dbident_lab_y - 0.5 #y label 1 location
  dbident_data_x1 <- dbident_lab_x1 #x data 1 location
  dbident_data_y1 <- dbident_lab_y1 - 1.4 #y data 1 location
  dbident_lab_x2 <- dbident_lab_x1 #x label 2 location
  dbident_lab_y2 <- dbident_data_y1 - 1.2 #y label 2 location
  dbident_data_x2 <- dbident_lab_x1 #x data 2 location
  dbident_data_y2 <- dbident_lab_y2 - 0.7 #y data 2 location
  
  #DB Records removed
  dbexcl_lab <- "Records removed before screening"
  dbexcl_lab1 <- "Dupicates"
  dbexcl_lab2 <- "Automation"
  dbexcl_lab3 <- "Other"
  dbexcl_xmin <- dbrrepexcl_xmin #as above
  dbexcl_xmax <- dbexcl_xmin + box_width #as above
  dbexcl_ymin <- dbrrecexcl_ymax + 1 #box bottom limit
  dbexcl_ymax <- dbexcl_ymin + box_height3  #box top limit
  dbexcl_lab_x <- dbexcl_xmin + ((dbexcl_xmax - dbexcl_xmin) / 2) #x label location
  dbexcl_lab_y <- dbexcl_ymax - 0.3 #as above
  dbexcl_data_x <- dbexcl_lab_x #x data location
  dbexcl_lab_y <- dbexcl_ymax - 0.3 #as above
  dbexcl_lab_x1 <- dbexcl_lab_x #x label 1 location
  dbexcl_lab_y1 <- dbexcl_lab_y - 0.5 #y label 1 location
  dbexcl_data_x1 <- dbexcl_lab_x1 #x data 1 location
  dbexcl_data_y1 <- dbexcl_lab_y1 - 1.4 #y data 1 location
  dbexcl_lab_x2 <- dbexcl_lab_x1 #x label 2 location
  dbexcl_lab_y2 <- dbexcl_data_y1 - 1.2 #y label 2 location
  dbexcl_data_x2 <- dbexcl_lab_x1 #x data 2 location
  dbexcl_data_y2 <- dbexcl_lab_y2 - 0.7 #y data 2 location
  dbexcl_lab_x3 <- dbexcl_lab_x2 #x label 2 location
  dbexcl_lab_y3 <- dbexcl_data_y2 - 0.7 #y label 2 location
  dbexcl_data_x3 <- dbexcl_lab_x2 #x data 2 location
  dbexcl_data_y3 <- dbexcl_lab_y3 - 0.7 #y data 2 location
  
  #other results identified
  otherident_lab <- "Records identified from:"
  otherident_lab1 <- "Websites"
  otherident_lab2 <- "Organisations"
  otherident_lab3 <- "Citations"
  otherident_xmin <- otherrepsought_xmin #as above
  otherident_xmax <- otherident_xmin + box_width #as above
  otherident_ymax <- dbexcl_ymax #box top limit
  otherident_ymin <- dbexcl_ymin #box bottom limit
  otherident_lab_x <- otherident_xmin + ((otherident_xmax - otherident_xmin) / 2) #x label location
  otherident_lab_y <- otherident_ymax - 0.3 #as above
  otherident_data_x <- otherident_lab_x #x data location
  otherident_lab_y <- otherident_ymax - 0.3 #as above
  otherident_lab_x1 <- otherident_lab_x #x label 1 location
  otherident_lab_y1 <- otherident_lab_y - 0.5 #y label 1 location
  otherident_data_x1 <- otherident_lab_x1 #x data 1 location
  otherident_data_y1 <- otherident_lab_y1 - 1.4 #y data 1 location
  otherident_lab_x2 <- otherident_lab_x1 #x label 2 location
  otherident_lab_y2 <- otherident_data_y1 - 1.2 #y label 2 location
  otherident_data_x2 <- otherident_lab_x1 #x data 2 location
  otherident_data_y2 <- otherident_lab_y2 - 0.7 #y data 2 location
  otherident_lab_x3 <- otherident_lab_x2 #x label 2 location
  otherident_lab_y3 <- otherident_data_y2 - 0.7 #y label 2 location
  otherident_data_x3 <- otherident_lab_x2 #x data 2 location
  otherident_data_y3 <- otherident_lab_y3 - 0.7 #y data 2 location
  
  #Previous studies
  prevdat_lab1 <- "Studies included in previous version of review"
  prevdat_lab2 <- "Reports of studies included in previous version of review"
  prevdat_xmin <- prev_xmin #box left limit
  prevdat_xmax <- prevdat_xmin + box_width #box right limit
  prevdat_ymin <- dbident_ymin #box bottom limit
  prevdat_ymax <- dbident_ymax #box top limit
  prevdat_loc_x1 <- prevdat_xmin + ((prevdat_xmax - prevdat_xmin) / 2) #x label location
  prevdat_loc_y1 <- prevdat_ymax - 1 #y label location
  prevdat_loc_x2 <- prevdat_loc_x1 #x label location
  prevdat_loc_y2 <- prevdat_ymax - ((prevdat_ymax - prevdat_ymin) / 2) - 0.5 #y label location
  prevdat_data_x1 <- prevdat_loc_x1
  prevdat_data_y1 <- prevdat_loc_y1 - 1.2
  prevdat_data_x2 <- prevdat_loc_x1
  prevdat_data_y2 <- prevdat_loc_y2 - 1.2
  
  #Create 'fake' data set to establish plot limits
  x <- c(basex, other_xmax)
  y <- c(basey, ident_ymax)
  df <- data.frame(x, y)
  
  #Generate plot
  canvas <- ggplot(df, aes(x = x, y = y)) +
    ggpubr::theme_transparent(base_size = 12, base_family = "") +
    scale_x_continuous(name="x") + 
    scale_y_continuous(name="y")
  
  plot <- canvas +
    statebins:::geom_rrect(mapping=aes(xmin=ident_xmin, xmax=ident_xmax, ymin=ident_ymin, ymax=ident_ymax), color="#bcd1ee", fill="#bcd1ee") + #identification box
    statebins:::geom_rrect(mapping=aes(xmin=screen_xmin, xmax=screen_xmax, ymin=screen_ymin, ymax=screen_ymax), color="#bcd1ee", fill="#bcd1ee") + #screening box
    statebins:::geom_rrect(mapping=aes(xmin=incl_xmin, xmax=incl_xmax, ymin=incl_ymin, ymax=incl_ymax), color="#bcd1ee", fill="#bcd1ee") + #included box
    statebins:::geom_rrect(mapping=aes(xmin=prev_xmin, xmax=prev_xmax, ymin=prev_ymin, ymax=prev_ymax), color="#dcdcdc", fill="#dcdcdc") + #previous box
    statebins:::geom_rrect(mapping=aes(xmin=dbr_xmin, xmax=dbr_xmax, ymin=dbr_ymin, ymax=dbr_ymax), color="#ffc125", fill="#ffc125") + #dbr box
    statebins:::geom_rrect(mapping=aes(xmin=other_xmin, xmax=other_xmax, ymin=other_ymin, ymax=other_ymax), color="#dcdcdc", fill="#dcdcdc") + #other box
    annotate(geom="text", x=ident_lab_x, y=ident_lab_y, label=ident_lab, color="black", size=font_size, angle = 90) + 
    annotate(geom="text", x=screen_lab_x, y=screen_lab_y, label=screen_lab, color="black", size=font_size, angle = 90) + 
    annotate(geom="text", x=incl_lab_x, y=incl_lab_y, label=incl_lab, color="black", size=font_size, angle = 90) + 
    annotate(geom="text", x=prev_lab_x, y=prev_lab_y, label=prev_lab, color="black", size=font_size) + 
    annotate(geom="text", x=dbr_lab_x, y=dbr_lab_y, label=dbr_lab, color="black", size=font_size) + 
    annotate(geom="text", x=other_lab_x, y=other_lab_y, label=other_lab, color="black", size=font_size) + 
    
    #otherident
    geom_rect(mapping=aes(xmin=otherident_xmin, xmax=otherident_xmax, ymin=otherident_ymin, ymax=otherident_ymax), color="#dcdcdc", fill='#dcdcdc') + #otherident box
    annotate(geom="text", x=otherident_lab_x, y=otherident_lab_y, label=otherident_lab, color="black", size=font_size) + #otherident label
    
    annotate(geom="text", x=otherident_lab_x1, y=otherident_lab_y1, label=otherident_lab1, color="black", size=font_size) + #otherident label 1
    geom_rect(mapping=aes(xmin=otherident_xmin, xmax=otherident_xmax, ymin=otherident_data_y1-0.2, ymax=otherident_data_y1+0.9), color="gray75", fill='gray75') + #dark background
    annotate(geom="text", x=otherident_xmin+col_mid_x, y=otherident_data_y1+0.4, label=columns, fontface=2, color="black", size=font_size-0.5) +
    geom_rect(mapping=aes(xmin=otherident_xmin, xmax=otherident_xmax, ymin=otherident_data_y1-0.6, ymax=otherident_data_y1-0.2), color="gray95", fill='gray95') + #light background
    annotate(geom="text", x=otherident_xmin+col_mid_x, y=otherident_data_y1-0.4, label=as.character(data$other_ident_web), fontface=2, color="black", size=font_size-0.5) +
    annotate(geom="text", x=otherident_lab_x2, y=otherident_lab_y2, label=otherident_lab2, color="black", size=font_size) + #otherident label 2
    geom_rect(mapping=aes(xmin=otherident_xmin, xmax=otherident_xmax, ymin=otherident_data_y2-0.2, ymax=otherident_data_y2+0.2), color="gray95", fill='gray95') + #light background
    annotate(geom="text", x=otherident_xmin+col_mid_x, y=otherident_data_y2, label=as.character(data$other_ident_org), fontface=2, color="black", size=font_size-0.5) +
    annotate(geom="text", x=otherident_lab_x3, y=otherident_lab_y3, label=otherident_lab3, color="black", size=font_size) + #otherident label 3
    geom_rect(mapping=aes(xmin=otherident_xmin, xmax=otherident_xmax, ymin=otherident_data_y3-0.2, ymax=otherident_data_y3+0.2), color="gray95", fill='gray95') + #light background
    annotate(geom="text", x=otherident_xmin+col_mid_x, y=otherident_data_y3, label=as.character(data$other_ident_cit), fontface=2, color="black", size=font_size-0.5) +
    
    #dbident
    geom_rect(mapping=aes(xmin=dbident_xmin, xmax=dbident_xmax, ymin=dbident_ymin, ymax=dbident_ymax), color="#dcdcdc", fill='#dcdcdc') + #dbident box
    annotate(geom="text", x=dbident_lab_x, y=dbident_lab_y, label=dbident_lab, color="black", size=font_size) + #dbident label
    annotate(geom="text", x=dbident_lab_x1, y=dbident_lab_y1, label=dbident_lab1, color="black", size=font_size) + #dbident label 1
    annotate(geom="text", x=dbident_lab_x2, y=dbident_lab_y2, label=dbident_lab2, color="black", size=font_size) + #dbident label 2
    
    geom_rect(mapping=aes(xmin=dbident_xmin, xmax=dbident_xmax, ymin=dbident_data_y1-0.2, ymax=dbident_data_y1+0.9), color="gray75", fill='gray75') + #dark background
    annotate(geom="text", x=dbident_xmin+col_mid_x, y=dbident_data_y1+0.4, label=columns, fontface=2, color="black", size=font_size-0.5) +
    geom_rect(mapping=aes(xmin=dbident_xmin, xmax=dbident_xmax, ymin=dbident_data_y1-0.6, ymax=dbident_data_y1-0.2), color="gray95", fill='gray95') + #light background
    annotate(geom="text", x=dbident_xmin+col_mid_x, y=dbident_data_y1-0.4, label=as.character(data$database_results), fontface=2, color="black", size=font_size-0.5) +
    geom_rect(mapping=aes(xmin=dbident_xmin, xmax=dbident_xmax, ymin=dbident_data_y2-0.2, ymax=dbident_data_y2+0.2), color="gray95", fill='gray95') + #light background
    annotate(geom="text", x=dbident_xmin+col_mid_x, y=dbident_data_y2, label=as.character(data$register_results), fontface=2, color="black", size=font_size-0.5) +
    
    #dbexcl
    geom_rect(mapping=aes(xmin=dbexcl_xmin, xmax=dbexcl_xmax, ymin=dbexcl_ymin, ymax=dbexcl_ymax), color="#dcdcdc", fill='#dcdcdc') + #dbexcl box
    annotate(geom="text", x=dbexcl_lab_x, y=dbexcl_lab_y, label=dbexcl_lab, color="black", size=font_size) + #dbexcl label
    
    annotate(geom="text", x=dbexcl_lab_x1, y=dbexcl_lab_y1, label=dbexcl_lab1, color="black", size=font_size) + #dbexcl label 1
    geom_rect(mapping=aes(xmin=dbexcl_xmin, xmax=dbexcl_xmax, ymin=dbexcl_data_y1-0.2, ymax=dbexcl_data_y1+0.9), color="gray75", fill='gray75') + #dark background
    annotate(geom="text", x=dbexcl_xmin+col_mid_x, y=dbexcl_data_y1+0.4, label=columns, fontface=2, color="black", size=font_size-0.5) +
    geom_rect(mapping=aes(xmin=dbexcl_xmin, xmax=dbexcl_xmax, ymin=dbexcl_data_y1-0.6, ymax=dbexcl_data_y1-0.2), color="gray95", fill='gray95') + #light background
    annotate(geom="text", x=dbexcl_xmin+col_mid_x, y=dbexcl_data_y1-0.4, label=as.character(data$db_excl_dup), fontface=2, color="black", size=font_size-0.5) +
    annotate(geom="text", x=dbexcl_lab_x2, y=dbexcl_lab_y2, label=dbexcl_lab2, color="black", size=font_size) + #dbexcl label 2
    geom_rect(mapping=aes(xmin=dbexcl_xmin, xmax=dbexcl_xmax, ymin=dbexcl_data_y2-0.2, ymax=dbexcl_data_y2+0.2), color="gray95", fill='gray95') + #light background
    annotate(geom="text", x=dbexcl_xmin+col_mid_x, y=dbexcl_data_y2, label=as.character(data$db_excl_aut), fontface=2, color="black", size=font_size-0.5) +
    annotate(geom="text", x=dbexcl_lab_x3, y=dbexcl_lab_y3, label=dbexcl_lab3, color="black", size=font_size) + #dbexcl label 3
    geom_rect(mapping=aes(xmin=dbexcl_xmin, xmax=dbexcl_xmax, ymin=dbexcl_data_y3-0.2, ymax=dbexcl_data_y3+0.2), color="gray95", fill='gray95') + #light background
    annotate(geom="text", x=dbexcl_xmin+col_mid_x, y=dbexcl_data_y3, label=as.character(data$db_excl_oth), fontface=2, color="black", size=font_size-0.5) +
    
    #dbrrecscr
    geom_rect(mapping=aes(xmin=dbrrecscr_xmin, xmax=dbrrecscr_xmax, ymin=dbrrecscr_ymin, ymax=dbrrecscr_ymax), color="#dcdcdc", fill='#dcdcdc') + #dbrrecscr box
    annotate(geom="text", x=dbrrecscr_lab_x, y=dbrrecscr_lab_y, label=dbrrecscr_lab, color="black", size=font_size) + #dbrrecscr label
    
    geom_rect(mapping=aes(xmin=dbrrecscr_xmin, xmax=dbrrecscr_xmax, ymin=dbrrecscr_data_y-0.2, ymax=dbrrecscr_data_y+0.9), color="gray75", fill='gray75') + #dark background
    annotate(geom="text", x=dbrrecscr_xmin+col_mid_x, y=dbrrecscr_data_y+0.4, label=columns, fontface=2, color="black", size=font_size-0.5) +
    geom_rect(mapping=aes(xmin=dbrrecscr_xmin, xmax=dbrrecscr_xmax, ymin=dbrrecscr_data_y-0.6, ymax=dbrrecscr_data_y-0.2), color="gray95", fill='gray95') + #light background
    annotate(geom="text", x=dbrrecscr_xmin+col_mid_x, y=dbrrecscr_data_y-0.4, label=as.character(data$records_screened), fontface=2, color="black", size=font_size-0.5) +
    
    #dbrrecexcl
    geom_rect(mapping=aes(xmin=dbrrecexcl_xmin, xmax=dbrrecexcl_xmax, ymin=dbrrecexcl_ymin, ymax=dbrrecexcl_ymax), color="#dcdcdc", fill='#dcdcdc') + #dbrrecexcl box
    annotate(geom="text", x=dbrrecexcl_lab_x, y=dbrrecexcl_lab_y, label=dbrrecexcl_lab, color="black", size=font_size) + #dbrrecexcl label
    
    geom_rect(mapping=aes(xmin=dbrrecexcl_xmin, xmax=dbrrecexcl_xmax, ymin=dbrrecexcl_data_y-0.2, ymax=dbrrecexcl_data_y+0.9), color="gray75", fill='gray75') + #dark background
    annotate(geom="text", x=dbrrecexcl_xmin+col_mid_x, y=dbrrecexcl_data_y+0.4, label=columns, fontface=2, color="black", size=font_size-0.5) +
    geom_rect(mapping=aes(xmin=dbrrecexcl_xmin, xmax=dbrrecexcl_xmax, ymin=dbrrecexcl_data_y-0.6, ymax=dbrrecexcl_data_y-0.2), color="gray95", fill='gray95') + #light background
    annotate(geom="text", x=dbrrecexcl_xmin+col_mid_x, y=dbrrecexcl_data_y-0.4, label=as.character(data$records_excluded), fontface=2, color="black", size=font_size-0.5) +
    
    #otherrepsought
    geom_rect(mapping=aes(xmin=otherrepsought_xmin, xmax=otherrepsought_xmax, ymin=otherrepsought_ymin, ymax=otherrepsought_ymax), color="#dcdcdc", fill='#dcdcdc') + #otherrepsought box
    annotate(geom="text", x=otherrepsought_lab_x, y=otherrepsought_lab_y, label=otherrepsought_lab, color="black", size=font_size) + #otherrepsought label
    
    geom_rect(mapping=aes(xmin=otherrepsought_xmin, xmax=otherrepsought_xmax, ymin=otherrepsought_data_y-0.2, ymax=otherrepsought_data_y+0.9), color="gray75", fill='gray75') + #dark background
    annotate(geom="text", x=otherrepsought_xmin+col_mid_x, y=otherrepsought_data_y+0.4, label=columns, fontface=2, color="black", size=font_size-0.5) +
    geom_rect(mapping=aes(xmin=otherrepsought_xmin, xmax=otherrepsought_xmax, ymin=otherrepsought_data_y-0.6, ymax=otherrepsought_data_y-0.2), color="gray95", fill='gray95') + #light background
    annotate(geom="text", x=otherrepsought_xmin+col_mid_x, y=otherrepsought_data_y-0.4, label=as.character(data$other_sought_reports), fontface=2, color="black", size=font_size-0.5) +
    
    #otherrepnotret
    geom_rect(mapping=aes(xmin=otherrepnotret_xmin, xmax=otherrepnotret_xmax, ymin=otherrepnotret_ymin, ymax=otherrepnotret_ymax), color="#dcdcdc", fill='#dcdcdc') + #otherrepnotret box
    annotate(geom="text", x=otherrepnotret_lab_x, y=otherrepnotret_lab_y, label=otherrepnotret_lab, color="black", size=font_size) + #otherrepnotret label
    
    geom_rect(mapping=aes(xmin=otherrepnotret_xmin, xmax=otherrepnotret_xmax, ymin=otherrepnotret_data_y-0.2, ymax=otherrepnotret_data_y+0.9), color="gray75", fill='gray75') + #dark background
    annotate(geom="text", x=otherrepnotret_xmin+col_mid_x, y=otherrepnotret_data_y+0.4, label=columns, fontface=2, color="black", size=font_size-0.5) +
    geom_rect(mapping=aes(xmin=otherrepnotret_xmin, xmax=otherrepnotret_xmax, ymin=otherrepnotret_data_y-0.6, ymax=otherrepnotret_data_y-0.2), color="gray95", fill='gray95') + #light background
    annotate(geom="text", x=otherrepnotret_xmin+col_mid_x, y=otherrepnotret_data_y-0.4, label=as.character(data$other_notretrieved_reports), fontface=2, color="black", size=font_size-0.5) +
    
    #dbrrepsought
    geom_rect(mapping=aes(xmin=dbrrepsought_xmin, xmax=dbrrepsought_xmax, ymin=dbrrepsought_ymin, ymax=dbrrepsought_ymax), color="#dcdcdc", fill='#dcdcdc') + #dbrrepsought box
    annotate(geom="text", x=dbrrepsought_lab_x, y=dbrrepsought_lab_y, label=dbrrepsought_lab, color="black", size=font_size) + #dbrrepsought label
    
    geom_rect(mapping=aes(xmin=dbrrepsought_xmin, xmax=dbrrepsought_xmax, ymin=dbrrepsought_data_y-0.2, ymax=dbrrepsought_data_y+0.9), color="gray75", fill='gray75') + #dark background
    annotate(geom="text", x=dbrrepsought_xmin+col_mid_x, y=dbrrepsought_data_y+0.4, label=columns, fontface=2, color="black", size=font_size-0.5) +
    geom_rect(mapping=aes(xmin=dbrrepsought_xmin, xmax=dbrrepsought_xmax, ymin=dbrrepsought_data_y-0.6, ymax=dbrrepsought_data_y-0.2), color="gray95", fill='gray95') + #light background
    annotate(geom="text", x=dbrrepsought_xmin+col_mid_x, y=dbrrepsought_data_y-0.4, label=as.character(data$dbr_sought_reports), fontface=2, color="black", size=font_size-0.5) +
    
    #dbrrepnotret
    geom_rect(mapping=aes(xmin=dbrrepnotret_xmin, xmax=dbrrepnotret_xmax, ymin=dbrrepnotret_ymin, ymax=dbrrepnotret_ymax), color="#dcdcdc", fill='#dcdcdc') + #dbrrepnotret box
    annotate(geom="text", x=dbrrepnotret_lab_x, y=dbrrepnotret_lab_y, label=dbrrepnotret_lab, color="black", size=font_size) + #dbrrepnotret label
    
    geom_rect(mapping=aes(xmin=dbrrepnotret_xmin, xmax=dbrrepnotret_xmax, ymin=dbrrepnotret_data_y-0.2, ymax=dbrrepnotret_data_y+0.9), color="gray75", fill='gray75') + #dark background
    annotate(geom="text", x=dbrrepnotret_xmin+col_mid_x, y=dbrrepnotret_data_y+0.4, label=columns, fontface=2, color="black", size=font_size-0.5) +
    geom_rect(mapping=aes(xmin=dbrrepnotret_xmin, xmax=dbrrepnotret_xmax, ymin=dbrrepnotret_data_y-0.6, ymax=dbrrepnotret_data_y-0.2), color="gray95", fill='gray95') + #light background
    annotate(geom="text", x=dbrrepnotret_xmin+col_mid_x, y=dbrrepnotret_data_y-0.4, label=as.character(data$dbr_notretrieved_reports), fontface=2, color="black", size=font_size-0.5) +
    
    #otherrep
    geom_rect(mapping=aes(xmin=otherrep_xmin, xmax=otherrep_xmax, ymin=otherrep_ymin, ymax=otherrep_ymax), color="#dcdcdc", fill='#dcdcdc') + #otherrep box
    annotate(geom="text", x=otherrep_lab_x, y=otherrep_lab_y, label=otherrep_lab, color="black", size=font_size) + #otherrep label
    
    geom_rect(mapping=aes(xmin=otherrep_xmin, xmax=otherrep_xmax, ymin=otherrep_data_y-0.2, ymax=otherrep_data_y+0.9), color="gray75", fill='gray75') + #dark background
    annotate(geom="text", x=otherrep_xmin+col_mid_x, y=otherrep_data_y+0.4, label=columns, fontface=2, color="black", size=font_size-0.5) +
    geom_rect(mapping=aes(xmin=otherrep_xmin, xmax=otherrep_xmax, ymin=otherrep_data_y-0.6, ymax=otherrep_data_y-0.2), color="gray95", fill='gray95') + #light background
    annotate(geom="text", x=otherrep_xmin+col_mid_x, y=otherrep_data_y-0.4, label=as.character(data$other_assessed), fontface=2, color="black", size=font_size-0.5) +
    
    #otherrepexcl
    geom_rect(mapping=aes(xmin=otherrepexcl_xmin, xmax=otherrepexcl_xmax, ymin=otherrepexcl_ymin, ymax=otherrepexcl_ymax), color="#dcdcdc", fill='#dcdcdc') + #otherrepexcl box
    annotate(geom="text", x=otherrepexcl_lab_x, y=otherrepexcl_lab_y, label=otherrepexcl_lab, color="black", size=font_size) + #otherrepexcl label
    
    geom_rect(mapping=aes(xmin=otherrepexcl_xmin, xmax=otherrepexcl_xmax, ymin=otherrepexcl_data_y-0.2, ymax=otherrepexcl_data_y+0.9), color="gray75", fill='gray75') + #dark background
    annotate(geom="text", x=otherrepexcl_xmin+col_mid_x, y=otherrepexcl_data_y+0.4, label=columns, fontface=2, color="black", size=font_size-0.5) +
    geom_rect(mapping=aes(xmin=otherrepexcl_xmin, xmax=otherrepexcl_xmax, ymin=otherrepexcl_data_y-0.6, ymax=otherrepexcl_data_y-0.2), color="gray95", fill='gray95') + #light background
    annotate(geom="text", x=otherrepexcl_xmin+col_mid_x, y=otherrepexcl_data_y-0.4, label=as.character(data$other_excluded_data), fontface=2, color="black", size=font_size-0.5) +
    
    annotate(geom="text", x=otherrepexcl_lab_x, y=otherrepexclreasons_lab_y, label=data$other_excluded, color="black", size=font_size, vjust=1) + #otherrepexcl reasons
    
    #dbrrep
    geom_rect(mapping=aes(xmin=dbrrep_xmin, xmax=dbrrep_xmax, ymin=dbrrep_ymin, ymax=dbrrep_ymax), color="#dcdcdc", fill='#dcdcdc') + #dbrrep box
    annotate(geom="text", x=dbrrep_lab_x, y=dbrrep_lab_y, label=dbrrep_lab, color="black", size=font_size) + #dbrrep label
    
    geom_rect(mapping=aes(xmin=dbrrep_xmin, xmax=dbrrep_xmax, ymin=dbrrep_data_y-0.2, ymax=dbrrep_data_y+0.9), color="gray75", fill='gray75') + #dark background
    annotate(geom="text", x=dbrrep_xmin+col_mid_x, y=dbrrep_data_y+0.4, label=columns, fontface=2, color="black", size=font_size-0.5) +
    geom_rect(mapping=aes(xmin=dbrrep_xmin, xmax=dbrrep_xmax, ymin=dbrrep_data_y-0.6, ymax=dbrrep_data_y-0.2), color="gray95", fill='gray95') + #light background
    annotate(geom="text", x=dbrrep_xmin+col_mid_x, y=dbrrep_data_y-0.4, label=as.character(data$dbr_assessed), fontface=2, color="black", size=font_size-0.5) +
    
    #dbrrepexcl
    geom_rect(mapping=aes(xmin=dbrrepexcl_xmin, xmax=dbrrepexcl_xmax, ymin=dbrrepexcl_ymin, ymax=dbrrepexcl_ymax), color="#dcdcdc", fill='#dcdcdc') + #dbrrepexcl box
    annotate(geom="text", x=dbrrepexcl_lab_x, y=dbrrepexcl_lab_y, label=dbrrepexcl_lab, color="black", size=font_size) + #dbrrepexcl label
    
    geom_rect(mapping=aes(xmin=dbrrepexcl_xmin, xmax=dbrrepexcl_xmax, ymin=dbrrepexcl_data_y-0.2, ymax=dbrrepexcl_data_y+0.9), color="gray75", fill='gray75') + #dark background
    annotate(geom="text", x=dbrrepexcl_xmin+col_mid_x, y=dbrrepexcl_data_y+0.4, label=columns, fontface=2, color="black", size=font_size-0.5) +
    geom_rect(mapping=aes(xmin=dbrrepexcl_xmin, xmax=dbrrepexcl_xmax, ymin=dbrrepexcl_data_y-0.6, ymax=dbrrepexcl_data_y-0.2), color="gray95", fill='gray95') + #light background
    annotate(geom="text", x=dbrrepexcl_xmin+col_mid_x, y=dbrrepexcl_data_y-0.4, label=as.character(data$dbr_excluded_data), fontface=2, color="black", size=font_size-0.5) +
    
    annotate(geom="text", x=dbrrepexcl_lab_x, y=dbrrepexclreasons_lab_y, label=data$dbr_excluded, color="black", size=font_size, vjust=1) + #dbrrepexcl reasons
    
    #new
    geom_rect(mapping=aes(xmin=new_xmin, xmax=new_xmax, ymin=new_ymin, ymax=new_ymax), color="#dcdcdc", fill='#dcdcdc') + #new box
    annotate(geom="text", x=new_lab_x1, y=new_lab_y1, label=new_lab1, color="black", size=font_size) + #new label 1
    annotate(geom="text", x=new_lab_x1, y=new_lab_y2, label=new_lab2, color="black", size=font_size) + #new label 2
    
    geom_rect(mapping=aes(xmin=new_xmin, xmax=new_xmax, ymin=new_data_y1-0.2, ymax=new_data_y1+0.9), color="gray75", fill='gray75') + #dark background
    annotate(geom="text", x=new_xmin+col_mid_x, y=new_data_y1+0.4, label=columns, fontface=2, color="black", size=font_size-0.5) +
    geom_rect(mapping=aes(xmin=new_xmin, xmax=new_xmax, ymin=new_data_y1-0.6, ymax=new_data_y1-0.2), color="gray95", fill='gray95') + #light background
    annotate(geom="text", x=new_xmin+col_mid_x, y=new_data_y1-0.4, label=as.character(data$new_studies), fontface=2, color="black", size=font_size-0.5) +
    geom_rect(mapping=aes(xmin=new_xmin, xmax=new_xmax, ymin=new_data_y2-0.2, ymax=new_data_y2+0.2), color="gray95", fill='gray95') + #light background
    annotate(geom="text", x=new_xmin+col_mid_x, y=new_data_y2, label=as.character(data$new_reports), fontface=2, color="black", size=font_size-0.5) +
    
    #total
    geom_rect(mapping=aes(xmin=tot_xmin, xmax=tot_xmax, ymin=tot_ymin, ymax=tot_ymax), color="#dcdcdc", fill='#dcdcdc') + #total box
    annotate(geom="text", x=tot_lab_x1, y=tot_lab_y1, label=tot_lab1, color="black", size=font_size) + #total label 1
    annotate(geom="text", x=tot_lab_x1, y=tot_lab_y2, label=tot_lab2, color="black", size=font_size) + #total label 2
    
    geom_rect(mapping=aes(xmin=tot_xmin, xmax=tot_xmax, ymin=tot_data_y1-0.2, ymax=tot_data_y1+0.9), color="gray75", fill='gray75') + #dark background
    annotate(geom="text", x=tot_xmin+col_mid_x, y=tot_data_y1+0.4, label=columns, fontface=2, color="black", size=font_size-0.5) +
    geom_rect(mapping=aes(xmin=tot_xmin, xmax=tot_xmax, ymin=tot_data_y1-0.6, ymax=tot_data_y1-0.2), color="gray95", fill='gray95') + #light background
    annotate(geom="text", x=tot_xmin+col_mid_x, y=tot_data_y1-0.4, label=as.character(data$total_studies), fontface=2, color="black", size=font_size-0.5) +
    geom_rect(mapping=aes(xmin=tot_xmin, xmax=tot_xmax, ymin=tot_data_y2-0.2, ymax=tot_data_y2+0.2), color="gray95", fill='gray95') + #light background
    annotate(geom="text", x=tot_xmin+col_mid_x, y=tot_data_y2, label=as.character(data$total_reports), fontface=2, color="black", size=font_size-0.5) +
      
    #previous
    geom_rect(mapping=aes(xmin=prevdat_xmin, xmax=prevdat_xmax, ymin=prevdat_ymin, ymax=prevdat_ymax), color="#dcdcdc", fill='#dcdcdc') + #previous box
    annotate(geom="text", x=prevdat_loc_x1, y=prevdat_loc_y1, label=prevdat_lab1, color="black", size=font_size) + #previous label 1
    annotate(geom="text", x=prevdat_loc_x2, y=prevdat_loc_y2, label=prevdat_lab2, color="black", size=font_size) + #previous label 2
    annotate(geom="text", x=prevdat_data_x1, y=prevdat_data_y1, label=data$previous_studies, color="black", size=font_size) + #previous data
    annotate(geom="text", x=prevdat_data_x2, y=prevdat_data_y2, label=data$previous_reports, color="black", size=font_size) + #previous data
    
    #arrows
    geom_segment(aes(x = dbident_xmin+(box_width/2), y = dbident_ymin-0.1, xend = dbrrecscr_xmin+(box_width/2), yend = dbrrecscr_ymax+0.1), lineend = 'round', linejoin = 'round', arrow=arrow(length = unit(2, "mm"), type = "closed"), colour = "gray40") +
    geom_segment(aes(x = dbident_xmax+0.1, y = dbident_ymin+(box_height2/2), xend = dbexcl_xmin-0.1, yend = dbident_ymin+(box_height2/2)), lineend = 'round', linejoin = 'round', arrow=arrow(length = unit(2, "mm"), type = "closed"), colour = "gray40") +
    geom_segment(aes(x = dbrrecscr_xmin+(box_width/2), y = dbrrecscr_ymin-0.1, xend = dbrrepsought_xmin+(box_width/2), yend = dbrrepsought_ymax+0.1), lineend = 'round', linejoin = 'round', arrow=arrow(length = unit(2, "mm"), type = "closed"), colour = "gray40") +
    geom_segment(aes(x = dbrrecscr_xmax+0.1, y = dbrrecexcl_ymin+(box_height1/2), xend = dbrrecexcl_xmin-0.1, yend = dbrrecexcl_ymin+(box_height1/2)), lineend = 'round', linejoin = 'round', arrow=arrow(length = unit(2, "mm"), type = "closed"), colour = "gray40") +
    geom_segment(aes(x = dbrrepsought_xmin+(box_width/2), y = dbrrepsought_ymin-0.1, xend = dbrrep_xmin+(box_width/2), yend = dbrrep_ymax+0.1), lineend = 'round', linejoin = 'round', arrow=arrow(length = unit(2, "mm"), type = "closed"), colour = "gray40") +
    geom_segment(aes(x = dbrrepsought_xmax+0.1, y = dbrrepsought_ymin+(box_height1/2), xend = dbrrepnotret_xmin-0.1, yend = dbrrepsought_ymin+(box_height1/2)), lineend = 'round', linejoin = 'round', arrow=arrow(length = unit(2, "mm"), type = "closed"), colour = "gray40") +
    geom_segment(aes(x = dbrrep_xmin+(box_width/2), y = dbrrep_ymin-0.1, xend = dbrrep_xmin+(box_width/2), yend = new_ymax+0.1), lineend = 'round', linejoin = 'round', arrow=arrow(length = unit(2, "mm"), type = "closed"), colour = "gray40") +
    geom_segment(aes(x = dbrrep_xmax+0.1, y = dbrrep_ymin+(box_height1/2), xend = dbrrepexcl_xmin-0.1, yend = dbrrep_ymin+(box_height1/2)), lineend = 'round', linejoin = 'round', arrow=arrow(length = unit(2, "mm"), type = "closed"), colour = "gray40") +
    #previous
    geom_segment(aes(x = new_xmin+(box_width/2), y = new_ymin-0.1, xend = new_xmin+(box_width/2), yend = tot_ymax+0.1), lineend = 'round', linejoin = 'round', arrow=arrow(length = unit(2, "mm"), type = "closed"), colour = "gray40") +
    geom_segment(aes(x = prevdat_xmin+(box_width/2), y = prevdat_ymin-0.1, xend = prevdat_xmin+(box_width/2), yend = tot_ymin+(box_height2/2)), lineend = 'round', linejoin = 'round', colour = "gray40") +
    geom_segment(aes(x = prevdat_xmin+(box_width/2), y = tot_ymin+(box_height2/2), xend = tot_xmin-0.1, yend = tot_ymin+(box_height2/2)), lineend = 'round', linejoin = 'round', arrow=arrow(length = unit(2, "mm"), type = "closed"), colour = "gray40") +
    #other
    geom_segment(aes(x = otherident_xmin+(box_width/2), y = otherident_ymin-0.1, xend = otherident_xmin+(box_width/2), yend = otherrepsought_ymax+0.1), lineend = 'round', linejoin = 'round', arrow=arrow(length = unit(2, "mm"), type = "closed"), colour = "gray40") +
    geom_segment(aes(x = otherrepsought_xmin+(box_width/2), y = otherrepsought_ymin-0.1, xend = otherrepsought_xmin+(box_width/2), yend = otherrep_ymax+0.1), lineend = 'round', linejoin = 'round', arrow=arrow(length = unit(2, "mm"), type = "closed"), colour = "gray40") +
    geom_segment(aes(x = otherrepsought_xmax+0.1, y = otherrepsought_ymin+(box_height1/2), xend = otherrepnotret_xmin-0.1, yend = otherrepsought_ymin+(box_height1/2)), lineend = 'round', linejoin = 'round', arrow=arrow(length = unit(2, "mm"), type = "closed"), colour = "gray40") +
    geom_segment(aes(x = otherrep_xmax+0.1, y = otherrep_ymin+(box_height1/2), xend = otherrepexcl_xmin-0.1, yend = otherrep_ymin+(box_height1/2)), lineend = 'round', linejoin = 'round', arrow=arrow(length = unit(2, "mm"), type = "closed"), colour = "gray40") +
    geom_segment(aes(x = otherrep_xmin+(box_width/2), y = otherrep_ymin-0.1, xend = otherrep_xmin+(box_width/2), yend = new_ymin+(box_height1/2)), lineend = 'round', linejoin = 'round', colour = "gray40") +
    geom_segment(aes(x = otherrep_xmin+(box_width/2), y = new_ymin+(box_height1/2), xend = new_xmax+0.1, yend = new_ymin+(box_height1/2)), lineend = 'round', linejoin = 'round', arrow=arrow(length = unit(2, "mm"), type = "closed"), colour = "gray40") 
  
  return(plot)

}


