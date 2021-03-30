#' Convert Data for LSR Prisma Flow Diagram between formats
#' 
#' @description Function to convert input data uploaded from format 
#' used in Approach 1 (all updates added as separate columns) to 
#' one of Approaches 2 to 4. Columns are summarised in a single 
#' column (Approach 2), base plus updates (Approach 3), or all 
#' updates and latest update (Approach 4). The function wraps 
#' `LSRPrisma_data()` so that data can be directly passed to the 
#' flow diagram creation function.
#' @param data Unprocessed data provided in accordance with the 
#' 'template.csv' file.
#' @param format The desired output approach based on the four 
#' potential layout formats for living systematic review PRISMA 
#' flow diagrams. Options are: 'approach1' with each update 
#' provided as a separate column; 'approach2' with the base and 
#' all updates combined into one column; 'approach3' with the 
#' base and all updates as two separate columns; and 'approach4' 
#' with the base plus previous updates and the final update 
#' provided as two separate columns.
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if 
#' @importFrom stats aggregate
#' @importFrom stringr str_split
#' @return List of objects needed by `LSRPRISMA_flow()`.
#' @examples 
#' \dontrun{
#' data <- read.csv('inst/extdata/approach1.csv')
#' flowdata <- LSRPrisma_convert(data, format = 'approach4')
#' 
#' }
#' @export
LSRPrisma_convert <- function (data,
                               format = 'approach1'){
  
  ## define approach
  # keep as approach1, no action necessary, wrap process data and return
  if(format == 'approach1'){
    
    # separate data columns
    cols <- ncol(data)
    rest <- data.frame(data[1:7])
    dataset <- data.frame(data[8:cols])
    
    # separate numeric from string rows
    dataset_1 <- dataset[1:17,]
    dataset_2 <- dataset[18,] #exclusion reasons dbr
    dataset_3 <- dataset[19,]
    dataset_4 <- dataset[20,] #exclusion reasons other
    dataset_5 <- dataset[21:24,]
    
    # convert data to numeric where not strings
    dataset_1 <- as.data.frame(dataset_1) %>%
      mutate_if(is.character, ~as.numeric(.))
    dataset_3 <- as.data.frame(dataset_3) %>%
      mutate_if(is.character, ~as.numeric(.))
    dataset_5 <- as.data.frame(dataset_5) %>%
      mutate_if(is.character, ~as.numeric(.))
    
    output <- LSRPrisma_data(data)
    return(output)
  
    # convert to approach 2 (all data in one column)  
  } else if (format == 'approach2'){
    
    # separate data columns
    cols <- ncol(data)
    rest <- data.frame(data[1:7])
    dataset <- data.frame(data[8:cols])
    
    # separate numeric from string rows
    dataset_1 <- dataset[1:17,]
    dataset_2 <- dataset[18,] #exclusion reasons dbr
    dataset_3 <- dataset[19,]
    dataset_4 <- dataset[20,] #exclusion reasons other
    dataset_5 <- dataset[21:24,]
    
    # convert data to numeric where not strings
    dataset_1 <- as.data.frame(dataset_1) %>%
      mutate_if(is.character, ~as.numeric(.))
    dataset_3 <- as.data.frame(dataset_3) %>%
      mutate_if(is.character, ~as.numeric(.))
    dataset_5 <- as.data.frame(dataset_5) %>%
      mutate_if(is.character, ~as.numeric(.))
    
    # sum rows without text (numeric values)
    newdat_1 <- rowSums(dataset_1)
    newdat_3 <- rowSums(dataset_3)
    newdat_5 <- rowSums(dataset_5)
    
    #combine rows with text (strings)
    # row 18
    dataset_2 <- process_excludes(dataset_2)
    # row 20
    dataset_4 <- process_excludes(dataset_4)
    
    # combine
    newdat <- c(newdat_1, dataset_2, newdat_3, dataset_4, newdat_5) # combine rows of new data back together
    newdat <- cbind(rest, newdat) # combine new data with label dataset
    
    # rename new column
    colnames(newdat)[8] <- 'Base to latest update'
    
    # process data and return
    output <- LSRPrisma_data(newdat)
    return(output)
    
  } else if (format == 'approach3'){
    
    # separate data columns
    cols <- ncol(data)
    rest <- data.frame(data[1:7])
    base <- data.frame(data[8])
    dataset <- data.frame(data[9:cols])
    
    # base
      # separate numeric from string rows
      dataset_1 <- base[1:17,]
      dataset_2 <- base[18,] #exclusion reasons dbr
      dataset_3 <- base[19,]
      dataset_4 <- base[20,] #exclusion reasons other
      dataset_5 <- base[21:24,]
      
      # convert data to numeric where not strings
      dataset_1 <- as.data.frame(dataset_1) %>%
        mutate_if(is.character, ~as.numeric(.))
      dataset_3 <- as.data.frame(dataset_3) %>%
        mutate_if(is.character, ~as.numeric(.))
      dataset_5 <- as.data.frame(dataset_5) %>%
        mutate_if(is.character, ~as.numeric(.))
      
      # sum rows without text (numeric values)
      newdat_1 <- rowSums(dataset_1)
      newdat_3 <- rowSums(dataset_3)
      newdat_5 <- rowSums(dataset_5)
      
      #combine rows with text (strings)
      # row 18
      dataset_2 <- process_excludes(dataset_2)
      # row 20
      dataset_4 <- process_excludes(dataset_4)
      
      # combine
      newdat_base <- c(newdat_1, dataset_2, newdat_3, dataset_4, newdat_5) # combine rows of new data back together
      
    # update
      # separate numeric from string rows
      dataset_1 <- dataset[1:17,]
      dataset_2 <- dataset[18,] #exclusion reasons dbr
      dataset_3 <- dataset[19,]
      dataset_4 <- dataset[20,] #exclusion reasons other
      dataset_5 <- dataset[21:24,]
      
      # convert data to numeric where not strings
      dataset_1 <- as.data.frame(dataset_1) %>%
        mutate_if(is.character, ~as.numeric(.))
      dataset_3 <- as.data.frame(dataset_3) %>%
        mutate_if(is.character, ~as.numeric(.))
      dataset_5 <- as.data.frame(dataset_5) %>%
        mutate_if(is.character, ~as.numeric(.))
      
      # sum rows without text (numeric values)
      newdat_1 <- rowSums(dataset_1)
      newdat_3 <- rowSums(dataset_3)
      newdat_5 <- rowSums(dataset_5)
      
      #combine rows with text (strings)
      # row 18
      dataset_2 <- process_excludes(dataset_2)
      # row 20
      dataset_4 <- process_excludes(dataset_4)
      
      # combine
      newdat_update <- c(newdat_1, dataset_2, newdat_3, dataset_4, newdat_5) # combine rows of new data back together
    
    # combine all
    newdat <- cbind(rest, newdat_base, newdat_update) # combine new data with label dataset
    
    # rename new column
    colnames(newdat)[8] <- 'Base search'
    colnames(newdat)[9] <- 'Updates'
    
    # process data and return
    output <- LSRPrisma_data(newdat)
    return(output)
    
  } else if (format == 'approach4'){
    
    # separate data columns
    cols <- ncol(data)
    rest <- data.frame(data[1:7])
    base_plus <- data.frame(data[8:(cols-1)])
    dataset <- data.frame(data[,cols])
    
    # base_plus
      # separate numeric from string rows
      dataset_1 <- base_plus[1:17,]
      dataset_2 <- base_plus[18,] #exclusion reasons dbr
      dataset_3 <- base_plus[19,]
      dataset_4 <- base_plus[20,] #exclusion reasons other
      dataset_5 <- base_plus[21:24,]
      
      # convert data to numeric where not strings
      dataset_1 <- as.data.frame(dataset_1) %>%
        mutate_if(is.character, ~as.numeric(.))
      dataset_3 <- as.data.frame(dataset_3) %>%
        mutate_if(is.character, ~as.numeric(.))
      dataset_5 <- as.data.frame(dataset_5) %>%
        mutate_if(is.character, ~as.numeric(.))
      
      # sum rows without text (numeric values)
      newdat_1 <- rowSums(dataset_1)
      newdat_3 <- rowSums(dataset_3)
      newdat_5 <- rowSums(dataset_5)
      
      #combine rows with text (strings)
      # row 18
      dataset_2 <- process_excludes(dataset_2)
      # row 20
      dataset_4 <- process_excludes(dataset_4)
      
      # combine
      newdat_base <- c(newdat_1, dataset_2, newdat_3, dataset_4, newdat_5) # combine rows of new data back together
    
    # update
      # separate numeric from string rows
      dataset_1 <- dataset[1:17,]
      dataset_2 <- dataset[18,] #exclusion reasons dbr
      dataset_3 <- dataset[19,]
      dataset_4 <- dataset[20,] #exclusion reasons other
      dataset_5 <- dataset[21:24,]
      
      # convert data to numeric where not strings
      dataset_1 <- as.data.frame(dataset_1) %>%
        mutate_if(is.character, ~as.numeric(.))
      dataset_3 <- as.data.frame(dataset_3) %>%
        mutate_if(is.character, ~as.numeric(.))
      dataset_5 <- as.data.frame(dataset_5) %>%
        mutate_if(is.character, ~as.numeric(.))
      
      # sum rows without text (numeric values)
      newdat_1 <- rowSums(dataset_1)
      newdat_3 <- rowSums(dataset_3)
      newdat_5 <- rowSums(dataset_5)
      
      #combine rows with text (strings)
      # row 18
      dataset_2 <- process_excludes(dataset_2)
      # row 20
      dataset_4 <- process_excludes(dataset_4)
      
      # combine
      newdat_update <- c(newdat_1, dataset_2, newdat_3, dataset_4, newdat_5) # combine rows of new data back together
      
    # combine all
    newdat <- cbind(rest, newdat_base, newdat_update) # combine new data with label dataset
    
    # rename new column
    colnames(newdat)[8] <- 'Previous searches'
    colnames(newdat)[9] <- 'Last update'
    
    # process data and return
    output <- LSRPrisma_data(newdat)
    return(output)
    
  }
  
  
}


#' Process exclusion data
#' 
#' @description Function converts raw exclusion reasons from separate 
#' columns into a single string, involving consolidation across 
#' duplicated reasons.
#' @param data Exclusions reasons imported in the template format.
#' @return A single string of exclusions reasons and n formatted 
#' for plotting.
process_excludes <- function(data){
  
  data <- sub('0', '', data) # remove 0 values (1)
  
  if(paste(data, collapse = '') == ""){ # if no text exists, stop and return an empty result
    output <- "0"
    return(output)
  } else {
    data <- data[data != ''] # remove 0 values (2)
    data <- paste(data, collapse = '; ') # combine all columns
    data <- unlist(stringr::str_split(data, '; ')) # separate by '; '
    data <- stringr::str_split_fixed(data, ", ", 2) # separate into dataframe by ', '
    data <- aggregate(as.numeric(data[,2])~data[,1],data=data,FUN=sum) # consolidate duplicate rows (reasons) by summing n
    output <- paste(paste(data[,1], data[,2], sep = ', '), collapse = '; ') # paste back into deduplicated and summed list
    return(output)
  }
  
}
