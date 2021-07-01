t <- tidyr::separate_rows(dataset[20,], names(dataset[20,]), sep='; ', convert = TRUE)
t <- data.frame(lapply(t, as.character), stringsAsFactors=FALSE)
t <- tidyr::pivot_longer(t, names(t), names_to = "col", values_to = "count")
t$count <- gsub('0', NA, t$count)
t <- t[complete.cases(t), ]
library(magrittr)
t <- t %>%
  tidyr::separate(count, c("reason", "n"), sep = ', ')
t <- t[!duplicated(t), ]
#format 1 - update by reason
t <- tidyr::pivot_wider(t, names_from = col, values_from = n)
options(warn=-1)
t <- cbind(t[,1], purrr::map2_dfc(colnames(t[,2:ncol(t)]), t[,2:ncol(t)], paste, sep = ' (n='))
options(warn=0)
t[,2:ncol(t)] <- lapply(t[,2:ncol(t)], function(x){paste0(x, '); ')})
t[] <- lapply(t, function(x) gsub("[{}]+", "", replace(x, grepl("=NA", x), '')))
if(ncol(t) > 2) {
  t$new <- do.call(paste0, c(t[,2:ncol(t)])) 
} else {
  t$new <- t[,2]
}
t$new <- substr(t$new, 1, nchar(t$new) - 2)
other_excluded <- paste(paste(t$reason, t$new, sep = ': '), collapse = '\n')



