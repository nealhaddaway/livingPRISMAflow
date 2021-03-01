library(ggplot2)
library(gridExtra)
#' Create 'fake' dataset to establish plot limits
x <- c(0,22)
y <- c(0, 30)
df <- data.frame(x, y)

#' Read in the data and select a wor for box1
dat <- read.csv(file.choose())
dataset <- data.frame(dat[,2:9])
names(dataset) <- gsub('..2','\n2',colnames(dataset))

box1dat <- paste(colnames(dataset), dataset[1,], sep = "\n")
box2dat <- paste(colnames(dataset), dataset[2,], sep = "\n")
box3dat <- paste(colnames(dataset), dataset[3,], sep = "\n")
box4dat <- paste(colnames(dataset), dataset[4,], sep = "\n")
box5dat <- paste(colnames(dataset), dataset[5,], sep = "\n")
box6dat <- paste(colnames(dataset), dataset[6,], sep = "\n")
box7dat <- paste(colnames(dataset), dataset[7,], sep = "\n")
box8dat <- paste(colnames(dataset), dataset[8,], sep = "\n")
box9dat <- paste(colnames(dataset), dataset[9,], sep = "\n")

excl_reasons <- c("No population of interest: n=2     Jan 2020", "No intervention of interest: n=1     Jan2020", "No intervention of interest: n=1     Aug 2020")

#' Box labels and positions
#' Box 1
lab1 <- "Records identified through database searching"
box1_xmin <- 0.9 #box left limit
box1_xmax <- xdim_set + box1_xmin + 0.2 #box right limit (set by number of data columns)
box1_ymin <- 25.9 #box bottom limit
box1_ymax <- 30.1 #box top limit
lab1_loc_x <- (box1_xmax - box1_xmin - 0.2) / 2 + (box1_xmin + 0.1) #x label location
lab1_loc_y <- box1_ymax - 0.9 #y label location
box1_data_loc_xmin <- box1_xmin + 0.3 #x data location
box1_data_loc_y <- box1_ymin + 1.3 #y data location



ggplot(df, aes(x = x, y = y)) +
  ggpubr::theme_transparent(base_size = 12, base_family = "")+
  scale_x_continuous(name="x") + 
  scale_y_continuous(name="y") +
  
  geom_rect(mapping=aes(xmin=box1_xmin, xmax=box1_xmax, ymin=box1_ymin, ymax=box1_ymax), color="black", alpha=0) + #records identified through electronic database searching
  geom_rect(mapping=aes(xmin=box2_xmin, xmax=box2_xmax, ymin=box2_ymin, ymax=box2_ymax), color="black", alpha=0) + #additional records identified through other sources
  geom_rect(mapping=aes(xmin=box3_xmin, xmax=box3_xmax, ymin=box3_ymin, ymax=box3_ymax), color="black", alpha=0) + #records after duplicates removed
  geom_rect(mapping=aes(xmin=box4_xmin, xmax=box4_xmax, ymin=box4_ymin, ymax=box4_ymax), color="black", alpha=0) + #records screened
  geom_rect(mapping=aes(xmin=box5_xmin, xmax=box5_xmax, ymin=box5_ymin, ymax=box5_ymax), color="black", alpha=0) + #records excluded
  geom_rect(mapping=aes(xmin=box6_xmin, xmax=box6_xmax, ymin=box6_ymin, ymax=box6_ymax), color="black", alpha=0) + #full text articles assessed for eligibility
  geom_rect(mapping=aes(xmin=box7_xmin, xmax=box7_xmax, ymin=box7_ymin, ymax=box7_ymax), color="black", alpha=0) + #full text articles excluded, with reasons
  geom_rect(mapping=aes(xmin=box8_xmin, xmax=box8_xmax, ymin=box8_ymin, ymax=box8_ymax), color="black", alpha=0) + #studies included in qualitative synthesis box
  geom_rect(mapping=aes(xmin=box9_xmin, xmax=box9_xmax, ymin=box9_ymin, ymax=box9_ymax), color="black", alpha=0) + #studies included in quantitative synthesis box
  
  annotate(geom="text", x=lab1_loc_x, y=lab1_loc_y-1, label="Records identified through\ndatabase searching up to Aug 2020\n(n = 150)", size=4.5) #box1 label
  
  
  