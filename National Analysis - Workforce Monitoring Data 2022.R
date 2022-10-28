# LIBRARY AND PACKAGES ---------------------------------------------------------
.libPaths("C:/R library")

if(!require("tidyverse")) {install.packages("tidyverse")}
library(tidyverse)
library(magrittr)
library(forcats)
if(!require("DataExplorer")) {install.packages("DataExplorer")}
library(DataExplorer)
if(!require("summarytools")) {install.packages("summarytools")}
library(summarytools)
if(!require("corrplot")) {install.packages("corrplot")}
library(corrplot)
if(!require("grid")) {install.packages("grid")}
library(grid)
if(!require("png")) {install.packages("png")}
library(png)
if(!require("gridExtra")) {install.packages("gridExtra")}
library(gridExtra)


# IMPORT DATA ------------------------------------------------------------------
NationalWorkforceData <- read.csv("Home Office - Police Workforce Data 2007-2022.csv")


# CLEAN AND TIDY DATA ----------------------------------------------------------
NationalWorkforceData %<>%
  select(-c(Geo.code, Region, Worker.type, Total..FTE.)) %>%
  rename(Total.headcount = Total..headcount.) %>%
  mutate(Total.headcount = as.numeric((gsub(",","", .$Total.headcount)))) %>%
  mutate(Year = as.factor(Year)) %>%
  mutate(across(where(is.character), as.factor))

# NA's are not missing, but counts of 0. Therefore, set NA = 0
NationalWorkforceData[is.na(NationalWorkforceData)] <- 0

# extract ranks of interest
levels(NationalWorkforceData$Rank.description)

RanksofInterest <- c("Chief Inspector", "Chief Superintendent", "Constable",
                     "Inspector", "Sergeant", "Superintendent", "Chief Officer")
GenderofInterest <- c("Female", "Male")

NationalWorkforceData %<>%
  filter(Rank.description %in% RanksofInterest) %>%
  filter(Sex %in% GenderofInterest) %>%
  droplevels(.)
  
NationalWorkforceData$Rank.description <- factor(NationalWorkforceData$Rank.description, 
                                                 levels = c("Constable", 
                                                            "Sergeant", 
                                                            "Inspector", 
                                                            "Chief Inspector",
                                                            "Superintendent",
                                                            "Chief Superintendent", 
                                                            "Chief Officer"))
# aggregate ranks Superintendent+ ...
NationalWorkforceData$Rank.description %<>%
  fct_collapse(Constable = c("Constable"), Sergeant = c("Sergeant"), Inspector = c("Inspector"),
                                `Chief Inspector` = c("Chief Inspector"), 
                             `Superintendent and Above` = c("Superintendent", "Chief Superintendent", "Chief Officer"))

# ...and then sum total headcount across the dataframe
NationalWorkforceData %<>%
  group_by(Year, Force.Name, Sex, Rank.description) %>%
  summarise(Count = sum(Total.headcount))

rm(RanksofInterest, GenderofInterest)


# ANALYSIS 2022 ----------------------------------------------------------------

# isolate 2022 data
NationalWorkforceData2022 <-
NationalWorkforceData %>%
  filter(Year == 2022)

# for loop - turn 2022 data per force into a list
forcelist <- list()
# can get a for loop to print the data in each force, but not run the crosstab
for (i in seq_along(levels(NationalWorkforceData2022$Force.Name))) {
  output <- NationalWorkforceData2022 %>% filter(Force.Name %in% c(levels(Force.Name)[i])) %>% print()
  forcelist[[i]] <- output
}
rm(output, i)

# for loop - chi.sq to each force in forcelist then output graphic of residuals
for (i in forcelist) {
  xtab <- xtabs(Count ~ Rank.description + Sex, data = i)
  X2result <- chisq.test(xtab)
  #op1 <- data.frame(cbind(X2result$observed, X2result$expected)) # leave for if counts are needed
  op2 <- round(X2result$residuals, 2)
  output_filename <- paste0(i$Force.Name,".png")
  png(output_filename)
  title <- i$Force.Name[[1]]
  corrplot(op2, is.corr = FALSE, addCoef.col = 1, method = 'square', cl.pos = 'n', col.lim = c(-8.5, 6), 
           tl.col = 'black', tl.srt = 45, title = title, mar=c(0,0,5,0), number.cex = 2.2, tl.cex = 2.2, cex.main = 2.5)
  dev.off()
}

# had read out 44 individual .png files - need to plot these together


# COMBINE INTO ONE PLOT --------------------------------------------------------
# manual process from here:
levels(NationalWorkforceData2022$Force.Name)

# grab each png
plot1 <- readPNG('Avon & Somerset.png')
plot2 <- readPNG('Bedfordshire.png')
plot3 <- readPNG('British Transport Police.png')
plot4 <- readPNG('Cambridgeshire.png')
plot5 <- readPNG('Cheshire.png')
plot6 <- readPNG('Cleveland.png')
plot7 <- readPNG('Cumbria.png')
plot8 <- readPNG('Derbyshire.png')
plot9 <- readPNG('Devon & Cornwall.png')
plot10 <- readPNG('Dorset.png')
plot11 <- readPNG('Durham.png')
plot12 <- readPNG('Dyfed-powys.png')
plot13 <- readPNG('Essex.png')
plot14 <- readPNG('Gloucestershire.png')
plot15 <- readPNG('Greater Manchester.png')
plot16 <- readPNG('Gwent.png')
plot17 <- readPNG('Hampshire.png')
plot18 <- readPNG('Hertfordshire.png')
plot19 <- readPNG('Humberside.png')
plot20 <- readPNG('Kent.png')
plot21 <- readPNG('Lancashire.png')
plot22 <- readPNG('Leicestershire.png')
plot23 <- readPNG('Lincolnshire.png')
plot24 <- readPNG('London, City of.png')
plot25 <- readPNG('Merseyside.png')
plot26 <- readPNG('Metropolitan Police.png')
plot27 <- readPNG('Norfolk.png')
plot28 <- readPNG('North Wales.png')
plot29 <- readPNG('North Yorkshire.png')
plot30 <- readPNG('Northamptonshire.png')
plot31 <- readPNG('Northumbria.png')
plot32 <- readPNG('Nottinghamshire.png')
plot33 <- readPNG('South Wales.png')
plot34 <- readPNG('South Yorkshire.png')
plot35 <- readPNG('Staffordshire.png')
plot36 <- readPNG('Suffolk.png')
plot37 <- readPNG('Surrey.png')
plot38 <- readPNG('Sussex.png')
plot39 <- readPNG('Thames Valley.png')
plot40 <- readPNG('Warwickshire.png')
plot41 <- readPNG('West Mercia.png')
plot42 <- readPNG('West Midlands.png')
plot43 <- readPNG('West Yorkshire.png')
plot44 <- readPNG('Wiltshire.png')

# arrange them in a grid
grid.arrange(rasterGrob(plot1),
             rasterGrob(plot2),
             rasterGrob(plot3),
             rasterGrob(plot4),
             rasterGrob(plot5),
             rasterGrob(plot6),
             rasterGrob(plot7),
             rasterGrob(plot8),
             rasterGrob(plot9),
             rasterGrob(plot10),
             rasterGrob(plot11),
             rasterGrob(plot12),
             rasterGrob(plot13),
             rasterGrob(plot14),
             rasterGrob(plot15),
             rasterGrob(plot16),
             rasterGrob(plot17),
             rasterGrob(plot18),
             rasterGrob(plot19),
             rasterGrob(plot20),
             rasterGrob(plot21),
             rasterGrob(plot22),
             rasterGrob(plot23),
             rasterGrob(plot24),
             rasterGrob(plot25),
             rasterGrob(plot26),
             rasterGrob(plot27),
             rasterGrob(plot28),
             rasterGrob(plot29),
             rasterGrob(plot30),
             rasterGrob(plot31),
             rasterGrob(plot32),
             rasterGrob(plot33),
             rasterGrob(plot34),
             rasterGrob(plot35),
             rasterGrob(plot36),
             rasterGrob(plot37),
             rasterGrob(plot38),
             rasterGrob(plot39),
             rasterGrob(plot40),
             rasterGrob(plot41),
             rasterGrob(plot42),
             rasterGrob(plot43),
             rasterGrob(plot44),
              ncol=11)


