#' Assignment:
#'   https://class.coursera.org/exdata-030/human_grading/view/courses/975125/assessments/4/submissions

library(plyr)
library(data.table)

data_url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
data_file <- "NEI_data.zip"
scc_file <- "Source_Classification_Code.rds"
nei_file <- "summarySCC_PM25.rds"
testing <- TRUE

if (!file.exists(nei_file)) {
    if (!file.exists(data_file)) {
        download.file(data_url, data_file, method="curl")
    }
    unzip(data_file)
    unlink(data_file)
}
if (!(testing & "NEI" %in% ls() & "SCC" %in% ls())) {
    NEI <- readRDS(nei_file)
    SCC <- readRDS(scc_file)
}

#' 1.  Have total emissions from PM2.5 decreased in the United States from
#'     1999 to 2008? Using the base plotting system, make a plot showing the
#'     total PM2.5 emission from all sources for each of the years 1999, 2002,
#'     2005, and 2008.
nei1 <- ddply(NEI, c("year"), .drop=FALSE, summarize, TotalEmissions=sum(Emissions))
barplot(nei1$TotalEmissions, axes=F, space=c(0,1), main="Total PM2.5 Emission (million tons)")
axis(side=2, labels=c(0:8), at=c(0, 1e6, 2e6, 3e6, 4e6, 5e6, 6e6, 7e6, 8e6))
axis(side=1, labels=nei1$year, at=seq_len(nrow(nei1))+0.5, tick=F)
