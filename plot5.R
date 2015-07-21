#' Assignment:
#'   https://class.coursera.org/exdata-030/human_grading/view/courses/975125/assessments/4/submissions

library(plyr)
library(ggplot2)

data_url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
data_file <- "NEI_data.zip"
scc_file <- "Source_Classification_Code.rds"
nei_file <- "summarySCC_PM25.rds"

if (!file.exists(nei_file)) {
    if (!file.exists(data_file)) {
        download.file(data_url, data_file, method="curl")
    }
    unzip(data_file)
    unlink(data_file)
}
if (!("NEI" %in% ls() & "SCC" %in% ls())) {
    NEI <- readRDS(nei_file)
    SCC <- readRDS(scc_file)
}

#' 5. How have emissions from motor vehicle sources changed from 1999–2008
#'    in Baltimore City?

scc5 <- c()
for (xc in 1:ncol(SCC)) {
    scc5 <- c(scc5, unlist(SCC[grepl("(motor|vehicle)", SCC[, xc], ignore.case=T, perl=T),]$SCC))
}
sccv5 <- SCC[unique(scc5),]$SCC
nei5 <- ddply(NEI[NEI$SCC %in% sccv5 & NEI$fips=="24510",], c("year"), .drop=FALSE, summarize, TotalEmissions=sum(Emissions))
ggplot(nei5, aes(year, TotalEmissions)) +
    geom_point(color="steelblue", size=4) +
    geom_smooth(method="lm") +
    labs(title="Total Emissions from Motor Vehicles in Baltimore City")
ggsave(filename = "plot5.png")
