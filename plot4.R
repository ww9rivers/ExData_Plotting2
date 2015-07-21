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

#' 4. Across the United States, how have emissions from coal combustion-related
#'    sources changed from 1999–2008?

scc4 <- c()
for (xc in 1:ncol(SCC)) {
    scc4 <- c(scc4, unlist(SCC[grepl("coal", SCC[, xc], ignore.case=T),]$SCC))
}
scc4 <- SCC[unique(scc4),]$SCC
nei4 <- ddply(NEI[NEI$SCC %in% scc4,], c("year"), .drop=FALSE, summarize, TotalEmissions=sum(Emissions))
ggplot(nei4, aes(year, TotalEmissions)) +
    geom_point(color="steelblue", size=4) +
    geom_smooth(method="lm") +
    labs(title="Total PM2.5 Emissions from Coal Combustion-Related Sources")
ggsave(filename = "plot4.png")
