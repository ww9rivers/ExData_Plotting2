#' Assignment:
#'   https://class.coursera.org/exdata-030/human_grading/view/courses/975125/assessments/4/submissions

library(plyr)
library(ggplot2)

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

#' 6. Compare emissions from motor vehicle sources in Baltimore City with
#'    emissions from motor vehicle sources in Los Angeles County, California
#'    (fips == "06037"). Which city has seen greater changes over time in
#'    motor vehicle emissions?

scc6 <- c()
for (xc in 1:ncol(SCC)) {
    scc6 <- c(scc6, unlist(SCC[grepl("(motor|vehicle)", SCC[, xc], ignore.case=T, perl=T),]$SCC))
}
sccv6 <- SCC[unique(scc6),]$SCC

nei6 <- data.frame()
nei6 <- ddply(NEI[NEI$SCC %in% sccv6 & NEI$fips=="24510",],
              c("year", "fips"),
              .drop=FALSE, summarize,
              TotalEmissions=sum(Emissions),
              city="Baltimore City")
nei6 <- rbind(nei6,
              ddply(NEI[NEI$SCC %in% sccv6 & NEI$fips=="06037",], c("year", "fips"),
                    .drop=FALSE, summarize,
                    TotalEmissions=sum(Emissions),
                    city="Los Angeles County"))
ggsave(filename = "plot6.png",
       plot=ggplot(nei6, aes(year, TotalEmissions)) +
           facet_grid(. ~ city) +
           geom_point(color="steelblue", size=4) +
           geom_smooth(method="lm") +
           labs(title="Comparison of Total Emissions from Motor Vehicles"))
