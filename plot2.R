#' Assignment:
#'   https://class.coursera.org/exdata-030/human_grading/view/courses/975125/assessments/4/submissions

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

#' 2.  Have total emissions from PM2.5 decreased in the Baltimore City,
#'     Maryland (fips == "24510") from 1999 to 2008? Use the base plotting
#'     system to make a plot answering this question.
nei2 <- ddply(NEI[NEI$fips=="24510",], c("year"), .drop=FALSE, summarize, TotalEmissions=sum(Emissions))
png("plot2.png")
barplot(nei2$TotalEmissions, axes=F, space=c(0,1), main="Total PM2.5 Emission in Baltimore City, MD (million tons)")
axis(side=2, labels=c(0:8), at=c(0, 1e6, 2e6, 3e6, 4e6, 5e6, 6e6, 7e6, 8e6))
axis(side=1, labels=nei2$year, at=seq_len(nrow(nei2))+0.5, tick=F)
dev.off()
