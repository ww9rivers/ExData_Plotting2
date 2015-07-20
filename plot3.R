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

#' 3. Of the four types of sources indicated by the type (point, nonpoint,
#'    onroad, nonroad) variable, which of these four sources have seen
#'    decreases in emissions from 1999–2008 for Baltimore City? Which have
#'    seen increases in emissions from 1999–2008? Use the ggplot2 plotting
#'    system to make a plot answer this question.
library(ggplot2)
plot.new()
nei3 <- ddply(NEI[NEI$fips=="24510",], c("year"), .drop=FALSE)
nei3$src <- factor(nei3$type)
ggplot(nei3, aes(year, Emissions)) +
    facet_grid(. ~ src) +
    geom_point(na.rm=T) +
    geom_smooth(method="lm")
ggsave(filename = "plot3.png", height=4, width=10)
