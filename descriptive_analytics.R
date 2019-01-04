# set working directory
setwd("C:\\Users\\Matthew A. Lanham\\Dropbox\\_Purdue\\_Teaching\\UR4A\\9_Descriptive Analytics")
# Load data
wineUrl <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data'
wine <- read.table(wineUrl, header=F, sep=',',  stringsAsFactors=F 
,col.names=c('Cultivar','Alcohol','Malic.acid','Ash','Alcalinity.of.ash'
,'Magnesium','Total.phenols','Flavanoids'
,'Nonflavanoid.phenols','Proanthocyanin','Color.intensity'
,'Hue','OD280.OD315.of.diluted.wines', 'Proline')
,colClasses=c("factor",rep("numeric",13)))

str(wine)
head(wine,n=2)

## Summarizing our data (review)
# summarize features
library(psych)
describe(wine)

# how many observations by cultivar class
table(wine$Cultivar)

# summarize features by cultivar
aggregate(.~Cultivar, data=wine, FUN=mean)

# Check for missing values
missing <- data.frame(sapply(wine, function(x) sum(is.na(x))))
names(missing) <- c("NumMissing")
missing

# Load in a custom function to assess data quality
source("DataQualityReport.R")
DataQualityReport(dataSetName=wine)                       # option 1
do.call("DataQualityReport", args=list(dataSetName=wine)) # option 2

## k-Means clustering
# obtain a training dataset that does not have the target variable
wineTrain <- wine[, which(names(wine) != "Cultivar")]

# set a seed to replicate results
set.seed(278613)

# perform k-means clustering
wineK3 <- kmeans(x=wineTrain, centers=3)

# inspect the clusters generated (size, cluster center/mean)
wineK3

## k-Means clustering (evaluation)
# visually inspect cluster formations
library(useful)
plot(wineK3, data=wineTrain)

## k-Means clustering (validation to actual values)
plot(wineK3, data=wine, class="Cultivar")

## k-Means clustering (Beware of initial algorithm starts)
set.seed(278613)
wineK3N25 <- kmeans(wineTrain, centers=3, nstart=25)

# see the cluster sizes with 1 random start
wineK3$size 

# see the cluster sizes with 25 random starts (and averaged over centroids)
wineK3N25$size

# compare clustering solution with 1 random start vs 25 random starts
source("multiplot.R") # allows one to plot multiple ggplot2 plots together
p1 <- plot(wineK3, data=wine, class="Cultivar")
p2 <- plot(wineK3N25, data=wine, class="Cultivar")
multiplot(p1, p2, cols=2)

## k-Means clustering (Hartigan's rule - Choosing the right number of k clusters)
library(useful)
wineBest <- FitKMeans(wineTrain, max.clusters=20, nstart=25, seed=278613)
wineBest
PlotHartigan(wineBest) 

## k-Means clustering (Choosing the right number of k clusters)
table(wine$Cultivar, wineK3N25$cluster)
plot(table(wine$Cultivar, wineK3N25$cluster)
, main ="Confusion Matrix for Wine Clustering"
, xlab ="Cultivar", ylab ="Cluster")

## k-Means clustering (Gap statistic - Choosing the right number of k clusters)
# shows the Gap statistic for a number of different clusters
library(cluster)
theGap <- clusGap(wineTrain, FUNcluster=pam, K.max=20)
gapDF <- as.data.frame(theGap$Tab)
gapDF

## k-Means clustering (Gap statistic - Choosing the right number of k clusters)
# logW curves
p3 <- ggplot(gapDF, aes(x=1: nrow(gapDF))) + 
    geom_line(aes(y=logW), color="blue") + 
    geom_point(aes(y=logW), color="blue") +
    geom_line(aes(y=E.logW), color="green") +
    geom_point(aes(y=E.logW), color="green") + 
    labs(x="Number of Clusters")
# gap curve
p4 <- ggplot(gapDF, aes(x=1: nrow(gapDF))) + 
    geom_line(aes(y=gap), color="red") + 
    geom_point(aes(y=gap), color="red") +
    geom_errorbar(aes(ymin=gap-SE.sim, ymax=gap + SE.sim), color =" red") + 
    labs(x="Number of Clusters", y="Gap")
multiplot(p3, p4, cols=2)

## K-medoids (Partitioning Around Medoids a.k.a PAM)
## PAM Example: World Bank data
myIndicators <- c("BX.KLT.DINV.WD.GD.ZS", "NY.GDP.DEFL.KD.ZG", "NY.GDP.MKTP.CD"
                  , "NY.GDP.MKTP.KD.ZG", "NY.GDP.PCAP.CD", "NY.GDP.PCAP.KD.ZG"
                  ,"TG.VAL.TOTL.GD.ZS")
library(WDI) 
# pull info on these indicators for all countries in our list
# not all countries have information for every indicator
# some countries do not have any data
wbInfo <- WDI(country="all", indicator=myIndicators, start=2011, end=2011, extra=T)
# get rid of aggregated info
wbInfo <- wbInfo[wbInfo$region != "Aggregates", ]
# get rid of countries where all the indicators are NA
wbInfo <- wbInfo[which(rowSums(!is.na(wbInfo[,myIndicators])) > 0), ]
# get rid of any rows where the iso is missing
wbInfo <- wbInfo[!is.na(wbInfo$iso2c), ] 
# set rownames so we know the country without using that for clustering
rownames(wbInfo) <- wbInfo$iso2c
# refactorize region, income and lending 
# this accounts for any changes in the levels
wbInfo$region <- factor(wbInfo$region)
wbInfo$income <- factor(wbInfo$income)
wbInfo$lending <- factor(wbInfo$lending)

## PAM Example: World Bank data (continued)
# find which columns to keep
# not those in this vector
keep.cols <- which(!names(wbInfo) %in% c("iso2c", "country", "year", "capital", "iso3c"))
# fit the clustering
wbPam <- pam(x=wbInfo[, keep.cols], k=12, keep.diss=T, keep.data=T)
# show the medoid observations
wbPam$medoids

## PAM Example: World Bank data (Cluster evalaution: Silhouette plot)
# make a silhouette plot
plot(wbPam, which.plots = 2, main ="")

## PAM Example: World Bank data (Cluster evalaution: Business context perspective)
getwd()
# download file programatically
#download.file(url="http://jaredlander.com/data/worldmap.zip"
#              ,destfile=paste0(getwd(),"/","worldmap.zip"), method="curl")
# unzip file programatically
# Of the 4 files, we only need to worry about the one ending in .shp because R will handle the rest. 
#unzip(zipfile="worldmap.zip")
# We read it in using readShapeSpatial from maptools.
library(maptools)
world <- readShapeSpatial("worldmap//world_country_admin_boundary_shapefile_with_fips_codes.shp")
head(world@data)
# There are some blatant discrepancies between the two-digit code in the World Bank shapefile and the two-digit code in the World Bank data pulled using WDI. Notably, Austria should be "AT," Australia "AU," Myanmar (Burma) "MM," Vietnam "VN" and so on.
library(dplyr)
world@data$FipsCntry <- as.character(recode(world@data$FipsCntry
                                            , AU="AT", AS="AU", VM="VN", BM="MM"
                                            , SP="ES", PO="PT", IC="IL", SF="ZA"
                                            , TU="TR", IZ="IQ", UK="GB", EI="IE"
                                            , SU="SD", MA="MG", MO="MA", JA="JP"
                                            , SW="SE", SN="SG")) 

## PAM Example: World Bank data (Cluster evalaution: Business context perspective) cont..
# make an id column using the rownames
world@data$id <- rownames(world@data)
# convert into a data.frame
library(broom)
# if this does not work
# (1) install rtools (https://cran.r-project.org/bin/windows/Rtools/)
# (2) install.packages("gpclib")
# (3) gpclibPermit() # this should return TRUE
gpclibPermitStatus()
library(gpclib)
library(rgdal)
world.df <- tidy(world, region="id")
head(world.df)

# Before we can join this to the clustering, we need to join FipsCntry back into world.df.
world.df <- left_join(world.df, world@data[,c("id", "CntryName", "FipsCntry")]
                      , by="id")
head( world.df)

# Now we can take the steps of joining in data from the clustering and the original World Bank data.
clusterMembership <- data.frame(FipsCntry = names(wbPam$clustering)
                                , Cluster = wbPam$clustering
                                , stringsAsFactors=F)
head(clusterMembership)
world.df <- left_join(world.df, clusterMembership, by="FipsCntry")
world.df $ Cluster <- as.character(world.df$Cluster)
world.df $ Cluster <- factor(world.df$Cluster, levels= 1:12)

# Building the plot itself requires a number of ggplot2 commands to format it correctly. 
ggplot() + geom_polygon(data=world.df, aes(x=long, y=lat, group=group
                                           , fill=Cluster, color=Cluster)) + 
    labs(x=NULL, y=NULL) + coord_equal() + 
    theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()
          , axis.text.x=element_blank(), axis.text.y=element_blank()
          , axis.ticks=element_blank(), panel.background=element_blank())

# Much like with K-means, the number of clusters in a K-medoids clustering must be specified. Something similar to Hartigan's Rule can be built using the dissimilarity information returned by pam.
wbPam$clusinfo



