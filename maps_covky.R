Sys.setlocale(category = "LC_ALL", locale = "cs_CZ.UTF-8")
memory.limit(size=4095)

library(pbtools)

library(shapefiles)
library(sp)
library(spatstat)
library(mapproj)
library(maptools)
library(rgdal)
library(ggmap)
library(rgeos)

library(readxl)

# Load map shape data
# setwd('~/Documents/Research/Geodata/CUZK//')
setwd('D:/usr/Geodata/CUZK/')
obce_mapfile <- readOGR(dsn='.',layer='SPH_OBEC', encoding = 'LATIN2')
# obce_mapfile <- thinnedSpatialPoly(obce_mapfile, 0.005, topologyPreserve = TRUE)

kraje_mapfile <- readOGR(dsn='.',layer='SPH_KRAJ', encoding = 'LATIN2')
kraje_mapfile <- thinnedSpatialPoly(kraje_mapfile, 0.005, topologyPreserve = TRUE)
kraje_mapdata <- fortify(kraje_mapfile, region="NAZEV_NUTS")
kraje_mapdata <- arrange(kraje_mapdata, order)

obce_coords <- data.frame(long=obce_coords(obce_mapfile)[, 1],lat=obce_coords(obce_mapfile)[, 2])
obce_coords[, 'NAZEV_LAU2'] <- as.character(obce_mapfile@data[,'NAZEV_LAU2'])
obce_coords[, 'KOD_LAU2'] <- as.character(obce_mapfile@data[,'KOD_LAU2'])

rm(obce_mapfile)
gc()

# obce_mapdata <- fortify(obce_mapfile, region='KOD_LAU2')

covky <- read_excel('C:/Users/boupet/Downloads/cov.xlsx',2)
sldb_kanalizace <- read_excel('C:/Users/boupet/Downloads/sldb_kanalizace.xlsx')
covky$KODOBCE <- as.character(covky$KODOBCE)

plotdata <- right_join(obce_mapfileset, covky, by=c('id'='KODOBCE'))
plotdata <- arrange(plotdata, order)

rm(obce_mapfileset)
gc()

ggplot(plotdata, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group, fill=Kraj, colour=stat)) +
  coord_map() +
  geom_text(aes(x=long, y=lat, label=NAZEV_LAU1), data=obce_coords)


covkyplotdata <- merge(obce_coords, covky, by.x="KOD_LAU2", by.y="KODOBCE")

kraje_mapdata <- merge(kraje_mapdata, sldb_kanalizace, by.x='id', by.y='kraj')

map <- get_stamenmap(location="Czech Republic", zoom=7)

ggmap(map,
      base_layer = ggplot() +
        geom_polygon(data = kraje_mapdata,
                     aes(y=lat, x=long, group=group, fill=group),
                     size=1, alpha=.6, colour="grey") +
#         geom_point(data=covkyplotdata,
#                    aes(long, lat, size=`Dosažená hodnota indikátoru`,
#                        colour=`Příspěvek Společenství - Sml/Dod`)) +
        facet_wrap(~ velikostobce) +
        scale_fill_discrete())

ggmap(map,
      base_layer = ggplot() +
        geom_polygon(data = kraje_mapdata,
                     aes(y=lat, x=long)))


ggplot(covky, aes(x = `Počet obyvatel celkem`)) + geom_histogram() + scale_x_log10()

loadcustomthemes()

ggplot(data=kraje_mapdata, aes(x=long, y=lat)) +
  geom_polygon(colour="white", aes(group=group, fill=kanalproc)) +
  geom_point(data=covkyplotdata, aes(x=long, y=lat, size=`Dosažená hodnota indikátoru`),
             colour='red', alpha=0.7, pch=20, fill='red') +
  facet_wrap(~ velikostobce) +
  scale_fill_distiller(type="seq", palette=1) +
  scale_size(range = c(3,12)) +
  theme(axis.text=element_blank(), panel.grid=element_blank()) +
  coord_map()
