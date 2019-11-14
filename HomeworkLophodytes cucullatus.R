# downloading GBIF data
library(spocc)
library(mapr)
library(ggplot2)
library(raster)
wc = getData('worldclim', var='bio', res=5, path='tmp')



spdist <- occ(query='Lophodytes cucullatus', from='gbif', limit =7500)

#No info is given
spdist

#tiblle to get data 
spdist$gbif$data

#look at S4
head(spdist$gbif$data)

#Convert to data frame
spdist_df = occ2df(spdist)


map_leaflet(spdist)
df = as.data.frame(occ2df(spdist$gbif))
map_leaflet(df[,c('name', 'longitude', 'latitude','locality', 'stateProvince', 'year', 'occurrenceID')])
ext = extent(-125, -55, 20, 60)
wc = crop(wc, ext)

#Make a data frame

wc_df = as.data.frame(wc, xy=TRUE)
sp_df = occ2df(spdist)
ggplot() +
  geom_raster(data = wc_df, aes(x = x, y = y, fill = bio1/10)) +
  geom_point(data=sp_df, aes(x=longitude, y=latitude), col='green') +
  coord_quickmap() +
  theme_bw() + 
  scale_fill_gradientn(colours=c('navy', 'white', 'darkred'),
                       na.value = "black")
#Exctract
extr = extract(wc, sp_df[,c('longitude', 'latitude')])
head(extr)
sp_ex = cbind(df[,c('name', 'longitude', 'latitude', 'stateProvince', 'year', 'occurrenceID')], extr)
sp_ex = na.omit(sp_ex)
head(sp_ex)

ggplot() +
  geom_raster(data = wc_df, aes(x = x, y = y, fill = bio1/10)) +
  geom_point(data=sp_ex, aes(x=longitude, y=latitude), col='orange') +
  coord_quickmap() +
  theme_bw() + 
  scale_fill_gradientn(colours=c('navy', 'white', 'darkred'),
                       na.value = "black")
