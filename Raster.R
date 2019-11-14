#Downloading and working with raster data
library(raster)
clim = getData('worldclim', var='bio', res=5, path='tmp')


# longmin latmin longmax latmax 
ext = extent(-74, -69, 40, 45)

#crop
c2 = crop(clim, ext)
#Plot Temp prob is BIO101=MeanT 120/10DegreeC
plot(c2[[1]])

library(ggplot2)

c2_df = as.data.frame(c2, xy = TRUE)
head(c2_df)

#ggplot

ggplot() +
  geom_raster(data = c2_df, 
              aes(x = x, y = y, fill = bio1)) +
  coord_quickmap()

#modifying base
base + theme_bw()

#Test colors
base + scale_fill_gradientn(colours=c('navy', 'white', 'darkred'), na.value = "black")

#Color pallettes
library(viridis)
base + scale_fill_gradientn(colours=viridis(10), na.value = "black")


#distrubituion of data in rasters
ggplot() +
  geom_histogram(data = c2_df, 
                 aes(x = bio1))