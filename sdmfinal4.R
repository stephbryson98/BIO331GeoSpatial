#Potential Distribution
taxon = "Nothofagus antarctica"
files = list.files('data', pattern = taxon, full.names=T)
print(files)
gri.files = files[grep('.gri', files)] #Only need one of the two files to read. R 'raster::stack()' finds the other.

mods = stack(gri.files)
names(mods) = c('current', '2070_2.6', '2070_8.5') ## Verify this is the same order as in gri.files object
plot(mods)

library(ggplot2)

shift85 = mods[[3]] - mods[[1]] # future 2.6 scenario minus current prediction

shift85_df = as.data.frame(shift85, xy=T)

(base85 = ggplot() +
    geom_raster(data = shift85_df, 
                aes(x = x, y = y, fill = layer)) + 
    coord_quickmap() +
    theme_bw() + 
    scale_fill_gradientn(colours=c('navy', 'white', 'darkred'), na.value = "black"))