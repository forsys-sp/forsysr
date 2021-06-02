
pacman::p_load(tidyverse, purrr, rnaturalearth, sf, raster, animation)
source('R/ce_misc_func.R')

# STEP 2: compile forsys results ...

# reference data...
load('data/hexnet_west.Rdata')

hex_p %>% plotHexPt('western_flag20_NLCD')

e_df %>% filter(SCN_ID == 1, YEAR == 11) %>%
  plotHexPt('EVENT', adjSize = 500, pal=c('red','yellow','blue','purple'))

e_df %>% filter(SCN_ID == 4, EVENT == 'T_F', YEAR == 2) %>%
  plotHexPt('EVENT', adjSize = 500, col='darkgreen')


e_df %>% filter(FIRE_NUMBER == 425057, SCN_ID == 4, EVENT == 'F_T') %>%
  plotHexPt('EVENT')

e_df %>%
  filter(YEAR > 0, SCN_ID == 5, EVENT %in% c('T_F','F_T')) %>%
  plotHexPt('OwnerCat', pch=15, cex=0.1, main=paste('Fire Future'))

plotHexPt(e_df %>% filter(YEAR > 0, SCN_ID == 1), 'EVENT', pch=15, cex=0.1, main=paste('Fire Future', scn))


plotHexPt(f_df %>% filter(SCN_ID == !!scn, FIRE_NUMBER == !!fn), 'OwnerCat', pch=15)
plotHexPt(f_df %>% filter(SCN_ID == !!scn, FIRE_YR == 4), 'OwnerCat', pch=15, cex=0.5)
plotHexPt(e_df %>% filter(YR == 3, grepl('etrt', EVENT)), col='green', pch=4, cex=0.5, main='USFS treatments')
plotHexPt(e_df %>% filter(YR == 5, grepl('fire', EVENT)), col='brown', pch=4, cex=0.5, main='USFS Fires')

ww_nf <- st_read('../../Dropbox/!gis/boundaries/USFS_Forests_NoHoles.shp') %>% st_transform(5070)

saveGIF({
  for(i in 1:20){
    par(mfrow=c(1,2))

    plot(1017236.4, -351979.5, pch=16, cex=0.1, ylim=c(1017236.4,3146786.5), xlim=c(-2292411.9,-351979.5), type='n', axes=F, xlab='', ylab='')
    plot(ww_nf %>% st_geometry(), col='grey', border=NA, add=T)
    plotHexPt(f_df %>% filter(SCN_ID == !!scn, FIRE_YR < i), col='salmon', pch=15, cex=0.25, add=T)
    sp::plot(ne_countries(continent = 'north america', returnclass = 'sf') %>% st_geometry() %>% st_transform(5070), add=T)
    sp::plot(ne_countries(country = 'united states of america', returnclass = 'sf') %>% st_geometry() %>% st_transform(5070), add=T)
    plotHexPt(f_df %>% filter(SCN_ID == !!scn, FIRE_YR == i), col='brown', pch=15, cex=0.25, main='WW fires', add=T)

    mtext(paste('Fires at YR', i))


    plot(1017236.4, -351979.5, pch=16, cex=0.1, ylim=c(1017236.4,3146786.5), xlim=c(-2292411.9,-351979.5), type='n', axes=F, xlab='', ylab='')
    plot(ww_nf %>% st_geometry(), col='grey', border=NA, add=T)
    plotHexPt(s_df %>% filter(ETrt_YR < i), col='lightgreen', pch=15, cex=0.25, main='Treatment', add=T)
    sp::plot(ne_countries(continent = 'north america', returnclass = 'sf') %>% st_geometry() %>% st_transform(5070), add=T)
    sp::plot(ne_countries(country = 'united states of america', returnclass = 'sf') %>% st_geometry() %>% st_transform(5070), add=T)
    plotHexPt(s_df %>% filter(ETrt_YR == i), col='chartreuse4', pch=15, cex=0.25, main='Treatment', add=T)

    mtext(paste('Treatment at YR', i))

  }
}, movie.name = "movie_3.gif", interval = 0.5, nmax = 30, ani.width = 800, ani.height = 400)


# GRAPHICS AND ANIMATION --------

pacman::p_load(gganimate, cowplot, animation)

require(animation)
saveGIF({
  for(i in 1:20){
    plot(tmp[tmp$YEAR <= i,'EVENT'], pch=16, cex=0.1, ylim=c(1017236.4,3146786.5), xlim=c(-2292411.9,-351979.5))
    plot(tmp[tmp$YEAR == i,'EVENT'], pch=16, cex=0.2, add=T)
  }
}, movie.name = "movie.gif", interval = 0.1, nmax = 30,
ani.width = 600)

plot(tmp[,'EVENT'], pch=16, cex=0.2)

