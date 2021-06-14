pacman::p_load(tidyverse, purrr, rnaturalearth, sf, raster, animation,
               rmapshaper, rnaturalearth, jpeg, png, grid, shades, data.table)

source('R/fire_misc_func.R')
source('R/fire_misc_animations_load.R')

# e_df <- read_delim('output/WW_10yr_FS_80p/stnd_Std_combined.csv', delim = ',') %>%
e_df <- read_delim('output/WW_10yr_FS_80p_Dyn/_stnd_Dyn_combined.csv', delim = ',') %>%
  left_join(hex %>% dplyr::select(CELL_ID, FLAG = western_flag20_NLCD))

# Misc plot examples .......

ani.options(interval = 1)
logo <- readPNG('../../Dropbox/FORSYS_logo.png')
logo2 <- readPNG('../../Dropbox/RMRS_logo.png')

e_df %>% filter(EVENT != 'T') %>%
  group_by(FUTURE, DECILE, FIRE_YR) %>%
  summarize_at(vars(AREA_HA, aTR_MS), sum, na.rm=T)

par(mar=rep(1,4)); plotBg(state = st, bbox=bbox)
pdat <- e_df %>% filter(FUTURE == 2, DECILE == 9, FIRE_YR == 10)
hex_ps <- hex_p %>%
  filter(CELL_ID %in% pdat$CELL_ID) %>%
  dplyr::select(CELL_ID) %>%
  left_join(pdat) %>%
  left_join(hex %>% dplyr::select(CELL_ID, OwnerCat, AREA_HA))
plotBg(state = st, bbox=bbox)
hex_ps %>% plotHexPt2('EVENT', adjSize = adj, col='red', add=T)

# Misc animations examples .......

bbox = st_as_sfc(st_bbox(hex_p)) %>% st_transform(5070) %>% st_bbox() %>% st_as_sfc()
bbox = st_as_sfc(st_bbox(c(xmin = -125, xmax = -119, ymax = 42, ymin = 38), crs=4326)) %>% st_transform(5070) %>% st_bbox() %>% st_as_sfc()

require(maptiles)
map_tiles <- bbox %>%
  st_bbox %>%
  st_as_sfc %>%
  st_buffer(100000) %>%
  get_tiles(zoom=8, crop = TRUE, provider = 'Esri.WorldTerrain') %>%  # umatilla
  stack()

future = 1; decile = 9
saveVideo(video.name = paste0('output_graphics/Omernik_WW_D',decile,'_F',future,'.mp4'),
  expr = {

    # extract event data for specific scenario rep
    pdat <- e_df %>% filter(FUTURE == !!future, DECILE == !!decile)

    # merge event data with hex
    hex_ps <- hex_p %>%
      filter(CELL_ID %in% pdat$CELL_ID) %>%
      dplyr::select(CELL_ID) %>%
      left_join(pdat) %>%
      left_join(hex %>% dplyr::select(CELL_ID, OwnerCat, AREA_HA))

    if(!is.null(bbox)) hex_ps_bb <- hex_ps %>% st_intersection(bbox) else hex_ps_bb <- hex_ps

    x <- (bbox %>% st_bbox %>% as.numeric)
    adj = 0.0002 * abs(x[1]-x[3]) + 50

    for(yr in 1:21){
      print(paste('year', yr))
      suppressMessages({

        # base plot
        layout(mat=matrix(c(1,1,1,2,2,2,3,4,5), ncol=3, byrow = T), heights = c(5,.5,1))

        par(mar=c(0,5,5,5))
        plot(bbox, col=NA, border=NA);
        plotRGB(map_tiles, add=T)
        plot(usfs, add=T, col=adjustcolor('black',0.2), border=adjustcolor('black',0))
        plot(states %>% st_geometry(), add=T)

        # plot events from previous years
        hex_ps_bb %>% filter(FIRE_YR < !!yr, EVENT != 'T_F', FLAG == 1) %>% plotHexPt2('EVENT', adjSize = adj, add=T, col='indianred4') # plot treatment
        hex_ps_bb %>% filter(FIRE_YR < !!yr, EVENT != 'T_F', FLAG == 0) %>% plotHexPt2('EVENT', adjSize = adj, add=T, col='indianred3') # plot treatment
        hex_ps_bb %>% filter(ETrt_YR < !!yr, EVENT != 'F_T') %>% plotHexPt2('EVENT', adjSize = adj, add=T, col='chartreuse4') # plot treatment
        hex_ps_bb %>% filter(ETrt_YR == !!yr, EVENT != 'F_T') %>% plotHexPt2('EVENT', adjSize = adj, add=T, col='green') # plot treatment
        hex_ps_bb %>% filter(ETrt_YR == !!yr, EVENT == 'F_T') %>% plotHexPt2('EVENT', adjSize = adj, add=T, col='black') # plot treatment over fire
        hex_ps_bb %>% filter(FIRE_YR == !!yr, EVENT != 'T_F') %>% plotHexPt2('EVENT', adjSize = adj, add=T, col='red') # plot fires
        hex_ps_bb %>% filter(FIRE_YR <= !!yr, EVENT == 'T_F') %>% plotHexPt2('EVENT', adjSize = adj, add=T, col='yellow3') # plot fires over treatment
        hex_ps_bb %>% filter(FIRE_YR == !!yr, EVENT == 'T_F') %>% plotHexPt2('EVENT', adjSize = adj, add=T, col='yellow') # plot fires over treatment

        plot(states %>% st_geometry(), add=T)
        plot(roads %>% st_geometry(), add=T, col=adjustcolor('black',0.5))

        # grid.raster(logo, x=0.90, y=.94, width=.15); grid.raster(logo2, x=0.90, y=.85, width=.10)
        grid.raster(logo, x=0.815, y=.92, width=.15); grid.raster(logo2, x=0.89, y=.92, width=.10)
        mtext(text=paste0('Year ', ifelse(yr>20, 20, yr), '\n(Decile ', decile, ', Future ', future,')'), side=3, line=-8, adj=0, cex=1.5, font=2)

        plot(1, axes=F, xlab='', ylab='', type='n')
        par(mar = rep(0,4))
        legend('bottom', cex=2, title = NA,
               legend=c('National forest','Wildfire','Treatments','Fire after treatment'),
               fill=c('grey80','brown','chartreuse4','yellow3'),
               ncol=4, box.lwd=NA, bg=NA, inset=0.25)

        if(yr > 0){

          e_df_dat <- hex_ps_bb %>% st_drop_geometry() %>% filter(FUTURE == !!future)

          a <- e_df_dat %>% filter(!is.na(FIRE_YR)) %>%
            mutate(YEAR = FIRE_YR, TYPE = 'FIRE') %>%
            mutate(EVENT = ifelse(EVENT == 'T_F', 'T_F', 'F')) %>%
            group_by(TYPE, YEAR, FLAG, EVENT) %>%
            summarize_at(vars(AREA_HA, aTR_MS), sum, na.rm=T)

          b <- e_df_dat %>% filter(!is.na(ETrt_YR)) %>%
            mutate(YEAR = ETrt_YR, TYPE = 'TREAT') %>%
            mutate(EVENT = ifelse(EVENT == 'F_T', 'F_T', 'T')) %>%
            group_by(TYPE, YEAR, FLAG, EVENT) %>%
            summarize_at(vars(AREA_HA, aTR_MS), sum, na.rm=T)

          ab <- bind_rows(a, b) %>%
            mutate(YEAR = factor(YEAR, 1:20)) %>%
            pivot_longer(cols = c(AREA_HA, aTR_MS), 'METRIC') %>%
            mutate(EVENT = paste(EVENT, FLAG)) %>%
            mutate(EVENT = factor(EVENT, c('T_F 1','F 1','F 0','F_T 1','T 1'))) %>%
            mutate(TYPE = paste(TYPE, METRIC))

          # # area burned subplot ....
          par(mar=c(2,5,2,1));
          gdat <- ab %>% ungroup %>%
            filter(TYPE == 'FIRE AREA_HA') %>%
            dplyr::select(TYPE, YEAR, EVENT, value) %>%
            complete(TYPE, YEAR, EVENT, fill = list(value=0)) %>%
            pivot_wider(id_cols=YEAR, names_from = EVENT, values_from=value)
          mx = max(apply(gdat[,-1],1,sum))
          barplot(gdat[1:yr,-1] %>% t, col=c('yellow3','brown','indianred'),
                  axes=F, ylim=c(0,mx), xlim=c(1,23), border=NA)
          title(main='Area burned', cex.axis=1.5, cex.lab=1.5, cex.main=2, line=0.5)
          title(ylab = 'M ha / yr', cex.axis=1.5, cex.lab=1.5, cex.main=2)
          axis(2, cex.axis=1.5)
          axis(1, at=seq(2,20,by = 2)*1.2, labels = seq(2,20,by = 2), cex.axis=1.5)

          # exposure subplot ....
          par(mar=c(2,5,2,1));
          gdat <- ab %>% ungroup %>%
            filter(TYPE == 'FIRE aTR_MS') %>%
            dplyr::select(TYPE, YEAR, EVENT, value) %>%
            complete(TYPE, YEAR, EVENT, fill = list(value=0)) %>%
            pivot_wider(id_cols=YEAR, names_from = EVENT, values_from=value)
          mx = max(apply(gdat[,-1],1,sum))
          barplot(gdat[1:yr,-1] %>% t, col=c('yellow3','brown','indianred'),
                  axes=F, ylim=c(0,mx), xlim=c(1,23), border=NA)
          title(main='Exposure', cex.axis=1.5, cex.lab=1.5, cex.main=2, line=0.5)
          title(ylab = 'Buildings / yr', cex.axis=1.5, cex.lab=1.5, cex.main=2)
          axis(2, cex.axis=1.5)
          axis(1, at=seq(2,20,by = 2)*1.2, labels = seq(2,20,by = 2), cex.axis=1.5)

          # exposure subplot ....
          par(mar=c(2,5,2,1));
          gdat <- ab %>% ungroup %>%
            filter(TYPE == 'TREAT AREA_HA') %>%
            dplyr::select(TYPE, YEAR, EVENT, value) %>%
            complete(TYPE, YEAR, EVENT, fill = list(value=0)) %>%
            pivot_wider(id_cols=YEAR, names_from = EVENT, values_from=value)
          mx = max(apply(gdat[,-1],1,sum))
          barplot(gdat[1:yr,-1] %>% t, col=c('chartreuse4','black'),
                  axes=F, ylim=c(0,mx), xlim=c(1,23), border=NA)
          title(main='Treatment', cex.axis=1.5, cex.lab=1.5, cex.main=2, line=0.5)
          title(ylab = 'ha / yr', cex.axis=1.5, cex.lab=1.5, cex.main=2)
          axis(2, cex.axis=1.5)
          axis(1, at=seq(2,20,by = 2)*1.2, labels = seq(2,20,by = 2), cex.axis=1.5)
        }
      })
    }
}, ani.width = 1000, ani.height = 1000, ani.res=50)
