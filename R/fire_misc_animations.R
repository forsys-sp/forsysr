pacman::p_load(tidyverse, purrr, rnaturalearth, sf, raster, animation,
               rmapshaper, rnaturalearth, jpeg, png, grid, shades, data.table)

source('R/fire_misc_func.R')
source('R/fire_misc_animations_load.R')

# e_df <- read_delim('output/WW_10yr_FS_80p/stnd_Std_combined.csv', delim = ',') %>%
e_df <- read_delim('output/WW_10yr_FS_80p/stnd_Std_combined.csv', delim = ',') %>%
  left_join(hex %>% dplyr::select(CELL_ID, FLAG = western_flag20_NLCD))

# Misc plot examples .......

ani.options(interval = 1)
logo <- readPNG('../../Dropbox/FORSYS_logo.png')
logo2 <- readPNG('../../Dropbox/RMRS_logo.png')

e_df %>% filter(EVENT != 'T_F') %>%
  group_by(SCN_ID) %>%
  summarize_at(vars(AREA_HA, aTR_MS), sum, na.rm=T) %>%
  arrange(-aTR_MS)

# Misc animations examples .......

scn = 27
# bbox = st_as_sfc(st_bbox(c(xmin = -125, xmax = -119, ymax = 40, ymin = 37), crs=4326)) %>% st_transform(5070)
bbox = st_as_sfc(st_bbox(c(xmin = -120, xmax = -116.25, ymax = 46.33, ymin = 43.4), crs=4326)) %>% st_transform(5070) %>% st_transform(5070) %>% st_bbox() %>% st_as_sfc()
bbox = st_as_sfc(st_bbox(c(xmin = -119, xmax = -118, ymax = 45.25, ymin = 44.55), crs=4326)) %>% st_transform(5070) %>% st_bbox() %>% st_as_sfc()
bbox = st_as_sfc(st_bbox(hex_p)) %>% st_transform(5070) %>% st_bbox() %>% st_as_sfc()

# bbox = st_as_sfc(st_bbox(c(xmin = -118.5, xmax = -117.7, ymax = 46, ymin = 45.3), crs=4326)) %>% st_transform(5070)
# bbox = st_as_sfc(st_bbox(states_w11 %>% filter(postal=='WA')))
# bbox = NULL

par(mar=rep(1,4)); plotBg(state = st, bbox=bbox)
if(!is.null(bbox)) plot(st_as_sf(bbox), add=T, lty=2)

require(maptiles)
map_tiles <- bbox %>%
  st_bbox %>%
  st_as_sfc %>%
  st_buffer(100000) %>%
  get_tiles(zoom=8, crop = TRUE, provider = 'Esri.WorldTerrain') %>%  # umatilla
  stack()

# %>%
  projectRaster(crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs',
                method = 'ngb')


plotRGB(map_tiles)

# pa <- st_read('~/Dropbox/!gis/Firesheds_CONUS.gdb/', 'ProjectAreas')
# pa2 <- pa %>% st_intersection(st_as_sf(bbox)) %>% st_transform(5070)

scn = 3
saveVideo(video.name = paste0('output_graphics/Dyn_Umatilla_',scn,'.mp4'),
  expr = {

    pdat <- e_df %>% filter(SCN_ID == !!scn)

    hex_ps <- hex_p %>%
      filter(CELL_ID %in% pdat$CELL_ID) %>%
      dplyr::select(CELL_ID) %>%
      left_join(pdat) %>%
      left_join(hex %>% dplyr::select(CELL_ID, OwnerCat, AREA_HA))

    pa_s <- pa %>% st_geometry() %>% st_intersection(bbox)

    # hex_ps_bb <- hex_ps %>% st_crop(bbox %>% st_buffer(0))
    if(!is.null(bbox)) hex_ps_bb <- hex_ps %>% st_intersection(bbox) else hex_ps_bb <- hex_ps

    x <- (bbox %>% st_bbox %>% as.numeric)
    adj = 0.0002 * abs(x[1]-x[3]) + 50

    for(yr in 1:21){
      print(paste('year', yr))
      suppressMessages({

        # base plot
        layout(mat=matrix(c(1,1,1,2,3,4), ncol=3, byrow = T), heights = c(5,1))
        par(mar=rep(1,4)); plotBg(state = st, bbox=bbox)

        plotRGB(map_tiles, add=T)
        plot(usfs, add=T, col=adjustcolor('black',0.2), border=adjustcolor('black',0))
        if(!is.null(bbox)) plot(st_as_sf(bbox), add=T, lty=3)

        alpha = 1

        # plot events from previous years
        hex_ps_bb %>% filter(FIRE_YR < !!yr, EVENT != 'T_F', FLAG == 1) %>% plotHexPt2('EVENT', adjSize = adj, add=T, col=adjustcolor('indianred4', alpha)) # plot treatment
        hex_ps_bb %>% filter(FIRE_YR < !!yr, EVENT != 'T_F', FLAG == 0) %>% plotHexPt2('EVENT', adjSize = adj, add=T, col=adjustcolor('indianred3', alpha)) # plot treatment
        hex_ps_bb %>% filter(ETrt_YR < !!yr, EVENT != 'F_T') %>% plotHexPt2('EVENT', adjSize = adj, add=T, col=adjustcolor('chartreuse4', alpha)) # plot treatment
        hex_ps_bb %>% filter(ETrt_YR == !!yr, EVENT != 'F_T') %>% plotHexPt2('EVENT', adjSize = adj, add=T, col=adjustcolor('green', alpha)) # plot treatment
        hex_ps_bb %>% filter(ETrt_YR == !!yr, EVENT == 'F_T') %>% plotHexPt2('EVENT', adjSize = adj, add=T, col=adjustcolor('black', alpha)) # plot treatment over fire
        hex_ps_bb %>% filter(FIRE_YR == !!yr, EVENT != 'T_F') %>% plotHexPt2('EVENT', adjSize = adj, add=T, col=adjustcolor('red', alpha)) # plot fires
        hex_ps_bb %>% filter(SCN_ID == !!scn, FIRE_YR <= !!yr, EVENT == 'T_F') %>% plotHexPt2('EVENT', adjSize = adj, add=T, col=adjustcolor('yellow3', alpha)) # plot fires over treatment
        hex_ps_bb %>% filter(SCN_ID == !!scn, FIRE_YR == !!yr, EVENT == 'T_F') %>% plotHexPt2('EVENT', adjSize = adj, add=T, col=adjustcolor('yellow', alpha)) # plot fires over treatment

        plot(states %>% st_geometry(), add=T)
        plot(roads %>% st_geometry(), add=T, col=adjustcolor('black',0.5))
        # plot(pa_s, add=T, border=adjustcolor('black',0.25), col=NA)
        # text(pp2$X, pp2$Y, labels=pp2$name)

        # grid.raster(logo, x=0.90, y=.94, width=.15); grid.raster(logo2, x=0.90, y=.85, width=.10)
        grid.raster(logo, x=0.845, y=.92, width=.15); grid.raster(logo2, x=0.92, y=.92, width=.10)
        mtext(text=paste0('Scenario ', scn, ' Year ', ifelse(yr>20, 20, yr)), side=3, line=-5, adj=0.5, cex=1.5, font=2)

        legend('bottomleft', cex=1.75, title = 'Legend',
               legend=c('National forest','New fire','Past fire - manageable','Past fire - unmanageable',
                        'New treatment', 'Past treatment','Fire after treatment','Treatment after fire'),
               fill=c('grey80','red','brown','indianred','green','chartreuse4','yellow3','black'), ncol=4, box.lwd=NA, bg=NA)

        if(yr > 0){

          e_df_dat <- hex_ps_bb %>% st_drop_geometry() %>% filter(SCN_ID == !!scn)

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



### ANIMATION VERSION 2.... SIMPLIFIED LEGEND


managed <- raster('../../Dropbox/!gis/ownership/PAD20_CONUS_Manage.tif') %>% raster::projectRaster(r, method='ngb')
owner <- raster('../../Dropbox/!gis/ownership/PAD20_CONUS_OwnName.tif') %>% raster::projectRaster(r, method='ngb')
usfs_protect <- owner == 31 & managed == 0
r2 <- rasterize(r, as(hex_p, 'Spatial'), field='western_flag20_NLCD')

# create raster grid for running intersect
res = 1000
r <- raster(extent(hex_p), resolution=res)
crs(r) <- crs(hex_p)
not_available <- fasterize(fp_i, r, field = 'FIRE_NU', fun = 'first', by='FIRE_YR')

bbox = st_as_sfc(st_bbox(c(xmin = -119, xmax = -118, ymax = 45.25, ymin = 44.55), crs=4326)) %>% st_transform(5070) %>% st_bbox() %>% st_as_sfc()
bbox = st_as_sfc(st_bbox(hex_p)) %>% st_transform(5070) %>% st_bbox() %>% st_as_sfc()

scn = 1
saveVideo(video.name = paste0('output_graphics/UM_',scn,'.mp4'),
          expr = {

            pdat <- e_df %>% filter(SCN_ID == !!scn)

            hex_ps <- hex_p %>%
              filter(CELL_ID %in% pdat$CELL_ID) %>%
              dplyr::select(CELL_ID) %>%
              left_join(pdat) %>%
              left_join(hex %>% dplyr::select(CELL_ID, OwnerCat, AREA_HA))

            pa_s <- pa %>% st_geometry() %>% st_intersection(bbox)

            # hex_ps_bb <- hex_ps %>% st_crop(bbox %>% st_buffer(0))
            if(!is.null(bbox)) hex_ps_bb <- hex_ps %>% st_intersection(bbox) else hex_ps_bb <- hex_ps

            x <- (bbox %>% st_bbox %>% as.numeric)
            adj = 0.0002 * abs(x[1]-x[3]) + 50

            for(yr in 1:21){
              print(paste('year', yr))
              suppressMessages({

                # base plot
                layout(mat=matrix(c(1,1,1,2,3,4), ncol=3, byrow = T), heights = c(5,1))
                par(mar=rep(1,4)); plotBg(state = st, bbox=bbox)

                plotRGB(map_tiles, add=T)
                plot(usfs, add=T, col=adjustcolor('black',0.2), border=adjustcolor('black',0))
                # if(!is.null(bbox)) plot(st_as_sf(bbox), add=T, lty=3)

                alpha = 1

                # plot events from previous years
                hex_ps_bb %>% filter(FIRE_YR < !!yr, EVENT != 'T_F', FLAG == 1) %>% plotHexPt2('EVENT', adjSize = adj, add=T, col=adjustcolor('indianred4', alpha)) # plot treatment
                # hex_ps_bb %>% filter(FIRE_YR < !!yr, EVENT != 'T_F', FLAG == 0) %>% plotHexPt2('EVENT', adjSize = adj, add=T, col=adjustcolor('indianred3', alpha)) # plot treatment
                hex_ps_bb %>% filter(ETrt_YR < !!yr, EVENT != 'F_T') %>% plotHexPt2('EVENT', adjSize = adj, add=T, col=adjustcolor('chartreuse4', alpha)) # plot treatment
                hex_ps_bb %>% filter(ETrt_YR == !!yr, EVENT != 'F_T') %>% plotHexPt2('EVENT', adjSize = adj, add=T, col=adjustcolor('green', alpha)) # plot treatment
                hex_ps_bb %>% filter(ETrt_YR == !!yr, EVENT == 'F_T') %>% plotHexPt2('EVENT', adjSize = adj, add=T, col=adjustcolor('black', alpha)) # plot treatment over fire
                hex_ps_bb %>% filter(FIRE_YR == !!yr, EVENT != 'T_F', FLAG == 1) %>% plotHexPt2('EVENT', adjSize = adj, add=T, col=adjustcolor('red', alpha)) # plot fires
                # hex_ps_bb %>% filter(SCN_ID == !!scn, FIRE_YR <= !!yr, EVENT == 'T_F') %>% plotHexPt2('EVENT', adjSize = adj, add=T, col=adjustcolor('yellow3', alpha)) # plot fires over treatment
                hex_ps_bb %>% filter(SCN_ID == !!scn, FIRE_YR == !!yr, EVENT == 'T_F') %>% plotHexPt2('EVENT', adjSize = adj, add=T, col=adjustcolor('yellow', alpha)) # plot fires over treatment

                plot(states %>% st_geometry(), add=T)
                plot(roads %>% st_geometry(), add=T, col=adjustcolor('black',0.5))


                # grid.raster(logo, x=0.90, y=.94, width=.15); grid.raster(logo2, x=0.90, y=.85, width=.10)
                grid.raster(logo, x=0.845, y=.92, width=.15); grid.raster(logo2, x=0.92, y=.92, width=.10)
                mtext(text=paste0('Year ', ifelse(yr>20, 20, yr), ' (Future ', scn, ')'), side=3, line=-5, adj=0.5, cex=1.5, font=2)

                legend('bottom', cex=2.5, title = '',
                       legend=c('National forest','Wildfire','Treatment'),
                       fill=c('grey80','brown','chartreuse4'), ncol=3, box.lwd=NA, bg=NA)

                if(yr > 0){

                  e_df_dat <- hex_ps_bb %>% st_drop_geometry() %>% filter(SCN_ID == !!scn)

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
                  title(ylab = 'ha / yr', cex.axis=1.5, cex.lab=1.5, cex.main=2)
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


