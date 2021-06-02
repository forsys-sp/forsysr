pacman::p_load(tidyverse, purrr, rnaturalearth, sf, raster, animation,
               rmapshaper, rnaturalearth, jpeg, png, grid, shades)

source('R/fire_misc_func.R')
source('R/fire_misc_animations_load.R')

e_df <- read_delim('output/events_WW_10yr_FS_80p_Ran_FScn1_10_ramp.csv', delim = ',') %>%
  left_join(hex %>% dplyr::select(CELL_ID, FLAG = western_flag20_NLCD))

e_df %>% group_by(SCN_ID, FIRE_YR, PA_ID, EVENT) %>% summarize(AREA_HA = sum(AREA_HA)) %>%
  pivot_wider(names_from = EVENT, values_from = AREA_HA, values_fill = 0) %>%
  left_join(p_df)

dev.off()
print(plot(1))
dev.set(dev.next())
# dev.new()
# plot(1)
# dev.set(2)

gdat <- e_df %>% filter(!is.na(FIRE_YR)) %>%
  group_by(SCN_ID, FIRE_YR) %>% summarize_at(vars(AREA_HA, aTR_MS), sum, na.rm=T)
max(gdat$AREA_HA)
max(gdat$aTR_MS)


# Misc plot examples .......

scn = 2
fn = 96486
bbox = NULL

f_df %>% filter(FIRE_NUMBER == !!fn, SCN_ID == !!scn) %>%
  plotHexPt(field = 'OWNER', adjSize=50, add=T)

e_df %>% filter(FIRE_NUMBER == !!fn, SCN_ID == !!scn) %>%
  left_join(hex %>% dplyr::select(CELL_ID, western_flag20_NLCD)) %>%
  plotHexPt('western_flag20_NLCD', adjSize=50)

pa <- st_read('~/Dropbox/!gis/Firesheds_CONUS.gdb/', 'ProjectAreas')
pa2 <- pa %>% st_intersection(st_as_sf(bbox)) %>% st_transform(5070)

# Misc animations examples .......

scn = 1
adj = 125 # bigger is smaller (state scale = 200-300, western-scale = 1000)
st =  'AZ'
st = NULL
yr = 10
# bbox = st_as_sfc(st_bbox(c(xmin = -125, xmax = -119, ymax = 40, ymin = 37), crs=4326)) %>% st_transform(5070)
# bbox = st_as_sfc(st_bbox(c(xmin = -120, xmax = -116, ymax = 46.33, ymin = 43.6422), crs=4326)) %>% st_transform(5070)
# bbox = st_as_sfc(st_bbox(c(xmin = -118.5, xmax = -117.7, ymax = 46, ymin = 45.3), crs=4326)) %>% st_transform(5070)
bbox = NULL

if(is.null(st)) adj = 400 else adj = 200
ani.options(interval = 1)
logo <- readPNG('../../Dropbox/FORSYS_logo.png')
adj = 100


scn = 3
# saveGIF(expr = {
saveVideo(expr = {

  pdat <- e_df %>% filter(SCN_ID == !!scn)

  hex_ps <-   hex_ps <- hex_p %>%
    filter(CELL_ID %in% pdat$CELL_ID) %>%
    dplyr::select(CELL_ID) %>%
    left_join(pdat) %>%
    left_join(hex %>% dplyr::select(CELL_ID, OwnerCat, AREA_HA))

  # hex_ps_bb <- hex_ps %>% st_crop(bbox %>% st_buffer(0))
  if(!is.null(bbox)) hex_ps_bb <- hex_ps %>% st_intersection(bbox) else hex_ps_bb <- hex_ps

  for(yr in 1:21){
    print(paste('year', yr))
    suppressMessages({

      layout(mat=matrix(c(1,1,1,2,3,4), ncol=3, byrow = T), heights = c(5,1))
      par(mar=rep(1,4)); plotBg(state = st, bbox=bbox)
      if(!is.null(bbox)) plot(st_as_sf(bbox), add=T, lty=2)

      # plot events from previous years
      pal = c('brown','indianred','brown','chartreuse4','chartreuse4') %>% saturation(scalefac(0.6)) %>% brightness(scalefac(1.1))
      hex_ps_bb %>% filter(YEAR < !!yr) %>% mutate(EVENT = paste0(EVENT, FLAG)) %>% plotHexPt2('EVENT', adjSize = adj, add=T, pal=pal)
      hex_ps_bb %>% filter(ETrt_YR == !!yr, EVENT != 'F_T') %>% plotHexPt2('EVENT', adjSize = adj, add=T, col='green') # plot treatment
      hex_ps_bb %>% filter(ETrt_YR == !!yr, EVENT == 'F_T') %>% plotHexPt2('EVENT', adjSize = adj, add=T, col='black') # plot treatment over fire
      hex_ps_bb %>% filter(FIRE_YR == !!yr, EVENT != 'T_F') %>% plotHexPt2('EVENT', adjSize = adj, add=T, col='red') # plot fires
      hex_ps_bb %>% filter(SCN_ID == !!scn, FIRE_YR <= !!yr, EVENT == 'T_F') %>% plotHexPt2('EVENT', adjSize = adj, add=T, col='yellow') # plot fires over treatment

      grid.raster(logo, x=0.88, y=.94, width=.20) # print homer in ll conrner
      mtext(text=paste0('Year ', ifelse(yr>20, 20, yr), '\nScenario ', scn), side=1, line=-2, adj=1, cex=1.5)

      legend('bottomleft', cex=1.75,
             legend=c('National forest','New fire','Past fire - manageable','Past fire - unmanageable',
                      'New treatment', 'Past treatment','Fire after treatment','Treatment after fire'),
             fill=c('grey80','red','brown','indianred','green','chartreuse4','yellow','black'), ncol=1, box.lwd=NA, bg=NA)

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
        par(mar=c(2,5,1,1));
        gdat <- ab %>% ungroup %>%
          filter(TYPE == 'FIRE AREA_HA') %>%
          dplyr::select(TYPE, YEAR, EVENT, value) %>%
          complete(TYPE, YEAR, EVENT, fill = list(value=0)) %>%
          pivot_wider(id_cols=YEAR, names_from = EVENT, values_from=value)
        mx = max(apply(gdat[,-1],1,sum))
        barplot(gdat[1:yr,-1] %>% t, col=c('yellow3','brown','indianred'),
                axes=F, ylim=c(0,mx), xlim=c(1,23), border=NA)
        title(ylab = 'M ha / yr', main='Area burned', cex.axis=1.5, cex.lab=1.5, cex.main=2)
        axis(2, cex.axis=1.5)
        axis(1, at=seq(2,20,by = 2)*1.2, labels = seq(2,20,by = 2), cex.axis=1.5)

        # exposure subplot ....
        par(mar=c(2,5,1,1));
        gdat <- ab %>% ungroup %>%
          filter(TYPE == 'FIRE aTR_MS') %>%
          dplyr::select(TYPE, YEAR, EVENT, value) %>%
          complete(TYPE, YEAR, EVENT, fill = list(value=0)) %>%
          pivot_wider(id_cols=YEAR, names_from = EVENT, values_from=value)
        mx = max(apply(gdat[,-1],1,sum))
        barplot(gdat[1:yr,-1] %>% t, col=c('yellow3','brown','indianred'),
                axes=F, ylim=c(0,mx), xlim=c(1,23), border=NA)
        title(ylab = 'Buildings / yr', main='Exposure', cex.axis=1.5, cex.lab=1.5, cex.main=2)
        axis(2, cex.axis=1.5)
        axis(1, at=seq(2,20,by = 2)*1.2, labels = seq(2,20,by = 2), cex.axis=1.5)

        # exposure subplot ....
        par(mar=c(2,5,1,1));
        gdat <- ab %>% ungroup %>%
          filter(TYPE == 'TREAT AREA_HA') %>%
          dplyr::select(TYPE, YEAR, EVENT, value) %>%
          complete(TYPE, YEAR, EVENT, fill = list(value=0)) %>%
          pivot_wider(id_cols=YEAR, names_from = EVENT, values_from=value)
        mx = max(apply(gdat[,-1],1,sum))
        barplot(gdat[1:yr,-1] %>% t, col=c('chartreuse4','black'),
                axes=F, ylim=c(0,mx), xlim=c(1,23), border=NA)
        title(ylab = 'M ha / yr', main='Treatment', cex.axis=1.5, cex.lab=1.5, cex.main=2)
        axis(2, cex.axis=1.5)
        axis(1, at=seq(2,20,by = 2)*1.2, labels = seq(2,20,by = 2), cex.axis=1.5)
      }
    })
  }
}, video.name = paste0('output_graphics/WW_simple_',scn,'.mp4'), ani.width = 1000, ani.height = 1000, ani.res=50)

