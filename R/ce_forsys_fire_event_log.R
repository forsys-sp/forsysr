# stand and project data are ForSys outputs; fire intersect data is created
# separately by crossing stands and FSIM fires; this script then combines all of
# these elements in a combined event log across all fire futures where, at least
# in the static version, the implementation schedule remains static and fire
# perimeters vary. In subsequent versions of this code, treatments will vary due
# to fire, either because burned areas scheduled for treatment will be
# reassigned, or treatments will be proactively reshuffled to address future
# fire.

pacman::p_load(rnaturalearth)

# read in data
s_df <- read_delim('output/WW_10yr_FS_80p/stnd_WW_10yr_FS_80p_Pr1.csv', delim=',')
p_df <- read_delim('output/WW_10yr_FS_80p/proj_WW_10yr_FS_80p.csv', delim=',')
f_df <- read_delim(input_stand_fire_intersect, delim=',')

# annual treatment target
# annual_constraint = 450000
# p_df <- p_df %>%
#   mutate(ETrt_YR = cumsum(ETrt_AREA_HA) %/% !!annual_project_target + 1) %>%
#   mutate(ETrt_YR = ifelse(weightedPriority == 0, NA, ETrt_YR))

# add treatment rank and year to stands
s_df <- s_df %>% left_join(p_df %>% dplyr::select(PA_ID, treatment_rank, ETrt_YR)) %>% filter(treatment_rank > 0)
f_df %>% filter(SCN_ID %in% c(1:5)) %>% group_by(SCN_ID, FIRE_YR, OWNER) %>% summarize_at(vars(AREA_HA), sum) %>% ggplot(aes(x = FIRE_YR, y = AREA_HA, fill=OWNER)) + geom_bar(stat = 'identity') + facet_wrap(~SCN_ID)
s_df %>% group_by(ETrt_YR) %>% summarize_at(vars(AREA_HA), sum)

outcome <- 'ETrt_aTR_MS_PCP'
pdat <- p_df %>% group_by(ETrt_YR) %>% summarize_at(vars(outcome), sum)
pdat %>% ggplot(aes(x = ETrt_YR, y = cumsum(get(outcome)))) + geom_line() + ylab(outcome)

# classify fire events for stands
out_s <- 1:30 %>% map(function(x){

  print(x)

  ss <- s_df %>%
    left_join(p_df %>% dplyr::select(PA_ID, ETRT_YR)) %>%
    full_join(f_df %>% filter(OWNER == 'USFS', SCN_ID == x)) %>%
    mutate(SCN_ID = x) %>%
    # left_join(f_df %>% filter(SCN_ID == x) %>% dplyr::select(CELL_ID, SCN_ID, FIRE_YR, FIRE_NUMBER)) %>%
    mutate(EVENT = case_when(
      ETrt_YR <= FIRE_YR ~ 'etrt_fire',
      FIRE_YR < ETrt_YR ~ 'fire_etrt',
      is.na(ETrt_YR) & !is.na(FIRE_YR) ~ 'fire',
      is.na(FIRE_YR) & !is.na(ETrt_YR) ~ 'etrt',
      is.na(ETrt_YR) & is.na(FIRE_YR) ~ 'none'
    ))

  # define year as the first between treatment or fire
  suppressWarnings(
    ss$YEAR <- ss %>% dplyr::select(ETrt_YR, FIRE_YR) %>% apply(1, min, na.rm=T)
  )

  # create output objects (selected stands for treatment & full etrt fire event log)
  selected_df <- ss %>% filter(ETrt_YR %>% is.na() == FALSE)
  events_df <- ss %>% dplyr::select(SCN_ID, CELL_ID, PA_ID, YEAR, ETrt_YR, FIRE_YR, FIRE_NUMBER, EVENT)

  return(list(selected_stands = selected_df, events = events_df))
})

# summarize project areas for specified outcomes by event type
outcome_vars = c('AREA_HA', 'aTR_MS_PCP')

x <- out_s[[2]][[1]]

out_p <- out_s %>% map(1) %>% map_dfr(function(x){
  cat('.')
  p_df2 <- x %>%
    # group_by(SCN_ID, PA_ID, EVENT, YEAR) %>%
    group_by(SCN_ID, EVENT, YEAR) %>%
    summarize_at(vars(output_fields), sum) %>%
    # pivot_wider(id_cols = c(SCN_ID, PA_ID, YEAR), names_from = EVENT, values_from = !!(outcome_vars), values_fill=0) %>%
    pivot_wider(id_cols = c(SCN_ID, YEAR), names_from = EVENT, values_from = !!(outcome_vars), values_fill=0) %>%
    arrange(YEAR)
  return(p_df2)
})

save(s_df, p_df, f_df, out_s, out_p, file = 'event_data.Rdata')

out_p[[1]] %>% apply(2, sum)
out_p[[1]]$YEAR %>% summary()

 # ARCHIVED TEST BELOW....





# load stands
load('../../Dropbox/!!projects/!archive/aa_10yr/hex.Rdata')
hex_p <- hex %>%
  filter(OwnerCat == 'USFS') %>%
  st_as_sf(coords = c("POINT_X", "POINT_Y"), crs = 5070)

p_df <- p_df %>% filter(treatment_rank > 0) %>%
  left_join(s_df %>% group_by(PA_ID) %>% summarize(s_cnt = n())) %>%
  mutate(pct_area_ETrt = ETrt_AREA_HA / ESum_AREA_HA)

lf_df <- f_df %>% left_join(hex %>% dplyr::select(CELL_ID, AREA_HA, OWNER = OwnerCat)) %>% group_by(SCN_ID, FIRE_NUMBER, OWNER) %>%
  summarize_at(vars(AREA_HA), sum) %>%
  arrange(-AREA_HA) %>% head(100)

lf_df

scn = 2
fn = 162046

# annual treatment target
annual_constraint = 450000
p_df <- p_df %>% mutate(ETrt_YR = cumsum(ETrt_AREA_HA) %/% !!annual_constraint + 1) %>% mutate(ETrt_YR = ifelse(weightedPriority == 0, NA, ETrt_YR))

# add treatment rank and year to stands
s_df <- s_df %>% left_join(p_df %>% dplyr::select(PA_ID, treatment_rank, ETrt_YR)) %>% filter(treatment_rank > 0)

# create event_log
e_df <- s_df %>% left_join(p_df) %>%
  full_join(f_df %>% filter(SCN_ID == !!scn) %>% dplyr::select(CELL_ID, FIRE_YR, FIRE_NUMBER)) %>%
  dplyr::select(CELL_ID, ETrt_YR, FIRE_YR, FIRE_NUMBER, PA_ID, AREA_HA) %>%
  mutate(EVENT = case_when(
    ETrt_YR <= FIRE_YR ~ 'etrt_fire',
    FIRE_YR < ETrt_YR ~ 'fire_etrt',
    is.na(ETrt_YR) & !is.na(FIRE_YR) ~ 'fire',
    is.na(FIRE_YR) & !is.na(ETrt_YR) ~ 'etrt',
    is.na(ETrt_YR) & is.na(FIRE_YR) ~ 'none'
  ))
suppressWarnings(
  e_df$YR <- e_df %>% dplyr::select(ETrt_YR, FIRE_YR) %>% apply(1, min, na.rm=T)
)
xtabs(~YR + EVENT, data=e_df)

plotHexPt(e_df %>% filter(YR > 0), 'EVENT', pch=15, cex=0.1, main=paste('Fire Future', scn))


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



