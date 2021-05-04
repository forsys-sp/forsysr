# stand and project data are ForSys outputs; fire intersect data is created
# separately by crossing stands and FSIM fires; this script then combines all of
# these elements in a combined event log across all fire futures where, at least
# in the static version, the implementation schedule remains static and fire
# perimeters vary. In subsequent versions of this code, treatments will vary due
# to fire, either because burned areas scheduled for treatment will be
# reassigned, or treatments will be proactively reshuffled to address future
# fire.

s_df <- read_delim('output/WW_10yr_FS_80p/stnd_WW_10yr_FS_80p_Pr1.csv', delim=',')
p_df <- read_delim('output/WW_10yr_FS_80p/proj_WW_10yr_FS_80p.csv', delim=',')
f_df <- read_delim(input_stand_fire_intersect, delim=',')

s_df <- s_df %>% left_join(p_df %>% dplyr::select(PA_ID, treatment_rank)) %>% filter(treatment_rank > 0)

# annual treatment target
annual_constraint = 800000

p_df <- p_df %>% mutate(ETRT_YR = cumsum(ETrt_AREA_HA) %/% !!annual_constraint + 1)

outcome <- 'ETrt_aTR_MS'
pdat <- p_df %>% group_by(ETRT_YR) %>% summarize_at(vars(outcome), sum)
pdat %>% ggplot(aes(x = ETRT_YR, y = cumsum(get(outcome)))) + geom_line() + ylab(outcome)

# classify events for selected stands
out_s <- 1:30 %>% map(function(x){
  print(x)
  ss <- s_df %>%
    left_join(p_df %>% dplyr::select(PA_ID, ETRT_YR)) %>%
    full_join(f_df %>% filter(OWNER == 'USFS', SCN_ID == x)) %>%
    # left_join(f_df %>% filter(SCN_ID == x) %>% dplyr::select(CELL_ID, SCN_ID, FIRE_YR, FIRE_NUMBER)) %>%
    mutate(EVENT = case_when(
      ETRT_YR <= FIRE_YR ~ 'etrt_fire',
      FIRE_YR < ETRT_YR ~ 'fire_etrt',
      is.na(ETRT_YR) & !is.na(FIRE_YR) ~ 'fire',
      is.na(FIRE_YR) & !is.na(ETRT_YR) ~ 'etrt',
      is.na(ETRT_YR) & is.na(FIRE_YR) ~ 'none'
    ))
  suppressWarnings(
    ss$YEAR <- ss %>% dplyr::select(ETRT_YR, FIRE_YR) %>% apply(1, min, na.rm=T)
  )

  selected_df <- ss %>% filter(ETRT_YR %>% is.na() == FALSE)
  events_df <- ss %>% dplyr::select(SCN_ID, CELL_ID, PA_ID, YEAR, ETRT_YR, FIRE_YR, FIRE_NUMBER, EVENT)

  return(list(selected_stands = selected_df, events = events_df))
})

# summarize project areas for specified outcomes by event type
outcome_vars = c('AREA_HA', 'aTR_MS_PCP')

out_p <- out_s %>% map(1) %>% map(function(x){
  cat('.')
  p_df2 <- x %>%
    group_by(SCN_ID, PA_ID, EVENT, YEAR) %>%
    summarize_at(vars(output_fields), sum) %>%
    pivot_wider(id_cols = c(SCN_ID, PA_ID, YEAR), names_from = EVENT, values_from = !!(outcome_vars), values_fill=0) %>%
    arrange(YEAR)
  return(p_df2)
})

save(s_df, p_df, f_df, out_s, out_p, file = 'event_data.Rdata')

out_p[[1]] %>% apply(2, sum)
out_p[[1]]$YEAR %>% summary()

 # ARCHIVED TEST BELOW....






p_df <- p_df %>% filter(treatment_rank > 0) %>%
  left_join(s_df %>% group_by(PA_ID) %>% summarize(s_cnt = n())) %>%
  mutate(pct_area_ETrt = ETrt_AREA_HA / ESum_AREA_HA)

lf_df <- f_df %>% left_join(hex %>% dplyr::select(CELL_ID, AREA_HA, OWNER = OwnerCat)) %>% group_by(SCN_ID, FIRE_NUMBER, OWNER) %>%
  summarize_at(vars(AREA_HA), sum) %>%
  arrange(-AREA_HA) %>% head(100)

scn = 12
fn = 162046

# annual treatment target
annual_constraint = 800000

# assign treatment year based on annual area treated constraint
p_df <- p_df %>% dplyr::select(PA_ID, weightedPriority, treatment_rank, ETrt_AREA_HA) %>%
  mutate(ETRT_YR = cumsum(ETrt_AREA_HA) %/% !!annual_constraint + 1)

# create event_log
e_df <- s_df %>% left_join(p_df) %>%
  left_join(f_df %>% filter(SCN_ID == !!scn)) %>%
  dplyr::select(CELL_ID, ETRT_YR, FIRE_YR, FIRE_NUMBER, PA_ID) %>%
  mutate(EVENT = case_when(
    ETRT_YR <= FIRE_YR ~ 'etrt_fire',
    FIRE_YR < ETRT_YR ~ 'fire_etrt',
    is.na(ETRT_YR) & !is.na(FIRE_YR) ~ 'fire',
    is.na(FIRE_YR) & !is.na(ETRT_YR) ~ 'etrt',
    is.na(ETRT_YR) & is.na(FIRE_YR) ~ 'none'
  ))
suppressWarnings(
  e_df$YEAR <- e_df %>% dplyr::select(ETRT_YR, FIRE_YR) %>% apply(1, min, na.rm=T)
)

plotHexPt(f_df %>% filter(SCN_ID == !!scn, FIRE_NUMBER == !!fn), 'OwnerCat', pch=15)
plotHexPt(f_df %>% filter(SCN_ID == !!scn, FIRE_YR == 4), 'OwnerCat', pch=15, cex=0.5)
plotHexPt(e_df %>% filter(YEAR == !!scn, grepl('etrt', EVENT)), pch=4, cex=0.5)
plotHexPt(e_df %>% filter(YEAR == !!scn, grepl('fire', EVENT)), pch=4, cex=0.5)



plotHexPt <- function(pdat, field=NULL, ...){
  hex_ps <- hex_p %>%
    filter(CELL_ID %in% pdat$CELL_ID) %>%
    dplyr::select(CELL_ID) %>%
    left_join(pdat) %>%
    left_join(hex %>% dplyr::select(CELL_ID, OwnerCat, AREA_HA))
  print(prettyNum(sum(hex_ps$AREA_HA), big.mark = ','))
  if(is.null(field)){
    plot(hex_ps %>% st_geometry(), ...)
  } else {
    plot(hex_ps[,field], ...)
  }
}

pdat <- e_df %>% filter(PA_ID == 25801)
hex_ps <- hex_p %>% filter(CELL_ID %in% pdat$CELL_ID) %>% dplyr::select(CELL_ID) %>% left_join(pdat)
plot(hex_ps[,'EVENT'], pch=15, add=T)

pdat <- f_df %>% filter(SCN_ID == 1) %>% filter(PA_ID == 25801)
hex_ps <- hex_p %>% filter(CELL_ID %in% pdat$CELL_ID) %>% dplyr::select(CELL_ID) %>% left_join(pdat)
plot(hex_ps[,'FIRE_YR'], pch=15)

# MISC PLOT SPACE --------
load('../../Dropbox/!!projects/!archive/aa_10yr/hex.Rdata')
hex_p <- hex %>% st_as_sf(coords = c("POINT_X", "POINT_Y"), crs = 5070)

pdat <- e_df %>% filter(YEAR == 5 & grepl('etrt|fire', EVENT))
hex_ps <- hex_p %>% filter(CELL_ID %in% pdat$CELL_ID) %>% dplyr::select(CELL_ID) %>% left_join(pdat) %>% filter(!is.na(EVENT))
plot(hex_ps[,'EVENT'], pch=15, cex=0.25, pal=c('blue','green','red','brown'))

pdat <- e_df %>% filter(FIRE_YR == 2)
hex_ps <- hex_p %>% filter(CELL_ID %in% pdat$CELL_ID) %>% dplyr::select(CELL_ID) %>% left_join(pdat) %>% filter(!is.na(EVENT))
plot(hex_ps[,'FIRE_YR'], pch=16, cex=0.5)

# MISC PLOT SPACE END --------

# fire future
scn_id = 1

# create event log for all replicates
future::plan(future::multisession, workers = 6)
event_df <- 1:30 %>% furrr::future_map_dfr(~create_event_log(s_df, p_df, f_df, .))

create_event_log <- function(s_df, p_df, f_df, scn_id){

  print(scn_id)

  suppressMessages({
    # combine stand, project, and fire tables, assign events, scn year
    e_df <- s_df %>% left_join(p_df) %>%
      left_join(f_df %>% filter(SCN_ID == !!scn_id)) %>%
      dplyr::select(CELL_ID, ETRT_YR, FIRE_YR) %>%
      mutate(EVENT = case_when(
        ETRT_YR <= FIRE_YR ~ 'etrt_fire',
        FIRE_YR < ETRT_YR ~ 'fire_etrt',
        is.na(ETRT_YR) & !is.na(FIRE_YR) ~ 'fire',
        is.na(FIRE_YR) & !is.na(ETRT_YR) ~ 'etrt',
        is.na(ETRT_YR) & is.na(FIRE_YR) ~ 'none'
      ))

    # assign replicate year
    suppressWarnings(
      e_df$YEAR <- e_df %>% dplyr::select(ETRT_YR, FIRE_YR) %>% apply(1, min, na.rm=T)
    )

    e_df <- e_df %>% arrange(YEAR, CELL_ID) %>% filter(YEAR != Inf)
    e_df <- bind_cols(REP = scn_id, e_df)
  })

  return(e_df)
}

# load stands
load('../../Dropbox/!!projects/!archive/aa_10yr/hex.Rdata')
hex_p <- hex %>%
  filter(OwnerCat == 'USFS') %>%
  st_as_sf(coords = c("POINT_X", "POINT_Y"), crs = 5070)

e_df2 <- event_df %>% left_join(s_df %>% dplyr::select(CELL_ID, AREA_HA, aTR_MS_PCP))

e_df2 %>% filter(EVENT == 'fire')

e_df2_g <- e_df2 %>% group_by(REP, YEAR, EVENT) %>%
  summarize_at(vars(AREA_HA, aTR_MS_PCP), sum) %>%
  group_by(YEAR, EVENT) %>%
  summarize_at("AREA_HA", list(mean = mean, min = min, max = max))


e_df2_g %>% filter(EVENT == 'etrt_fire') %>% ggplot(aes(x=YEAR, y=mean, group=EVENT, color=EVENT)) + geom_line()


event_df %>% filter(REP == 1) %>% filter(EVENT %in% c('etrt','etrt_fire','fire_etrt'))
tmp <- hex_p %>% right_join(event_df %>% filter(REP == 1))

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



