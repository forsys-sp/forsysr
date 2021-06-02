pacman::p_load(raster, fasterize, sf, tictoc, dplyr,
               purrr, ggplot2, cowplot, progressr,
               tidyr, caret, future, tictoc)


# load hex data for  western US
load('data/hexnet_west.Rdata')

# convert hex into sf points
hex_p <- hex %>% st_as_sf(coords = c("POINT_X", "POINT_Y"), crs = 5070)
hex_dat <- hex %>% dplyr::select(CELLID = CELL_ID, OWNER = OwnerCat, PA_ID, AREA_HA)

# load fire perimeters from fSIM geodatabase
fp <- st_read('~/Dropbox/!gis/FSim19_RND600years_WestUS.gdb/', 'Perimeters_R1_R6')
fp <- st_read('~/Dropbox/!gis/Ecoregion_futures.gdb/', 'Perims')

# divide FSIM years into 20 years folds (n = 30)
set.seed(1); fsim_yrs <-  unique(fp$YEAR)[createFolds(unique(fp$YEAR)) %>% unlist() %>% as.numeric()]

# create raster grid for running intersect
res = 1000
r <- raster(extent(hex_p), resolution=res)

# create cell intersect for 100 fire futures

# loop through 30 scenarios
fsim_ww <- 1:30 %>% map(function(i){

  print(i)

  # identify fsim years for scenario i
  fsim_yrs_i <- fsim_yrs[1:20 + 20*(i-1)]

  # pull future fire perimeters
  fp_i <- fp %>%
    filter(YEAR %in% fsim_yrs_i) %>%
    mutate(FIRE_YR = factor(YEAR, fsim_yrs_i) %>% as.numeric()) %>%
    arrange(FIRE_YR, START_DAY) %>%
    mutate(SCN_ID = !!i) %>%
    mutate(FIRE_NU = paste0(Pyrome, FIRE_NUMBE) %>% as.numeric())

  # convert fire future into raster stack
  fpr_i <- fasterize(fp_i, r, field = 'FIRE_NU', fun = 'first', by='FIRE_YR')

  # intersect fire data and cell id
  fhi_1 <- terra::extract(fpr_i, hex_p) %>%
    as.data.frame() %>%
    setNames(., nm = paste0('y',1:20))

  # pivot to long from
  fhi_2 <- fhi_1 %>%
    bind_cols(OWNER = hex_dat$OWNER, .) %>%
    bind_cols(CELLID = hex_dat$CELLID, .) %>%
    pivot_longer(-c(OWNER, CELLID), names_to = "FIRE_YR", values_to = 'FIRE_NUMBER') %>%
    filter(complete.cases(.)) %>%
    mutate(FIRE_YR = gsub('y', '', FIRE_YR) %>% as.numeric)

  # combine intersect by cell, yr, and fire number
  fhi_3 <- fhi_2 %>%
    group_by(CELLID, OWNER, FIRE_YR, FIRE_NUMBER) %>%
    summarize(PXCNT = n(), BURN_HA = PXCNT * res^2/10000) %>%
    left_join(hex_dat %>% dplyr::select(CELLID, PA_ID, AREA_HA)) %>%
    arrange(FIRE_YR, CELLID)

  # subset to first fire and stand burn > 50%
  fhi_4 <- fhi_3 %>%
    group_by(CELLID, OWNER) %>%
    arrange(FIRE_YR) %>%
    slice_head(n = 1) %>%
    arrange(FIRE_YR, CELLID)

  # tag output w/ fire scenario
  out <- cbind(SCN_ID = i, fhi_4)

})

# combine ouputs for 30 replicates into singel dataframe, then write to write to csv
fsim_ww %>% map_dfr(function(x){
  out <- x %>% dplyr::select(SCN_ID, CELL_ID = CELLID, PA_ID, FIRE_YR, FIRE_NUMBER, OWNER, AREA_HA) %>%
    arrange(SCN_ID, CELL_ID, FIRE_YR)
}) %>% write.csv('data/hexnet_west_fsim19_30reps_intersect.csv', row.names = F)

