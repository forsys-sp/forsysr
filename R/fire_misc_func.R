logisticFunc = function(yr, mid = 5, normalize = T, start = 0, end = 1){
  y <- 1/(1+(exp(-1 * (yr - mid))))
  if(normalize) y <- (y-min(y))/(max(y) - min(y))
  y = start + (y * abs(start - end))
  return(y)
}

process_outputs <- function(
  config_file=''
  ) {

  # If a config file has been selected, source it to read in variables
  if (length(config_file) > 0) {
    configuration_file <- config_file
    setwd(dirname(configuration_file))
    source(configuration_file, local = TRUE)
  } else {

  }

  relative_output_path = glue('output/{scenario_name}')

  # combine stand level outputs & tag w/ forsys type
  list.files(relative_output_path, pattern = 'stnd', full.names = T) %>%
    map_dfr(~read.csv(.)) %>% relocate(SCN_ID) %>%
    # process_stand_outputs(forsys_type = 'Dyn') %>%
    write.csv(paste0(relative_output_path, '/stnd_combined.csv'), row.names = F)

  # combine project level outputs & tag w/ forsys type
  list.files(relative_output_path, pattern = 'proj', full.names = T) %>%
    map_dfr(~read.csv(.)) %>% relocate(SCN_ID) %>%
    write.csv(paste0(relative_output_path, '/proj_combined.csv'), row.names = F)

}

process_stand_outputs <- function(lf, forsys_type){

  if(exists('f_df') == FALSE) f_df <- data.table::fread('data/hexnet_west_fsim19_30reps_intersect.csv')
  if(exists('hex') == FALSE) hex <- fread('data/hexnet_west_fs.csv')

  out <- lf %>%
    purrr::map_dfr(read.csv) %>%
    dplyr::select(-AREA_HA, -aTR_MS) %>%
    full_join(f_df %>% filter(OWNER == 'USFS', SCN_ID %in% c(1:10))) %>%
    left_join(hex %>% dplyr::select(CELL_ID, AREA_HA, aTR_MS)) %>%
    mutate(TYPE = !!forsys_type) %>%
    mutate(EVENT = case_when(
      ETrt_YR <= FIRE_YR ~ 'T_F', # treatment then fire
      FIRE_YR < ETrt_YR ~ 'F_T', # fire then treatment
      is.na(ETrt_YR) & !is.na(FIRE_YR) ~ 'F', # fire alone
      is.na(FIRE_YR) & !is.na(ETrt_YR) ~ 'T', # treatment alone
      is.na(ETrt_YR) & is.na(FIRE_YR) ~ 'N' # no event
    ))

  # take first event as event year
  suppressWarnings(
    out$YEAR <- out %>% dplyr::select(ETrt_YR, FIRE_YR) %>% apply(1, min, na.rm=T)
  )

  # simplify data before writing to csv
  out <- out %>%
    arrange(TYPE, SCN_ID, YEAR, PA_ID, CELL_ID) %>%
    dplyr::select(-AREA_HA, -aTR_MS, -OWNER) %>%
    left_join(hex %>% dplyr::select(CELL_ID, AREA_HA, aTR_MS)) %>%
    mutate(AREA_HA = round(AREA_HA), aTR_MS = round(aTR_MS, 6)) %>%
    relocate(TYPE, SCN_ID, YEAR, PA_ID)

  return(out)
}

addImg <- function(
  obj, # an image file imported as an array (e.g. png::readPNG, jpeg::readJPEG)
  x = NULL, # mid x coordinate for image
  y = NULL, # mid y coordinate for image
  width = NULL, # width of image (in x coordinate units)
  interpolate = TRUE # (passed to graphics::rasterImage) A logical vector (or scalar) indicating whether to apply linear interpolation to the image when drawing.
){
  if(is.null(x) | is.null(y) | is.null(width)){stop("Must provide args 'x', 'y', and 'width'")}
  USR <- par()$usr # A vector of the form c(x1, x2, y1, y2) giving the extremes of the user coordinates of the plotting region
  PIN <- par()$pin # The current plot dimensions, (width, height), in inches
  DIM <- dim(obj) # number of x-y pixels for the image
  ARp <- DIM[1]/DIM[2] # pixel aspect ratio (y/x)
  WIDi <- width/(USR[2]-USR[1])*PIN[1] # convert width units to inches
  HEIi <- WIDi * ARp # height in inches
  HEIu <- HEIi/PIN[2]*(USR[4]-USR[3]) # height in units
  rasterImage(image = obj,
              xleft = x-(width/2), xright = x+(width/2),
              ybottom = y-(HEIu/2), ytop = y+(HEIu/2),
              interpolate = interpolate)
}

writeToPptx <- function(ggplot_obj, filename = 'R_output_slides.pptx'){
  require(rvg); require(officer)
  graph <- dml(ggobj = ggplot_obj, pointsize = 20)
  doc <- read_pptx(filename)
  doc <- add_slide(doc)
  doc <- ph_with(x = doc, graph, location = ph_location_type(type = "body"), bg = 'transparent', pointsize = 24)
  print(doc, target = filename)
}

plotHexPt <- function(pdat, field=NULL, adjSize=NULL, plotSt=NULL,...){

  if(!is.null(bbox)) hex_p_bbox <- hex_p %>% st_crop(bbox) else hex_p_bbox <- hex_p

  hex_ps <- hex_p %>%
    filter(CELL_ID %in% pdat$CELL_ID) %>%
    dplyr::select(CELL_ID) %>%
    left_join(pdat) %>%
    left_join(hex %>% dplyr::select(CELL_ID, OwnerCat, AREA_HA))

  if(!is.null(adjSize)) size = hex_ps$AREA_HA/adjSize else size = rep(1, nrow(hex_ps))

  if(!is.null(plotSt)){
    states_sub <- states %>% filter(postal %in% plotSt) %>% st_geometry()
    plot(states_sub, col=NA, border=NA);
    plot(usfs, col='grey90', border=NA, add=T);
    plot(states %>% st_geometry(), add=T)

    print(prettyNum(sum(hex_ps$AREA_HA), big.mark = ','))
    if(is.null(field)){
      plot(hex_ps %>% st_geometry(), add=T, ...)
    } else {
      plot(hex_ps[,field], pch=16, cex=size, add=T, ...)
    }

  } else {
    print(prettyNum(sum(hex_ps$AREA_HA), big.mark = ','))
    if(is.null(field)){
      plot(hex_ps %>% st_geometry(), ...)
    } else {
      plot(hex_ps[,field], pch=16, cex=size, ...)
    }
  }
}

plotHexPt2 <- function(hex_ps, field=NULL, adjSize=NULL, plotSt=NULL,...){

  if(!is.null(adjSize)) size = hex_ps$AREA_HA/adjSize else size = rep(1, nrow(hex_ps))

  if(!is.null(plotSt)){
    states_sub <- states %>% filter(postal %in% plotSt) %>% st_geometry()
    plot(states_sub, col=NA, border=NA);
    plot(usfs, col='grey90', border=NA, add=T);
    plot(states %>% st_geometry(), add=T)

    print(prettyNum(sum(hex_ps$AREA_HA), big.mark = ','))
    if(is.null(field)){
      plot(hex_ps %>% st_geometry(), add=T, ...)
    } else {
      plot(hex_ps[,field], pch=15, cex=size, add=T, ...)
    }

  } else {
    print(prettyNum(sum(hex_ps$AREA_HA), big.mark = ','))
    if(is.null(field)){
      plot(hex_ps %>% st_geometry(), ...)
    } else {
      plot(hex_ps[,field], pch=15, cex=size, ...)
    }
  }
}

plotBg <- function(state=NULL, bbox=NULL){
  if(is.null(bbox)){
    if(is.null(state)){
      plot(states_w11 %>% st_geometry(), col=NA, border=NA);
    } else {
      plot(states_w11 %>% filter(postal %in% state) %>% st_geometry(), col=NA, border=NA);
    }
  } else {
    plot(bbox, col=NA, border=NA);
  }

  plot(usfs, col='grey90', border=NA, add=T);
  plot(states_w11 %>% st_geometry(), add=T)
  plot(states %>% st_geometry(), add=T)

}

