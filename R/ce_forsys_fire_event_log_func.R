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
