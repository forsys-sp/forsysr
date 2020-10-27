grouped_firesheds <- group_by(standDT, FSHED_ID, OwnerCat)

fireshed_summary <- summarize(grouped_firesheds, 
                              total_area_ha = sum(AREA_HA), 
                              total_exposure = sum(HUSUM_STND), 
                              available_area_ha = sum(AREA_HA[man_alldis == 1 & HUSUM_STND > 0]), 
                              available_exposure = sum(HUSUM_STND[man_alldis == 1 & HUSUM_STND > 0]))
fwrite(fireshed_summary, "output/fireshed_ownership_summary_east.csv")
