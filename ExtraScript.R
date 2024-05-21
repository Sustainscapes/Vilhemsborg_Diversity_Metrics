library(data.table)
library(terra)

export_richness <- function(Results, path){
  Temp <- as.numeric(terra::rast(path))
  Temp[!is.na(Temp)] <- 0
  Richness <- Temp
  Results <- Results[cell > 0 & !is.na(cell)  & cell <= ncell(Temp)]
  terra::values(Richness)[as.numeric(Results$cell),] <- Results$SR
  names(Richness) <- paste("Richness", unique(Results$Landuse), sep = "_")
  BDRUtils::write_cog(Richness, paste0("Results/Richness/Richness_",unique(Results$Landuse), ".tif"))
}

export_pd <- function(Results, path){
  Temp <- as.numeric(terra::rast(path))
  Temp[!is.na(Temp)] <- 0
  PD <- Temp
  Results <- Results[cell > 0 & !is.na(cell) & cell <= ncell(Temp)]

  terra::values(PD)[as.numeric(Results$cell),] <- Results$PD
  names(PD) <- paste("PD", unique(Results$Landuse), sep = "_")
  BDRUtils::write_cog(PD, paste0("Results/PD/PD_",unique(Results$Landuse), ".tif"))
}


path <- "DirDownscaledCroped/LU_Aarhus.tif"

for(i in 1:8){
  Results <- targets::tar_read("PhyloDiversity", branches = i)
  export_pd(Results = Results, path = path)

}

Richness <- list.files("Results/Richness/", pattern = "\\.tif", full.names = T) |> terra::rast()


PD <- list.files("Results/PD/", pattern = "\\.tif", full.names = T) |> terra::rast()

library(tidyterra)
library(ggplot2)


ggplot() + geom_spatraster(data = Richness) + theme_bw() + facet_wrap(~lyr) + scale_fill_wiki_c(direction = -1)

ggplot() + geom_spatraster(data = PD) + theme_bw() + facet_wrap(~lyr) + scale_fill_wiki_c(direction = -1)
