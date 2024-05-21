GetLandusePresences <- function(folder, Landuse){
  DT <- list.files(path = paste0(folder, Landuse, "/"), full.names = T) |>
    purrr::map(data.table::fread) |>
    purrr::keep(function(x) ncol(x) == 3) |>
    data.table::rbindlist()
  colnames(DT) <- c("cell", "species", "Landuse")
  return(DT)
}

calc_rarity_weight <- function(df){

  JF <- as.data.table(df)


  JF <- JF[, .N, by = species]

  national.occ <- JF$N
  names(national.occ) <- JF$species


  rarity.weights <- rWeights(national.occ)
  return(rarity.weights)
}

calc_rarity <- function(Fin, RW){
  Fin <- as.data.table(Fin)
  Landuse <- unique(Fin$Landuse)
  Fin[,Pres := 1]
  Fin[, species := stringr::str_replace_all(species, " ", "_")]
  Fin <- Fin[cell > 0 & !is.na(cell)]
  Fin2 <- dcast(Fin, cell~species, value.var="Pres", fill = 0)
  Fin2 <- tibble::column_to_rownames(as.data.frame(Fin2), "cell")
  colnames(Fin2) <- stringr::str_replace_all(colnames(Fin2), "_", " ")
  Fin2 <- t(Fin2)
  Rarity <- Rarity::Irr(assemblages = Fin2, W = RW)
  Rarity <- as.data.frame(Rarity)
  Rarity$Landuse <- Landuse
  Rarity <- tibble::rownames_to_column(Rarity,var = "cell")
  return(Rarity)
}

export_rarity <- function(Results, path){
  Temp <- as.numeric(terra::rast(path))
  Temp[!is.na(Temp)] <- 0
  Rarity <- Temp
  names(Rarity) <- paste("Rarity", unique(Results$Landuse), sep = "_")
  terra::values(Rarity)[Results$cell] <- Results$Irr
  BDRUtils::write_cog(Rarity, paste0("Results/Rarity/Rarity_",unique(Results$Landuse), ".tif"))
  paste0("Results/Rarity/Rarity_",unique(Results$Landuse), ".tif")
}


generate_tree <- function(DF){
  Tree <- readr::read_csv(DF) |>
    dplyr::select(species, genus, family) |>
    dplyr::distinct() |>
    V.PhyloMaker::phylo.maker()
  return(Tree)
}

calc_pd <- function(Fin, Tree){
  Fin <- as.data.table(Fin)
  Leaves <- Tree$scenario.3$tip.label
  Landuse <- unique(Fin$Landuse)
  Fin[,Pres := 1]
  Fin[, species := stringr::str_replace_all(species, " ", "_")]

  Fin2 <- dcast(Fin, cell~species, value.var="Pres", fill = 0)
  Index <- which(colnames(Fin2) %in% Leaves)
  Mat <- as.matrix(Fin2)[,Index]

  PD <- picante::pd(samp = Mat, Tree$scenario.3, include.root = F) |>
    as.data.table()
  PD[, PD := ifelse(is.na(PD), 0 , PD)]

  PD$cell <- Fin2$cell
  PD$Landuse <- Landuse
  return(PD)
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

export_richness <- function(Results, path){
  Temp <- as.numeric(terra::rast(path))
  Temp[!is.na(Temp)] <- 0
  Richness <- Temp
  Results <- Results[cell > 0 & !is.na(cell)  & cell <= ncell(Temp)]
  terra::values(Richness)[as.numeric(Results$cell),] <- Results$SR
  names(Richness) <- paste("Richness", unique(Results$Landuse), sep = "_")
  BDRUtils::write_cog(Richness, paste0("Results/Richness/Richness_",unique(Results$Landuse), ".tif"))
  paste0("Results/Richness/Richness_",unique(Results$Landuse), ".tif")
}
