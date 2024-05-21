# _targets.R file
library(targets)
source("R/functions.R")
library(crew)
library(tarchetypes)




tar_option_set(packages = c("data.table", "dplyr", "janitor", "magrittr", "purrr", "Rarity", "readxl","SDMWorkflows", "stringr", "tidyr", "tibble","terra", "V.PhyloMaker", "BDRUtils", "readr", "picante"),
               controller = crew_controller_local(workers = 3),
               error = "null") # Force skip non-debugging outdated targets)


list(
#Path to the Habitat model raster output of potential habitat types
  tar_target(LanduseSuitability,
             "HabSutDownscaledCroped/Aarhus.tif",
             format = "file"),

#Path to the raster of current land use in terms of habitat types
  tar_target(LandUseTiff,
             "DirDownscaledCroped/LU_Aarhus.tif",
             format = "file"),
  tar_target(Clean_Species,
            "Clean_Species.csv",
             format = "file"),

#Generating a phylogenetic tree of the observed species
#For GBIF data:
  tar_target(Phylo_Tree, generate_tree(Clean_Species)),
#For GBIF+Field data:
#  tar_target(Phylo_Tree_field, generate_tree(joint_data)),

#Modelling the the species distribution based on the habitat types in the raster map of present nature
  #tar_target(ModelAndPredict, ModelAndPredictFunc(DF =  Presences, file = LandUseTiff),
             #pattern = map(Presences)),
             #iteration = "group"),
  #tar_target(Thresholds, create_thresholds(Model = ModelAndPredict,reference = Presences, LandUseTiff),
             #pattern = map(ModelAndPredict, Presences),
             #iteration = "group"),

#Creates a lookup table for suitable habitat types for each species
  #tar_target(LookUpTable, Generate_Lookup(Model = ModelAndPredict, Thresholds = Thresholds)),
#We just use the lookup table that Derek has already created for all of DK
#  tar_target(LookUpTable,
#             Make_Look_Up_Table("species_lookup.xlsx")),
#  tar_target(LanduseTable, generate_landuse_table(path = LanduseSuitability)),
#  tar_target(Long_LU_table, Make_Long_LU_table(DF = LanduseTable)),

#Final presences GBIF:
#  tar_target(Final_Presences, make_final_presences(Long_LU_table, Long_Buffer_gbif, LookUpTable),
#             pattern = map(Long_Buffer_gbif)),
#  tar_target(exported_gbif_presences, export_final_presences(Final_Presences, folder = "GBIF_Final_Presences"),
#             pattern = map(Final_Presences),
#             format = "file"),
#  #tarchetypes::tar_group_by(joint_final_presences, as.data.frame(Final_Presences), Landuse),
  tar_target(Landuse, c("ForestDryPoor",  "ForestDryRich", "ForestWetPoor",
                                   "ForestWetRich",  "OpenDryPoor", "OpenDryRich",
                                   "OpenWetPoor", "OpenWetRich")),
  tar_target(
    joint_final_presences,
    GetLandusePresences(folder = "GBIF_Final_Presences/", Landuse = Landuse),
    pattern = map(Landuse)
 ),
tar_target(
  joint_final_presences_field,
  GetLandusePresences(folder = "Field_Final_Presences/",Landuse = Landuse),
  pattern = map(Landuse)
),

#output for GBIF data:
  tar_target(rarity_weight, calc_rarity_weight(joint_final_presences)),
  tar_target(rarity, calc_rarity(joint_final_presences, rarity_weight),
             map(joint_final_presences)),
  tar_target(PhyloDiversity,
             calc_pd(joint_final_presences, Phylo_Tree),
             map(joint_final_presences)),
  tar_target(name = output_Richness,
             command = export_richness(Results = PhyloDiversity, path = LandUseTiff),
             map(PhyloDiversity),
             format = "file"),
  tar_target(name = output_PD,
             command = export_pd(Results = PhyloDiversity, path = LandUseTiff),
             map(PhyloDiversity),
             format = "file"),
  tar_target(name = output_Rarity,
             command = export_rarity(Results = rarity, path = LandUseTiff),
             map(rarity),
             format = "file"),

#output for the GBIF+field data:
  #tar_target(rarity_weight_field, calc_rarity_weight(joint_final_presences_field)),
  #tar_target(rarity_field, calc_rarity(joint_final_presences_field, rarity_weight_field),
  #           map(joint_final_presences_field),
  #           iteration = "group"),
  tar_target(PhyloDiversity_field,
             calc_pd(joint_final_presences_field, Phylo_Tree),
            map(joint_final_presences_field))#,
#  tar_target(Richness_field, GetRichness(joint_final_presences_field)),
#  tar_target(name = output_Richness_field,
#             command = export_richness_field(Results = PhyloDiversity_field, path #= LandUseTiff),
#             map(PhyloDiversity_field),
#             format = "file"),
#  tar_target(name = output_PD_field,
#             command = export_pd_field(Results = PhyloDiversity_field, path = #LandUseTiff),
#             map(PhyloDiversity_field),
#             format = "file"),
#  tar_target(name = output_Rarity_field,
#             command = export_rarity_field(Results = rarity_field, path = #LandUseTiff),
#             map(rarity_field),
#             format = "file")
)

