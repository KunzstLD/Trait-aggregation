# NOA
# Gastropoda
trait_dat$Traits_US_LauraT_pp_harmonized.rds[family %in% c("Acroloxidae",
                                                           "Valvatidae",
                                                           "Planorbidae",
                                                           "Lymnaeidae",
                                                           "Pilidae", 
                                                           "Ancylidae",
                                                           "Ampullaridae",
                                                           "Pleuroceridae",
                                                           "Physidae",
                                                           "Thiaridae"),
                                             order := "Gastropoda"]

# Venerida
trait_dat$Traits_US_LauraT_pp_harmonized.rds[family == "Corbiculidae", 
                                             order := "Venerida"]

# Anthoathecata
trait_dat$Traits_US_LauraT_pp_harmonized.rds[family == "Clavidae", 
                                             order := "Anthoathecata"]

# Trichoptera
trait_dat$Traits_US_LauraT_pp_harmonized.rds[family %in% c("Limnephilinae", 
                                                           "Goerinae"),
                                             order := "Trichoptera"]

# Polychaeta
trait_dat$Traits_US_LauraT_pp_harmonized.rds[family %in% c("Nerillidae", 
                                                           "Aeolosomatidae"),
                                             order := "Polychaeta"]

# Arachnida
trait_dat$Traits_US_LauraT_pp_harmonized.rds[family == "Uchidastygacaridae", 
                                             order := "Arachnida"]


# AUS
# Gastropoda
trait_dat$Trait_AUS_harmonized.rds[family %in% c(
  "Buccinidae",
  "Physidae",
  "Nassariidae",
  "Glacidorbidae",
  "Potamididae",
  "Onchidiidae",
  "Planorbidae",
  "Planorbidae/Physidae",
  "Lymnaeidae",
  "Pilidae"
),
order := "Gastropoda"]

# Bivalvia
trait_dat$Trait_AUS_harmonized.rds[family == "Pisidiidae",
                                   order := "Bivalvia"]

# Sacroptiformes
# Actually a true order
trait_dat$Trait_AUS_harmonized.rds[family == "Ramsayellidae",
                                   order := "Sacroptiformes"]

# Hirudinea
trait_dat$Trait_AUS_harmonized.rds[family == "Richardsonianidae",
                                   order := "Hirudinea"]

# Littorinimorpha
trait_dat$Trait_AUS_harmonized.rds[family %in% c("Naticidae",
                                                 "Pomatiopsidae"),
                                   order := "Littorinimorpha"]

# Arachnida
trait_dat$Trait_AUS_harmonized.rds[family == "Oribatidae",
                                   order := "Hirudinea"]

# Platyhelminthes
trait_dat$Trait_AUS_harmonized.rds[family == "Didymorchidae",
                                   order := "Platyhelminthes"]

# CHECK
print(lapply(trait_dat, function(y) y[is.na(order), unique(family)]))
