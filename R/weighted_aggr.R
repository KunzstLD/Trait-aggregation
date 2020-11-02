compute_weights_alt <-
  function(init, preproc) {
    
    # init
    N_species_init <- init[!is.na(species), .N, by = "genus"]
    N_genus_init <-
      init[is.na(species) & !is.na(genus), .N, by = "family"]
    N_family_init <- init[is.na(species) &
                            is.na(genus) & !is.na(family), .N, by = "order"]
    
    # preproc
    N_species_preproc <- preproc[!is.na(species), .(species,
                                                    family,
                                                    order,
                                                    .N),
                                 by = "genus"]
    N_genus_preproc <-
      preproc[is.na(species) & !is.na(genus), .(genus,
                                                order,
                                                .N),
              by = "family"]
    N_family_preproc <-
      preproc[is.na(species) &
                is.na(genus) & !is.na(family), .(family,
                                                 .N),
              by = "order"]
    
    # merge
    weight_species <- merge(
      N_species_preproc,
      N_species_init,
      by = "genus",
      suffixes = c("_preproc", "_init")
    )
    weight_genus <- merge(
      N_genus_preproc,
      N_genus_init,
      by = "family",
      suffixes = c("_preproc", "_init")
    )
    weight_family <- merge(
      N_family_preproc,
      N_family_init,
      by = "order",
      suffixes = c("_preproc", "_init")
    )
    
    # rbind & calculate weights
    weights <- rbind(weight_species,
                     weight_genus,
                     weight_family,
                     fill = TRUE)
    weights[, coeff := N_preproc / N_init]
    
    # needs to be by order, family or something -> probably don't use
    weights[, stand_coeff := coeff / sum(coeff)]
    
    # create taxa column for merge
    weights[, taxa := coalesce(species, genus, family)]
    return(weights)
  }

test <- mapply(function(x, y) {
  compute_weights_alt(init = x,
                      preproc = y)
},
trait_dat,
preproc_dat,
SIMPLIFY = FALSE)

# merge weight back
test_aus <- test$Trait_AUS_harmonized.rds
trait_dat_aus <- preproc_dat$Trait_AUS_harmonized

# create taxa column
trait_dat_aus[, taxa := coalesce(species, genus, family)]

# merge
trait_dat_aus[test_aus,
              `:=`(coeff = i.coeff),
              on = "taxa"]

# multiply all traits with coeffs and normalize again
trait_cols <- grep("species|genus|family|order|taxa|coeff", 
                   names(trait_dat_aus), 
                   value = TRUE,
                   invert = TRUE)


trait_dat_aus[, stand_coeff := coeff/sum(coeff), 
              by = "family"]

trait_dat_aus[, (trait_cols) := lapply(.SD, function(y) y*stand_coeff), 
              .SDcols = trait_cols]

trait_dat_aus[family == "Leptoceridae", lapply(.SD, sum),
              .SDcols = trait_cols,
              by = "family"]

trait_dat_aus[family == "Leptoceridae", ]
