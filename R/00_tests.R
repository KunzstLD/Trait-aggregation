library(data.table)
#
test <-
  data.table(
    x = c(1, 1, 1, 1, 2),
    y = c(0, 1, 2, 3, 4),
    z = c(0, 0, 3, 0, 0),
    taxa = c("A", "A", "A", "B", "B")
)


#
test[,
     lapply(.SD, function(y) {
       Mode(x = y)
     }),
     .SDcols = c("x", "y", "z"),
     by = taxa]

test[, c(lapply(.SD, function(y) {
  if (length(unique(y)) == length(y) & length(y) > 1) {
    max(y)
    # e.g. in case (0,0,3)
  } else if (Mode(y) == 0 & !all((y) == 0))  {
    Mode(y[y != 0])
  }
  else{
    Mode(y)
  }
}), .N),
.SDcols =  c("x", "y", "z"),
by = taxa]

# median(c(0, 0 , 0 , 0 , 1, 2, 0, 0))


# think about: if there's no clear majority vote "winner" then take the maximum (but within the majority votes)
# -> How to implement?
x <- c(4, 1 , 1, 2, 2, 0, 0, 3)

ux <- unique(x)
ux[which.max(tabulate(match(x, ux)))]
? which.max()



#--------------------------------------------------------------------------------------
# test <- AUS_subset[grepl("Dytiscidae", family), .(volt_bi_multi, volt_semi, volt_uni, 
#                                           species, genus, family, order)]
# AUS_subset[grepl("Hydrophilidae", family), ]
Trait_AUS[, .(nr_families = .N), 
          by = .(family)] %>% 
  .[order(nr_families),]

test <- Trait_AUS[grepl("Hydroptilidae", family), .(resp_gil, resp_pls,
                                                    resp_spi, resp_teg,
                                                    species, genus,
                                                    family, order)]
# change frequency of an affinity score
# into probability per genus
# -> category zero is no category?
test[, .(resp_gil_fc = as.factor(resp_gil),
         species,
         genus, 
         family, 
         order)] %>%
#  .[resp_gil_fc != 0, ] %>% 
  .[ , .(nr_genus = .N, 
         resp_gil_fc, 
         species, 
         family, 
         order), 
     by = .(genus)] %>% 
  .[, .(nr_categ_per_trait = .N, 
        nr_genus, 
        species,
        genus,
        family,
        order), 
    by = .(genus, resp_gil_fc)] %>% 
  .[, .(prob = nr_categ_per_trait/nr_genus,
        resp_gil_fc,
        species,
        genus,
        family,
        order)]
