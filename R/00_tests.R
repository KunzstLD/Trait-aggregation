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
