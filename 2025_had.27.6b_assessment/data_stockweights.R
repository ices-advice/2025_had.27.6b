#  Create the stock weights file which is the same as the catch weights

inroute.tab <-"data/catch data/"
outdir <-"data/stock weights/"

cw <-read.ices(paste0(inroute.tab,"cw.dat"))

write.ices(cw,paste0(outdir,"sw.dat"))

