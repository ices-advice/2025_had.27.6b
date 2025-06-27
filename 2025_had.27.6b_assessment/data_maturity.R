#######################################################################
# Maturity ogive calculations for Rockall haddock              #
#######################################################################
last.yr <-2024

# directories
inroute.tab <-"boot/data/"
output.dir <- "data/maturity/"

mo <-read.ices(paste0(inroute.tab,"mo.dat"))

nms <-c(rownames(mo),as.character(last.yr))

mo.new <-rbind(mo,mo[nrow(mo),])
rownames(mo.new) <-nms

write.ices(mo.new,paste0(output.dir,"mo.dat"))


