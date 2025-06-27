# natural mortality - post DEWK decisions

output.dir <- "data/natural mortality/"
ac <- as.character

# apply Lorenzen to raw catch weights

# Read in catch weights
cw <- read.ices("data/catch data/cw.dat")

# Apply Lorenzen
# put in g
cw <-cw*1000

nm <- 3*cw^-0.29

#average over time and then scale
nm.mean <-colMeans(nm)

nm.new <- as.data.frame(lapply(0.2*nm.mean/nm.mean[8], rep, dim(cw)[1]))
rownames(nm.new) <-rownames(cw)
colnames(nm.new) <-colnames(cw)

write.ices(as.matrix(nm.new),paste0(output.dir,"nm.dat"))
