
#A1 (a)
plotPois = function(k, l, lambda){
  x = seq(k, l, 1);
  y = dpois(x, lambda)
  plot(x, y, type = 'l', xlab ="x axis", ylab = "y axis")
  return(max(y)) 
}

plotPois(1, 19, 15)

#A1 (b)

plotGeom = function(k, l, p){
  x = seq(k, l, 1);
  y = dgeom(x, p)
  plot(x, y, type = 'l', xlab ="x axis", ylab = "y axis")
  return(sum(y))
}

plotGeom(1, 19, 0.2)


#A2
#a)
values = function(name){
  sample = scan(name)
  valVector = vector()
  valVector[1] = median(sample)
  valVector[2] = mean(sample)
  valVector[3] = sd(sample)
  valVector[4] = as.vector(quantile(sample))[2]
  valVector[5] = as.vector(quantile(sample))[4]
  return(valVector)
}

values("esantion.txt")

#b)

filterAberantValues = function(name, name2){
  sample = scan(name);
  m = mean(sample);
  s = sd(sample);
  vec = vector();
  for (i in 1:length(sample)) {
    if(!(sample[i] > m + 2*s || sample[i] < m - 2*s))
      vec = c(vec, sample[i])
  }
  write(vec, file = name2, ncolumns = 1, sep = "\n")
}

filterAberantValues("esantion.txt", "in.txt")

#c)

plotClean = function(name)
{
  brk = seq()
  sample = scan(name);
  brk = seq(40, max(sample), 5)
  hist(sample, breaks = brk, col = "blue", right = T )
}

filterAberantValues("esantion.txt", "in.txt")

plotClean("in.txt")



