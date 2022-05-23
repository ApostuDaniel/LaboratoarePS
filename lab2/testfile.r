#I.1
sample = scan("sample1.txt")
stem(sample)

#I.2
tablou = read.csv("unemploy2012.csv", header = T, sep = ';')
rate = tablou[['rate']]
intreruperi = seq(0, 14, 2)
intreruperi[length(intreruperi) + 1] = 30
hist(rate, breaks = intreruperi, right = T, freq = F)

#I.3
table = read.csv("life_expect.csv", header = T, sep = ',')
men = table[['male']]
women = table[['female']]
a = 7
hist(men, breaks = a, right = F, freq = T)
hist(women, breaks = a, right = F, freq = T)

#II.1
sample = scan("sample1.txt")
mean(sample)
median(sample)

#II.2
table = read.csv("life_expect.csv", header = T, sep = ',')
men = table[['male']]
women = table[['female']]
mean(men)
median(men)
mean(women)
median(women)
#II.3

modul = function(x){
  len = max(x)
  frecventa = replicate(len, 0)
  for (variable in x) {
    frecventa[variable] = frecventa[variable] + 1;
  }
  max_indice = max(frecventa)
  mod = c()
  for (i in 1:len) {
    if(frecventa[i] == max_indice)
      mod = c(mod, i)
  }
  return(mod)
}

sample = scan("sample3.txt")
modul(sample)

#III.1

outliers_mean = function(x){
  m = mean(x)
  s = sd(x)
  outliers = vector()
  j = 0
  for (i in 1:length(x)) {
    if(x[i] < m-2*s || x[i] > m +2*s){
      j = j+1
      outliers[j] = sample[i]
    }
  }
  return(outliers)
}

sample = c(1, 91, 38, 72, 13, 27, 11, 85, 5, 22, 20, 19, 8, 17, 11, 15, 13, 23, 14, 17)
outliers_mean(sample)


#III.2
outliers_iqr = function(x){
  q1 = as.vector(quantile(x))[2]
  q3 = as.vector(quantile(x))[4]
  iqr = q3 - q1
  outliers = vector()
  j = 0
  for (i in 1:length(x)) {
    if(x[i] < q1 - 1.5 * iqr || x[i] > q3 +1.5*iqr){
      j = j+1
      outliers[j] = sample[i]
    }
  }
  return(outliers)
}

sample = c(1, 91, 38, 72, 13, 27, 11, 85, 5, 22, 20, 19, 8, 17, 11, 15, 13, 23, 14, 17)
outliers_iqr(sample)


#III.3
sample = scan("sample2.txt")
outliers_mean(sample)
outliers_iqr(sample)
summary(sample)

  
  

