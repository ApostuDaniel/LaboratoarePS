
d_normala=function(miu, sigmap)
{
  t = seq(-6, 6, length = 400);
  f = 1/sqrt(sigmap)*sqrt(2*pi)*exp(-(t-miu)^2/(2*sigmap))
  plot(t, f, type = "l", lwd = 1)
}

d_normala(1, 8)

#######################################################################
#X v.a. reala distribuita dupa N(1, 3^2)
x=7
#f(x) = P(X<=x)

y = pnorm(2.34, mean=1, sd=3)
y
qnorm(y, mean=1, sd=3)



alfa = 0.1
sample_mean = 20
n = 100
sigma = sqrt(9)
critical_z = qnorm(1 - alfa/2, 0, 1)
a = sample_mean - critical_z*sigma/sqrt(n)
b = sample_mean + critical_z*sigma/sqrt(n)
interval = c(a, b)
interval

#####################################################################

#III.1

zconfidence_interval = function(alfa, mean, n,  sigmap)
{
  sigma = sqrt(sigmap);
  critical_z = qnorm(1 - alfa/2, 0, 1)
  a = mean - critical_z*sigma/sqrt(n)
  b = mean + critical_z*sigma/sqrt(n)
  return(c(a, b, b-a)) 
}

#III.2
zconfidence_interval(0.1, 67.53, 25, 100)
#III.3
zconfidence_interval(0.05, 5, 0.5^2, 50)


#III.5
zconfidence_interval(0.1, 60, 35, 25)
zconfidence_interval(0.05, 60, 35, 25)
zconfidence_interval(0.01, 60, 35, 25)


##############################################################

alfa = 0.05
sample_mean = 3.3
n = 60
s = 0.4
se = s/sqrt(n)
critical_t = qt(1 - alfa/2, n - 1)
a = sample_mean - critical_t*se
b = sample_mean + critical_t*se
interval = c(a, b)
interval


#IV.1

t_conf_interval=function(alfa, sample_mean, n, s )
{
  se = s/sqrt(n)
  critical_t = qt(1 - alfa/2, n - 1)
  a = sample_mean - critical_t*se
  b = sample_mean + critical_t*se
  return(c(a, b, b-a))
}
#IV.2
t_conf_interval(0.01, 44.65, 196, sqrt(2.25))

#IV.3
#a
t_conf_interval(0.01, 12, 49, 1.75)
t_conf_interval(0.05, 12, 49, 1.75)
#b
t_conf_interval(0.05, 13.5, 49, 1.25)

assumption_testing = function(alfa, n, succeses, p0, testType)#testType = 1 => ipoteza asimetrica la dreapta
{                                                             #testType = -1 => ipoteza asimetrica la stinga
  p_prim = succeses/n;                                         #testType = 0 => ipoteza simetrica
  z_score = (p_prim - p0)/sqrt(p0*(1 - p0)/n)
  if(testType == -1)
  {
    critical_z = qnorm(alfa, 0, 1)
    return(z_score < critical_z)
  }
  else if(testType == 1)
  {
    critical_z = qnorm(1-alfa, 0, 1)
    return(z_score > critical_z)
  }
  else if(testType == 0)
  {
    critical_z = qnorm(1-alfa/2, 0, 1)
    return(abs(z_score) < abs(critical_z))
  }
}

assumption_testing(0.01, 100, 63, 0.6, 1)


#V.2
assumption_testing(0.05, 150, 20, 0.1, 1)

