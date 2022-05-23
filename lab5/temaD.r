estimator_cu_deviatie=function(alfa, sample_mean, n, s )
{
  se = s/sqrt(n)
  critical_t = qt(1 - alfa/2, n - 1)
  a = sample_mean - critical_t*se
  b = sample_mean + critical_t*se
  return(c(a, b))
}

estimator_cu_dispersie = function(alfa, mean, n,  sigmap)
{
  sigma = sqrt(sigmap);
  critical_z = qnorm(1 - alfa/2, 0, 1)
  a = mean - critical_z*sigma/sqrt(n)
  b = mean + critical_z*sigma/sqrt(n)
  return(c(a, b)) 
}

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

#D.1

estimator_cu_deviatie(0.05, 490, 150, 85)

#D.2

estimator_cu_deviatie(0.01, 101.5, 25, 5)

#D.3
#USA
assumption_testing(0.01, 1179, 0.085* 1179, 0.08, 1)
assumption_testing(0.05, 1179, 0.085* 1179, 0.08, 1)
#Romania
assumption_testing(0.01, 125, 24, 0.171, 1)
assumption_testing(0.05, 125, 24, 0.171, 1)

#D.4
assumption_testing(0.01, 250, 95, 0.4, 0)
assumption_testing(0.05, 250, 95, 0.4, 0)
