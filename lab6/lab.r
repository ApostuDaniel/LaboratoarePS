
z_test = function(type, n, population_mean, sample_mean, alfa, sigma)
{
  z_score = (sample_mean - population_mean)/(sigma/sqrt(n));
  if(type == 1){
    critical_z = qnorm(1 - alfa)
    return(z_score > critical_z)
    }
  else if(type == -1){
    critical_z = qnorm(alfa)
    return(z_score < critical_z );
    }
  else {
    critical_z = qnorm(1 - alfa/2)
    return(abs(z_score) > abs(critical_z))
  }

}

#I.2
z_test(-1, 49, 90, 88, 0.05, 144^(1/2));

#I.4
z_test(-1, 100, 21, 20.5, 0.01, 2.5)

#II.1
t_test = function(type, n, population_mean, sample_mean, alfa, sd)
{
  t_score = (sample_mean - population_mean)/(sd/sqrt(n));
  if(type == 1){
    critical_t = qt(1 - alfa, n-1)
    return(t_score > critical_t)
  }
  else if(type == -1){
    critical_t = qt(alfa, n-1)
    return(t_score < critical_t );
  }
  else {
    critical_t = qt(1 - alfa/2, n-1)
    return(abs(t_score) > abs(critical_t))
  }
}

t_test_data = function(x, type, population_mean, alfa)
{
  return(t_test(type, length(x), population_mean, mean(x), alfa, sd(x)));
}

#II.2
x = c(36, 32, 28, 33, 41, 28, 31, 26, 29, 34)
t_test_data(x, 0, 34, 0.01)

#II.4

x = scan("h.txt")

t_test_data(x , 0, 80, 0.01)
t_test_data(x , 0, 80, 0.05)

#III.1

z_test_means = function(type, n1, n2, sample_mean1, sample_mean2 , alfa, sigma1, sigma2, m0)
{
  combined_sigma = sqrt(sigma1^2/n1 + sigma2^2/n2);
  z_score = (sample_mean1 - sample_mean2 - m0)/combined_sigma
  if(type == 1){
    critical_z = qnorm(1 - alfa)
    return(z_score > critical_z)
  }
  else if(type == -1){
    critical_z = qnorm(alfa)
    return(z_score < critical_z );
  }
  else {
    critical_z = qnorm(1 - alfa/2)
    return(abs(z_score) > abs(critical_z))
  }
  
}


z_test_means(0, 80, 70, 160, 155, 0.01, 3.24, 2.25, 0)














