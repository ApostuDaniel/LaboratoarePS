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

#E.1

z_test(-1, 50, 30, 27, 0.01, 3.5)

#E.2

z_score = 2.05
alfa1 = 0.01;
critical_z1 = qnorm(1 - alfa1/2)
(abs(z_score) > abs(critical_z1))#FALSE, deci nesemnificativ pentru 1% nivel de semnificatie
alfa2 = 0.05
critical_z2 = qnorm(1 - alfa2/2)#TRUE, deci semnificativ pentru 5% nivel de semnificatie
(abs(z_score) > abs(critical_z2))


#E.3

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

#datele pentru n1, samplemean1 si sigma1 vor fi cele de la PhoneSkyGSM
#iar complementarele vor fi de la SkyPhoneGSM
#pentru a arata daca PhoneSkyGSM are servici mai bune trebuie sa aratam ca diferenta 
#intre ele e mai mare decit m0 = 0 cu 1% respectiv 5% semnificatie

z_test_means(1, 224, 200, 8.1, 7.8, 0.01, 0.92, 1.15, 0)#cu adevarat PhoneSkyGSM ofera servicii mai bune, pentru 1% si 5%.
z_test_means(1, 224, 200, 8.1, 7.8, 0.05, 0.92, 1.15, 0)

#E.4

#datele pentru n1, samplemean1 si sigma1 vor fi cele de la a prima universitate
#iar complementarele vor fi de la doua

z_test_means(1, 50, 50, 102, 109, 0.01, 8.3, 7.5, 0)
#FALSE, deci numarul de ore de voluntariat de la prima universitate nu
#e mai mare ca cel de la a doua universitate, cu un nivel de semnificatie de 1%


#E.5
#ipoteza nula: dispersile nu difera
#ipoteza alternativa: primul absolvent are o dispersie mai mare

n = 22
s1 = 2.15
s2 = 1.95
alfa = 0.01
F_score = (s1^2)/(s2^2);
critical_F = qf(1-alfa, n-1, n-1);
F_score
critical_F
#F_score = 1.215648 < 2.857371 , deci ipoteza alternativa se respinge



