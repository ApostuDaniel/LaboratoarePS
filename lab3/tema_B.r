
pi = 3.14159265358
#B1

elipsoid_volume = function(N) {
  N_C = 0;
  for(i in 1:N) {
    x = runif(1, -3, 3);
    y = runif(1, -2, 2);
    z = runif(1, -4, 4);
    if((x^2/9 + y*y/4 + z*z/16) <= 1) N_C = N_C + 1;
  }
  return(6*4*8*N_C/N); 
}
actual = 4*pi*2*4;
estimation = vector();
estimation[1] = elipsoid_volume(10000)
estimation[2] = elipsoid_volume(20000)
estimation[3] = elipsoid_volume(50000)
for (i in estimation){
  print(abs(i - actual));
  print(abs(i - actual)/abs(actual));
}

#B2

#0<=x<=2;  0<=x<=4/3

trinagle_area = function(N){
  N_C = 0;
  for (i in 1:N){
    {
      x = runif(1,0,2)
      y = runif(1,0,3/4)
      if(x >= 0 && y <= x && 2*x + y <= 4) N_C = N_C + 1;
    }
    return(2*(3/4)*N_C/N)
  }
}

trinagle_area(20000)

#B3
#a

MC_integration1 = function(N)
{
  suma = 0;
  for(i in 1:N)
  {
    x = runif(1,0,1)
    suma = suma + 1/(1 + sqrt(x))
  }
  return(suma/N)
}

actual = 2 - 2*log(2);
mc = MC_integration1(20000)
abs(mc - actual)

#b

MC_integration2 = function(N)
{
  suma = 0;
  for(i in 1:N)
  {
    x = rexp(1,1)
    suma = suma + 1/(1 + x*x)/exp(-x)
  }
  return(suma/N)
}

actual = pi/2;
mc =  MC_integration2(20000)
abs(mc - actual)

#B4

four_servers = function(N)
{
  sum = 0
  for(i in 1:N)
  {
    latency = rpois(1,4)
    sum  = sum + latency
    server = runif(1,0,1);
    if(server < 0.35) sum = sum + rgamma(1,5,3)
    else if(server >= 0.35 && server < 0.50) sum = sum + rgamma(1, 5, 2)
    else if(server >= 0.5 && server < 0.70) sum = sum + rgamma(1, 4, 3)
    else sum = sum + rgamma(1, 6, 4)
  }
  return(sum/N)
}

four_servers(20000)

#B5
#p = 0.07, q = 0.55, k = 10
#v[i] == 1 -> computerul i este infectat 
#a)
infection = function(LAN, p, q, k)
{
  day = 1;
  while(sum(LAN) > 0)
  {
    s = sum(LAN);
    for(i in 1:s)
    {
      for (i in 1:length(LAN)){
        if(LAN[i] != 1 ){
          x = runif(1, 0, 1);
          if(x <= p){ LAN[i] = 1}
        }
      }
    }
    if(day > 1){
      to_check = k;
      while(to_check > 0 && sum(LAN) != 0)
      {
        comp = sample(1:length(LAN), 1);
        if(LAN[comp] == 1)
        {
          to_check = to_check - 1;
          y = runif(1, 0, 1);
          if(y <= q) {LAN[comp] = 0;}
        }
      }
    }
    a = length(LAN)
    b = sum(LAN)
    if(k < s*((a-s)*p - (a-s)*p*(1-p))) return(-1);
    day = day + 1;
  }
  return(day);
}

medium = function(len, p, k, q, n)
{
  sum = 0;
  for(i in 1:n)
  {
    LAN = rep(0, len)
    for(j in 1:3)
    {
      index = sample(1:len, 1)
      LAN[index] = 1;
    }
    sum = sum + infection(LAN,p,q,k);
  }
  return (sum/n);
}

medium(20, 0.07, 10, 0.55, 100);

#b)
#Takes a long time to show results
before10Days = function(n)
{
  sum = 0;
  for(i in 1:n)
  {
    m = medium(20, 0.07, 10, 0.55, 100);
    if(m > 10) sum = sum + 1;
  }
  return (sum/n);
}

before10Days(100);

#c)
alfa = 1 - 0.95;
z = qnorm(alfa/2);
epsilon = 0.01;
p = 0.36      #value of before10Days when I tested it
n_min = p*(1-p)*(z/epsilon)^2;
before10Days(n_min + 1)



##labwork, noo need to look over

# sfera_volume = function(N) {
#   N_C = 0;
#   for(i in 1:N) {
#     x = runif(1, -1, 1);
#     y = runif(1, -1, 1);
#     z = runif(1, -1, 1);
#     if((x*x + y*y + z*z) <= 1) N_C = N_C + 1;
#   }
#   return(8*N_C/N);
# }
# 
# v = sfera_volume(50000)
# 
# abs(v - 4*pi/3 )
# 
# 
# p_area = function(N){
#   N_C = 0;
#   for (i in 1:N){
#     {
#       x = runif(1,0,2)
#       y = runif(1,0,2)
#       if(y <= -2*x*x + 5*x-2) N_C = N_C + 1;
#     }
#     return(4*N_C/N)
#   }
# }
# 
# p_area(10000)
# 
# MC_integration2 = function(N)
# {
#   suma = 0;
#   for(i in 1:N)
#   {
#     x = runif(1,0,pi)
#     suma = suma + sin(x)^2
#   }
#   return(pi*suma/N)
# }
# 
# abs(MC_integration2(10000) - pi/2)
# abs(MC_integration2(10000) - pi/2)/abs(pi/2)
# 
# 
# f=function(x)
# {
#   if(x<1) return(0)
#   return(1/(4*x*x-1))
# }
# 
# MC_integration3 = function(N)
# {
#   suma = 0;
#   for(i in 1:N)
#   {
#     x = rexp(1,1)
#     suma = suma + f(x)/exp(-x)
#   }
#   return(suma/N)
# }
# 
# abs(MC_integration3(10000) - log(3)/4)
# abs(MC_integration3(10000) - log(3)/4)/abs(log(3)/4)
# 
# MC_integration4 = function(N)
# {
#   suma = 0;
#   for(i in 1:N)
#   {
#     x = rexp(1,1)
#     suma = suma + exp(-2*x*x)/exp(-x)
#   }
#   return(suma/N)
# }
# 
# abs(MC_integration4(10000) - sqrt(pi/8))
# abs(MC_integration4(10000) - sqrt(pi/8))/abs(sqrt(pi/8))
# 
# z=rep(0,30)
# for(i in 1:30)
# {
#   z[i] = MC_integration4(10000)
# }
# 
# mean(z)
# sd(z)
# summary(z)

michal <- function(xx, m=10)
{
  ##########################################################################
  #
  # MICHALEWICZ FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUTS:
  #
  # xx = c(x1, x2)
  # m = constant (optional), with default value 10
  #
  ##########################################################################
  
  ii <- c(1:length(xx))
  sum <- sum(sin(xx) * (sin(ii*xx^2/pi))^(2*m))
  
  y <- -sum
  return(y)
}

rastr <- function(xx)
{
  ##########################################################################
  #
  # RASTRIGIN FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################
  
  d <- length(xx)
  
  sum <- sum(xx^2 - 10*cos(2*pi*xx))
  
  y <- 10*d + sum
  return(y)
}

sumsqu <- function(xx)
{
  ##########################################################################
  #
  # SUM SQUARES FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################
  
  ii <- c(1:length(xx))
  sum <- sum(ii*xx^2)
  
  y <- sum
  return(y)
}

s = c(3, 4, 5);

sumsqu(s)

schwef <- function(xx)
{
  ##########################################################################
  #
  # SCHWEFEL FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################
  
  d <- length(xx)
  
  sum <- sum(xx*sin(sqrt(abs(xx))))
  
  y <- 418.9829*d - sum
  return(y)
}

s = c(3, 4, 5)
schwef(s)












