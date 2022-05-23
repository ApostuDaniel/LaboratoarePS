#C1

n_queens = function(n)
{
  UnusedRows = c(1:n);
  row = rep(0, n);
  i = 1;
  while(length(UnusedRows) > 0 && i <= n)
  {
   r =  sample(UnusedRows,1);
   row[i] = r;
   i = i+1;
   if (i == n+1) break;
   UnusedRows = UnusedRows[UnusedRows != row[i-1]]
   for(j in 1:length(row))
   {
     high = row[j] + (i - j);
     low = row[j] - (i - j);
     if(high >=1 && high <=n)
     UnusedRows = UnusedRows[UnusedRows != high];
     if(low >=1 && low <=n)
       UnusedRows = UnusedRows[UnusedRows != low];
   }
  }
  if(length(UnusedRows) == 0 && i < n) return("Fail")
  else return(row);
}

#C.2

randMedian = function(S)
{
  n = length(S);
  x = (n^3)^(1/4);
  R = sample(S, trunc(x), replace = TRUE);
  R = sort(R);
  d = R[trunc(x/2 - n^(1/2))]
  u = R[trunc(x/2 + n^(1/2))]
  C = S[ S <= u & S >= d];
  ld = length(S[S < d]);
  lu = length(S[S > u]);
  if(ld > n/2 || lu > n/2 || length(C) > 4 * trunc(x))
    return("no median found");
  C = sort(C)
  return(C[trunc(n/2) - ld + 1])
}

S = sample(1:200, 51)
randMedian(S)
median(S)


#C.3 

#a
polinomValue = function(f, p)
{
  sum = 0;
  for(i in 1:length(f))
    sum = sum + f[i] * p^(i-1);
  return(sum);
}

polinomEquality = function(f, g, h)
{
  n = length(f);
  p = sample(1:n*3, 1);
  if(polinomValue(f, p) * polinomValue(g, p) == polinomValue(h, p))
    {return(TRUE);}
  else
   { return(FALSE);}
}

f = c(1, 1, 2);
g = c(3, 5, 1);
h = c(3, 8, 12, 11, 2);
polinomEquality(f, g, h)
#b
repeats = log(0.0001, 2/3)
repeats


#C.4

find_r_and_p = function(n)
{
  if(n <=1 ) return(c(-1, -1))
  
  for(r in 1:log(n-1,2))
  {
    p = (n-1)/2^r
    if(p == trunc(p) && p%%2 == 1)
    {
      return(c(p, r));
    }
  }
  return(c(-1, -1))
}

randIsPrime = function(n)
{
  values = find_r_and_p(n);
  if(values[1] == -1)
    return(FALSE)
  p = values[1];
  r = values[2];
  a = sample(1:n-1, 1);
  y = a^p%%n
  for(i in 1:r)
  {
    if(y^2%%n == 1 && y != 1 && y != n-1)
      return(FALSE)
    y = y^2%%n
  }
  if(y != 1) return(FALSE)
  else return(TRUE)
}

randIsPrime(17)






