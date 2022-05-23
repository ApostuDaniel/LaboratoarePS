factura = function(x){
  print(max(x));
  print(min(x));
  print(sum(x)/length(x));
  print(sum(x));
  count = 0;
  for(i in 1:length(x))
  {
    if(x[i] > 40){
      count = count + 1;
    }
  }
  print(count);
  print(count/12*100);
}

ex2b = function(t){
  x = scan(t);
  a = vector();
  a = (x - min(x))/max(x);
  return(a);
}

b = c(46, 33, 39, 37, 46, 30, 48, 32, 49, 35, 36, 48);
k = ex2b("data.txt");
k

ex2d = function(x){
  a = vector(length = length(x));
  for(i in 1:(length(x)-1))
  {
    a[i] = max(x)/min(x[i+1:length(x)]);
  }
  return(a);
}

BNP = function(n, p){
  x = seq(0,n,1);
  y = (dbinom(x, n, p));
  barplot(y, space = 0, main='barplot', sub = 'subtitlul', xlab ='axa x', ylab = 'axa y')
}

maxbnp = function(n, p)
{
  x = seq(0,n,1);
  y = (dbinom(x, n, p));
  return(max(y));
}

sumFirstknp = function(n, p, k)
{
  x = seq(0,n,1);
  y = (dbinom(x, n, p));
  return(sum(y[1:k]));
}

GNP = function(n, p, k){
  x = seq(0,n,1);
  y = (dgeom(x, p));
  plot(x[1:k], y[1:k], type = 'S', main='barplot', sub = 'subtitlul', xlab ='axa x', ylab = 'axa y')
}

GNP(16, 0.3, 13)


