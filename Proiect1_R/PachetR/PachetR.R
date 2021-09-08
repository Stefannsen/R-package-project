#install.packages("devtools")
#install.packages("pracma")
#writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
#Sys.which("make")
#install.packages("jsonlite", type = "source")
# 1
normConst <- function(f, a, b)
{
  1 / integrate(Vectorize(f), a, b)$value
}

proba1 <- function(x) {x^2 }
normConst(proba1,0,1)


# 5
# Media
media <- function(f,a = -Inf,b = Inf)
{
  aux <- function(x)
  {
    x * f(x) * normConst(f,a,b)
  }
  
  integrate(aux,a,b)$value
  
}

media2 <- function(f,a = -Inf,b = Inf)
{
  aux <- function(x)
  {
    x ^ 2 * f(x) * normConst(f,a,b)
  }
  
  integrate(aux,a,b)$value
  
}

#integrand <- function(x) {1}
#media(integrand,0,1)
#media2(integrand,0,1)
#media2(integrand,0,1) - media(integrand,0,1) ^ 2


# Dispersia
dispersia <- function(f, a = -Inf, b = Inf)
{
  media2(f,a,b) - media(f,a,b) ^ 2
}

# Momente
momentInitial <- function(f,a,b,r)
{
  aux <- function(x)
  {
    x ^ r * f(x) * normConst(f,a,b)
  }
  
  integrate(aux,a,b)$value
}

momentCentrat <- function(f,a,b,r)
{
  aux <- function(x)
  {
    medie <- media(f,a,b)
    (x - medie) ^ r * f(x) * normConst(f,a,b)
  }
  
  integrate(aux,a,b)$value
}

#momentInitial(integrand,0,1,1)
#momentCentrat(integrand,0,1,1)
#functie <- function(x) {  4 * x - 2 * x ^ 2 }
#normConst(functie,0,2)
#3/8
#media(functie,0,2)
#1
#dispersia(functie,0,2)
#1/5
#momentInitial(functie,0,2,4)
#momentCentrat(functie,0,2,4)
#normConst(g,0.1,1/3)


# 7
P <- function(f,a,b,x = a,y = b,x1 = a, y1 = b)
{
  pmf <- function(x)
  {
    f(x) * normConst(f,a,b)
  }
  if(x == y)
    0
  else
    if(x < y & x1 == a & y1 == b)
    {
      if(x < a)
        x <- a
      if(y > b)
        y <- b
      integrate(Vectorize(pmf),x,y)$value
    }
  else
    if(x < y & x1 < y1)
    {
      if(x1 < a)
        x1 <- a
      if(y1 > b)
        y1 <- b
      mini <- min(x,x1)
      maxi <- max(y,y1)
      if(mini < a)
        mini <- a
      if(maxi > b)
        maxi <- b
      integrate(Vectorize(pmf),mini,maxi)$value / integrate(Vectorize(pmf),x1,y1)$value   
    }
  else
    warning("Date incorecte!")
}

g <- function(x) {3}
#integrate(Vectorize(g),0.1,1/3)$value
1/8
P(g,0,1/3,0.1,1)
#P(g,100,1)

#functie2 <- function(x,y) {1/36}
#integral2(functie2,1,6,1,6)$Q
#3/16


# 10

fX <- function(f,x,a,b)
{
  aux <- function(y)
  {
    f(x,y)
  }
  integrate(Vectorize(aux),a,b)$value
}

fY <- function(f,y,a,b)
{
  aux <- function(x)
  {
    f(x,y)
  }
  integrate(Vectorize(aux),a,b)$value
}

cov <- function(f,a,b,c,d)
{
  library(pracma)
  aux <- function(x,y)
  {
    x * y * f(x,y)
  }
  
  fX1 <- function(x)
  {
    x * fX(f,x,c,d)
  }
  
  fY1 <- function(y)
  {
    y * fY(f,y,a,b)
  }
  
  integral2(aux,c,d,a,b)$Q - integrate(Vectorize(fX1),a,b)$value * integrate(Vectorize(fY1),c,d)$value
  
}

cor <- function(f,a,b,c,d)
{
  
  fX1 <- function(x)
  {
    x * fX(f,x,c,d)
  }
  
  fY1 <- function(y)
  {
    y * fY(f,y,a,b)
  }
  
  fX2 <- function(x)
  {
    fX1(x^2) * fX1(x)^2
  }
  
  fY2 <- function(y)
  {
    fY1(y^2) * fY1(y)^2
  }
  
  cov(f,a,b,c,d) / sqrt(integrate(Vectorize(fX2),a,b)$value * integrate(Vectorize(fY2),c,d)$value)
  
}

#pmf <- function(x,y) {8/3 * x^3 * y}
#fX(pmf,1,1,2)
#fY(pmf,1,0,1)
#cov(pmf,1,2,0,1)
#cor(pmf,1,2,0,1)


# 11
Px <- function(f,a,b,c,d) # F(b) = P(X <= b)
{
  fX1 <- function(x)
  {
    fX(f,x,c,d)
  }
  
  integrate(Vectorize(fX1),a,b)$value
}

Py <- function(f,a,b,c,d)
{
  fY1 <- function(x)
  {
    fY(f,x,c,d)
  }
  integrate(Vectorize(fY1),a,b)$value
}

#fun <- function(x,y) { 3/2 * ( x^2 + y^2 )}
#Px(fun,0,0.5,0,1)
#5/16
#Py(fun,0,0.5,0,1)


# 2
verificare_densitate <- function(functie)
{
  ok <- 0
  y <- integrate(Vectorize(functie), -Inf, Inf)
  for(val in seq(-10000,10000,by=0.001))
  {
    if(functie(val)<0)
    {
      ok <- 1
      break;
    }
    
  }
  z <- round(y$value,digits=7)
  if(ok==0&&z==1)
    print("Functia este densitate de probabilitate")
  else
    print("Functia nu este densitate de probabilitate")
}


# 4
reprezentare_grafica <- function(functie,a,b)
{
  x <- seq(a,b,0.01)
  y <- c()
  y1 <- c()
  F1 <- function(x,functie)
  {
    y <- integrate(Vectorize(functie), -Inf, x)
    return(y$value)
  }
  for(i in x)
  {
    y <- c(y,F1(i,functie))
  }
  for(i in x)
  {
    y1 <- c(y1,functie(i))
  }
  plot(x,y, col="red",main="Reprezentare grafica densitate si functie de repartitie",type = "l")
  lines(x,y1, col="blue")
  legend("topleft",
         c("Densitate","Functia de repartitie"),
         fill=c("blue","red"))
}


# 6
medie_g <- function(g,densitate)
{
  force(g)
  force(densitate)
  y <- integrate(Vectorize(function(x)(g(x)*densitate(x))), -Inf, Inf)
  return(y$value)
}

dispersie_g <- function(g,densitate)
{
  force(g)
  force(densitate)
  h <- function(x){g(x)*densitate(x)}
  y <- integrate(Vectorize(h), -Inf, Inf)
  a <- y$value
  force(h)
  k <- function(x){g(x)*h(x)}
  y <- integrate(Vectorize(k), -Inf, Inf)
  b <- y$value
  return(b-a^2)
}


# 12
sum_var <- function(f,g,val)
{
  force(g)
  force(f)
  sum <- function(z) (integrate (Vectorize(function(t) (f(t) * g(z - t))), -Inf, +Inf))
  return(sum(val))
}

dif_var <- function(f,g,val)
{
  force(g)
  force(f)
  dif <- function(z) (integrate (Vectorize(function(t) (f(t) * g(t - z))), -Inf, +Inf))
  return(dif(val))
}

