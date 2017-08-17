
# Kolmogorov - Smirnov test -----------------------------------------------

# test nieparametryczny używany do porównywania rozkładów jednowymiarowych cech statystycznych. 
# Istnieją dwie główne wersje tego testu – dla jednej próby i dla dwóch prób.
# 
# Test dla jednej próby (zwany też testem zgodności λ Kołmogorowa) sprawdza, czy rozkład w populacji dla pewnej zmiennej losowej, 
# różni się od założonego rozkładu teoretycznego, gdy znana jest jedynie pewna skończona liczba obserwacji tej zmiennej 
#
# Istnieje też wersja testu dla dwóch prób, pozwalająca na porównanie rozkładów dwóch zmiennych losowych. Jego zaletą jest wrażliwość zarówno na różnice w położeniu, 
# jak i w kształcie dystrybuanty empirycznej porównywanych próbek.

?ks.test()

A <- rnorm(100)
B <- rnorm(100 , mean = 0, sd = 2)
C <- rnorm(100, mean = 1, sd = 1)

# in the two-sample case alternative = "greater" includes distributions for which x is stochastically smaller than y 

x<-seq(-2,4, 0.1)

ks.test(A, B)
ks.test(A, C)
ks.test(B, C)
ks.test(B, C, alternative = "greater")   # poprawnie  (mean(B) < mean(C))
ks.test(C, B, alternative = "greater")  # niepoprawnie

ecdf  # empirical cumulative distribution

# the CDF of x lies above and hence to the left of that for y)

cdfA <- ecdf(A)
cdfB <- ecdf(B)
cdfC <- ecdf(C)

plot(cdfB)
plot(cdfC)


z <- seq(-3, 3, 1)

cdfB(z)
cdfC(z)

cdfB(z) > cdfC(z)





