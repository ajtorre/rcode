n<-1000
p<-5
tmp<-exp(matrix(rnorm(n*p),nrow=n, ncol=p))
probs<-tmp/rowSums(tmp)
smp<-rep(0,n)
set.seed(1)
system.time(
  for (i in seq_len(n))
    smp[i]<-sample(p,1,prob=probs[i,])
)

smp

?sample
sample()
methods(sample)
getMethod(sample)
getAnywhere(sample)

rm(n)
rm(p)
rm(tmp)
rm(smp)

rm(probs)
library(bootstrap)
library(simEd)

n<-10000
p<-5
tmp<-exp(matrix(rnorm(n*p),nrow=n, ncol=p))
probs<-tmp/rowSums(tmp)
smp<-rep(0,n)
set.seed(1)
system.time(
  for (i in seq_len(n))
    smp[i]<-sample(p,1,prob=probs[i,])
)


n<-10000
p<-5
tmp<-exp(matrix(rnorm(n*p),nrow=n, ncol=p))
probs<-tmp/rowSums(tmp)
smp<-rep(0,n)
set.seed(1)
system.time(
  for (i in seq_len(n))
    smp[i]<-.Internal(sample(p,1,replace=FALSE,prob=probs[i,]))
)


qnorm(runif(1e7))


n<-10000
p<-5
tmp<-exp(matrix(runif(n*p) ,nrow=n, ncol=p))
probs<-tmp/rowSums(tmp)
smp<-rep(0,n)
set.seed(1)
system.time(
  for (i in seq_len(n))
    smp[i]<-max.col(probs[i,], ties.method = "random")
)
    
probs


pnorm(runif(1e6))

n<-10000
p<-5
tmp<-exp(matrix(runif.acomp(n,p),nrow=n, ncol=p))
probs<-tmp/rowSums(tmp)
smp<-rep(0,n)
set.seed(1)
system.time(
  for (i in seq_len(n))
    smp[i]<-.Internal(sample(p,1,replace=FALSE,prob=probs[i,]))
)

tmp
smp
set.seed(1)
save(.Random.seed, file='tmp.Rda')
rnorm(1)

load('tmp.Rda')
rnorm(1)

temp1<-function(){
  load('tmp.Rda')
  a<-rnorm(1)
  print(a)
}

temp1()
temp1()

set.seed(1)
rnorm(1)



choose(5,4)
lchoose(5,4)
#for first term of 3
exp(lchoose(5,4))
7^7
exp(7*log(7))
((7^7)*((12-7)^(12-7)))/(12^12)
((7^7)*(5^5))/(12^12)
#for second term of 3
(exp(7*log(7)+5*log(5)))/(exp(12*log(12)))
((12^12)/((7^7)*(12-7)^(12-7)))^(11)
exp(((12*log(12))/(7*log(7)*(12-7)*log(12-7))))

#for third term of 3
testin<-exp(12*log(12))/exp(7*log(7)+(12-7)*log(12-7))
exp(11*log(testin))

#for fourth term of 3:
(.5)^(7*11)
exp(7*11*log(.5))

#for fifth term of 3:
(.5)^((12-7)*11)
#exp((12-7)*11*(log(.5)))
list(list("a","b","c"), list("d", "e","f"),list("g","h","i"))

library(assertthat)
#thisworks!
denominator_formula<-function(n,p,k,q){
  assert_that(is.numeric(n))
  assert_that(is.numeric(p))
  assert_that(is.numeric(k))
  assert_that(is.numeric(q))
  first_term<-lchoose(n,k)
  second_term<-k*log(k)+(n-k)*log(n-k)-n*log(n)
  third_term<-q*(n*log(n)-k*log(k)-(n-k)*log(n-k))
  fourth_term<-k*q*log(p)
  fifth_term<-(n-k)*q*log(1-p)
  total_product<-exp(first_term+second_term+third_term+fourth_term+fifth_term)
  return(total_product)
}

denominator_formula(12,0.3,5,0.5)

choose(12,5)
is.numeric(.5)


denominator_formula(12,.3,0,.5)
lchoose(12,0)
choose(12,0)
exp(lchoose(12,0))
exp(0)
choose(12,12)










denominator_formula(12,.3,0,.5)
denominator_formula(12,.3,12,.5)
(.3)^(12*.5)
denominator_formula(12,0.3,7,0.5)


x<-list(1:9)
typeof(x)
sum(sapply(x, denominator_formula, n=10, p=0.3,q=0.5))+first_k(10,.3,.5)+last_k(10,.5,.3)
lapply(x, denominator_formula, n=10,p=0.3,q=0.5)

first_k<-function(n,p,q){
  only_term5<-exp(n*q*log(1-p))
return(only_term5)}

last_k<-function(n,p,q){
  only_term4<-exp(n*q*log(p))
  return(only_term4)
}

x1<-list(1:99)

library(proftools)

microbenchmark((sapply(x1, denominator_formula, n=100, p=0.3,q=0.5))+first_and_last_k(10,.3,.5))

first_and_last_k<-function(n,p,q){
  only_term5<-exp(n*q*log(1-p))
  only_term4<-exp(n*q*log(p))
  return(sum(only_term4,only_term5))
}

(.7)^(2)
(.3)^(2)

x<-list(1:1999)
rprof(sum(sapply(x, denominator_formula, n=2000, p=0.3,q=0.5))+first_and_last_k(2000,.5,.3))
choose(5,4)
exp(lchoose(5,4))

two_thousand<-c(1:1999)
microbenchmark(sum(denominator_formula(n=2000, p=0.3, k=two_thousand, q=.5))+first_and_last_k(2000,.5,.3))
(.5)^(2000*.5)

library(ggplot2)

tm<-microbenchmark(sum(sapply(x, denominator_formula, n=2000, p=0.3,q=0.5))+first_and_last_k(2000,.5,.3),
               sum(denominator_formula(n=2000, p=0.3, k=two_thousand, q=.5))+first_and_last_k(2000,.5,.3)), times=100L)
autoplot(tm)

get_total<-function(n,p,q){
  x<-list(1:(n-1))
  final_sums<-sum(sapply(x,denominator_formula,n=n,p=p,q=q))+first_and_last_k(n,p,q)
  return(final_sums)
}

get_vector_total<-function(n,p,q){
  x<-c(1:(n-1))
  final_sums<-sum(denominator_formula(n=n,p=p,k=x,q=q))+first_and_last_k(n,p,q)
  return(final_sums)
}
get_vector_total(2000,.3,.5)
tm<-microbenchmark(get_total(2000,.3,.5),
                   get_vector_total(2000,.3,.5), times=100L)
autoplot(tm)
rm(tm)
rm(tm1)
rm(tm2)
rm(tm)
rm(tm1)
rm(tmp1)
#```{r}
#use microbenchmark for n=2000
tmp1<-microbenchmark(get_total(2000,.3,.5),
                     get_vector_total(2000,.3,.5), times=100L)
autoplot(tmp1)



n<-10000
p<-5
tmp<-exp(matrix(runif(n*p),nrow=n, ncol=p))
probs<-tmp/rowSums(tmp)
smp<-rep(0,n)
set.seed(1)
#system.time(
  #for (i in seq_len(n))
    #smp[i]<-.Internal(sample(p,1,replace=FALSE,prob=probs[i,]))
#)
library(rbenchmark)
benchmark(replications = 1000,
          test1 = for(i in seq_len(n)) smp[i] <-sample(p,1,prob=probs[i,]),
          #test2 = apply(probs, MARGIN = 1, FUN = function(x) sample(p, 1, prob = x)),
          test3= replicate(n, sample(p, 1, prob =runif(5))))
(apply(probs, MARGIN=1, FUN=function(x) sample(p,1,prob=x)))
system.time(replicate(n,sample(p,1,prob = runif(5))))

library(matrixStats)

n<-10000
p<-5
tmp<-exp(matrix(runif(n*p),nrow=n, ncol=p))
probs<-tmp/rowSums(tmp)
smp1<-rowOrderStats(probs)
smp1

n<-10000
p<-5
tmp<-exp(matrix(runif(n*p),nrow=n, ncol=p))
probs<-tmp/rowSums(tmp)
smp<-rep(0,n)
set.seed(1)
system.time(
for (i in seq_len(n))
smp[i]<-which.max(probs[i,]))

smp
