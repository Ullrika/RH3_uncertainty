
doit <- function(i,a,b){
prior_A = 0.5
prior_A_imp = seq(0.01,0.99,by=0.01)

n = 20
#lik_A = runif(n)
#lik_notA = runif(n)
lik_A = rbeta(n,a,b)
lik_notA = rbeta(n,a,b)



#post_A = lik_A*prior_A / (lik_A*prior_A + lik_notA*(1-prior_A))
LRs = lik_A/lik_notA
#hist(log(LRs))

LR_prod = prod(LRs)
LR_range = range(LRs)

prior_odds = prior_A/(1-prior_A)
prior_odds_imp = prior_A_imp/(1-prior_A_imp)

# prior 0-100% update with LR that is the product of all studies
post_A_prod = 1/(1 + LR_prod^-1*prior_odds_imp^-1)
post_A_prod_range = range(post_A_prod)

# prior 50% update with lowest and highest LR 
post_A_range = 1/(1 + LR_range^-1*prior_odds^-1)


data.frame(p_low = c(post_A_prod_range[1],post_A_range[1]),
           p_high = c(post_A_prod_range[2],post_A_range[2]),
           what = c("imp","lik"),iter = i, y = i + c(0,0.5))

}

df <- do.call('rbind',lapply(1:4,function(i){doit(i,a=0.01,b=0.01)}))

library(ggplot2)
ggplot(df,aes(x=p_low,y=y,col=what)) + 
geom_segment(aes(xend=p_high,yend=y))


## likelihood ratio only
#  log(lik_evid_pos/lik_evid_neg)

  prior_A = 0.5
  prior_A_imp = seq(0.01,0.99,by=0.01)
  
  n = 10
  #a = 0.6
  #b = 0.6
  a = 5
  b = 5
  lik_A = rbeta(n,a,b)
  hist(lik_A)
  lik_notA = runif(n)
  #post_A = lik_A*prior_A / (lik_A*prior_A + lik_notA*(1-prior_A))
  LRs = lik_A/lik_notA
  hist(LRs)
  
  LR_prod = prod(LRs)
  LR_range = range(LRs)
  
  prior_odds = prior_A/(1-prior_A)
  prior_odds_imp = prior_A_imp/(1-prior_A_imp)
  
  # prior 0-100% update with LR that is the product of all studies
  post_A_prod = 1/(1 + LR_prod^-1*prior_odds_imp^-1)
  post_A_prod_range = range(post_A_prod)
  
  # prior 50% update with lowest and highest LR 
  post_A_range = 1/(1 + LR_range^-1*prior_odds^-1)
  
  post_A_prod_range
  post_A_range
  