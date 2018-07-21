rm(list=ls())
library(tidyverse)
library(GGally)
library(summarytools)
library(rstan)
options(mc.cores = parallel::detectCores())
library(shinystan)
library(plotly)
library(gridExtra)

# 読み込みと下処理 ----------------------------------------------------------------
bs <- read_csv("baseball.csv") %>% 
        mutate(Name = as.factor(Name),
              team = as.factor(team),
              position=as.factor(position),
              bloodType=as.factor(bloodType),
              throw.by=as.factor(throw.by),
              batting.by=as.factor(batting.by))
pokemon <- read_csv("pokemon.csv") %>% 
  mutate(ID = as.factor(ID),
         Name = as.factor(Name),
         Type1 = as.factor(Type1),
         Type2 = as.factor(Type2),
         Legendary = as.factor(Legendary))
# 基本的な分布の確認 ---------------------------------------------------------------

view(dfSummary(bs))
view(dfSummary(pockemon))

bs %>% filter(position!='投手') %>% 
  select(salary,years,height,weight) %>% 
  ggpairs()

bs %>% filter(position=='投手') %>% 
  select(salary,years,height,weight) %>% 
  ggpairs()

pokemon %>% select(-ID,-Name,-Type1,-Type2) %>% ggpairs()

# 分析開始 --------------------------------------------------------------------


# 一変量の推定
## 平均身長
bs %>% select(height) %>% na.omit() %>% unlist() -> height
model1 <- stan_model('01_normal.stan')
dataset <- list(N=NROW(height),X=height)
fit1 <- sampling(model1,data=dataset,
                 warmup=1000,iter=2000,chains=4)
# 結果の確認
fit1

# MCMCサンプルの取り出し
fit1.mcmc <- rstan::extract(fit1,pars=c('mu','sig')) %>% as.data.frame()
# サンプルの長さを確認
NROW(fit1.mcmc)
# 事後分布
fit1.mcmc %>% ggplot(aes(x=mu))+geom_histogram(binwidth = 0.01)
fit1.mcmc %>% ggplot(aes(x=sig))+geom_histogram(binwidth=0.01)
# 同時分布
fit1.mcmc %>% ggplot(aes(x=mu,y=sig))+geom_point() -> gg
ggplotly(gg)
x_c <- fit1.mcmc$mu %>% cut(.,50)
y_c <- fit1.mcmc$sig %>% cut(.,50)
z <- table(x_c, y_c)
plot_ly(z=z,type="surface")

# 本当は事前分布を入れたほうがいい
model1k <- stan_model('02_normal1.stan')
fit1k <- sampling(model1k,data=dataset,
                 warmup=1000,iter=2000,chains=4)
# 結果の確認
fit1k
# MCMCサンプルの取り出し
fit1k.mcmc <- rstan::extract(fit1k,pars=c('mu','sig')) %>% as.data.frame()
# サンプルの長さを確認
NROW(fit1k.mcmc)
# 事後分布
fit1k.mcmc %>% ggplot(aes(x=mu))+geom_histogram(binwidth = 0.01)
fit1k.mcmc %>% ggplot(aes(x=sig))+geom_histogram(binwidth=0.01)
# 同時分布
fit1k.mcmc %>% ggplot(aes(x=mu,y=sig))+geom_point() -> gg
ggplotly(gg)
x_c <- fit1k.mcmc$mu %>% cut(.,50)
y_c <- fit1k.mcmc$sig %>% cut(.,50)
z <- table(x_c, y_c)
plot_ly(z=z,type="surface")


## 平均体重(事前予測分布，事後予測分布)
bs %>% select(weight) %>% na.omit() %>% unlist() -> weight
dataset <- list(N=NROW(weight),X=weight)
fit2 <- sampling(model1,data=dataset,
                 warmup=2000,iter=5000,chains=4)
# 結果の確認
fit2

model2 <- stan_model('03_normal.stan')
fit2 <- sampling(model2,data=dataset,
                 warmup=2000,iter=5000,chains=4)
# 結果の確認
fit2

## 描画して確認
fit2.mcmc <- rstan::extract(fit2,pars=c("mu","sig","mu_pre",'sig_pre',"pred1","pred2")) %>% as.data.frame()
### 事前分布
fit2.mcmc %>% select(mu_pre,sig_pre) %>% gather(key,val,factor_key=T) %>% 
  ggplot(aes(x=val,color=key,fill=key))+geom_histogram()+
  facet_wrap(~key,scales='free') ->g1
### 事前予測分布
fit2.mcmc %>% select(pred1) %>% ggplot(aes(x=pred1))+geom_histogram() -> g2
### 事後分布
fit2.mcmc %>% select(mu,sig) %>% gather(key,val,factor_key=T) %>% 
  ggplot(aes(x=val,color=key,fill=key))+geom_histogram()+
  facet_wrap(~key,scales='free') -> g3
### 事後予測分布
fit2.mcmc %>% select(pred2) %>% ggplot(aes(x=pred2))+geom_histogram() ->g4


# 個別表示
g1
g2
g3
g4
# まとめて表示
grid.arrange(g1,g2,g3,g4,ncol = 2)

## 平均年俸ーおや，分布が違うぞ
bs %>% select(salary) %>% ggplot(aes(x=salary))+geom_histogram(binwidth=1000)
bs %>% select(salary) %>% na.omit() %>% unlist() -> sal
dataset <- list(N=NROW(sal),X=sal,Lower=0,Upper=1000000)
model2k <- stan_model("04_normal2.stan")
fit3 <- sampling(model2k,dataset)
fit3
mean(bs$salary,na.rm=T)
## 描画して確認
fit3.mcmc <- rstan::extract(fit3,pars=c("mu","sig","mu_pre",'sig_pre',"pred1","pred2")) %>% as.data.frame()
### 事前分布
fit3.mcmc %>% select(mu_pre,sig_pre) %>% gather(key,val,factor_key=T) %>% 
  ggplot(aes(x=val,color=key,fill=key))+geom_histogram()+
  facet_wrap(~key,scales='free') ->g1
### 事前予測分布
fit3.mcmc %>% select(pred1) %>% ggplot(aes(x=pred1))+geom_histogram() -> g2
### 事後分布
fit3.mcmc %>% select(mu,sig) %>% gather(key,val,factor_key=T) %>% 
  ggplot(aes(x=val,color=key,fill=key))+geom_histogram()+
  facet_wrap(~key,scales='free') -> g3
### 事後予測分布
fit3.mcmc %>% select(pred2) %>% ggplot(aes(x=pred2))+geom_histogram() ->g4
# 個別表示
g1
g2
g3
g4
# まとめて表示
grid.arrange(g1,g2,g3,g4,ncol = 2)

data.frame(val=c(bs$salary,fit3.mcmc$pred2),
           key=factor(c(rep(1,NROW(bs)),rep(2,NROW(fit3.mcmc))),
                      labels=c("data","post pred. dist.")) )%>% 
ggplot(aes(x=val,fill=key))+
  geom_histogram(binwidth=1000,alpha=0.5)
# geom_density(alpha=0.5)+ theme(legend.position = 'none')
#  facet_wrap(~key,scales='free')  

## 打率の推定
## 身長と体重の相関

# 二変数の推定
## セリーグとパリーグの体格差
## セリーグとパリーグの年俸差

# 回帰分析
## ホームランをうつには（ポアソン回帰
## グラウンドに銭は埋まっているか（チームごとの階層
## 盗塁をするには(混合分布)

# クラスター分析
## ポケモンのグループ化
## ポケモンの因子分析
## ポケモンの潜在クラス分析
