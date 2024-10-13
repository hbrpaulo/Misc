# tentativa de verificar tendencia nas distancia por regressao ----
suavizacao <- zoo::rollmean(resi_cs_middle$diffs2, k = 5)
reg_dist2_cs <- lm(suavizacao ~ poly(1:length(suavizacao), degree = 2, raw = TRUE))
summary(reg_dist2_cs)
par(mfrow = c(1, 1))
suavizacao_lm <- reg_dist2_cs$fitted.values
index <- 1:length(rollmedia)
ts.plot(cbind(suavizacao, reg_dist2_cs$fitted.values), type = 'l', 
        ylab = 'rollmean(dist2)')


# acessar/criar dados de residuo cox-snell
# tentativa 01 ----
# ñ consegui extrair coxsnell e a linha da lista
CS_residuals <- tibble(coxSnell = D__Cs__Snr$RESI$Dados$CS,
                       censor = D__Cs__Snr$RESI$Dados$C)
CS_residuals$coxSnell %>% plot(., col = CS_residuals$censor+8, pch = 19)
# plot(qexp(), CS_residuals$coxSnell, CS_residuals$censor)
# problema: nao sei como reproduzir o graficos dos residuos a partir disso

data <- readxl::read_excel('input/Exemplo1_Comparativo_Fornecedores.xlsx')

library(survival)
survfit(Surv(data$Tempo, data$Censura)~data$Fornecedor)
cox.zph(Surv(data$Tempo, data$Censura)~Fornecedor, data = data)
colnames(data)

fdata <- flchain[flchain$futime >=7,]
fdata$group <- factor(1+ 1*(fdata$flc.grp >7) + 1*(fdata$flc.grp >9), levels=1:3,
                      labels=c("FLC < 3.38", "3.38 - 4.71", "FLC 4.71"))
cfit <- survreg(Surv(futime, death) ~ group, data=fdata)

summary(cfit)

# tentativa 02 ----
# https://stats.stackexchange.com/questions/246812/cox-snell-residuals-in-r
# ñ encontrei onde fica a linha referencia
# plot fica muito diferente do survplot

library(survival)
library(rms)
lungCC <- lung[complete.cases(lung[,c("age","sex","ph.karno")]),] ## instead of using na.action
psmE <- psm(Surv(time,status)~age+sex+ph.karno,dist="exponential",data=lungCC)
residE <- residuals(psmE)
psmW <- psm(Surv(time,status)~age+sex+ph.karno,dist="weibull",data=lungCC)
residW <- residuals(psmW)
psmLN <- psm(Surv(time,status)~age+sex+ph.karno,dist="lognormal",data=lungCC)
residLN <- residuals(psmLN)
psmLL <- psm(Surv(time,status)~age+sex+ph.karno,dist="loglogistic",data=lungCC)
residLL <- residuals(psmLL)
# par(mfrow=c(2,2))
# survplot(residE,main="Exponential",ylab="Complement of residual CDF")
# survplot(residW,main="Weibull",ylab="Complement of residual CDF")
# survplot(residLN,main="Lognormal",ylab="Complement of residual CDF")
# survplot(residLL,main="Log Logistic",ylab="Complement of residual CDF")
plot(residE,main="Exponential",ylab="Complement of residual CDF")
survplot(residE,main="Exponential",ylab="Complement of residual CDF")
print.AsIs(residE)
plot(residE)
curve(dexp, from = 0, to = 1, add = TRUE)
lines(qnorm())
# plot(residW,main="Weibull",ylab="Complement of residual CDF")
# plot(residLN,main="Lognormal",ylab="Complement of residual CDF")
# plot(residLL,main="Log Logistic",ylab="Complement of residual CDF")

# tentativa 03 ----
# https://search.r-project.org/CRAN/refmans/flexsurv/html/coxsnell_flexsurvreg.html

library(flexsurv)
fitg <- flexsurvreg(formula = Surv(futime, fustat) ~ age, data = ovarian, dist = "gengamma")
cs <- coxsnell_flexsurvreg(fitg)

## Model appears to fit well, with some small sample noise 
surv <- survfit(Surv(cs$est, ovarian$fustat) ~ 1)
plot(surv, fun="cumhaz")
abline(0, 1, col="red")

# tentativa 04 ----

rm(list=ls(all=TRUE))    

desmame<-read.table("https://docs.ufpr.br/~giolo/Livro/ApendiceA/desmame.txt",h=T)  
attach(desmame)
require(survival)
ekm<- survfit(Surv(tempo,cens)~V4)
summary(ekm)
survdiff(Surv(tempo,cens)~V4, rho=0)
plot(ekm, lty=c(1,4), mark.time=F, xlab="Tempo até o desmame (meses)", ylab="S(t)")
text(18.5, 0.93, c("Dificuldades para Amamentar"), bty="n", cex=0.85)
legend(15.5, 0.9, lty=c(4), c("Sim"), bty="n", cex=0.8)
legend(18.5, 0.9, lty=c(1), c("Não"), bty="n", cex=0.8)

ajust1<-survreg(Surv(tempo,cens)~V1+V3+V4+V6, dist='lognorm')
ajust1
summary(ajust1)

xb<-ajust1$coefficients[1] + ajust1$coefficients[2]*V1 + ajust1$coefficients[3]*V3 +
  ajust1$coefficients[4]*V4 + ajust1$coefficients[5]*V6
sigma<-ajust1$scale
res<-(log(tempo)-(xb))/sigma                   # resíduos padronizados
resid<-exp(res)                                # exponencial dos resíduos padronizados
ekm<- survfit(Surv(resid,cens)~1)
resid<-ekm$time
sln<-pnorm(-log(resid))
par(mfrow=c(1,2))
plot(ekm$surv,sln, xlab="S(ei*): Kaplan-Meier", ylab="S(ei*): Log-normal padrão", pch=16)
plot(ekm, conf.int=F, mark.time=F, xlab="Resíduos (ei*)", ylab="Sobrevivência estimada", pch=16)
lines(resid, sln, lty=2)
legend(1.3,0.8, lty=c(1,2), c("Kaplan-Meier","Log-normal padrão"), cex=0.8, bty="n")

ei<- -log(1-pnorm(res))                          # resíduos de Cox-Snell
ekm1<-survfit(Surv(ei,cens)~1)
t<-ekm1$time
st<-ekm1$surv
sexp<-exp(-t)
par(mfrow=c(1,2))
plot(st, sexp, xlab="S(ei): Kaplan-Meier", ylab="S(ei): Exponencial padrão", pch=16)
plot(ekm1, conf.int=F, mark.time=F, xlab="Resíduos de Cox-Snell", ylab="Sobrevivência estimada")
lines(t, sexp, lty=4)
legend(1.0, 0.8, lty=c(1,4), c("Kaplan-Meier","Exponencial padrão"), cex=0.8, bty="n")

# tentativa 05 ----
# https://tbrieder.org/epidata/course_reading/e_tableman.pdf
# nao achei a tabela cns2
attach(cns2)
rc <- abs(STATUS - cns2.coxint6$residuals) # Cox-Snell
# residuals!
km.rc <- survfit(Surv(rc,STATUS) ~ 1)
summary.km.rc <- summary(km.rc)
rcu <- summary.km.rc$time # Cox-Snell residuals of
# uncensored points.
surv.rc <- summary.km.rc$surv
plot(rcu,-log(surv.rc),type="p",pch=".",
     xlab="Cox-Snell residual rc",ylab="Cumulative hazard on rc")
abline(a=0,b=1); abline(v=0); abline(h=0)
