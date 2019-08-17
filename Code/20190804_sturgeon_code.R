## Brett van Poorten and Bill Pine
## Sturgeon Code.R
## January 2, 2019
### R code reproduces Bill's SturMod 2.0 Excel file

### adjustments based on recommended edits by Ed Camp and Brett incorporated January 29, 2019

library(ggplot2)
library(reshape)

###------------------------------------------------------###
###--------------------- Parameters ---------------------###
###------------------------------------------------------###

n.st <- 2  # number of pre-recruit stanzas [either 1 (hatchery fish are stocked 
#as eyed eggs) or 2 (hatchery fish are stocked sometime later so wild fish 
#have a period without competition with hatchery fish)]
### NOTE: if n.st==1, you assume fish are stocked after all density dependence is complete;
###       if n.st==2, you assume fish are stocked in the middle of density dependence (so they impact abundance)
AR <- 1  # age at recruitment (end of density-dependent mortality)
A <- 50  # oldest modelled age-class
FIT <- FALSE   # set to true to estimate R0 and second stage U
T1 <- 20       # time starts in 1901; T1 is the year when intense fishing stops (1920)
T2 <- 47       # T2 is the year the dam building began (1947; coffer dams put in; access to habitat lost)
T3 <- 84       # T3 is the year all fishing stopped (1983)
T4 <- 123      # T4 is recovery year period
Ttot <- 185    # Ttot is the total number of years evaluated (until 2085)
T82 <- 83      # T82 is year 1983 first year Zehfuss et al. open estimates at JWLD
T83 <- 84      # T83 is year 1984
T84 <- 85      # T85 is year 1985
T85 <- 86      # T86 is year 1986
T86 <- 87      # T87 is year 1987
T87 <- 88      # T88 is year 1988
T88 <- 89      # T89 is year 1989
T89 <- 90      # T90 is year 1990
T90 <- 91      # T91 is year 1991
T92 <- 93      # T92 is year 1993 and is from USFWS as in Pine and Martel
T98 <- 99      # T98 is year 1999 and is from USFWS as in Pine and Martel
T2003 <- 104    # T2003 is year 2004 and is from USFWS as in Pine and Martel
input <- list()

# Growth
# wild population
input$Linf <- 220       # von Bertalanffy asymptotic length
input$K <- 0.13         # von Bertalanffy 'metabolic' parameter
input$to <- 0           # von Bertalanffy time at zero-length (x-intercept)
input$a <- 6.11e-6      # length-weight scalar
input$b <- 3            # length-weight power parameter

# stocked population
input$Linf.s <- 220
input$K.s <- 0.13
input$to.s <- 0
input$a.s <- 6.11e-6
input$b.s <- 3

# Spawning
# wild population
input$Skip <- TRUE                # skip-spawning? (TRUE/FALSE)
input$Nsk <- 1 # NoSkipInt        # interval between spawning events
input$mai <- 8 # 1st mat          # initial maturation age
input$mah <- 10 # age50matfe      # age at 50% maturity
input$mant <- 5 # yearsofmat      # number of years needed for the entire population to mature
input$mxnt <- 4 # max int         # average skip-spawning interval

# stocked population
input$Skip.s <- TRUE              # skip-spawning? (TRUE/FALSE) 
input$Nsk.s <- 1 # NoSkipInt2     # interval between spawning events
input$mai.s <- 8 # 1st mat2       # initial maturity age
input$mah.s <- 10 # age50matfe2   # age at 50% maturity
input$mant.s <- 5 # yearsofmat2   # number of years needed for entire population to mature 
input$mxnt.s <-4 # max int2       # average skip-spawning interval

# Mortality / Survival
input$M <- .12  #(not used)
input$relmort <- 0.95 # S  #(not used)
input$Mad <- 0.095                # minimum adult mortality (at Linf)
input$Mad.rec <- 0.095            # minimum adult mortality during recovery period
input$u <- 1                      # exploitation rate stage 1
input$bu <- 0.089                 # exploitation rate stage 2
input$bu2 <- 0.0                  # exploitation rate stage 3
input$dome <- TRUE                # dome-shaped exploitation rate? (TRUE/FALSE)
input$lvuln <- 45                 # Length-at-50% vulnerability for ascending curve
input$lvulnsd <- 10               # standard deviation for ascending curve
input$hvuln <- 200                # Length-at-50% vulnerability for descending curve
input$hvulnsd <- 10               # standard deviation for descending curve

# stocked population
input$M.s <- .12  #(not used)
input$S.s <- 0.95  #(not used)
input$Mad.s <- 0.1                # minimum adult mortality (at Linf)
input$Mad.s.rec <- 0.1            # minumum adult mortality during recovery
input$lvuln.s <- 7                # Length-at-50% vulnerability for ascending curve
input$lvulnsd.s <- 10             # standard deviation for ascending curve
input$hvuln.s <- 45               # Length-at-50% vulnerability for descending curve
input$hvulnsd.s <- 10             # standard deviation for descending curve

# Population
input$Ro <- c(9970,2605)          # unfished recruitment (before and after dam)
input$recK <- 5                   # recruitment compensation ratio
input$SXR <- 100                  # initial percent of wild recruitment
input$fid <- 0.5                  # Fidelity to stocked population

# Recruitment modifiers
input$aint <- 2                   # recruitment interval
input$astr <- 1                   # relative strength of strong years
ifelse(input$aint==1,{
  input$astr <- 1
  input$Nstr <- 1
},
input$Nstr <- (input$aint-input$astr)/(input$aint-1))  # Normal strength
input$Cstr <- 1                   # Egg prod divider
input$stock <- 500                # Number stocked (if stocking allowed)
input$Ms <- c(1,1)                # relative mortality of each pre-recruit stanza relative to each other (length must equal N.st)
input$Bs <- c(0.5,0.5)            # relative density effect of each pre-recruit stanza relative to each other (length must equal N.st)

DF <-data.frame(State = c("Unexploited",rep("|",T3-1),"End of Harvest",1:100),
                Year = c(0:(T3),1985:2084),
                Anomoly = c(rep(c(rep(input$Nstr,input$aint-1),input$astr),length=Ttot)),
                Stock = c(rep(FALSE,120),rep(TRUE,20),rep(FALSE,45)),
                U = c(rep(input$u,T1),rep(input$bu,T3-T1),rep(input$bu2,Ttot-T3)),
                Dam =c(rep(1,T2),rep(2,Ttot-T2)),
                stringsAsFactors = FALSE)

###------------------------------------------------------###
###--------------------- Functions ----------------------###
###------------------------------------------------------###

# function that calculates all initial values and vectors
"Initialization" <- function(input){  
  # Note: objects ending in '.s' refer to stocked components
  # weight at maturity
  Wmat <- input$a*(input$Linf*(1-exp(-input$K*(input$mai-1))))^input$b
  Wmat.s <- Wmat
  
  Age <- AR:A
  # number of age-classes
  n.age <- length(Age)
  # length-at-age
  Length <- input$Linf * (1 - exp(-input$K*(Age-input$to)))
  # weight-at-age
  Wt <- input$a*Length^input$b
  # fecundity-at-age
  Fec <- pmax(0,Wt-Wmat)
  # skip spawning frequency (maybe?)
  Sks <- rep(0,n.age)
  ifelse( input$Skip,
          Sks <- pmax(0,(((1/input$mant)/exp(-1))/(input$mah-input$mai))*
                        (Age-input$mai)*exp(-(Age-input$mai)/(input$mah-input$mai))+
                        (1/input$mxnt)^(input$Linf/Length)),
          Sks <- 1/input$Nsk)
  # fecundity-at-age adjusted for skip spawning
  AdjFec <- Fec*Sks
  # Lorenzen-type length-based mortality
  Mu <- rep(0,n.age)
  Mu <- input$Mad*(input$Linf/Length)
  # length-based vulnerability (either logistic or dome-shaped double logistic)
  vuln <- rep(0,n.age)
  ifelse( !input$dome,
          vuln <- 1/(1+exp(-(c(0,Length)-input$lvuln)/input$lvulnsd)),
          vuln <- 1/(1+exp(-(c(0,Length)-input$lvuln)/input$lvulnsd))-
            1/(1+exp(-(c(0,Length)-input$hvuln)/input$hvulnsd)))
  # survivorship
  La <- rep(0,n.age)
  La[1] <- 1
  LaBurn <- Laf <- La
  for(i in 2:A){
    La[i] <- La[i-1]*exp(-Mu[i-1])  # survivorship absent fishing
    LaBurn[i] <- LaBurn[i-1]*(1-input$bu*vuln[i-1])*exp(-Mu[i-1]) # survivorship during burn-in
    Laf[i] <- Laf[i-1]*(1-input$u*vuln[i-1])*exp(-Mu[i-1])  # survivorship during fishing
  }
  
  # all the same vectors as above, but for stocked fish
  Length.s <- input$Linf.s * (1 - exp(-input$K.s*(Age-input$to.s)))
  Wt.s <- input$a.s*Length.s^input$b.s
  Fec.s <- pmax(0,Wt.s-Wmat.s)
  Sks.s <- rep(0,n.age)
  ifelse( input$Skip.s,
          Sks.s <- pmax(0,(((1/input$mant.s)/exp(-1))/(input$mah.s-input$mai.s))*
                          (Age-input$mai.s)*exp(-(Age-input$mai.s)/(input$mah.s-input$mai.s))+
                          (1/input$mxnt.s)^(input$Linf.s/Length.s)),
          Sks.s <- 1/input$Nsk.s)
  AdjFec.s <- Fec.s*Sks.s
  Mu.s <- rep(0,n.age)
  Mu.s <- input$Mad.s*(input$Linf.s/Length.s)
  vuln.s <- rep(0,n.age)
  ifelse( !input$dome,
          vuln.s <- 1/(1+exp(-(c(0,Length.s)-input$lvuln.s)/input$lvulnsd.s)),
          vuln.s <- 1/(1+exp(-(c(0,Length.s)-input$lvuln.s)/input$lvulnsd.s))-
            1/(1+exp(-(c(0,Length.s)-input$hvuln.s)/input$hvulnsd.s)))
  La.s <- rep(0,n.age)
  La.s[1] <- 1
  LaBurn.s <- Laf.s <- La.s
  for(i in 2:A){
    La.s[i] <- La.s[i-1]*exp(-Mu.s[i-1])
    LaBurn.s[i] <- LaBurn.s[i-1]*(1-input$bu*vuln.s[i-1])*exp(-Mu.s[i-1])
    Laf.s[i] <- Laf.s[i-1]*(1-input$u*vuln.s[i-1])*exp(-Mu.s[i-1])
  }
  out <- list()
  out$Age <- Age
  out$Length <- Length
  out$Wt <- Wt
  out$Fec <- Fec
  out$Sks <- Sks
  out$AdjFec <- AdjFec
  out$Mu <- Mu
  out$vuln <- vuln
  out$La <- La
  out$LaBurn <- LaBurn
  out$Laf <- Laf
  out$Length.s <- Length.s
  out$Wt.s <- Wt.s
  out$Fec.s <- Fec.s
  out$Sks.s <- Sks.s
  out$AdjFec.s <- AdjFec.s
  out$Mu.s <- Mu.s
  out$vuln.s <- vuln.s
  out$La.s <- La.s
  out$LaBurn.s <- LaBurn.s
  out$Laf.s <- Laf.s
  return(out)
}

### PRELIMINARY CALCULATIONS
"Prelim.Calc" <- function(input,dam.opt){
  init <- Initialization(input)
  
  # equilibrium unfished eggs per recruit
  EPRo <- sum(init$AdjFec*init$La)
  
  # unfished egg abundance
  ifelse(input$SXR==100,
         Eo <- input$Ro[dam.opt]*EPRo/input$Cstr,
         Eo <- (input$Ro[dam.opt]/2*EPRo)/input$Cstr)
  
  # fished eggs per recruit
  EpRf <- sum(init$AdjFec*init$Laf)
  
  # Beverton-Holt alpha parameter (maximum recruits per spawner)
  reca <- input$recK/EPRo
  reca.st <- vector()  # alpha for each pre-recruit stanza
  for(i in 1:n.st){
    reca.st[i] <- exp(log(reca)*input$Ms[i]/sum(input$Ms[1:n.st]))
  }
  
  # Beverton-Holt beta parameter (carrying capacity parameter)
  recb <- (input$recK-1)/(Eo)
  recb.st <- vector()  # beta for each pre-recruit stanza
  den <- vector()
  tmp.reca.st <- c(1,reca.st)
  for(i in 1:n.st)
    den[i] <- input$Bs[i]*prod(tmp.reca.st[1:i])
  recb.st <- input$Bs[1:n.st]*recb/sum(den[1:n.st])
  
  # effective egg abundance
  EfecEo <- max(0,(reca*EpRf-1)/(recb*EpRf))
  
  # unfished eggs per recruit for stocked fish
  EPRo.s <- sum(init$AdjFec.s*init$La.s)
  
  # unfished egg abundance for stocked fish
  ifelse(input$SXR==100,
         Eo.s <- input$Ro[dam.opt]*EPRo.s,
         Eo.s <- (input$Ro[dam.opt]/2*EPRo.s))
  
  # fished eggs per recruit for stocked fish
  EpRf.s <- sum(init$AdjFec.s*init$Laf.s)
  
  # Beverton-Holt alpha parameter for stocked fish (maximum recruits per spawner)
  reca.s <- input$recK/EPRo.s
  
  # Beverton-Holt beta parameter for stocked fish (carrying capacity parameter)
  recb.s <- (input$recK-1)/Eo.s
  
  # effective egg abundance for stocked fish
  EfecEo.s <- max(0,(reca.s*EpRf.s-1)/(recb.s*EpRf.s))
  
  # unfished abundance
  N0 <- sum(input$Ro[dam.opt]*init$La)
  
  out<-list()
  out$EPRo <- EPRo
  out$Eo <- Eo
  out$EpRf <- EpRf
  out$reca <- reca
  out$recb <- recb
  out$reca.st <- reca.st
  out$recb.st <- recb.st
  out$EfecEo <- EfecEo
  out$EPRo.s <- EPRo.s
  out$Eo.s <- Eo.s
  out$EpRf.s <- EpRf.s
  out$reca.s <- reca.s
  out$recb.s <- recb.s
  out$EfecEo.s <- EfecEo.s
  out$N0 <- N0
  return(out)
}

# Use brute-force to calculate MSY and UMSY
"MSY" <- function(par.input=input,vals=DF){
  U.opt <- seq(0,0.5,length=500)                # evaluate over a range of exploitation rates
  VB <- VN <- YB <- YN <- Rf <- matrix(nrow=500,ncol=2)    # set up matrices of vulnerable biomass, vulnerable numbers and yield across U and dam (before and after)
  init <- Initialization(par.input)
  EPRo <- sum(init$AdjFec*init$La)             # unfished eggs per recruit
  for(dam in 1:2){                             # evaluate without (1) and with (2) dam
    for(i in 1:500){
      par.input$u <- U.opt[i]
      init <- Initialization(par.input)
      EpRf <- sum(init$AdjFec*init$Laf)        # fished eggs per recruit at U=U.opt[i]
      # calculate equilibrium fished recruits
      Rf[i,dam] <- par.input$Ro[dam]*EPRo/EpRf*(par.input$recK*EpRf/EPRo-1)/(par.input$recK-1)
      VB[i,dam] <- Rf[i,dam]*sum(init$Wt*init$vuln[2:(n.age+1)]*init$Laf)  # vulnerable biomass
      VN[i,dam] <- Rf[i,dam]*sum(init$vuln[2:(n.age+1)]*init$Laf)          # vulnerable numbers of fish 
      YB[i,dam] <- sum(par.input$u*VB[i,dam])       # yield in biomass
      YN[i,dam] <- sum(par.input$u*VN[i,dam])       # yield in biomass
    }
  }
  out <- list()
  out$YB <- YB
  out$YN <- YN
  out$U <- U.opt
  out$VB <- VB
  out$VN <- VN
  out$Rf <- Rf
  return(out)
}


### POPULATION ABUNDANCE TABLES
"Dynamic.Mod" <- function(par.input=input,vals=DF){
  init <- Initialization(par.input)
  
  Age <- init$Age
  Length <- init$Length
  Wt <- init$Wt
  Fec <- init$Fec
  Sks <- init$Sks
  AdjFec <- init$AdjFec
  Mu <- init$Mu
  vuln <- init$vuln
  La <- init$La
  LaBurn <- init$LaBurn
  Laf <- init$Laf
  Length.s <- init$Length.s
  Wt.s <- init$Wt.s
  Fec.s <- init$Fec.s
  Sks.s <- init$Sks.s
  AdjFec.s <- init$AdjFec.s
  Mu.s <- init$Mu.s
  vuln.s <- init$vuln.s
  La.s <- init$La.s
  LaBurn.s <- init$LaBurn.s
  Laf.s <- init$Laf.s
  Mu.rec <- par.input$Mad.rec*(input$Linf/Length)
  Mu.s.rec <- par.input$Mad.s.rec*(input$Linf/Length)
  
  yr <- vals$Year
  anom <- vals$Anomoly
  rel <- vals$Stock
  U <- vals$U
  Dam <- vals$Dam
  n.age <- length(Age)
  
  calcs <- Prelim.Calc(par.input,1)   # preliminary calculations for pre-dam condition
  EPRo <- calcs$EPRo
  Eo <- list()
  Eo[[1]] <- calcs$Eo
  EpRf <- calcs$EpRf
  reca <- calcs$reca
  recb <- list()
  recb[[1]] <- calcs$recb
  reca.st <- calcs$reca.st
  recb.st <- list()
  recb.st[[1]] <- calcs$recb.st
  EfecEo <- list()
  EfecEo[[1]] <- calcs$EfecEo
  EPRo.s <- calcs$EPRo.s
  Eo.s <- list()
  Eo.s[[1]] <- calcs$Eo.s
  EpRf.s <- calcs$EpRf.s
  reca.s <- calcs$reca.s
  recb.s <- list()
  recb.s[[1]] <- calcs$recb.s
  EfecEo.s <- list()
  EfecEo.s[[1]] <- calcs$EfecEo.s
  N0 <- calcs$N0
  
  calcs2 <- Prelim.Calc(par.input,2)   # preliminary calculations for post-dam condition
  Eo[[2]] <- calcs2$Eo
  recb[[2]] <- calcs2$recb
  recb.st[[2]] <- calcs2$recb.st
  EfecEo[[2]] <- calcs2$EfecEo
  Eo.s[[2]] <- calcs2$Eo.s
  recb.s[[2]] <- calcs2$recb.s
  EfecEo.s[[2]] <- calcs2$EfecEo.s
  
  Nt <- vector()       # abundance of wild fish per year
  Bt <- vector()       # biomass of wild fish per year
  Eggs <- vector()     # wild eggs per year
  Nt.s <- vector()     # abundance of stocked fish per year
  Bt.s <- vector()     # biomass of stocked fish per year
  Eggs.s <- vector()   # eggs of stocked fish per year
  Eggs.t <- vector()   # total eggs per year
  rat <- vector()      # ratio of stocked to wild eggs per year
  R.t <- vector()      # total recruits per year
  p.egg <- vector()    # proportion of total eggs that are wild-origin
  p.egg.s <- vector()  # proportion of total eggs that are hatchery-origin
  dyn.EPR <- rep(0,Ttot)  # dynamic eggs per recruit (used for true dynamic SPR)
  R <- matrix(nrow=n.st,ncol=Ttot)        # wild recruits per year
  R.s <- matrix(nrow=n.st,ncol=Ttot)      # stocked recruits per year
  N.w <- matrix(nrow=Ttot+1,ncol=A)  # matrix of wild fish
  N.s <- N.w                        # matrix of stocked fish
  
  # simulate equilibrium initial abundance (before dam)
  Sint <- c(rep(1,par.input$Nsk))
  N.w[1,1] <- par.input$Ro[1]*La[1]*par.input$SXR/100        # initial unfished wild recruits
  N.w[1,2:A] <- N.w[1,1]*La[2:A]                  # initial unfished wild age structure
  N.s[1,1] <- par.input$Ro[1]*La.s[1]*(1-par.input$SXR/100)  # initial unfished stocked recruits
  N.s[1,2:A] <- N.s[1,1]*La.s[2:A]                # initial unfished stocked age structure
  # simulate over years
  for(y in 1:Ttot){
    if(y >= T3){
      Mu <- Mu.rec
      Mu.s <- Mu.s.rec
    }
    Eggs[y] <- max(1e-5,sum(N.w[y,]*AdjFec))      # eggs produced from wild population
    Eggs.s[y] <- max(1e-5,sum(N.s[y,]*AdjFec.s))  # eggs produced from stocked population
    Eggs.t[y] <- Eggs[y]+Eggs.s[y]                # total egg production
    rat[y] <- Eggs.s[y]/Eggs[y]                   # ratio of stocked to wild eggs
    R.t[y] <- max(0,reca.st[1]*Eggs.t[y]/(1+recb.st[[Dam[y]]][1]*Eggs.t[y]))*anom[y]  # Total recruitment to first stanza (includes anomoly)
    p.egg[y] <- Eggs[y]/Eggs.t[y]                 # proportion of eggs that are wild
    p.egg.s[y] <- Eggs.s[y]/Eggs.t[y]             # proportion of eggs that are stocked
    R[1,y] <- R.t[y]*p.egg[y]+R.t[y]*p.egg.s[y]*(1-par.input$fid)  # wild recruits to stanza 1 (accounts for stocked eggs 'naturalizing')
    R.s[1,y] <- R.t[y]*p.egg.s[y]*par.input$fid       # stocked recruits to stanza 1
    ifelse(n.st==1,                               # if there is only 1 recruitment stanza
           N.w[y+1,1] <- R[1,y],                       # age-1 fish are equal to calculated recruits
           {                                           # if there are 2 recruitment stanzas (where stocked fish are now added)
             tot.R <- R[1,y]+R.s[1,y]+par.input$stock*rel[y]           # competition for resources includes all fish (including stocked)
             N.w[y+1,1] <- reca.st[2]*R[1,y]/(1+recb.st[[Dam[y]]][2]*tot.R)  # wild recruits to population are negatively impacted by competition from all
           })
    for(a in 1:A){
      dyn.EPR[y] <- dyn.EPR[y]+(N.w[y,a]*AdjFec[a]+N.s[y,a]*AdjFec.s[a])/(N.w[max(y-a+1,1),1]+N.s[max(y-a+1,1),1])
    }
    N.w[y+1,2:A] <- pmax(0,N.w[y,1:(A-1)]*(1-U[y]*vuln[1:(A-1)])*exp(-Mu[1:(A-1)])) # wild abundance of older fish is impacted by length-specific natural mortality and exploitation
    ifelse(n.st==1,                                      # if there is only 1 recruitment stanza
           N.s[y+1,1] <- R.s[1,y]+rel[y]*par.input$stock,     # age-1 stocked fish are equal to calculated recruits
           {                                              # if there are 2 recruitment stanzas (where stocked fish are now added)
             tot.R <- R[1,y]+R.s[1,y]+rel[y]*par.input$stock  # competition for resources includes all fish (including stocked)
             R.s.tmp <- R.s[1,y]+rel[y]*par.input$stock       # hatchery fish...
             N.s[y+1,1] <- reca.st[2]*R.s.tmp/(1+recb.st[[Dam[y]]][2]*tot.R)})  # hatchery recruits to population are negatively impacted by competition from all
    N.s[y+1,2:A] <- pmax(0,N.s[y,1:(A-1)]*(1-U[y]*vuln.s[1:(A-1)])*exp(-Mu.s[1:(A-1)]))  # hatchery recruits to population are negatively impacted by length-specific natural mortality and exploitation
  }
  # Annual summaries
  Nt <- pmax(0,rowSums(N.w))   # total wild abundance per year
  Bt <- rowSums(sweep(N.w,2,Wt,'*'))  # total wild biomass per year
  Nt.s <- pmax(0,rowSums(N.s))  # total stocked abundance per year
  Bt.s <- rowSums(sweep(N.s,2,Wt.s,'*'))  # total stocked biomass per year
  N.sum <- Nt+Nt.s  # total abundance per year
  B.sum <- Bt+Bt.s  # total biomass per year
  vuln.N <- rowSums(sweep(N.w,2,vuln[AR:A],'*')+sweep(N.s,2,vuln.s[AR:A],'*'))  # vulnerable abundance per year
  a4N <- rowSums(N.w[,4:A])
  a2N <- rowSums(N.w[,2:A])
  vuln.B <- rowSums(sweep(N.w,2,(vuln[AR:A]*Wt),'*')+sweep(N.s,2,(vuln.s[AR:A]*Wt.s),'*'))  # vulnerable biomass per year
  av.vuln.wt <- vuln.B/vuln.N  # average vulnerable weight per year
  N.harv <- U*vuln.N[1:Ttot]           # numbers harvested per year
  B.harv <- U*vuln.B[1:Ttot]           # biomass harvested per year
  p.w <- Nt / (Nt + Nt.s)              # proportion wild
  p.s <- Nt.s / (Nt + Nt.s)            # proportion stocked
  p.orig.N <- Nt/Nt[1]                 # abundance relative to unfished
  p.orig.B <- Bt/Bt[1]                 # biomass relative to unfished
  p.orig.vulnN <- vuln.N/vuln.N[1]
  dyn.SPR <- Eggs.t/Eggs.t[1]          # eggs relative to unfished (not really SPR)
  dyn.SPR2 <- dyn.EPR/EPRo             # true dynamic SPR
  
  out <- list()
  out$Nt <- Nt
  out$Bt <- Bt
  out$Eggs <- Eggs
  out$Nt.s <- Nt.s
  out$Bt.s <- Bt.s
  out$Eggs.s <- Eggs.s
  out$Eggs.t <- Eggs.t
  out$N.sum <- N.sum
  out$a4N <- a4N
  out$B.sum <- B.sum
  out$vuln.N <- vuln.N
  out$vuln.B <- vuln.B
  out$av.vuln.wt <- av.vuln.wt
  out$N.harv <- N.harv
  out$B.harv <- B.harv
  out$p.w <- p.w
  out$p.s <- p.s
  out$p.orig.N <- p.orig.N
  out$p.orig.B <- p.orig.B
  out$dyn.SPR <- dyn.SPR
  out$dyn.SPR2 <- dyn.SPR2
  out$rat <- rat
  out$R.t <- R.t
  out$p.egg <- p.egg
  out$p.egg.s <- p.egg.s
  out$R <- R
  out$R.s <- R.s
  out$N.w <- N.w
  out$N.s <- N.s
  out$pN02023 <- p.orig.N[T4]
  out$prec <- a4N[T3]/4195
  out$Nclose <- a2N[T3]
  return(out)
}

###------------------------------------------------------###
###-------------------- Initialize ----------------------###
###------------------------------------------------------###

theta <- c(log(0.5/(1-0.5)),log(0.12/(1-0.12)))  # estimate two stages of exploitation

fr <- function(theta){
  trial.inp <- input
  trial.DF <- DF
  trial.DF$Stock <- rep(FALSE,Ttot)
  trial.DF$U[1:T1] <- 1/(1+exp(-theta[1]))
  trial.DF$U[(T1+1):T3] <- 1/(1+exp(-theta[2]))
  trial.inp$Ro <- c(14452,3770)#c(9970,2605)
  dyn <- Dynamic.Mod(trial.inp,trial.DF)
  SS <- vector()
  # fit to unfished abundance
  SS[1] <- (log(dyn$Nclose)-log(282))^2  # fit to 1985 abundance
  SS[2] <- (log(dyn$a4N[109])-log(2000))^2 # fit to 2009 abundance
  
  return(sum(SS))
}

if(FIT){
  est<-optim(theta,fr,method="BFGS",control=list(maxit=1000))
}

###------------------------------------------------------###
###---------------------- Figures -----------------------###
###------------------------------------------------------###

# inputs are different starting values (lower, upper 95% CI) 
#    for unfished recruitment (R0), stage-1 (yrs 1-12) and stage-2 exploitation rate (U for years 13-24)
#    Baseline estimates are based on age-4 fish at unfished (with a dam: Ahrens and Pine 2013): 4,195 (CI 1,932-6,066),
#    age-4 in 1985 (Wooley and Crateau 1985): 282, and recent estimates (Pine and Martell 2009): 2000
#L95.R0=c(4625,1200),mu.R0=c(9970,2605),U95.R0=c(14452,3770),L95.U1=1,mu.U1=1,U95.U1=1,L95.U2=0.094,mu.U2=0.089,U95.U2=0.077
"Scenario1" <- function(L95.R0=c(4625,1200),mu.R0=c(9970,2605),U95.R0=c(14452,3770),L95.U1=1,mu.U1=1,U95.U1=1,L95.U2=0.094,mu.U2=0.089,U95.U2=0.077){
  base.inp <- input
  base.DF <- DF
  base.DF$Stock <- rep(FALSE,Ttot)
  base.inp$Ro <- mu.R0
  base.DF$U = c(rep(mu.U1,T1),rep(mu.U2,T3-T1),rep(input$bu2,Ttot-T3))
  mid.est <- Dynamic.Mod(base.inp,base.DF)
  base.inp$Ro <- L95.R0
  base.DF$U = c(rep(L95.U1,T1),rep(L95.U2,T3-T1),rep(input$bu2,Ttot-T3))
  low.est <- Dynamic.Mod(base.inp,base.DF)
  base.inp$Ro <- U95.R0
  base.DF$U = c(rep(U95.U1,T1),rep(U95.U2,T3-T1),rep(input$bu2,Ttot-T3))
  upp.est <- Dynamic.Mod(base.inp,base.DF)
  cat("Median SPR: ",mid.est$dyn.SPR2[T4],"\n")
  cat("CI: ",low.est$dyn.SPR2[T4],", ",upp.est$dyn.SPR2[T4],"\n")
  plot(upp.est$N.sum[1:Ttot],t="n",xaxt="n",yaxt="n",ylab="Thousands of sturgeon",
       xlab="Year",xaxs="i",font.lab=2,yaxs="i",ylim=c(-1,50000))
  polygon(x=c(1:Ttot,rev(1:Ttot)),y=c(low.est$N.sum[1:Ttot],rev(upp.est$N.sum[1:Ttot])),
          col="grey",border="blue",lty=2)
  lines(mid.est$N.sum[1:Ttot],lty=1,lwd=2)
  abline(v=T2+1,col="purple",lty=2,lwd=2)
  abline(v=T3,col="red",lty=2,lwd=2)
  abline(v=T4,col="green",lty=2,lwd=2)
  abline(h=8784,col="brown",lwd=2)
  axis(1,at=seq(0,Ttot,20),seq(1900,2085,20))
  axis(1,at=seq(0,Ttot,5),labels=FALSE,lwd=0.5)
  axis(2,at=seq(0,100000,10000),seq(0,100,10))
  axis(2,at=seq(0,100000,5000),labels=FALSE,lwd=0.5)
  # put Wooley and Crateau 1985 estimate on the figure
  points(T3,282,pch=19,col="purple")
  points(T82,111,pch=17,col="purple")
  points(T83,119,pch=17,col="purple")
  points(T84,117,pch=17,col="purple")
  points(T85,108,pch=17,col="purple")
  points(T86,103,pch=17,col="purple")
  points(T87,88,pch=17,col="purple")
  points(T88,89,pch=17,col="purple")
  points(T89,90,pch=17,col="purple")
  points(T90,117,pch=17,col="purple")
  points(T92,155,pch=17,col="purple")
  points(T98,260,pch=17,col="purple")
  points(T2003,350,pch=17,col="purple")
  lines(rep(T3,2),c(181,645),col="purple")
  points(109,2000/(mid.est$a4N[109]/mid.est$N.sum[109]),pch=19,col="purple")
  legend("topright",lty=c(1,2,1),legend=c("Mean","95%  limits","Carrying capacity"),
         bty="n",col=c("black","blue","brown"),lwd=2,cex=0.9)
}
#pdf("Figure 1.pdf",width=11,height=6)
Scenario1()
#dev.off()

# inputs include baseline recruitment (R0), baseline 2nd stage exploitation (U.base)
#   and then proportional change in 1st and 2nd exploitation rates to achieve 
#   50, 75, 95 and 99% unfished abundance (N0) by 2023
Scenario2 <- function(mu.R0=c(9970,2605),U.base=0.089,p.50=0.985,p.75=0.83,p.95=0.56,p.99=0.5){
  base.inp <- input
  base.DF <- DF
  base.DF$Stock <- rep(FALSE,Ttot)
  base.inp$Ro <- mu.R0
  base.DF$U = c(rep(input$u,T1),rep(U.base,T3-T1),rep(input$bu2,Ttot-T3))
  baseline.est <- Dynamic.Mod(base.inp,base.DF)   # base model run
  base.DF$U = base.DF$U * p.50
  est.50 <- Dynamic.Mod(base.inp,base.DF)   # model run ending with 50% N0 by 2023
  base.DF$U = base.DF$U * p.75
  est.75 <- Dynamic.Mod(base.inp,base.DF)   # model run ending with 75% N0 by 2023
  base.DF$U = base.DF$U * p.95
  est.95 <- Dynamic.Mod(base.inp,base.DF)   # model run ending with 95% N0 by 2023
  base.DF$U = base.DF$U * p.99
  est.99 <- Dynamic.Mod(base.inp,base.DF)   # model run ending with 99% N0 by 2023
  cat("To attain 50% carrying capacity, N would have to have been ",est.50$N.sum[T3]/baseline.est$N.sum[T3]," higher in 1984\n")
  cat("To attain 75% carrying capacity, N would have to have been ",est.75$N.sum[T3]/baseline.est$N.sum[T3]," higher in 1984\n")
  cat("To attain 95% carrying capacity, N would have to have been ",est.95$N.sum[T3]/baseline.est$N.sum[T3]," higher in 1984\n")
  cat("To attain 99% carrying capacity, N would have to have been ",est.99$N.sum[T3]/baseline.est$N.sum[T3]," higher in 1984\n")
  cat("Baseline SPR: ",baseline.est$dyn.SPR2[T4],"\n")
  cat("50% N0 SPR: ",est.50$dyn.SPR2[T4],"\n")
  cat("75% N0 SPR: ",est.75$dyn.SPR2[T4],"\n")
  cat("95% N0 SPR: ",est.95$dyn.SPR2[T4],"\n")
  cat("99% N0 SPR: ",est.99$dyn.SPR2[T4],"\n")
  plot(baseline.est$N.sum[1:Ttot],t="l",lwd=2,xaxt="n",yaxt="n",ylim=c(-1,50000),
       ylab="Thousands of sturgeon",xlab="Year",xaxs="i",font.lab=2,yaxs="i")
  lines(est.50$N.sum[1:Ttot],col="blue",lwd=2)
  lines(est.75$N.sum[1:Ttot],col="grey",lwd=2)
  lines(est.95$N.sum[1:Ttot],col="darkgreen",lwd=2)
  lines(est.99$N.sum[1:Ttot],col="orange",lwd=2)
  lines(baseline.est$N.sum[1:Ttot],col="black",lwd=2)
  axis(1,at=seq(0,Ttot,20),seq(1900,2085,20))
  axis(1,at=seq(0,Ttot,5),labels=FALSE,lwd=0.5)
  axis(2,at=seq(0,100000,10000),seq(0,100,10))
  axis(2,at=seq(0,100000,5000),labels=FALSE,lwd=0.5)
  abline(v=T2+1,col="purple",lty=2,lwd=2)
  abline(v=T3,col="red",lty=2,lwd=2)
  abline(v=T4,col="green",lty=2,lwd=2)
  abline(h=8784,col="brown",lwd=2)
  legend("topright",lty=1,legend=c("Baseline","50% recovered","75% recovered",
                                   "95% recovered","99% recovered","Carrying capacity"),
         bty="n",col=c("black","blue","grey","darkgreen","orange","brown"),lwd=2,cex=0.9)
}
#pdf("Figure 2.pdf",width=11,height=6)
#Scenario2()
#dev.off()

"Scenario3" <- function(mu.R0=c(9970,2605),U.base=0.089){
  base.inp <- input
  base.DF <- DF
  base.DF$Stock <- rep(FALSE,Ttot)
  base.inp$Ro <- mu.R0
  base.DF$U = c(rep(input$u,T1),rep(U.base,T3-T1),rep(input$bu2,Ttot-T3))
  baseline.est <- Dynamic.Mod(base.inp,base.DF)   # base model run
  base.DF$U = c(rep(input$u,T1),rep(U.base,T3-T1),rep(0.01,Ttot-T3))
  U01.est <- Dynamic.Mod(base.inp,base.DF)   # additional exploitation pressure after 'closure'
  base.DF$U = c(rep(input$u,T1),rep(U.base,T3-T1),rep(0.05,Ttot-T3))
  U05.est <- Dynamic.Mod(base.inp,base.DF)   # additional exploitation pressure after 'closure'
  base.DF$U = c(rep(input$u,T1),rep(U.base,T3-T1),rep(0.1,Ttot-T3))
  U1.est <- Dynamic.Mod(base.inp,base.DF)   # additional exploitation pressure after 'closure'
  base.DF$U = c(rep(input$u,T1),rep(U.base,T3-T1),rep(input$bu2,Ttot-T3))
  base.inp$Mad.rec <- 0.145
  M145.est <- Dynamic.Mod(base.inp,base.DF)   # additional exploitation pressure after 'closure'
  base.inp$Mad.rec <- 0.12
  M12.est <- Dynamic.Mod(base.inp,base.DF)   # additional exploitation pressure after 'closure'
  base.inp$Mad.rec <- 0.105
  M105.est <- Dynamic.Mod(base.inp,base.DF)   # additional exploitation pressure after 'closure'
  base.inp$Mad.rec <- 0.085
  M085.est <- Dynamic.Mod(base.inp,base.DF)   # additional exploitation pressure after 'closure'
  base.inp$Mad.rec <- 0.07
  M07.est <- Dynamic.Mod(base.inp,base.DF)   # additional exploitation pressure after 'closure'
  cat("U=0.01 SPR: ",baseline.est$dyn.SPR2[T4],"\n")
  cat("U=0.5 SPR: ",U01.est$dyn.SPR2[T4],"\n")
  cat("U=1 SPR: ",U1.est$dyn.SPR2[T4],"\n")
  cat("M=0.145 SPR: ",M145.est$dyn.SPR2[T4],"\n")
  cat("M=0.12 SPR: ",M12.est$dyn.SPR2[T4],"\n")
  cat("M=0.105 SPR: ",M105.est$dyn.SPR2[T4],"\n")
  cat("M=0.085 SPR: ",M085.est$dyn.SPR2[T4],"\n")
  cat("M=0.07 SPR: ",M07.est$dyn.SPR2[T4],"\n")
  
  layout(matrix(1:2,nrow=1))
  par(mar=c(5,4,1,0))
  plot(baseline.est$N.sum[1:Ttot],t="l",lwd=2,xaxt="n",yaxt="n",ylim=c(-1,50000),
       ylab="Thousands of sturgeon",xlab="Year",xaxs="i",font.lab=2,yaxs="i")
  lines(U01.est$N.sum[1:Ttot],col="blue",lwd=2)
  lines(U05.est$N.sum[1:Ttot],col="grey",lwd=2)
  lines(U1.est$N.sum[1:Ttot],col="darkgreen",lwd=2)
  lines(baseline.est$N.sum[1:Ttot],col="black",lwd=2)
  axis(1,at=seq(0,Ttot,20),seq(1900,2085,20))
  axis(1,at=seq(0,Ttot,5),labels=FALSE,lwd=0.5)
  axis(2,at=seq(0,100000,10000),seq(0,100,10))
  axis(2,at=seq(0,100000,5000),labels=FALSE,lwd=0.5)
  abline(v=T2+1,col="purple",lty=2,lwd=2)
  abline(v=T3,col="red",lty=2,lwd=2)
  abline(v=T4,col="green",lty=2,lwd=2)
  abline(h=8784,col="brown",lwd=2)
  legend("topleft","A",text.font=2,cex=1.2,bty="n")
  legend("topright",lty=1,legend=c("Baseline","U=0.01","U=0.05",
                                   "U=0.10","Carrying capacity"),
         bty="n",col=c("black","blue","grey","darkgreen","brown"),lwd=2,cex=0.9)
  
  par(mar=c(5,2,1,2))
  plot(baseline.est$N.sum[1:Ttot],t="l",lwd=2,xaxt="n",yaxt="n",ylim=c(0,50000),
       ylab="",xlab="Year",xaxs="i",font.lab=2,yaxs="i")
  lines(M145.est$N.sum[1:Ttot],col="blue",lwd=2)
  lines(M12.est$N.sum[1:Ttot],col="darkgreen",lwd=2)
  lines(M105.est$N.sum[1:Ttot],col="orange",lwd=2)
  lines(M085.est$N.sum[1:Ttot],col="grey",lwd=2)
  lines(M07.est$N.sum[1:Ttot],col="purple",lwd=2)
  lines(baseline.est$N.sum[1:Ttot],col="black",lwd=2)
  axis(1,at=seq(0,Ttot,20),seq(1900,2085,20))
  axis(1,at=seq(0,Ttot,5),labels=FALSE,lwd=0.5)
  axis(2,at=seq(0,100000,5000),labels=FALSE,lwd=0.5)
  abline(v=T2+1,col="purple",lty=2,lwd=2)
  abline(v=T3,col="red",lty=2,lwd=2)
  abline(v=T4,col="green",lty=2,lwd=2)
  abline(h=9000,col="brown",lwd=2)
  legend("topleft","B",text.font=2,cex=1.2,bty="n")
  legend("topright",lty=1,legend=c("Baseline","M=0.146","M=0.120",
                                   "M=0.105","M=0.085","M=0.070","Carrying capacity"),
         bty="n",col=c("black","blue","darkgreen","orange","grey","purple","brown"),lwd=2,cex=0.9)
}
#pdf("Figure 3.pdf",width=11,height=6)
#Scenario3()
#dev.off()

"Scenario4" <- function(mu.R0=c(9970,2605),U.base=0.089){
  base.inp <- input
  base.DF <- DF
  base.DF$Stock <- rep(FALSE,Ttot)
  base.inp$Ro <- mu.R0
  base.DF$U = c(rep(input$u,T1),rep(U.base,T3-T1),rep(input$bu2,Ttot-T3))
  baseline.est <- Dynamic.Mod(base.inp,base.DF)   # base model run
  base.inp$astr <- 2
  base.inp$aint <- 2
  base.inp$Nstr <- (base.inp$aint-base.inp$astr)/(base.inp$aint-1)  # Normal strength
  base.DF$Anomoly = c(rep(c(rep(base.inp$Nstr,base.inp$aint-1),base.inp$astr),length=Ttot))  
  skip2 <- Dynamic.Mod(base.inp,base.DF)   # spawn every 2nd year
  base.inp$aint <- 4
  base.inp$Nstr <- (base.inp$aint-base.inp$astr)/(base.inp$aint-1)  # Normal strength
  base.DF$Anomoly = c(rep(c(rep(base.inp$Nstr,base.inp$aint-1),base.inp$astr),length=Ttot))  
  skip4 <- Dynamic.Mod(base.inp,base.DF)   # spawn every 4th year
  base.inp$aint <- 5
  base.inp$Nstr <- (base.inp$aint-base.inp$astr)/(base.inp$aint-1)  # Normal strength
  base.DF$Anomoly = c(rep(c(rep(base.inp$Nstr,base.inp$aint-1),base.inp$astr),length=Ttot))  
  skip5 <- Dynamic.Mod(base.inp,base.DF)   # spawn every 5th year
  
  base.inp$astr <- 2
  base.inp$aint <- 1
  base.inp$Nstr <- (base.inp$aint-base.inp$astr)/(base.inp$aint-1)  # Normal strength
  base.DF$Anomoly = c(rep(c(rep(base.inp$Nstr,base.inp$aint-1),base.inp$astr),length=Ttot))  
  base.DF$Anomoly = c(rep(1,T3),rep(1.25,Ttot-T3))
  inc.rec <- Dynamic.Mod(base.inp,base.DF)   # Increase carrying capacity by 25%
  cat("Spawn every 2nd year SPR: ",skip2$dyn.SPR2[T4],"\n")
  cat("Spawn every 4th year SPR: ",skip4$dyn.SPR2[T4],"\n")
  cat("Spawn every 5th year SPR: ",skip5$dyn.SPR2[T4],"\n")
  cat("Spawn every 2nd year SPR: ",inc.rec$dyn.SPR2[T4],"\n")
  cat("Increase carrying capacity SPR: ",baseline.est$dyn.SPR2[T4],"\n")
  
  layout(matrix(1:2,nrow=1))
  par(mar=c(5,4,1,0))
  plot(baseline.est$N.sum[1:Ttot],t="l",lwd=2,xaxt="n",yaxt="n",ylim=c(-1,50000),
       ylab="Thousands of sturgeon",xlab="Year",xaxs="i",font.lab=2,yaxs="i")
  lines(skip2$N.sum[1:Ttot],col="blue",lwd=2)
  lines(skip4$N.sum[1:Ttot],col="grey",lwd=2)
  lines(skip5$N.sum[1:Ttot],col="darkgreen",lwd=2)
  lines(baseline.est$N.sum[1:Ttot],col="black",lwd=2)
  axis(1,at=seq(0,Ttot,20),seq(1900,2085,20))
  axis(1,at=seq(0,Ttot,5),labels=FALSE,lwd=0.5)
  axis(2,at=seq(0,100000,10000),seq(0,100,10))
  axis(2,at=seq(0,100000,5000),labels=FALSE,lwd=0.5)
  abline(v=T2+1,col="purple",lty=2,lwd=2)
  abline(v=T3,col="red",lty=2,lwd=2)
  abline(v=T4,col="green",lty=2,lwd=2)
  abline(h=8784,col="brown",lwd=2)
  legend("topleft","A",text.font=2,cex=1.2,bty="n")
  legend("topright",lty=1,legend=c("Baseline","1 of 2","1 of 4",
                                   "1 of 5","Carrying capacity"),
         bty="n",col=c("black","blue","grey","darkgreen","brown"),lwd=2,cex=0.9)
  
  par(mar=c(5,2,1,2))
  plot(baseline.est$N.sum[1:Ttot],t="l",lwd=2,xaxt="n",yaxt="n",ylim=c(-1,50000),
       ylab="",xlab="Year",xaxs="i",font.lab=2,yaxs="i")
  lines(inc.rec$N.sum[1:Ttot],col="blue",lwd=2)
  lines(baseline.est$N.sum[1:Ttot],col="black",lwd=2)
  axis(1,at=seq(0,Ttot,20),seq(1900,2085,20))
  axis(1,at=seq(0,Ttot,5),labels=FALSE,lwd=0.5)
  axis(2,at=seq(0,100000,5000),labels=FALSE,lwd=0.5)
  abline(v=T2+1,col="purple",lty=2,lwd=2)
  abline(v=T3,col="red",lty=2,lwd=2)
  abline(v=T4,col="green",lty=2,lwd=2)
  abline(h=8784,col="brown",lwd=2)
  legend("topleft","B",text.font=2,cex=1.2,bty="n")
  legend("topright",lty=1,legend=c("Baseline","+25% increase",
                                   "Carrying capacity"),
         bty="n",col=c("black","blue","brown"),lwd=2,cex=0.9)
}
#pdf("Figure 4.pdf",width=11,height=6)
#Scenario4()
#dev.off()

"Scenario5" <- function(mu.R0=c(9970,2605),U.base=0.089){
  base.inp <- input
  base.DF <- DF
  base.DF$Stock <- rep(FALSE,Ttot)
  base.inp$Ro <- mu.R0
  base.DF$U = c(rep(input$u,T1),rep(U.base,T3-T1),rep(input$bu2,Ttot-T3))
  baseline.est <- Dynamic.Mod(base.inp,base.DF)   # base model run
  base.inp$stock <- 5000
  base.DF$Stock <- c(rep(FALSE,T3),rep(TRUE,5),rep(FALSE,Ttot-T3-5))
  stock.5.5000 <- Dynamic.Mod(base.inp,base.DF)   # stock 500 fish for 5 years
  base.DF$Stock <- c(rep(FALSE,T3),rep(TRUE,20),rep(FALSE,Ttot-T3-20))
  stock.20.5000 <- Dynamic.Mod(base.inp,base.DF)   # stock 500 fish for 20 years
  base.inp$stock <- 2500
  base.DF$Stock <- c(rep(FALSE,T3),rep(TRUE,5),rep(FALSE,Ttot-T3-5))
  stock.5.2500 <- Dynamic.Mod(base.inp,base.DF)   # stock 500 fish for 5 years
  base.DF$Stock <- c(rep(FALSE,T3),rep(TRUE,20),rep(FALSE,Ttot-T3-20))
  stock.20.2500 <- Dynamic.Mod(base.inp,base.DF)   # stock 500 fish for 20 years
  base.DF$Anomoly = c(rep(1,T3),rep(1.25,Ttot-T3))
  base.DF$Stock <- rep(FALSE,Ttot)
  inc.rec <- Dynamic.Mod(base.inp,base.DF)   # spawn every 5th year
  cat("Stock 2500 for 5 years SPR: ",baseline.est$dyn.SPR2[T4],"\n")
  cat("Stock 5000 for 5 years SPR: ",baseline.est$dyn.SPR2[T4],"\n")
  cat("Stock 2500 for 20 years SPR: ",baseline.est$dyn.SPR2[T4],"\n")
  cat("Stock 5000 for 20 years SPR: ",baseline.est$dyn.SPR2[T4],"\n")
  
  layout(matrix(1:2,nrow=1))
  par(mar=c(5,4,1,0))
  plot(baseline.est$N.sum[1:Ttot],t="l",lwd=2,xaxt="n",yaxt="n",ylim=c(-1,50000),
       ylab="Thousands of sturgeon",xlab="Year",xaxs="i",font.lab=2,yaxs="i")
  lines(stock.5.2500$N.sum[1:Ttot],col="blue",lwd=2)
  lines(stock.5.5000$N.sum[1:Ttot],col="grey",lwd=2)
  lines(inc.rec$N.sum[1:Ttot],col="darkgreen",lwd=2)
  lines(baseline.est$N.sum[1:Ttot],col="black",lwd=2)
  axis(1,at=seq(0,Ttot,20),seq(1900,2085,20))
  axis(1,at=seq(0,Ttot,5),labels=FALSE,lwd=0.5)
  axis(2,at=seq(0,100000,10000),seq(0,100,10))
  axis(2,at=seq(0,100000,5000),labels=FALSE,lwd=0.5)
  abline(v=T2+1,col="purple",lty=2,lwd=2)
  abline(v=T3,col="red",lty=2,lwd=2)
  abline(v=T4,col="green",lty=2,lwd=2)
  abline(h=8784,col="brown",lwd=2)
  legend("topleft","A",text.font=2,cex=1.2,bty="n")
  legend("topright",lty=1,legend=c("Baseline","Stock 2500/yr","Stock 5000/yr",
                                   "+25% recruitment","Carrying capacity"),
         bty="n",col=c("black","blue","grey","darkgreen","brown"),lwd=2,cex=0.9)
  
  par(mar=c(5,2,1,2))
  plot(baseline.est$N.sum[1:Ttot],t="l",lwd=2,xaxt="n",yaxt="n",ylim=c(-1,50000),
       ylab="",xlab="Year",xaxs="i",font.lab=2,yaxs="i")
  lines(stock.20.2500$N.sum[1:Ttot],col="blue",lwd=2)
  lines(stock.20.5000$N.sum[1:Ttot],col="grey",lwd=2)
  lines(inc.rec$N.sum[1:Ttot],col="darkgreen",lwd=2)
  lines(baseline.est$N.sum[1:Ttot],col="black",lwd=2)
  axis(1,at=seq(0,Ttot,20),seq(1900,2085,20))
  axis(1,at=seq(0,Ttot,5),labels=FALSE,lwd=0.5)
  axis(2,at=seq(0,100000,5000),labels=FALSE,lwd=0.5)
  abline(v=T2+1,col="purple",lty=2,lwd=2)
  abline(v=T3,col="red",lty=2,lwd=2)
  abline(v=T4,col="green",lty=2,lwd=2)
  abline(h=8784,col="brown",lwd=2)
  legend("topleft","B",text.font=2,cex=1.2,bty="n")
  legend("topright",lty=1,legend=c("Baseline","Stock 2500/yr","Stock 5000/yr",
                                   "+25% increase","Carrying capacity"),
         bty="n",col=c("black","blue","grey","darkgreen","brown"),lwd=2,cex=0.9)
}
#pdf("Figure 5.pdf",width=11,height=6)
#Scenario5()
#dev.off()

"Age.Structure" <- function(mu.R0=c(9970,2605),U.base=0.089){
  base.inp <- input
  base.DF <- DF
  base.DF$Stock <- rep(FALSE,Ttot)
  base.inp$Ro <- mu.R0
  base.DF$U = c(rep(input$u,T1),rep(U.base,T3-T1),rep(input$bu2,Ttot-T3))
  baseline.est <- Dynamic.Mod(base.inp,base.DF)   # base model run
  data <- melt(baseline.est$N.w)#/rowSums(baseline.est$N.w))
  
  f <- ggplot(data,aes(x=X1,y=X2,fill=(value))) + 
    geom_raster(interpolate=TRUE) +
    scale_x_continuous(limits=range(data$X1),expand=c(0,0),
                       breaks=seq(20,180,20),labels=seq(1920,2080,20)) +
    scale_y_continuous(limits=range(data$X2),expand=c(0,0)) +
    labs(x="Year",y="Age-class") +
    #scale_fill_gradient2("Proportional\nabundance",low="red",mid="yellow",high="purple",
    #                     midpoint=0.05)+
    scale_fill_gradientn("Abundance", colours=rainbow(7),limits=c(0,500)) +
    geom_vline(xintercept=c(T2+1,T3,T4),colour="black",lty=2) + 
    theme_classic(base_size=16) + 
    theme(axis.title=element_text(face="bold"))
  x_intercept <- seq(20,180,20)
  y_intercept <- seq(5,50,5)
  f + geom_vline(xintercept=x_intercept, color="white", size=0.2) +
    geom_hline(yintercept=y_intercept, color="white", size=0.2)
}

Appendix <- function(mu.R0=c(9970,2605),U.base=0.089){
  base.inp <- input
  base.DF <- DF
  base.DF$Stock <- rep(FALSE,Ttot)
  base.inp$Ro <- mu.R0
  base.DF$U = c(rep(input$u,T1),rep(U.base,T3-T1),rep(input$bu2,Ttot-T3))
  baseline.est <- Dynamic.Mod(base.inp,base.DF)$N.sum[1:Ttot]   # base model run
  base.inp$recK <- 3
  reck3 <- Dynamic.Mod(base.inp,base.DF)$N.sum[1:Ttot]   # model run ending with 50% N0 by 2023
  base.inp$recK <- 7
  reck7 <- Dynamic.Mod(base.inp,base.DF)$N.sum[1:Ttot]   # model run ending with 75% N0 by 2023
  base.inp$recK <- 10
  reck10 <- Dynamic.Mod(base.inp,base.DF)$N.sum[1:Ttot]   # model run ending with 95% N0 by 2023
  plot(baseline.est,t="l",lwd=2,xaxt="n",yaxt="n",ylim=c(-1,50000),
       ylab="Thousands of sturgeon",xlab="Year",xaxs="i",font.lab=2,yaxs="i")
  lines(reck3,col="blue",lwd=2)
  lines(reck7,col="grey",lwd=2)
  lines(reck10,col="darkgreen",lwd=2)
  lines(baseline.est,col="black",lwd=2)
  axis(1,at=seq(0,Ttot,20),seq(1900,2085,20))
  axis(1,at=seq(0,Ttot,5),labels=FALSE,lwd=0.5)
  axis(2,at=seq(0,100000,10000),seq(0,100,10))
  axis(2,at=seq(0,100000,5000),labels=FALSE,lwd=0.5)
  abline(v=T2+1,col="purple",lty=2,lwd=2)
  abline(v=T3,col="red",lty=2,lwd=2)
  abline(v=T4,col="green",lty=2,lwd=2)
  abline(h=8784,col="brown",lwd=2)
  legend("topright",lty=1,legend=c("Baseline",expression(paste(kappa,"=3")),
                                   expression(paste(kappa,"=7")),
                                   expression(paste(kappa,"=10")),
                                   "Carrying capacity"),
         bty="n",col=c("black","blue","grey","darkgreen","brown"),lwd=2,cex=0.9)
}