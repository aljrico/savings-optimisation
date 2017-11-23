
# Script Concerning some paper --------------------------------------------

capK=0
alfa=0.0343
s=0.1544
capA=1
gamma=1-(alfa/(s*s*capA))
gamma
r=0
capT=30

#Savings scheme 1 T=30
x0=100
a=0

#Savings scheme 2 T=30
#x0=10
#a=10

#Savings scheme 3 T=30
#x0=155
#a=5



# G value
G=138.3333333333333333 #in IME paper
F=83.3333333333333333333


theta=alfa/s

deltaC=rep(a,capT)
deltaC[capT]=0

gdet<-function(tt){
	xxx=0
	for(ii in tt:capT){xxx=xxx+deltaC[ii]}
	return(xxx)
}


##
## vecgdet contains g(0), g(1), ... g(capT) so, capT+1 cells
##
vecgdet=array(c(rep(a,capT),0), capT+1)
for(ii in 1:capT){
	vecgdet[ii]=gdet(ii)
}



dplusG<-function(t,y){
	xxx<-(1/(s*capA*sqrt(capT-t)))*(log(y/G)+(s*s*capA*capA/2)*(capT-t))
	return(xxx)
}

dminusG<-function(t,y){
	xxx<-(1/(s*capA*sqrt(capT-t)))*(log(y/G)-(s*s*capA*capA/2)*(capT-t))
	return(xxx)
}


dplusF<-function(t,y){
	xxx<-(1/(s*capA*sqrt(capT-t)))*(log(y/F)+(s*s*capA*capA/2)*(capT-t))
	return(xxx)
}

dminusF<-function(t,y){
	xxx<-(1/(s*capA*sqrt(capT-t)))*(log(y/F)-(s*s*capA*capA/2)*(capT-t))
	return(xxx)
}

z0value=function(z0){
	xx=z0-(z0)*pnorm(dplusG(0,(z0)))+G*pnorm(dminusG(0,(z0)))+
		F*pnorm(-dminusF(0,z0))-(z0)*pnorm(-dplusF(0,(z0)))
	xx2=(xx-refx0)*(xx-refx0)
	return(xx2)
}
#z0=optim(x0, z0value)$par
refx0=x0+vecgdet[1]
z0=optimize(z0value,c(0,10*refx0))$minimum
z0=z0-vecgdet[1]


set.seed(2341001)  #### Seed number
Nrep=10000         #### Number of runs 100,000=1 minute computer time
w=rnorm(capT*Nrep)
matw=array(w, dim=c(Nrep,capT))


levels=c(0.01, 0.025, (1:19)/20, 0.975, 0.99)

percex=array(0, dim=c(length(levels),1))

g00=vecgdet[1]
for (kk in 1:length(levels))
{
	betap=s*capA*sqrt(capT)*qnorm(levels[kk])+(r+theta*s*capA-0.5*s*s*capA*capA)*capT
	percex[kk,1]=max(F,min(G,(z0+g00)*exp(betap)))
}

percex


levels2=c((1:9999)/10000)
percex2=array(0, dim=c(length(levels2),1))

g00=vecgdet[1]
for (kk in 1:length(levels2))
{
	betap=s*capA*sqrt(capT)*qnorm(levels2[kk])+(r+theta*s*capA-0.5*s*s*capA*capA)*capT
	percex2[kk,1]=max(F,min(G,(z0+g00)*exp(betap)))
}

percex

probHitG=which.max(percex2)/100
probHitG
probHitF=length(which(percex2==min(percex2)))/100
probHitF
