####netmeta####
library(netmeta)		#To do network meta-analysis by frequentist methods.
library(tcltk2)		#To set working directory.
#library(openxlsx)		#To save table data as Excel xlsx file.

##Data import via the clipboard.
#dat=read.delim("clipboard",sep="\t",header=TRUE)		#For Windows to enter data via clipboard.
#dat=read.delim(pipe("pbpaste"),sep="\t",header=TRUE);	#For Mac to enter data via clipboard.

##Check type of effect measure.
em = dat[1,"effect.measure"]
pa = colnames(dat)[5]
ref = dat[1,"reference"]

if(em == ""){
print("Set type of effect measure.")
}
if(ref == ""){
print("Set the reference arm, such as, Placebo.")
}
print(paste("Type of effetc measure is ",em," and reference arm is ",ref,".",sep=""))

head(dat)	#The first six lines of the data.

#Set working directory to paths to save PNG or CSV files. If cancel, no saving of these results.
paths = ""
fils=tk_choose.dir(default="",caption="Select a folder to save files (working directory)");if(!is.na(fils)){paths=paste(fils,"/",sep="");setwd(paths)}

###Reformat and calcutate the data when pa != "TE".
######################
###Binary outcome & Single format:
if(pa=="event"){
pw=pairwise(studlab=study.id,treat=treatment,n=n,event=event,data=dat,sm=em)
}
###Continuous outcome & Single format:
if(pa=="mean"){
pw=pairwise(studlab=study.id, treat=treatment,n=n, mean=mean, sd=sd, data=dat, sm=em)
}
###Hazard Ratio or other comparative effect measures: ln(HR), ln(OR), ln(RR), RD, MD.
if(pa=="TE"){
pw=dat
}
###Data check: studlab, treat1, treat2, TE, seTE, etc.
names(pw)	#Label names.
head(pw)	#Data format. 

###Do network meta-analysis with effect estimates in each pair, and create forest plot, network graph.
nmr=netmeta(TE,seTE,treat1,treat2,studlab,random=TRUE,reference=ref,data=pw, sm=em)
#nmr

#Network graph with the number of studies. *Thickness proportional to no of studies.
dev.new();netgraph(nmr,points=TRUE,cex.points=3,cex=0.8,rotate=0,col="gray",col.points="black",number.of.studies=TRUE,lwd=5)	
#Network graph with the number of studies. *Thickness proportional to no of studies.
dev.new();netgraph(nmr,points=TRUE,cex.points=3,cex=0.8,rotate=0,col="gray",col.points="black",number.of.studies=FALSE,lwd=5)	

dev.new();forest(nmr,ref=ref)	#Forest plot with reference base as ref.

#Net league comparisons.
league=netleague(nmr,rondom=TRUE,common=FALSE)
#league

##Net heat graph.
nh = 0
dev.new()
result = try(netheat(nmr), silent=TRUE)
if(inherits(result, "try-error")){
print("Net heat graph could not be created.")
dev.off()
}else{
netheat(nmr)	#Net heat plot.
nh = 1
}

#netmeasures(nmr)

###Check ranking.
netrank(nmr)	#P-score
set.seed(1909)
ran=rankogram(nmr)
netrank(ran)	#SUCRA
#Plot rankogram.
dev.new();plot(ran)
#ran

###Node splitting and plot, some with prediction ranges.
splitr = netsplit(nmr,show="all",common=FALSE,random=TRUE)	#All comparisons.
splitb = netsplit(nmr,show="both",common=FALSE,random=TRUE)	#Only with both comparisons.

heinum=length(splitb$k)					#Number of nodes of all
heinumb=length(splitr$x$comparisons)		#Number of nodes of both

fs=12
sc=1

fsa=12
sca=1
fsb=12
scb=1

wid=700		#Plot width in pixels.

#Node-splitting forest plot of all comparisons: direct, indirect, and network estimates without prediction.
hei=68*heinum	#Plot height in pixels.
if(heinum>15){
fsa=12*11/heinum
sca=fsa/10.5
}
dev.new();forest(splitr,fontsize = fsa, spacing = sca, width=wid,height=hei,prediction = FALSE, addrow.subgroups = FALSE)

#Node-splitting forest plot of both direct and indirect, and network estimates.
heib=68*heinumb
if(heinumb>15){
fsb=12*11/heinumb
scb=fsb/10.5
}
dev.new();forest(splitb,fontsize = fsb, spacing = scb, width=wid,height=heib,prediction = FALSE, addrow.subgroups = FALSE)

#Each intervention to control only network estimates with prediction.
dev.new();forest(splitr, fontsize = 10, spacing = 1, show = "all", only.reference = TRUE, prediction = TRUE, direct = FALSE, indirect = FALSE)

####SUCRA calculation. (To compare with netmeta SUCRA,the result sucrap is not in use.)
rankr=rankogram(nmr)
rankr
sucmo = rankr$cumrank.matrix.random
sucm = sucmo
coln = ncol(sucm)
rown = nrow(sucm)
sai = coln - 1
sucrao = rep(0,coln)
for(i in 1:rown){
adsu=0
for(j in 1:sai){
adsu = adsu + sucmo[i,j]
sucm[i,coln - j + 1] = sucmo[i,coln - j]
}
sucrao[i] = adsu
}

sucra=sucrao/(coln - 1)
sucrap = round(sucra*100)

jun = seq(1,coln)
ichi = 1
for(i in 0:100){
for(j in 1:coln){
if(sucrap[j] == 100 - i){
jun[ichi] = j
ichi = ichi + 1
}
}
}
#Rank with 95%CI.
rankci = function(rno){
rk=rankr$ranking.matrix.random[rno,]
stnum=length(rk)
rnknum=trunc(1000*rk)
rnk=rep(1,rnknum[1])
for(i in 2:stnum){
rnk=c(rnk,rep(i,rnknum[i]))
}
rmedi=median(rnk)
rci=quantile(rnk,c(0.025,0.975))
rlow=rci[[1]]
rup=rci[[2]]
rres=paste(rmedi,"(",rlow,":",rup,")",sep="")
return(rres)
}

###SUCRA plot.
for(ver in 1:coln){
ra = coln/ver
if(ra < ver){
hor = ceiling(ra)
break
}
}
########
pscr=netrank(nmr)$ranking.random	#P score
sucs=ran$ranking.random	#SUCRA
sucsp=round(100*sucs)
rnkord=data.frame(matrix("",nrow=coln,ncol=4))
colnames(rnkord)=c("Intervention","P score","SUCRA","Rank (95%CI)")
###Decreasing order from the top to the bottom of sucra values.
dev.new();
par(mfrow=c(ver,hor), oma=c(0,0,1,0))
trts = rownames(rankr$cumrank.matrix.random)
tnum = length(trts)
maxn = tnum + 1
said = tnum - 1
x = seq(1:maxn)
for(i in 2:tnum){
x[i] = x[i]-0.5
}
x[maxn] = tnum
rit=tnum - 1
for(i in 1:tnum){
rnkord[i,1]=trts[jun[i]]
rnkord[i,2]=pscr[[jun[i]]]
rnkord[i,3]=sucs[[jun[i]]]
rkci=rankci(jun[i])
rnkord[i,4]=rkci
y = c(sucmo[jun[i],1],sucmo[jun[i],1:rit],sucmo[jun[i],said])
plot(y ~ x, type="l",ylab="Probability", ylim=c(0,1),xlab = paste(trts[jun[i]],"  ", sucsp[[jun[i]]],rkci), xlim=c(1,tnum), xaxt="n")
axis(1, at=seq(1,tnum,by=1))
}
title(mai="SUCRA", outer=TRUE, line=-1)

#####Save plots as  PNG files.######
if(paths != ""){
#Save the summary effect sizes with prediction as a PNG file.
png("summary-effect-pred.png")
forest(splitr, fontsize = 10, spacing = 1, show = "all", only.reference = TRUE, prediction = TRUE, direct = FALSE, indirect = FALSE)
dev.off()

#Save node-splitting plots as PNG file, all and both.
png("node-split-all.png",height=hei,width=wid)
forest(splitr,fontsize = fs, spacing = sc, prediction = FALSE, addrow.subgroups = FALSE)
dev.off()

png("node-split-both.png",height=heib,width=wid)
forest(splitb,fontsize = fs, spacing = sc, prediction = FALSE, addrow.subgroups = FALSE)
dev.off()

#Save the summary effect sizes with prediction, each intervention to control onlyas a PNG file.
png("summary-effect-pred.png")
forest(splitr, fontsize = 10, spacing = 1, show = "all", only.reference = TRUE, prediction = TRUE, direct = FALSE, indirect = FALSE)
dev.off()

#Save the rankogram as a PNG file.
png("rankogram.png")
plot(ran)
dev.off()

#
if(nh==1){
png("net-heat-graph.png")
netheat(nmr)	#Net heat plot.
dev.off()
}
#Save the network graph as a PNG file in the working directory.
#With the number of studies.
png("network-graph-with-ns.png",width=1500,height=1000)
netgraph(nmr,points=TRUE,cex.points=3,cex=2,rotate=0,col="gray",col.points="black",number.of.studies=TRUE,lwd=5)
dev.off()
#Without the number of studies.
png("network-graph-no-ns.png",width=1500,height=1000)
netgraph(nmr,points=TRUE,cex.points=3,cex=2,rotate=0,col="gray",col.points="black",number.of.studies=FALSE,lwd=5)
dev.off()

#Save the forest plot as a PNG file in the working directory.
png("forest-plot.png")
forest(nmr,ref=ref)
dev.off()

#Save SUCRA plots as PNG file.
png("SUCRA.png")
par(mfrow=c(ver,hor), oma=c(0,0,1,0))
trts = rownames(rankr$cumrank.matrix.random)
tnum = length(trts)
maxn = tnum + 1
said = tnum - 1
x = seq(1:maxn)
for(i in 2:tnum){
x[i] = x[i]-0.5
}
x[maxn] = tnum
rit=tnum - 1
for(i in 1:tnum){
y = c(sucmo[jun[i],1],sucmo[jun[i],1:rit],sucmo[jun[i],said])
plot(y ~ x, type="l",ylab="Probability", ylim=c(0,1),xlab = paste(trts[jun[i]],"  ", sucsp[[jun[i]]],rkci), xlim=c(1,tnum), xaxt="n")
axis(1, at=seq(1,tnum,by=1))
}
title(mai="SUCRA", outer=TRUE, line=-1)
dev.off()

#####Save table data as CSV files.
#Net league table.
write.csv(league$random, "netleague.csv",row.names=TRUE)	#Net league file as csv: Lower triangle: nma estimates, upper triangle: direct estimates.

##################################################################
###Create table of direct, indirect, network estimates with 95%CI######
stla=splitr$random$comparison
comno=length(stla)
ilab=rep("",comno)
clab=rep("",comno)

for(i in 1:comno){
stic=strsplit(stla[i],":")
ilab[i]=stic[[1]][1]
clab[i]=stic[[1]][2]
}
des=splitr$direct.random$TE
dlw=splitr$direct.random$lower
dup=splitr$direct.random$upper
ies=splitr$indirect.random$TE
ilw=splitr$indirect.random$lower
iup=splitr$indirect.random$upper
nes=splitr$random$TE
nlw=splitr$random$lower
nup=splitr$random$upper

if(em == "RR" || em == "OR" || em == "HR"){
des=exp(des)
dlw=exp(dlw)
dup=exp(dup)
ies=exp(ies)
ilw=exp(ilw)
nes=exp(nes)
nlw=exp(nlw)
nup=exp(nup)
}

nmas=data.frame(matrix("",nrow=comno,ncol=11))
colnames(nmas)=c("Intervention","Comparator",paste("Direct",em),"95%CI","CoE",paste("Indirect",em),"95%CI","CoE",paste("Network",em),"95%CI","CoE")
keta=2
for(i in 1:comno){
nmas[i,1]=ilab[i]
nmas[i,2]=clab[i]

if(is.na(des[i])){
nmas[i,3]="NA"
nmas[i,4]="NA"
}else{
nmas[i,3]=round(des[i],keta)
nmas[i,4]=paste(round(dlw[i],keta)," - ",round(dup[i],keta))
}
if(is.na(ies[i])){
nmas[i,6]="NA"
nmas[i,7]="NA"
}else{
nmas[i,6]=round(ies[i],keta)
nmas[i,7]=paste(round(ilw[i],keta)," - ",round(iup[i],keta))
}
if(is.na(nes[i])){
nmas[i,9]="NA"
nmas[i,10]="NA"
}else{
nmas[i,9]=round(nes[i],keta)
nmas[i,10]=paste(round(nlw[i],keta)," - ",round(nup[i],keta))
}
}

write.csv(nmas,"nma-estimate-table.csv",row.names=FALSE)
####SUCRA and P score table####
write.csv(rnkord,"SUCRA-P-score-Rank-CI.csv",row.names=FALSE)
}
###########################################

