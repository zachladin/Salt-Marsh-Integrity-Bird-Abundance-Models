#read in SMI data

#test makeGMMumf #WORKS
region2.data<-read.csv(paste(regionDataFilepath,"2_CapeCod-CascoBay.data.csv",sep="/"),header=TRUE)


#getAbundanceNWR

getAbundanceRegion(dataIn=region2.data, speciesName="SALS")


#simulate data
dataSimRegionFull(dataIn=region2.data,modIn=mod.all,declineRate="-0.10",nyrs=5,nvisits=3)

i=NULL

