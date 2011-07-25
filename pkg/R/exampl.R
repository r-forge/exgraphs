#single

y=rnorm(10000,0,100)
x=rnorm(10000,1000,100)
breakX<- seq(min(x),max(x),length.out=101)
breakY<- seq(min(y),max(y),length.out=101)
counts=table(cut(x,breaks=c(breakX[-101],Inf)),
cut(y,breaks=c(breakY[-101],Inf) ))
xcenter <- (breakX[-1]+breakX[-101])/2
ycenter <- (breakY[-1]+breakY[-101])/2
gridScatterPlot(counts,x.center=xcenter, y.center=ycenter,radi=300)

#multiple class

z=sample(c(1,2,3), 10000,TRUE)
counts.array<- array(NA,c(100,100,3))
for(i in 1:3){
counts.array[,,i]=table(cut(x,breaks=c(breakX[-101],Inf)),
cut(y,breaks=c(breakY[-101],Inf) ))
}
gridScatterPlot(counts.array,x.center=xcenter, y.center=ycenter,radi=300,alpha=0.7)

# with color
gridScatterPlot(counts.array,x.center=xcenter, y.center=ycenter,radi=300,alpha=0.7,col=c("red", "blue","yellow"))
