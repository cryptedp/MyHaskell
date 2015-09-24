 sieveSunandram:: Integer->[Integer]
 sieveSunandram n = [2*x+1| i<- [1..], j<-[1..],x<-[1..n],  i+j+2*i*j<=n,even x, x/=(2*i+1)*(2*j+1)]