Simplex <- function() {
#library(boot)
library(lpSolve)
x="";
while (x!="max" && x!="min"){
  x = readline("Enter la nature du PL (max/min) :")
}
nombrevar=as.integer("a");
while (is.na(nombrevar)) {
  nombrevar = as.integer(readline("Le nombre de variables : "))
}
nombrecontr=as.integer("a");
while (is.na(nombrecontr)){
  nombrecontr = as.integer(readline("Le nombre de contraintes : "))
}
print("La fonction objective :")
coefobj=c()
for (i in 1:nombrevar) {
  x1=as.integer("a");
  while (is.na(x1)){
    x1=as.numeric(readline("le coefficient de x : "))
  }
  coefobj=c(coefobj,x1);
}
f=paste(x,"(Z)=",sep="")
for (i in 1:nombrevar) {
  if(i==1){
    if(coefobj[i]==1) f=paste(f,"X",i,sep="")
    else if(coefobj[i]!=0) f=paste(f,coefobj[i],"X",i,sep="")
  }
  else {
    if(coefobj[i]==1) f=paste(f,"+","X",i,sep="")
    else if(coefobj[i]!=0) f=paste(f,"+",coefobj[i],"X",i,sep="")
  }
}
print(f)
coefcontr=matrix(, nrow = nombrecontr, ncol = nombrevar)
coefb=c()
types=c()
constraint_variables=c()
for (i in 1:nombrecontr) {
  print( paste( "contrainte ",i," : ",sep="") )
  for (j in 1:nombrevar) {
    x11=as.integer("a");
    while (is.na(x11)){
      x11=as.numeric( readline( paste( "le coefficient de x",j," : ",sep="" ) ) )
    }
    coefcontr[i,j]=x11
    if(x11!=0){
      constraint_variables=c(constraint_variables,j)
    }
  }
  b1=as.integer("a");
  while (is.na(b1)){
    b1=as.numeric(readline("b1 : "))
  }
  coefb=c(coefb,b1)
  type1=""
  while(type1!="=" && type1!=">" && type1!="<" && type1!=">=" && type1!="<="){
    type1=readline("type : ")
  }
  types=c(types,type1)
}

result <- lp(x, coefobj, coefcontr, types,coefb, compute.sens=TRUE)
print(result)
print(result$solution)

#result = simplex(
#	a = z,
#	A1 = A,
#	b1 = b,
#	maxi = TRUE
#)

}

Simplex()
