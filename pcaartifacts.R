X = runif(1000); X = sort(X)*2
Y = rbinom(1000,size = 1,.5)
Z = cbind(X,Y)
D = dist(Z); D = as.matrix(D)
S = exp(-D)
eig = eigen(S)
colors = rep("red",1000)
colors[Y==1] = "blue"
k = 0
k = k+1; plot(eig$vec[,k], col = colors)

