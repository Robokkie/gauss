#function for read line using sep ","
readstring <- function () {
	Str <- readline("Enter:")
	as.numeric(unlist(strsplit(Str, ",")))
}

irekae <- function(a,col){
	maxid <- which.max(a[,col])
	temp <- a[maxid,]
	a[maxid,] <- a[col,]
	a[col,] <- temp
}


# --main-- #
n <- as.numeric(readline("行列の次数は?:"))

A <- matrix(0,n,n)
Ab <- matrix(0,n,n+1)

print("係数行列Aをカンマ区切りで入力")
for(i in 1:n){
	s <- sprintf("%d行目",i)
	print(s)
	A[i,] <- readstring()
}

print("定数項行列bをカンマ区切りで入力")
b <- readstring()

Ab[1:n,1:n] <- A
Ab[,n+1] <- b

# ガウスの消去法によるLU分解
l <- diag(n)
u <- A

for(i in 1:n){
	l[,i] <- u[,i]
	
	irekae(u,i)
	p <- u[i,i]
	if(abs(p)<1.0e-6){
		stop("一意解を持ちません")
	}
	#第i行を(i,i)成分で除算
	u[i,] <- u[i,]/u[i,i]

	#第i行より下の行は
	j<-i+1
	while(j<=n){
		u[j,] <- u[j,] - u[j,i]*u[i,]
		j<-j+1
	}
}
# Lの上三角成分を捨てる
l[upper.tri(l)] <- 0


# 処理用にlをコピー
lt <- l

# 前進代入によるyの特定(Ly=b)
t <- proc.time()
y <- numeric(n)
for(i in 1:n){
	s<-0
	b[i] <- b[i]/lt[i,i]
	lt[i,] <- lt[i,]/lt[i,i]
	j<-i-1
	while(j>=1){
		s <- s + lt[i,j]*y[j]
		j <- j-1
	}
	y[i] <- b[i]-s
}

# 逆進代入によるxの特定(Ux=y)
x <- numeric(n)
for(i in n:1){
	s<-0
	j<-i+1
	while(j<=n){
		s <- s + u[i,j]*x[j]
		j <- j+1
	}
	x[i] <- y[i]-s
}
t2 <- proc.time() - t

ts<- t1+t2

output <- data.frame(x)
output
write.table(output, "output.csv", quote=F, sep=",", col.names=F, append=F)