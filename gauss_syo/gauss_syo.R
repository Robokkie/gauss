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

# 前進消去により上三角行列に変換
# 上から順に処理（N回）
for(i in 1:n){
	irekae(Ab,i)
	p <- Ab[i,i]
	if(abs(p)<1.0e-6){
		stop("一意解を持ちません")
	}
	#第i行を(i,i)成分で除算（N+1-i回の除算）
	Ab[i,] <- Ab[i,]/Ab[i,i]

	#第i行を使って，下方の行を掃き出す（N-i回の乗算）
	j <- i+1
	while(j<=n){
		Ab[j,] <- Ab[j,] - Ab[j,i]*Ab[i,]
		j<-j+1
	}
}

# 逆進代入により解を求める
x <- numeric(n)
for(i in n:1){
	s<-0
	j<-i+1
	while(j<=n){
		s <- s + Ab[i,j]*x[j]
		j <- j+1
	}
	x[i] <- Ab[i,n+1]-s
}

output <- data.frame(Ab)
output
write.table(output, "output.csv", quote=F, sep=",", col.names=F, append=F)