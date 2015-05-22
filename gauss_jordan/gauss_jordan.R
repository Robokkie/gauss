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

#入力用部分
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

#掃き出し（n行に処理）
for(i in 1:n){
	irekae(Ab,i)
	p <- Ab[i,i]
	if(abs(p)<1.0e-6){
		stop("一意解を持ちません")
	}
	#第i行を(i,i)成分で除算（n-i回）
	Ab[i,] <- Ab[i,]/Ab[i,i]
	
	#第i行を使って他の行から掃き出しを行う（n-1行に処理*n-i列に処理）
	for(j in 1:n){
		if(j==i){
			j <- j+1
			next
		}
		Ab[j,] <- Ab[j,] - Ab[j,i]*Ab[i,]
		j<-j+1
	}
}

x <- Ab[,n+1]