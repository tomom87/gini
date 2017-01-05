Gini.index<-function( x, y,                    # 度数ベクトル
                        main="",               # 図のタイトル（省略時は何も書かない）
                        xlab="",               # x 軸の名前（省略時は何も書かない）
                        ylab=""                # y 軸の名前（省略時は何も書かない）
                        )
{
        stopifnot(y >= 0)                       # yが負でないことを確認
        n <- length(y)                          # データの個数
        ya <- cumsum(y)                         # yの累積度数
        ys <- sum(y)
        ya <- c(0, ya/ys)                       # yの累積相対度数
    stopifnot(x >= 0)                       　　# yが負でないことを確認
        xa <- cumsum(x)                         # xの累積度数
        xs <- sum(x)
        xa <- c(0, xa/xs)                         # xの累積相対度数
        plot(xa, ya, type="l", col="blue",        # これを結ぶとローレンツ曲線
          main=main, xlab=xlab, ylab=ylab)
        abline(v=c(0, 1), h=c(0, 1),            # 外周と，
               coef=c(0, 1))                   # 対角線（原点を通る，傾き 1 の直線）を描く
    a <- xa[-1] - xa[-length(xa)]      # 台形の高さ（ベクトル）
    b <- ya[-1] + ya[-length(xa)]      # 上底＋下底（ベクトル）
# 両ベクトルの内積はローレンツ曲線の面積の2倍
    return((1-a%*%b))            　    # ジニ係数　1からベクトル内積を引く
}

d <-read.csv("data.csv", header=T)
d$order <- d$Y/d$X
d <- d[order(d$order),]					# 収入/人口（Y/X）でソート
attach(d)

Gini.index(X,Y)
