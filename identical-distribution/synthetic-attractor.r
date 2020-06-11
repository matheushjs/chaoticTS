require(tseriesChaos);

# HAHAHAHAH this below is awesome
# x = seq(1, 100, length=1000);
# y = sin(2*pi*x);

# This difeomorphism applies to y
D = function(y){
	1 / (1 + exp(-pi*y))
}

# This one applies to x
Dx = function(x){
	int = x %/% (2*pi);
	frac = x %% (2*pi);
	peak = pi**2;

	int*2*pi + frac**( 1 - ((frac - 0)*(2*pi - frac) / peak / 2) );
}

#x = seq(0, 8*pi, length=10000)
#y = sin(Dx(x))
#plot(x, y);
#plot(embedd(y, m=2, d=140), pch=19, col="#00000011")

series = function(n = 100){
	t0 = runif(n=1, max=1000, min=0);
	t = t0 + 1:n * 40/9999;
	D(sin(2*pi*t));
}

#x = seq(0, 20, length=9999);
#y = D(sin(2*pi*x));
#plot(embedd(y, m=2, d=10)[1:1000,], pch=19, col="#00000033")

plot(embedd(series(1000), m=2, d=10), pch=19, col="#00000011");

all = list()
for(i in 1:10000){
	all[[i]] = embedd(series(60), m=2, d=10);
}

plotAll = function(idx=1){
	mat = matrix(ncol=2, nrow=length(all));
	for(i in 1:length(all))
		mat[i,] = all[[i]][idx,];
	plot(mat, pch=19, col="#00000004");
}

dev.new(width=6, height=6);
plotAll(1);
savePlot("synthetic-equidistribution1.png");

dev.new(width=6, height=6);
plotAll(50);
savePlot("synthetic-equidistribution2.png");

graphics.off();
