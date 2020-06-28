require(tseriesChaos);

# HAHAHAHAH this below is awesome
# x = seq(1, 100, length=1000);
# y = sin(2*pi*x);

# This difeomorphism applies to y
D = function(y){
	1 / (1 + exp(-6*pi*y))
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

x = seq(0, 20, length=9999);
y = D(sin(2*pi*x));
plot(embedd(y, m=2, d=10)[1:1000,], pch=19, col="#00000033")

