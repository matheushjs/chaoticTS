require(tseriesChaos);

f = function(x) 4 * x * (1 - x);
fn = function(x, n){
	res = rep(x, n);
	for(i in 2:n) res[i] = f(res[i-1]);
	return(res);
}

plot.arrows = function(emb){
	from = emb[1:(nrow(emb)-1),];
	to   = emb[2:nrow(emb),];
	diff = to - from;
	to = to - diff * 0.03;
	from = from + diff * 0.03;

	arrows(from[,1], from[,2], to[,1], to[,2]);
}

graphics.off();
dev.new(width=10, height=10);

logistic.ts = fn(0.3333333, 10);
emb = embedd(logistic.ts, m=2, d=1);
plot(emb, type="l", xlim=c(0, 1), ylim=c(0, 1), xlab="", ylab="");
#plot.arrows(emb);
points(emb, pch=19, col=grey(seq(0, 0.7, length=nrow(emb))), cex=2.3);
savePlot("logistic-sequence.png");

dev.new(width=10, height=10);
lorenz.ts <- sim.cont(lorenz.syst, 0, 100, 0.05, start.x=c(5,5,5), parms=c(10, 28, -8/3), obs.fun = function(x) x[1]);
emb = embedd(lorenz.ts, m=2, d=4)[1:40 + 90,];
plot(emb, type="l", xlab="", ylab="");
points(emb, pch=19, col=grey(seq(0, 0.7, length=nrow(emb))), cex=2.3);
savePlot("lorenz-sequence.png");


dev.new(width=10, height=10);
N = nrow(emb);
train = emb[sample(20:(N-1)),];
test = emb[N,];
plot(train, type="l", xlab="", ylab="");
points(train, pch=19, col=grey(seq(0, 0.7, length=nrow(train))), cex=2.3);
points(test[1], test[2], pch=19, col=4, cex=4);
savePlot("lorenz-sequence2.png");
