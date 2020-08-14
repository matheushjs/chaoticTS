require(tseriesChaos);
#require(RTisean);

graphics.off();
dev.new(width=0.8*12, height=0.8*8);

norm1 = function(vec){
	sum(abs(vec));
}

norm2 = function(vec){
	sqrt(sum(vec**2));
}

normInf = function(vec){
	max(abs(vec));
}

divergence.plot = function(data, from=20, to=3996, by=5, shuffle=FALSE, main=""){
	result = NULL
	
	if(shuffle){
		data = data[sample(1:nrow(data)),];
	}

	for(i in seq(from, to, by=by)){
		nrows = nrow(data);
		half1 = data[1:i,];
		#half2 = data[(nrows/2 + 1):(nrows/2 + i),];
		half2 = data[(i+1):(2*i),];
		d = norm2(colMeans(half1) - colMeans(half2));
		result = rbind(result, c(i, d));
		plot(result, xlab="size of each half", ylab="divergence", type="l", xlim=c(0, to), ylim=c(1e-3, max(result[,2])), main=main, log="y");
	}
	result;
}

#lorenz.ts <- sim.cont(lorenz.syst, 0, 24000, 0.03,
#     start.x=c(5,5,5), parms=c(10, 28, -8/3), obs.fun = function(x)
#     x[1]);
     #sqrt(sum(x**2)));

#log.ts = logistic(iter=40000);

m = 3;
d = 3;
data = embedd(lorenz.ts, m=m, d=d);
#data = data[seq(1, nrow(data), by=5),]

result = divergence.plot(data, to=2*197000, by=2*100, shuffle=F, main=paste("Divergences for the Lorenz Map (non-shuffled, rate 0.05s, m = ", m, ", d = ", d, ")."));

range = max(data) - min(data);
x = seq(0, max(result[,1]) * 1.1, length=1000);
lines(x, sqrt(  (range**2 / (-2*x)) * log(0.1/(2*m*d))  ), col=2)
lines(x, sqrt(  (range**2 / (-2*x)) * log(0.05/(2*m*d)) ), col=3)
lines(x, sqrt(  (range**2 / (-2*x)) * log(0.01/(2*m*d)) ), col=4)


#abline(h = result[nrow(result),2], col=5, lwd=1);
legend("bottom", legend=paste(result[nrow(result),2]), col=5, lwd=3);

legend("topright", c(expression(eta == 0.1), expression(eta == 0.05), expression(eta == 0.01)), col=c(2, 3, 4), lwd=1)

savePlot("independence-criterion.png");
