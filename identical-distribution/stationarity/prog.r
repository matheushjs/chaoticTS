require(tseriesChaos);
require(RTisean);

embedd = tseriesChaos::embedd;
sim.cont = tseriesChaos::sim.cont;

graphics.off();
dev.new(width=0.8*12, height=0.8*8);

logistic = function(iter=40000, r=3.8){
	res = rep(0, iter);
	res[1] = runif(min=0, max=1, n=1);
	for(i in 2:iter)
		res[i] = r*res[i-1]*(1 - res[i-1]);
	res;
}

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
		plot(result[,1]*2, result[,2],  xlab="sample size", ylab=expression(paste("divergence", phantom(1), epsilon, phantom(1))), type="l", ylim=c(1e-3, max(result[,2])), main=main, log="y");
	}
	result;
}

lorenz = function(N=10000, t.step=0.03, recalc=TRUE){
	t.end = N * t.step;

	if(recalc){
		mylorenz.ts <<- sim.cont(lorenz.syst, 0, t.end, t.step,
			 start.x=c(5,5,5), parms=c(10, 28, -8/3), obs.fun = function(x)
			 x[1]);
		 #sqrt(sum(x**2)));
	}

	m = 3;
	d = 3;
	data = embedd(mylorenz.ts, m=m, d=d);

	data;
}

logistic.map = function(){
	log.ts = logistic(iter=100000, r=2);
	#plot(log.ts);

	m = 2;
	d = 1;
	data = embedd(log.ts, m=m, d=d);
	plot(data);
	locator(1);
	#data = data[seq(1, nrow(data), by=5),]

	result = divergence.plot(data, to=49900, by=60, shuffle=F, main=paste("Divergences for the Logistic Map (non-shuffled, rate 1s, m = ", m, ", d = ", d, ")."));

	range = max(data) - min(data);
	x = seq(0, max(result[,1]) * 1.1, length=1000);
	lines(x, sqrt(  (range**2 / (-2*x)) * log(0.1/(2*m*d))  ), col=2)
	lines(x, sqrt(  (range**2 / (-2*x)) * log(0.05/(2*m*d)) ), col=3)
	lines(x, sqrt(  (range**2 / (-2*x)) * log(0.01/(2*m*d)) ), col=4)

	#abline(h = result[nrow(result),2], col=5, lwd=1);
	legend("bottom", legend=paste(result[nrow(result),2]), col=5, lwd=3);

	legend("topright", c(expression(eta == 0.1), expression(eta == 0.05), expression(eta == 0.01)), col=c(2, 3, 4), lwd=1)

	savePlot("independence-criterion-logistic.png");
}

N = 1500000;
m = 3;
d = 3;

#logistic.map();
data = lorenz(N, recalc=TRUE); # 1,500,000 uses up about 60 MB
writeBin(data[,1], "lorenz-million.dat", size=8);

# resample as needed
#data = data[seq(1, nrow(data), by=5),]

result = divergence.plot(data, to=N/2, by=N%/%1000, shuffle=F, main=paste("Divergences for the Lorenz Map (non-shuffled, rate 0.03s, m = ", m, ", d = ", d, ")."));

range = max(data) - min(data);
x = seq(0, max(result[,1]) * 1.1, length=1000);
lines(2*x, sqrt(  (range**2 / (-x)) * log(0.1/(2*m*d))  ), col=2)
lines(2*x, sqrt(  (range**2 / (-x)) * log(0.05/(2*m*d)) ), col=3)
lines(2*x, sqrt(  (range**2 / (-x)) * log(0.01/(2*m*d)) ), col=4)

#abline(h = result[nrow(result),2], col=5, lwd=1);
#legend("bottom", legend=paste(result[nrow(result),2]), col=5, lwd=3);

legend("topright", c(expression(eta == 0.1), expression(eta == 0.05), expression(eta == 0.01)), col=c(2, 3, 4), lwd=1)

savePlot("independence-criterion-lorenz.png");

