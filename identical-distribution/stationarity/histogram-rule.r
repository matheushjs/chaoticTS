#require(tseriesChaos);
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

divergence.plot = function(data, box.x, box.y, box.z, from=20, to=3996, by=5, shuffle=FALSE, main=""){
	result = NULL

	inBox = (data[,1] > box.x[1]) & (data[,1] < box.x[2]);
	inBox = inBox & (data[,2] > box.y[1]) & (data[,2] < box.y[2]);
	inBox = inBox & (data[,3] > box.z[1]) & (data[,3] < box.z[2]);
	
	if(shuffle){
		data = data[sample(1:nrow(data)),];
	}

	for(i in seq(from, to, by=by)){
		nrows = nrow(data);
		half1 = inBox[1:i];
		half2 = inBox[(i+1):(2*i)];

		freq1 = sum(half1) / length(half1);
		freq2 = sum(half2) / length(half2);

		result = rbind(result, c(i, freq1, freq2, abs(freq1 - freq2)));
		plot(result[,1], result[,4], xlab="size of each half", ylab="histogram divergence", type="l", xlim=c(0, to), ylim=c(1e-5, max(result[,2])), main=main, log="y");
	}
	result;
}

lorenz = function(N=10000, t.step=0.03, recalc=TRUE){
	xrange = c(-19.11338, 19.1377);
	yrange = c(-19.11338, 19.1377);
	zrange = c(-19.11338, 19.1377); # Yes they're all equal

	n = 4;
	box.x = min(xrange) + c(n-2, n) * diff(xrange) / 10;
	box.y = box.x;
	box.z = box.x; # The dataset is spread over the identity digonal, that's why we make them equal

	t.end = N * t.step;
	if(recalc){
		lorenz.ts <<- sim.cont(lorenz.syst, 0, t.end, t.step,
			 start.x=c(5,5,5), parms=c(10, 28, -8/3), obs.fun = function(x)
			 x[1]);
		 #sqrt(sum(x**2)));
	}

	print(rbind(box.x, box.y, box.z));

	m = 3;
	d = 3;
	data = embedd(lorenz.ts, m=m, d=d);
	#data = data[seq(1, nrow(data), by=5),]

	rgl::plot3d(data);
	rgl::lines3d(t(rbind(box.x, box.y, box.z)), col=2, lwd=3);

	inBox = (data[,1] > box.x[1]) & (data[,1] < box.x[2]);
	inBox = inBox & (data[,2] > box.y[1]) & (data[,2] < box.y[2]);
	inBox = inBox & (data[,3] > box.z[1]) & (data[,3] < box.z[2]);
	
	print(inBox[seq(1, 10000, by=97)]);
	freq = sum(inBox) / nrow(data);

	print(freq);

	result = divergence.plot(data, box.x, box.y, box.z,
							 to=N/2, by=N%/%1000, shuffle=F, main=paste("Divergences for the Lorenz Map (non-shuffled, rate 0.03s, m = ", m, ", d = ", d, ")."));

	x = seq(0, max(result[,1]) * 1.1, length=1000);
	lines(x, sqrt(  (1 / (-2*x)) * log(0.05/2)  ), col=2)
	lines(x, sqrt(  (1 / (-2*x)) * log(0.001/2)  ), col=3)
	lines(x, sqrt(  (1 / (-2*x)) * log(1e-10/2)  ), col=4)

	legend("topright", c(expression(eta == 0.05), expression(eta == 0.001), expression(eta == 1e-10)), col=c(2, 3, 4), lwd=1)

	savePlot("histogram-rule-lorenz.png");
	result;
}

result = lorenz(5000000, recalc=FALSE); # 1,500,000 uses up about 60 MB
