require(tseriesChaos);
require(RTisean);

embedd = tseriesChaos::embedd;
sim.cont = tseriesChaos::sim.cont;
lorenz.s0 = c(9.227021424263037729929, 15.117356048661351408668, 17.911512308502612000893);

if(length(lorenz.ts) == 2001)
	lorenz.ts = NULL;

graphics.off();
dev.new(width=0.8*12, height=0.8*8);

logistic = function(iter=40000, r=3.8){
	res = rep(0, iter);
	res[1] = runif(min=0, max=1, n=1);
	for(i in 2:iter)
		res[i] = r*res[i-1]*(1 - res[i-1]);
	res;
}

lorenz.real = function(N=1000, t.step=0.03){
	data = matrix(0, nrow=N, ncol=3);
	
	t.end = N * t.step;
	gambiarra = sim.cont(lorenz.syst, 0, t.end, t.step,
		 start.x=lorenz.s0, parms=c(10, 28, -8/3), obs.fun = function(x) list(x));

	for(i in 1:N){
		data[i,] = gambiarra[[i]][[1]];
	}

	data;
}

lorenz.embedded = function(N=1000, m=3, d=3, t.step=0.03){
	t.end = N * t.step;
	series = sim.cont(lorenz.syst, 0, t.end, t.step,
		 start.x=lorenz.s0, parms=c(10, 28, -8/3), obs.fun = function(x)
		 x[1]);

	data = embedd(series, m=m, d=d);
	data;
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

divergence.plot = function(data, box.x, box.y, box.z, from=20, to=3996, by=5, shuffle=FALSE, smooth=TRUE){
	result = NULL;

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
	}
	if(smooth){
		sm = smooth.spline(result[,1], result[,4], df=30);
		lines(sm, lwd=2, col="#00007788");
	} else {
		lines(result[,1], result[,4], lwd=2, col="#000000AA");
	}
	result;
}

lorenz.single = function(data){
	N = nrow(data);
	m = 3;
	d = 3;

	xrange = range(data[,1]);
	yrange = range(data[,2]);
	zrange = range(data[,3]);

	#xrange = c(-19.11338, 19.1377);
	#yrange = c(-19.11338, 19.1377);
	#zrange = c(-19.11338, 19.1377); # Yes they're all equal

	# Real lorenz system
	#xrange = c(5.3, 14.1);
	#yrange = c(11.25, 19.3);
	#zrange = c(16.5, 24.35);

	n = 2;
	box.x = min(xrange) + (c(n-3, n) + 1) * diff(xrange) / 10;
	box.y = min(yrange) + (c(n-3, n) + 3) * diff(yrange) / 10;
	box.z = min(zrange) + (c(n-3, n) + 7) * diff(zrange) / 10;

	plot(0, 1, xlab="size of each half", ylab="histogram divergence", type="l", xlim=c(0, N/2), ylim=c(1e-5, 1), log="y",
			 main=paste("Divergences for the Lorenz Map (non-shuffled, rate 0.03s, m = ", m, ", d = ", d, ")."), col="#FFFFFF");

	rgl::plot3d(data);
	rgl::lines3d(t(rbind(box.x, box.y, box.z)), col=2, lwd=3);

	inBox = (data[,1] > box.x[1]) & (data[,1] < box.x[2]);
	inBox = inBox & (data[,2] > box.y[1]) & (data[,2] < box.y[2]);
	inBox = inBox & (data[,3] > box.z[1]) & (data[,3] < box.z[2]);
	
	print(inBox[seq(1, 10000, by=97)]);
	freq = sum(inBox) / nrow(data);

	print(freq);

	result = divergence.plot(data, box.x, box.y, box.z,
							 to=N/2, by=N%/%1000, shuffle=F, smooth=F);

	x = seq(0, max(result[,1]) * 1.1, length=1000);
	lines(x, sqrt(  (1 / (-2*x)) * log(0.05/2)  ), col=2)
	lines(x, sqrt(  (1 / (-2*x)) * log(0.001/2)  ), col=3)
	lines(x, sqrt(  (1 / (-2*x)) * log(1e-10/2)  ), col=4)

	legend("topright", c(expression(eta == 0.05), expression(eta == 0.001), expression(eta == 1e-10)), col=c(2, 3, 4), lwd=1)
	legend("topleft", "the probability to exceed the\ncolored lines is at most eta");

	#savePlot("histogram-rule-lorenz.png");
	result;
}

lorenz.all = function(data, divisions=10){
	N = nrow(data);
	m = 3;
	d = 3;

	xrange = range(data[,1]);
	yrange = range(data[,2]);
	zrange = range(data[,3]);

	plot(0, 1, xlab="size of each half", ylab="histogram divergence", type="l", xlim=c(0, N/2), ylim=c(1e-6, 1), log="y",
			 main=paste("Divergences for the Lorenz Map (non-shuffled, rate 0.03s, m = ", m, ", d = ", d, ")."), col="#FFFFFF");

	for(i in seq(0, divisions-1))
	for(j in seq(0, divisions-1))
	for(k in seq(0, divisions-1)){
		box.x = min(xrange) + c(i, i+1) * diff(xrange) / divisions;
		box.y = min(yrange) + c(j, j+1) * diff(yrange) / divisions;
		box.z = min(zrange) + c(k, k+1) * diff(zrange) / divisions;

		cat(i, j, k, sep="/");
		cat("\n");
		print(rbind(box.x, box.y, box.z));

		# if resampling
		#data = data[seq(1, nrow(data), by=5),]

		#rgl::plot3d(data);
		#rgl::lines3d(t(rbind(box.x, box.y, box.z)), col=2, lwd=3);

		inBox = (data[,1] > box.x[1]) & (data[,1] < box.x[2]);
		inBox = inBox & (data[,2] > box.y[1]) & (data[,2] < box.y[2]);
		inBox = inBox & (data[,3] > box.z[1]) & (data[,3] < box.z[2]);
		
		print(inBox[seq(1, 10000, by=97)]);
		freq = sum(inBox) / nrow(data);

		print(freq);

		if(freq == 0)
			break;

		result = divergence.plot(data, box.x, box.y, box.z,
								 to=N/2, by=N%/%1000, shuffle=F);
	}

	x = seq(0, max(result[,1]) * 1.1, length=1000);
	lines(x, sqrt(  (1 / (-2*x)) * log(0.05/2)  ), col=2);
	lines(x, sqrt(  (1 / (-2*x)) * log(0.001/2)  ), col=3);
	lines(x, sqrt(  (1 / (-2*x)) * log(1e-10/2)  ), col=4);

	legend("topright", c(expression(eta == 0.05), expression(eta == 0.001), expression(eta == 1e-10)), col=c(2, 3, 4), lwd=1);
	legend("topleft", "the probability to exceed the\ncolored lines is at most eta");

	savePlot("histogram-rule-lorenz.png");
}

#data = lorenz.real(500000); # 1,500,000 uses up about 60 MB

#result = lorenz.all(data);
result = lorenz.single(data);