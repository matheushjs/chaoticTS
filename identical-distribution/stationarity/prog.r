require(tseriesChaos)

graphics.off();
dev.new(width=0.8*12, height=0.8*8);

lorenz.ts <- sim.cont(lorenz.syst, 0, 300, 0.03,
     start.x=c(5,5,5), parms=c(10, 28, -8/3), obs.fun = function(x)
     x[1]);
     #sqrt(sum(x**2)));

data = embedd(lorenz.ts, m=3, d=3);
result = NULL

norm = function(vec){
	sqrt(sum(vec**2));
}

#data = data[sample(1:nrow(data)),];
for(i in 5:3996){
	nrows = nrow(data);
	half1 = data[1:i,];
	half2 = data[(nrows/2 + 1):(nrows/2 + i),];
	d = max(abs(colMeans(half1) - colMeans(half2)));
	result = rbind(result, d);
	plot(5:i, result, xlab="size of each half", ylab="divergence", type="l", xlim=c(0, 4000));
}

range = max(data) - min(data);
x = seq(0, 4000, length=1000);
lines(x, sqrt( range * log(0.1/(2*3)) / (-2*x) ), col=2)
lines(x, sqrt( range * log(0.05/(2*3)) / (-2*x) ), col=3)
lines(x, sqrt( range * log(0.01/(2*3)) / (-2*x) ), col=4)

legend("topright", c(expression(eta == 0.1), expression(eta == 0.05), expression(eta == 0.01)), col=c(2, 3, 4), lwd=1)

savePlot("independence-criterion.png");
