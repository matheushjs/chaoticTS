require(tseriesChaos);

#lorenz.ts <- sim.cont(lorenz.syst, 0, 40000, 0.05,
#     start.x=c(5,5,5), parms=c(10, 28, -8/3), obs.fun = function(x) x[1]);

lorenz.ts = read.table("lor.dat");

states = embedd(lorenz.ts, m=2, d=4);

plotIdx = function(idx){
	maxLength = nrow(states);

	all = NULL;
	for(i in 1:4000){
		rand = round(runif(n=1, min=1, max=maxLength-idx));
		state = states[rand+idx,];
		all = rbind(all, state);
	}

	plot(all, pch=19, col="#00000033");
}

dev.new(width=6, height=6);
plotIdx(1);
savePlot("lorenz-equidistribution1.png");

dev.new(width=6, height=6);
plotIdx(10);
savePlot("lorenz-equidistribution2.png");

graphics.off();
