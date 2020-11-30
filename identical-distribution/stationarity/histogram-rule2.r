require(tseriesChaos);
require(RTisean);
require(colorspace)
require(viridis)

#palette(qualitative_hcl("Set 2", n=9));
palette(c("black", "blue", "red"))

embedd = tseriesChaos::embedd;
sim.cont = tseriesChaos::sim.cont;
lorenz.s0 = c(9.227021424263037729929, 15.117356048661351408668, 17.911512308502612000893);

if(length(lorenz.ts) == 2001)
	lorenz.ts = NULL;

graphics.off();
dev.new(width=0.6*12, height=0.6*8);

logistic = function(iter=40000, r=4){
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

norm1 = function(vec){ sum(abs(vec)); }
norm2 = function(vec){ sqrt(sum(vec**2)); }
normInf = function(vec){ max(abs(vec)); }

embedd.custom = function(series, indices){
	subseries = list();
	for(l in indices){
		subseries[[length(subseries) + 1]] = series[l:length(series)];
	}

	minLength = length(series) - max(indices);
	result = NULL;
	for(i in 1:length(subseries)){
		result = cbind(result, subseries[[i]][1:minLength] );
	}

	result;
}


