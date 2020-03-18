require(tseriesChaos);
require(viridis);

N_POINTS = 5;
N_SAMPLES = 15;

X <- as.numeric(sim.cont(lorenz.syst, 0, 1000, 0.05,
	start.x=c(5,5,5), parms=c(10, 28, -8/3),
	obs.fun = function(x) x[1]));
Y <- as.numeric(sim.cont(lorenz.syst, 0, 1000, 0.05,
	start.x=c(5,5,5), parms=c(10, 28, -8/3),
	obs.fun = function(x) x[2]));
Z <- as.numeric(sim.cont(lorenz.syst, 0, 1000, 0.05,
	start.x=c(5,5,5), parms=c(10, 28, -8/3),
	obs.fun = function(x) x[3]));

m = data.frame(X=X, Y=Y, Z=Z);
len = nrow(m);

graphics.off();
dev.new(width=12, height=6);
set.seed(1);
plot(-10000, -10000, xlim=range(m$X)*0.8, ylim=range(m$Z)+c(3, -5));

for(i in seq(N_SAMPLES)){
	start_point = floor(runif(min=1, max=len - N_POINTS, n=1));
	slice = start_point:(start_point + N_POINTS - 1);
	selection = m[slice,c("X","Z")];
	from = selection[1:(N_POINTS-1),];
	to   = selection[2:N_POINTS,];
	diff = to - from;
	to   = to - 0.1*diff;
	#lines(m$X[slice], m$Z[slice]);
	arrows(from[,1], from[,2],
		   to[,1], to[,2],
		   length=0.1);
	#points(m$X[slice], m$Z[slice], pch=19, cex=1.2, col=grey(seq(0, 0.5, length=N_SAMPLES)[i]));
	points(m$X[slice], m$Z[slice], pch=19, cex=1.2, col=viridis(N_SAMPLES + 1)[i]);
}
savePlot("possible_random_experiments.png");


dev.new(width=10, height=8);
plot(-10000, -10000, xlim=range(m$X), ylim=range(m$Z));

set.seed(1);
N_SAMPLES = 1000;
POINT_IDX = 1;
start_point = floor(runif(min=1, max=len - POINT_IDX + 1, n=N_SAMPLES));
idx = start_point;
points(m$X[idx], m$Z[idx], pch=19, cex=1);
#savePlot("possible_first_point.png");

set.seed(NULL);
