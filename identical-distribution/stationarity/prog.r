require(tseriesChaos)

data = embedd(lorenz.ts, m=3, d=1);
result = NULL

norm = function(vec){
	sqrt(sum(vec**2));
}

for(i in 10:996){
	half1 = data[1:i,];
	half2 = data[(i+1):(2*i),];
	d = abs( norm(colMeans(half1)) - norm(colMeans(half2)) )
	result = rbind(result, d);
	plot(result, type="l");
}

