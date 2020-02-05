require(tseriesChaos);

df = read.csv("../sunspot.csv", header=F, sep=";");
colnames(df) = c("year", "month", "numdate", "mean", "sd", "samples", "def-or-prov");

m = 4;
d = 34;

# x1 x2 x3
# x2 x3 x4 ...
emb = embedd(df$mean, m=m, d=d);

# Receives the dataset, which is a matrix with rows (x1, x2, x3, y)
# And receives the query point (x1, x2, x3)
dwnn = function(dataset, query, sigma=0.5){
	yIdx = ncol(dataset);
	squareDist = sum((dataset[,1:(yIdx-1)] - query)**2);
	weights = exp(-squareDist / (2 * sigma**2));
}
