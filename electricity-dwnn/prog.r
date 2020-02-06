require(tseriesChaos);

# Receives the dataset, which is a matrix with rows (x1, x2, x3, y)
# And receives the query point (x1, x2, x3)
dwnn = function(dataset, query, sigma=0.5){
	yIdx   = ncol(dataset);
	nSamps = nrow(dataset);
	X = as.matrix(dataset[,1:(yIdx-1)], ncol=1);
	Y = as.matrix(dataset[,yIdx], ncol=1);

	if(!is.matrix(query))
		query = matrix(query, nrow=1)

	# Each column of squareDist and weights are relative to each query
	squareDist = apply(query, 1, function(row) rowSums(X - row)**2);
	weights = exp(-squareDist / (2 * sigma**2));

	result = (t(weights) %*% Y) / colSums(weights);
	return(result);
}

report = function(correct, obtained){
	squares = (correct - obtained)**2
	naCount = sum(is.na(squares));
	rmse = sqrt(mean(squares[!is.na(squares)]));
	cat("RMSE: ", rmse, "\tNA samples: ", naCount, "\n");
}


df = read.csv("../sunspot.csv", header=F, sep=";");
colnames(df) = c("year", "month", "numdate", "mean", "sd", "samples", "def-or-prov");

m = 4;
d = 34;

# x1 x2 x3
# x2 x3 x4 ...
emb = embedd(df$mean, m=m, d=d);

beginTest = floor(nrow(emb) * 0.7);
train = emb[1:(beginTest-1),];
test  = emb[beginTest:nrow(emb),];

# Predict without replacement
Y = dwnn(train, test[,1:(m-1)], sigma=0.5);

plot(test[,m], type="l");
lines(Y, col=2);
report(test[,m], Y);
savePlot("dwnn-no-replacement.png");

# Predict with replacement
buffer = matrix(0, ncol=m, nrow=nrow(test));
buffer[1,] = test[1,];
buffer[1,m] = dwnn(train, test[1,1:(m-1)]);
for(i in 2:nrow(test)){
	query = buffer[i-1,2:m];
	y = dwnn(train, query);
	buffer[i,] = c(query, y);
}

plot(test[,m], type="l");
lines(buffer[,m], col=2);
report(test[,m], buffer[,m]);
savePlot("dwnn-with-replacement.png");
