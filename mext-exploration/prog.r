require(tseriesChaos);
require(rnn);
require(viridis)

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
	squares1 = (correct - obtained)**2
	squares2 = (correct - mean(correct))**2
	naCount = sum(is.na(squares1));
	nmse = sum(squares1) / sum(squares2);
	cat("NMSE: ", nmse, "\tNA samples: ", naCount, "\n");
	return(nmse);
}

predict.dwnn = function(emb, train.size=0.7, tune=F){
	m = ncol(emb);

	beginTest = floor(nrow(emb) * train.size);
	train = emb[1:(beginTest-1),];
	test  = emb[beginTest:nrow(emb),];

	# Predict without replacement
	bestSigma = 0.037;
	
	if(tune){
		sigmaSeq = seq(0.001, 0.1, length=100);
		#sigmaSeq = seq(1, 2, length=50);
		rmseVal = rep(0, length(sigmaSeq));
		for(i in seq_along(sigmaSeq)){
			Y = dwnn(train, test[,1:(m-1)], sigma=sigmaSeq[i]);
			squares = (test[,m] - Y)**2
			naCount = sum(is.na(squares));
			rmse = sqrt(mean(squares[!is.na(squares)]));
			print(rmse)
			rmseVal[i] = rmse;
			plot(sigmaSeq[1:i], rmseVal[1:i], col=2, pch=19, xlim=range(sigmaSeq));
		}
		savePlot("rmse_by_sigma.png");

		bestSigma = sigmaSeq[which.min(rmseVal)];
		cat("Best sigma is: ", bestSigma, "\n");
	}

	Y = dwnn(train, test[,1:(m-1)], sigma=bestSigma);
	plot(test[,m][1:300], type="l", xlab="Time", ylab="Sunspots", lwd=3);
	points(Y[1:300], col=viridis(5)[4], pch=19, cex=1.2);
	lines(Y[1:300], col=viridis(5)[4], lwd=3);
	rmse = report(test[,m], Y);
	savePlot("dwnn-no-replacement.png");
	return(rmse);

	# Predict with replacement
	#buffer = matrix(0, ncol=m, nrow=nrow(test));
	#buffer[1,] = test[1,];
	#buffer[1,m] = dwnn(train, test[1,1:(m-1)], sigma=0.19);
	#for(i in 2:nrow(test)){
	#	query = buffer[i-1,2:m];
	#	y = dwnn(train, query, sigma=0.19);
	#	buffer[i,] = c(query, y);
	#}

	#plot(test[,m], type="l", xlab="Time", ylab="Sunspots");
	#lines(buffer[,m], col=2);
	#report(test[,m], buffer[,m]);
	#savePlot("dwnn-with-replacement.png");
}

predict.chaotic.rnn = function(emb, train.size=0.7){
	m = ncol(emb);

	X = emb[,1:(m-1)];
	Y = emb[,m];

	# Convert to array as desired by the `rnn` package
	X = array(X, dim=c(dim(X), 1));
	Y = array(Y, dim=c(length(Y), 1));

	# Separate in train / test
	beginTest = floor(nrow(emb) * train.size);
	trainX = X[1:(beginTest-1),,,drop=F];
	trainY = Y[1:(beginTest-1),,drop=F];
	testX  = X[beginTest:nrow(emb),,,drop=F];
	testY  = Y[beginTest:nrow(emb),,drop=F];

	model <- trainr(Y=trainY,
		X=trainX,
		learningrate   = 0.07,
		# learningrate   = 0.04,   # For m=8, d=17
		hidden_dim     = 18,
		batch_size     = 100,
		numepochs      = 200,
		momentum       = 0.1,
		use_bias       = TRUE,
		seq_to_seq_unsync=TRUE);
	
#	model <- trainr(Y=trainY,
#		X=trainX,
#		learningrate   = 0.06,
#		hidden_dim     = 30,
#		batch_size     = 100,
#		numepochs      = 1000,
#		seq_to_seq_unsync=TRUE);

	# predict
	preds <- predictr(model, testX);

	plot(testY, type="l", xlab="Time", ylab="Sunspots", lwd=3);
	lines(preds[,1], col=2, lwd=3, lty=3);
	rmse = report(testY, preds[,1]);
	savePlot("chaotic-rnn.png");

	return(rmse);
}

graphics.off();
dev.new(width=14, height=6);

df = read.csv("../sunspot.csv", header=F, sep=";");
colnames(df) = c("year", "month", "numdate", "mean", "sd", "samples", "def-or-prov");
series = df$mean;

#series = read.table("../lor63.dat", header=F)$V1[17001:20000];
#series = sin(2*pi*seq(0,100, length=5000))

# Take care here
# lags = c(0,34,34*2,34*3) will feed 34 first to the RNN, and 34*3 lastly (more near the RNN output)
#emb = embedd(df$mean, m=4, d=1);

series = (series / max(abs(series)) + 1) / 2.4;
emb = embedd(series, lags=c(0,c(4,3,2,1)));
#emb = embedd(series, lags=c(0,3,6));

#A = emb[,1:(ncol(emb - 1))];
#B = emb[,ncol(emb)];
#prev = df$mean[1:(length(df$mean)-1)];
#prev = rev(rev(prev)[1:length(B)]);
#emb = cbind(A, prev, B);

rmse1 = predict.dwnn(emb);
#rmse2 = predict.chaotic.rnn(emb);
#print(c(rmse1, rmse2));
