require(tseriesChaos);
require(forecast);
require(rnn);

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


predict.dwnn = function(df, train.size=0.7){
	m = 4;
	d = 34;

	# x1 x2 x3
	# x2 x3 x4 ...
	emb = embedd(df$mean, m=m, d=d);

	beginTest = floor(nrow(emb) * train.size);
	train = emb[1:(beginTest-1),];
	test  = emb[beginTest:nrow(emb),];

	# Predict without replacement
	#sigmaSeq = seq(0.01, 0.5, length=100);
	#rmseVal = rep(0, length(sigmaSeq));
	#plot(sigmaSeq, rep(-1, length(sigmaSeq)), ylim=c(0, 100), cex=0);
	#for(i in seq_along(sigmaSeq)){
	#	Y = dwnn(train, test[,1:(m-1)], sigma=sigmaSeq[i]);
	#	squares = (test[,m] - Y)**2
	#	naCount = sum(is.na(squares));
	#	rmse = sqrt(mean(squares[!is.na(squares)]));
	#	rmseVal[i] = rmse;
	#	points(sigmaSeq[i], rmse, col=2, pch=19);
	#}
	#savePlot("rmse_by_sigma.png");
	#barplot(diff(rmseVal), type="l", names.arg=round(sigmaSeq[2:length(sigmaSeq)], 2));
	#savePlot("diff_barplot.png");
	#locator(1);

	Y = dwnn(train, test[,1:(m-1)], sigma=0.19);
	plot(test[,m], type="l");
	lines(Y, col=2);
	report(test[,m], Y);
	savePlot("dwnn-no-replacement.png");

	# Predict with replacement
	buffer = matrix(0, ncol=m, nrow=nrow(test));
	buffer[1,] = test[1,];
	buffer[1,m] = dwnn(train, test[1,1:(m-1)], sigma=0.19);
	for(i in 2:nrow(test)){
		query = buffer[i-1,2:m];
		y = dwnn(train, query, sigma=0.19);
		buffer[i,] = c(query, y);
	}

	plot(test[,m], type="l");
	lines(buffer[,m], col=2);
	report(test[,m], buffer[,m]);
	savePlot("dwnn-with-replacement.png");
}

predict.arima = function(df, train.size=0.7){
	beginTest = floor(length(df$mean) * train.size);
	train = df$mean[1:(beginTest-1)];
	test  = df$mean[beginTest:length(df$mean)];
	
	model = auto.arima(train);
	Y = predict(model, n.ahead=length(test));
	pred = Y$pred;
	se   = Y$se; # standard errors

	plot(test, type="l");
	lines(as.numeric(pred), col=2);
	savePlot("autoarima-onetime-prediction.png");

	predictions = c();
	for(i in 1:300){
		cat("Progress [Retrained ARIMA]:", i, "\n");
		model = auto.arima(train);
		Y = predict(model, n.ahead=1);
		pred = Y$pred[1];
		se   = Y$se; # standard errors
		predictions = c(predictions, pred);
		train = c(train, pred);
	}

	plot(test[1:300], type="l");
	lines(1:300, predictions, col=2);
	savePlot("autoarima-retrained-prediction.png");
}

predict.chaotic.rnn = function(df, train.size=0.7){
	m = 4;
	d = 34;

	# Unfortunately we will have to normalize the time series this time\
	df$mean = df$mean / (max(df$mean) * 1.2);

	# x1 x2 x3
	# x2 x3 x4 ...
	emb = embedd(df$mean, m=m, d=d);
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
		learningrate   = 0.08,
		hidden_dim     = 30,
		batch_size     = 100,
		numepochs      = 100,
		seq_to_seq_unsync=TRUE);
	
	# For m=8, d=34
#	model <- trainr(Y=trainY,
#		X=trainX,
#		learningrate   = 0.06,
#		hidden_dim     = 30,
#		batch_size     = 100,
#		numepochs      = 100,
#		seq_to_seq_unsync=TRUE);

	# predict
	preds <- predictr(model, testX);

	plot(testY, type="l");
	lines(preds[,1], col=2);
	report(testY, preds[,1]);
	savePlot("chaotic-rnn.png");
}



df = read.csv("../sunspot.csv", header=F, sep=";");
colnames(df) = c("year", "month", "numdate", "mean", "sd", "samples", "def-or-prov");

# predict.dwnn(df);
# predict.arima(df);
predict.chaotic.rnn(df);
