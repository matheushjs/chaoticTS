require(tseriesChaos)

data = read.csv("NOAA-vancouver.csv");
N = nrow(data);

prcp = data$PRCP;
series = NULL;
for(i in 1:(length(prcp) - 30)){
	month = prcp[i:(i+30)];
	month = month[!is.na(month)];
	series = c(series, mean(month));
}

# make it have the correct length, by appending NAs
series = c(series, rep(NA, N - length(series)));
data[,"PRCP30"] = series;

series = NULL;
for(i in 1:(length(prcp) - 7)){
	week = prcp[i:(i+7)];
	week = week[!is.na(week)];
	series = c(series, mean(week));
}

series = c(series, rep(NA, N - length(series)));
data[,"PRCP7"] = series;

tmax = data$TMAX;
idx = which(is.na(tmax));
for(i in idx){
	many = tmax[c(i-2*365, i-2*365+1, i-365, i-366, (i-3):(i+3) )];
	many = many[!is.na(many)];
	tmax[i] = mean(many);
}

tmin = data$TMIN;
idx = which(is.na(tmin));
for(i in idx){
	many = tmin[c(i-2*365, i-2*365+1, i-365, i-366, (i-3):(i+3) )];
	many = many[!is.na(many)];
	tmin[i] = mean(many);
}

data$TMAX = tmax;
data$TMIN = tmin;
data[,"TAVG"] = (tmax + tmin)/2;

# Remove some columns
data = data[,-c(1,3,5,6,9)];

write.csv(data, "NOAA-vancouver-clean.csv", row.names=F);
