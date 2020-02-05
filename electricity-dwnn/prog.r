require(tseriesChaos);

df = read.csv("../electricity-normalized.csv");

# sort dataset by date
df = df[sort.list(df$date),];

# The sampling time is not uniform; we select a region where it is uniform.
peaks = which(abs(diff(df$date)) > 0.05);
df = df[(peaks[2]+1):peaks[3],];
