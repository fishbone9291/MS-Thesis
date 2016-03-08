install.packages('corrgram')
library(corrgram)
corrgram(afs_casement, upper.panel = panel.pts, diag.panel = panel.density, main ="Linear Relationships Between Numeric Variables")
