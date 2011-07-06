# Title: Blood Transfusion Service Center Data Set
# Source: UCI Machine Learning Repository
# Original Source: Data taken from the Blood Transfusion Service Center in Hsin-Chu City 
# in Taiwan -- this is a classification problem.

# Data Set Characteristics: Multivariate
# Number of Instances: 748
# Area: Business
# Attribute Characteristics: Real
# Number of Attributes: 5
# Date Donated: 2008-10-03

download.file(
  url = "http://archive.ics.uci.edu/ml/machine-learning-databases/blood-transfusion/transfusion.names",
  destfile = "transfusion.names"
)
download.file(
  url = "http://archive.ics.uci.edu/ml/machine-learning-databases/blood-transfusion/transfusion.data",
  destfile = "transfusion.data"
)
