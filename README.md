# classify

classify is a framework to streamline the evaluation of supervised
classification methods. 

Classifiers come and go as fads, while some disciplines prefer specific
classification methods over others. To reduce subjectivity in classifier
specification, `classify` seeks to determine the best classifiers on a variety of benchmark data sets with a collection of benchmark metrics. 

Typically, classifier superiority is determined by classification error rate (sometimes called
probability of misclassification or confusion rate). To assess classification
efficacy, we utilize the following error rate estimators:
* random split / Monte-Carlo cross-validation
* cross-validation (KNOWN ISSUE)
* bootstrap (NOT IMPLEMENTED YET)
* .632 (NOT IMPLEMENTED YET)
* .632+ (NOT IMPLEMENTED YET)
* apparent (NOT IMPLEMENTED YET)

Other metrics, such as sensitivity/specificity, are planned.
