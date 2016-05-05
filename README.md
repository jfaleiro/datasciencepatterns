Patterns in Data Science
========================

This is a collection of code excerpts, mostly in R, to address common puzzles in data sciences. They are organized by different bodies of knowledge and should serve as something similar to a cheat sheet. I am still in the process of gathering them from a number of different projects and course notes done in the past, so more content to be added and what is already in here might get further organized (time allowing).

Most of this content is not original, and is either copied from my own other projects or (the absolute majority) from lectures and class notes of the [Data Science Specialization at Johns Hopkins](http://www.jhsph.edu/departments/biostatistics).

**Note:** github has size limitations on serving HTML content, and most of this material is rendered as an HTML output from R chunks. If you have problems visualizing the content, click on the **[MD]** or **[RMD]** for the markup document.

[Reproducible Research](https://cdn.rawgit.com/jfaleiro/datasciencepatterns/master/ReproducibleResearch/index.html)
---

**[[MD]](ReproducibleResearch/index.md) // [[RMD]](ReproducibleResearch/index.Rmd)**

Reproducible research is the idea that data analyses, and more generally, scientific claims, are published with their data and software code so that others may verify the findings and build upon them. The need for reproducibility is increasing dramatically as data analyses become more complex, involving larger datasets and more sophisticated computations. Reproducibility allows for people to focus on the actual content of a data analysis, rather than on superficial details reported in a written summary. In addition, reproducibility makes an analysis more useful to others because the data and code that actually conducted the analysis are available. 

Sample Online Research [(Browse the Code)](https://github.com/jfaleiro/stormanalysis)

* [Health and Economic Impact of Severe Weather Events](http://rpubs.com/jfaleiro/severeweatherimpact)

[Statistical Inference](https://cdn.rawgit.com/jfaleiro/datasciencepatterns/master/StatisticalInference/index.html) 
---------------------

**[[MD]](StatisticalInference/index.md) // [[RMD]](StatisticalInference/index.Rmd)**

Statistical inference is the process of drawing conclusions about populations or scientific truths from data. There are many modes of performing inference including statistical modeling, data oriented strategies and explicit use of designs and randomization in analyses.

Sample Simulations [(Browse the Code)](https://github.com/jfaleiro/inferentialsimulations)

* [Comparative Analysis of Exponential Distributions and Properties of the Central Limit Theorem](https://github.com/jfaleiro/inferentialsimulations/blob/master/ExponentialDistributions.pdf)
* [Response Effect of Vitamin C on Odontoblast Length](https://github.com/jfaleiro/inferentialsimulations/blob/master/VitamicCEffectsOnOdontoblasts.pdf)

[Regression Models](https://cdn.rawgit.com/jfaleiro/datasciencepatterns/master/RegressionModels/index.html) 
-----------------

**[[MD]](RegressionModels/index.md) // [[RMD]](RegressionModels/index.Rmd)**

Linear models, as their name implies, relates an outcome to a set of predictors of interest using linear assumptions. Regression models, a subset of linear models, are the most important statistical analysis tool in data sciences. These notes cover regression analysis, least squares and inference using regression models.

Sample Report [(Browse the Code)](https://github.com/jfaleiro/carbonfootprint)

* [Predicting the Carbon Footprint of Automobiles Through Regression Models](https://github.com/jfaleiro/automobilecarbonfootprint/blob/master/PredictingAutomobileConsumption.pdf)

Machine Learning
----------------

Annotations and patterns related to machine learning came out to be very long, so we divided them further into 4 groups:

* [Prediction, Errors and Cross Validation](https://cdn.rawgit.com/jfaleiro/datasciencepatterns/master/MachineLearning/Prediction/index.html)

* [The Caret Package](https://cdn.rawgit.com/jfaleiro/datasciencepatterns/master/MachineLearning/CaretPackage/index.html)

* [Prediction Algorithms](https://cdn.rawgit.com/jfaleiro/datasciencepatterns/master/MachineLearning/Algorithms/index.html)

* [Regularized Regression and Combining Predictors](https://cdn.rawgit.com/jfaleiro/datasciencepatterns/master/MachineLearning/CombiningRegressors/index.html)



Annotations on tools and techniques for understanding, building, and testing prediction functions. Common tasks performed by data scientists and data analysts in prediction and machine learning. Basic grounding concepts related to selection of training and tests sets, overfitting, and error rates. A wide range of model based and algorithmic machine learning methods including regression, classification trees, Naive Bayes, and random forests. The complete process of building prediction functions including data collection, feature creation, algorithms, and evaluation.

[Data Products](https://cdn.rawgit.com/jfaleiro/datasciencepatterns/master/DataProducts/index.html) 
---------------

**[[MD]](DataProduct/index.md) // [[RMD]](DataProduct/index.Rmd)**

A data product is the final result of a statistical analysis for larger audiences. Data products automate complex analysis tasks or use technology to expand the utility of a data informed model, algorithm or inference. These are notes about the basics of creating data products as web applications, R packages, and interactive graphics.

Sample Applications [(Browse the Code)](https://github.com/jfaleiro/carbonfootprint)

* [Browsing Features Related to the Carbon Footprint of mid-70's Automobiles](https://jfaleiro.shinyapps.io/carbonfootprint/)
* [Data Analysis of Automobile Carbon Footprint](https://rpubs.com/jfaleiro/carbonfootprint)


