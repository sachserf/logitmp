\name{logitmp}
\alias{logitmp}
\title{
logistic model performance (logitmp)
}
\description{
The function computes several performance measures
for logistic regression models. Furthermore it creates a ROC-plot and a calibration-plot.
There are several options to customize the output.
}
\usage{
logitmp(list_of_models,
        plots = TRUE,
        abbreviations = TRUE,
        abbreviations_names = "data",
        abbreviations_id = "numeric",
        percentual = TRUE,
        annotations = FALSE,
        color = FALSE)
}
\arguments{
  \item{list_of_models}{
Requires a list containing objects of type glm(formula, family=binomial(logit)). Even if there is only one glm-object it should be stored inside a list (e.g. list(my_model)).
}
  \item{plots}{
A logical value indicating wether the plots should be printed or not.
}
  \item{abbreviations}{
A logical value indicating whether the names of the objects in 'list_of_models' should be abbreviated or not. For enhanced readability the default is 'TRUE'.
}
  \item{abbreviations_names}{
If 'abbreviations=TRUE': Label for the objects in 'list_of_models'. Default is "data".
}
  \item{abbreviations_id}{
If 'abbreviations=TRUE': A character vector specifying the ID for 'list_of_models'. Choose between numbers ("numeric"), uppercase letters ("LETTERS") or lowercase letters ("letters"). Default is "numeric".
}
  \item{percentual}{
A logical value indicating wether an additional confusion matrix (given the percentual values of the standard confusion matrix) should be printed or not.
}
  \item{annotations}{
A logical value. If 'TRUE' supplemental information about the measures is printed.
}
  \item{color}{
A logical value indicating whether the plots should be coloured or not.
}
}
\value{
The output is a list containing four dataframes.
\item{confusion_matrix }{contains the confusion matrix}
\item{calibration_plot_data }{contains additional data about the calibration plot}
\item{ROC_data }{contains additional data about the ROC-plot}
\item{measures }{contains a dataframe of the measures for classification quality}
By calling the function some pre-formatted tables and plots will be printed on the screen.
}
\references{
For further information please refer to:

Crawley, Michael J. 2007. The R Book. Chichester, England; Hoboken, N.J.: Wiley.

Fielding, Alan H., and John F. Bell. 1997. “A Review of Methods for the Assessment of Prediction Errors in Conservation Presence/absence Models.” Environmental Conservation 24 (01): 38–49.

Powers, David Martin. 2011. “Evaluation: From Precision, Recall and F-Measure to ROC, Informedness, Markedness and Correlation.” Journal of Machine Learning Technologies 2 (1): 37–63.

Allouche, Omri, Asaf Tsoar, and Ronen Kadmon. 2006. “Assessing the Accuracy of Species Distribution Models: Prevalence, Kappa and the True Skill Statistic (TSS).” Journal of Applied Ecology 43 (6): 1223–32.

Subhash R. Lele, Jonah L. Keim and Peter Solymos (2014). ResourceSelection: Resource Selection (Probability) Functions for Use-Availability Data. R package version 0.2-4.

Minato Nakazawa (2014). fmsb: Functions for medical statistics book with some demographic data. R package version 0.4.4.

Xavier Robin, Natacha Turck, Alexandre Hainard, Natalia Tiberti, Frédérique Lisacek, Jean-Charles Sanchez and Markus Müller (2011). pROC: an open-source package for R and S+ to analyze and compare ROC curves. BMC Bioinformatics, 12, p. 77
}

\author{
Frederik Sachser
}
\note{
Indexing and formatting is straightforward. For example try:

logitmp(list(your_model_here))[["measures"]][c("kappa", "AUC", "Nagelkerke R²"),]
}
\section{Warning }{The function will install and attach
the packages 'ResourceSelection' (for hoslem.test) and 'pROC' (for AUC and ROC-plot).
If attached the package 'fmsb' will be detached.}
\seealso{
ResourceSelection::hoslem.test

fsmb::NagelkerkeR2

pROC::roc
}
\examples{
testdata <- iris[-which(iris$Species == "setosa"),]
testdata$observed <- ifelse(testdata$Species == "virginica", 0,1)
testdata <- testdata[sample(row.names(testdata), size = 85, replace = FALSE),]
iris_model_sepal <- glm(observed ~ Sepal.Length + Sepal.Width, data = testdata, family=binomial(logit))
iris_model_petal <- glm(observed ~ Petal.Length + Petal.Width, data = testdata, family=binomial(logit))
logitmp(list(iris_model_petal, iris_model_sepal))
}
\keyword{ logistic regression }
\keyword{ model performance }
