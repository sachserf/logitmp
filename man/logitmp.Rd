\name{logitmp}
\alias{logitmp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
logistic model performance (logitmp)
}
\description{
The function combines several performance measures
for logistic regression models. Furthermore it creates a ROC-plot and a calibration-plot.
There are several options to customize the output.

The function will install and attach
the packages 'ResourceSelection' (for hoslem.test) and 'pROC' (for AUC and ROC-plot).
If attached the package 'fmsb' will be detached.
}
\usage{
logitmp(list_of_models,
        plots = TRUE,
        abbreviations = TRUE,
        abbreviations_names = "data",
        abbreviations_id = c("numeric", "letters", "LETTERS"),
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
If 'abbreviations=TRUE': A character vector specifying the ID for 'list_of_models'. Choose between numbers, uppercase or lowercase letters. Default is "numeric".
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
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Frederik Sachser
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
