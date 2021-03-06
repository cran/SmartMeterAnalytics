% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/F_smote_oversampling.R
\name{smote}
\alias{smote}
\title{Synthetic minority oversampling (SMOTE)}
\usage{
smote(
  Variables,
  Classes,
  subset_use = NULL,
  k = 5,
  use_nearest = TRUE,
  proportions = 0.9,
  equalise_with_undersampling = FALSE,
  safe = FALSE
)
}
\arguments{
\item{Variables}{the \link{data.frame} of independent variables that should be used to create new instances}

\item{Classes}{the class labels in the prediction problem}

\item{subset_use}{a specific subset only is used for the oversampling. If \link{NULL}, everything is used.}

\item{k}{the number of neigbours for generation}

\item{use_nearest}{should only the nearest neighbours be used? (very slow)}

\item{proportions}{to which proportion (of the biggest class) should the classes be equalized}

\item{equalise_with_undersampling}{should additional undersampling be performed?}

\item{safe}{should a safe version of SMOTE be used?}
}
\value{
a list containing new independent variables \link{data.frame} and new class labels
}
\description{
Performs oversampling by creating new instances.
}
\details{
SMOTE is used to generate synthetic datapoints of a smaller class, for example
to overcome the problem of imbalanced classes in classification.
}
\author{
Ilya Kozlovskiy, Konstantin Hopf \email{konstantin.hopf@uni-bamberg.de}
}
