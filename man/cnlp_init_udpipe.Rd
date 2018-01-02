% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/backend_udpipe.R
\name{cnlp_init_udpipe}
\alias{cnlp_init_udpipe}
\title{Interface for initializing the udpipe backend}
\usage{
cnlp_init_udpipe(model_name = NULL, model_path = NULL)
}
\arguments{
\item{model_name}{string giving the model namel.
Defaults to "english" if NULL.
Ignored if \code{model_path} is not NULL.}

\item{model_path}{provide a full path to a model file.}
}
\description{
This function must be run before annotating text with
the tokenizers backend. It sets the properties for the
spaCy engine and loads the file using the R to Python
interface provided by reticulate.
}
\examples{
\dontrun{
cnlp_init_udpipe(model_name = "english")
}

}
\author{
Taylor B. Arnold, \email{taylor.arnold@acm.org}
}