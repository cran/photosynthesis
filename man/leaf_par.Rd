% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/leaf-par.R
\name{leaf_par}
\alias{leaf_par}
\title{S3 class leaf_par}
\usage{
leaf_par(.x, use_tealeaves)
}
\arguments{
\item{.x}{A list to be constructed into \strong{leaf_par}.}

\item{use_tealeaves}{Logical. Should leaf energy balance be used to calculate leaf temperature (T_leaf)? If TRUE, \code{\link[tealeaves:tleaves]{tleaf()}} calculates T_leaf. If FALSE, user-defined T_leaf is used. Additional parameters and constants are required, see \code{\link[=make_parameters]{make_parameters()}}.}
}
\value{
Constructor function for leaf_par class. This function ensures that leaf
parameter inputs are properly formatted.
}
\description{
S3 class leaf_par
}
