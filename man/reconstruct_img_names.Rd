% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_raw_metadata.R
\name{reconstruct_img_names}
\alias{reconstruct_img_names}
\title{Recover original labels from data structure}
\usage{
reconstruct_img_names(df_structure, pattern)
}
\arguments{
\item{df_structure}{The df with the data structure.}

\item{pattern}{The labeling pattern followed by the original image filenames,
as a regex expression with named groups.}
}
\value{
A list of reconstructed file names (without filetype extension)
}
\description{
Inverse of extract_data_structure, i.e. given a dataframe with the
data structure (site, species, tree, woodpiece, sample, image), and the
labeling pattern followed by the original image filenames, we reconstruct
the filenames.
This requires that all files follow the same labeling pattern.
}
\details{
NOTE: At the moment, only tested with following pattern:
\verb{\{site\}_\{species\}_\{tree\}\{woodpiece\}_\{sample\}_\{image\}}
where tree is assumed to be a two-character code, and woodpiece is optional.
}
