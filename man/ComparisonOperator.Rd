% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TextOperators.R
\name{ComparisonOperator}
\alias{ComparisonOperator}
\title{Create a Comparison Search Operator}
\usage{
ComparisonOperator(attribute, value, comparison_type)
}
\arguments{
\item{attribute}{The attribute to be compared. This should be the field within the RCSB PDB that you want to evaluate.}

\item{value}{The value to compare against. This is the reference value for the comparison.}

\item{comparison_type}{A string specifying the type of comparison (e.g., 'equal', 'greater_than', 'less_than').
Supported comparison types are 'equal', 'not_equal', 'greater_than', 'less_than', etc.}
}
\value{
An object of class `ComparisonOperator` that can be used in search queries to retrieve entries
        where the attribute meets the specified comparison criteria.
}
\description{
Constructs a `ComparisonOperator` object for search operations that perform comparison checks on attribute values.
This operator allows for evaluating attributes using comparison operators such as 'equal', 'greater_than',
or 'less_than', making it suitable for numerical and date-based searches.
}
\examples{
# Search for entries where an attribute equals a specific value
operator <- ComparisonOperator(attribute = "rcsb_entry_info.resolution_combined",
                                value = 2.0, comparison_type = "EQUAL")
operator
}
