\name{pmg-dynamic}
\alias{dLatticeExplorer}


\title{"Dynamic" widgets for pmg}
\description{
	We call a widget "dynamic" if it updates itself immediately
	when an event occurs, such as a drag and drop, or a change in
	some value. The dynamic widgets documented here, are meant to
	provide quick, easy (but limited) access to R's modeling
	functions, R's significance tests, and R's lattice functions
}
\usage{
dLatticeExplorer(container = NULL, ...)
}
\arguments{
  \item{container}{A container to attach the object to}
  \item{\dots}{Currently ignored}
}

\details{
	For each "dynamic" widget, the variables can be specified by
	drag and drop, or by editing the widget. The bold-face areas
	of each widget can be edited by clicking on them or by
	dropping values. If the drop value comes from a column of an
	\code{idf} instance, then when that column is edited, the
	dynamic widget is updated. Such variables can not be edited or
	changed. Other variables may, such as writing powers, or
	applying functions.

	The "dynamic" widgets are meant for easy exploration, but not
	for saving of actions. 

	The \code{ilatticeexplorer} function creates a dynamic
	graphing widget based on  \code{lattice} graphics. Up to three
	variables (only 2 for univariate graphs) may be dropped on the
	widget. The order is for univariate graphs: \code{~x} then
	\code{~x | y}. And for bivariate graphs \code{x}, \code{x ~
	y}, \code{x ~ y | z}. The panel functions add to the plots of
	dots by, typically, incorporating some trend line.

}
\value{
	Although there are methods for \code{dModelsDialog}, these
	widgets aren't meant to be interacted with from the command line.
	}
\author{John Verzani}
\note{
	Some of the usability was inspired by the Fathom software.
}
\examples{
\dontrun{
dLatticeExplorer()
}
}
\keyword{interface }% at least one, from doc/KEYWORDS
