% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FigFordeling.R
\name{FigFordeling}
\alias{FigFordeling}
\title{Fordelingsfigur}
\usage{
FigFordeling(
  AggVerdier,
  tittel = "mangler tittel",
  hovedgrTxt = "",
  smltxt = "",
  N,
  Nfig = 0,
  retn = "H",
  subtxt = "",
  utvalgTxt = "",
  grtxt,
  grtxt2 = "",
  medSml = 0,
  antDes = 1,
  fargepalett = "BlaaOff",
  outfile = ""
)
}
\arguments{
\item{AggVerdier}{Aggregerte verdier (andeler) som skal visualiseres. Liste med to vektorer Hoved og Rest}

\item{tittel}{Figurtittel}

\item{hovedgrTxt}{hovedgruppe, eks. hele landet eller UNN, Tromsø}

\item{smltxt}{sammenligningstekst, eks. landet forøvrig}

\item{N}{Totalt antall. Listevariabel: N$Hoved/Rest - antall i hovedgruppa/sammenligningsgruppa}

\item{Nfig}{Totalt antall til visning i tekstetikett. Kan ha ulikt antall når sammensatte variabler.}

\item{retn}{V-vertikale søyler. H-horisontale søyler}

\item{utvalgTxt}{Tekst over flere linjer som angir hvilket utvalg som er gjort}

\item{grtxt}{Tekst som angir navn på hver av gruppene i fordelinga.Vektor med lenge lik antall grupper/søyler}

\item{grtxt2}{Evt. undertekst til hver av gruppene. Vektor med lenge lik antall grupper/søyler}

\item{medSml}{0-med sammenlikningsgruppe, 1-uten sammenlikningsgruppe}

\item{antDes}{antall desimaler i visning av andel som prosent}

\item{fargepalett}{hvilken fargepalett fra "rapfig::figfiltype" som skal benyttes}

\item{outfile}{hvordan figuren skal vises: ''-på Skjerm, "filnavn.pdf/png/.." - gir fil av angitt type.}
}
\value{
Søylediagram (fordeling).
}
\description{
Denne funksjonen lager et horisontalt eller vertikalt søylediagram som viser andeler (fordeling)
av en gitt variabel, basert på de aggregerte verdier som gis inn til funksjonen.
}
