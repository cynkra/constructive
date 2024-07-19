# yearmon

    Code
      x <- zoo::as.yearmon("2007-12")
      construct(x)
    Output
      zoo::as.yearmon("Dec 2007")
    Code
      construct(x, opts_yearmon("yearmon"))
    Output
      zoo::yearmon(2007.9166666666667)

