# AsIs

    Code
      construct(I(month.abb))
    Output
      I(c(
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
        "Oct", "Nov", "Dec"
      ))

---

    Code
      construct(I(head(cars, 2)))
    Output
      I(data.frame(speed = c(4, 4), dist = c(2, 10)))

