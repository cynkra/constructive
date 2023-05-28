test_that("s4", {
  expect_snapshot({
    track <- setClass("track", slots = c(x="numeric", y="numeric"))
    construct(track)

    t1 <- track(x = 1:3, y = 4:6)
    construct(t1)

    trackCurve <- setClass("trackCurve",  slots = c(smooth = "numeric"), contains = "track")
    construct(trackCurve)

    t1s <- trackCurve(t1, smooth = 1:3)
    construct(t1s)

    construct(prototype(1, a = 2))
    construct(getClass("numeric"))
  })
})
