# Encoding

    Code
      construct(data.frame(x = c("ü", "a"), y = c(
        "loooooooooooooooooooooooooooooooooong_enough_for_multiline_output")))
    Output
      data.frame(
        x = c("\U{FC}", "a"),
        y = "loooooooooooooooooooooooooooooooooong_enough_for_multiline_output"
      )

# non UTF-8 encodings with UTF-8 system

    Code
      x <- iconv("hello world", to = "latin1")
      construct(x)
    Output
      "hello\xa0world" |> (`Encoding<-`)("latin1")
    Code
      x <- iconv("こんにちは", to = "shift_jis")
      construct(x)
    Output
      "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"
    Code
      x <- "hello\xa0world"
      Encoding(x) <- "latin1"
      construct(x)
    Output
      "hello\xa0world" |> (`Encoding<-`)("latin1")

