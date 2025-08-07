# construct_diff

    Code
      construct_diff(list(a = head(cars, 2), b = "aaaaaaaaaaaaaaaaaaaa", c = "Foo"),
      list(a = head(iris, 1), b = "aaaaaaaaaaaaaaaaaaaa", c = "foo"), interactive = FALSE)
    Output
      < list(a = head(cars, 2), b = "aaaaaa..  > list(a = head(iris, 1), b = "aaaaaa..
      @@ 1,5 @@                                @@ 1,11 @@                             
        list(                                    list(                                
      <   a = data.frame(speed = 4, dist = c(  >   a = data.frame(                    
      : 2, 10)),                               ~                                      
      ~                                        >     Sepal.Length = 5.1,              
      ~                                        >     Sepal.Width = 3.5,               
      ~                                        >     Petal.Length = 1.4,              
      ~                                        >     Petal.Width = 0.2,               
      ~                                        >     Species = factor("setosa", levels
      ~                                        :  = c("setosa", "versicolor", "virgini
      ~                                        : ca"))                                
      ~                                        >   ),                                 
          b = "aaaaaaaaaaaaaaaaaaaa",              b = "aaaaaaaaaaaaaaaaaaaa",        
      <   c = "Foo"                            >   c = "foo"                          
        )                                        )                                    
    Code
      construct_diff(list(a = head(cars, 2), b = "aaaaaaaaaaaaaaaaaaaa", c = "Foo"),
      list(a = head(iris, 1), b = "aaaaaaaaaaaaaaaaaaaa", c = "foo"), interactive = FALSE)
    Output
      < list(a = head(cars, 2), b = "aaaaaa..  > list(a = head(iris, 1), b = "aaaaaa..
      @@ 1,5 @@                                @@ 1,11 @@                             
        list(                                    list(                                
      <   a = data.frame(speed = 4, dist = c(  >   a = data.frame(                    
      : 2, 10)),                               ~                                      
      ~                                        >     Sepal.Length = 5.1,              
      ~                                        >     Sepal.Width = 3.5,               
      ~                                        >     Petal.Length = 1.4,              
      ~                                        >     Petal.Width = 0.2,               
      ~                                        >     Species = factor("setosa", levels
      ~                                        :  = c("setosa", "versicolor", "virgini
      ~                                        : ca"))                                
      ~                                        >   ),                                 
          b = "aaaaaaaaaaaaaaaaaaaa",              b = "aaaaaaaaaaaaaaaaaaaa",        
      <   c = "Foo"                            >   c = "foo"                          
        )                                        )                                    
    Code
      construct_diff(list(a = head(cars, 2), b = "aaaaaaaaaaaaaaaaaaaa", c = "Foo"),
      list(a = head(iris, 1), b = "aaaaaaaaaaaaaaaaaaaa", c = "foo"), opts_data.frame(
        "read.table"), interactive = FALSE)
    Output
      < list(a = head(cars, 2), b = "aaaaaa..  > list(a = head(iris, 1), b = "aaaaaa..
      @@ 1,9 @@                                @@ 1,11 @@                             
        list(                                    list(                                
      <   a = read.table(header = TRUE, text   >   a = data.frame(                    
      : = "                                    ~                                      
      < speed dist                             >     Sepal.Length = 5.1,              
      <    4.   2.                             >     Sepal.Width = 3.5,               
      <    4.  10.                             >     Petal.Length = 1.4,              
      < "),                                    >     Petal.Width = 0.2,               
      ~                                        >     Species = factor("setosa", levels
      ~                                        :  = c("setosa", "versicolor", "virgini
      ~                                        : ca"))                                
      ~                                        >   ),                                 
          b = "aaaaaaaaaaaaaaaaaaaa",              b = "aaaaaaaaaaaaaaaaaaaa",        
      <   c = "Foo"                            >   c = "foo"                          
        )                                        )                                    
    Code
      construct_diff(1, 1)
    Message
      No difference to show!
    Code
      construct_diff("é", iconv("é", to = "latin1"), interactive = FALSE)
    Output
      < "é"                                 > iconv("é", to = "latin1")         
      @@ 1 @@                               @@ 1 @@                             
      < "\U{E9}"                            > "\xe9" |> (`Encoding<-`)("latin1")

