# construct_diff

    Code
      construct_diff(list(a = head(cars, 2), b = "aaaaaaaaaaaaaaaaaaaa", c = "Foo"),
      list(a = head(iris, 1), b = "aaaaaaaaaaaaaaaaaaaa", c = "foo"))
    Output
      < list(a&#x00A0;=&#x00A0;head(cars,&#..  > list(a&#x00A0;=&#x00A0;head(iris,&#..
      @@ 1,5 @@                                @@ 1,11 @@                             
        list(                                    list(                                
      <   a = data.frame(speed = c(4, 4), dis  >   a = data.frame(                    
      : t = c(2, 10)),                         ~                                      
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

