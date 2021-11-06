object playground extends App{

  var x = -20 % 256
  x = if (x < 0) x + 256 else x
  print(x)

}
