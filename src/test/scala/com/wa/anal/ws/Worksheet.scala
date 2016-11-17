package com.wa.anal.ws

import com.anal.wa.NGramsGenerator

object Worksheet extends App {
  println(NGramsGenerator.generate("this is my life", 1, 5).toList)
}