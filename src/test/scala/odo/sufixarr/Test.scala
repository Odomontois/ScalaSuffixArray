package odo.suffixarr

object Test{
  def main(args: Array[String]): Unit = {
    //    val string = "rikki-tikki-tikka"
        val string = args.applyOrElse(0, (_: Int) => "cabbage")
        implicit val char = BoundEnum.char.slice('a', 'g')
        val issa = new SA_ISMaker(string)
        println(issa.showTypeMap)
        println(issa.bucketHeads.toSeq)
        println(issa.bucketTails.toSeq)

        println(issa.start.guessLMSSort.induceSortL.induceSortS.summarize)


  }
}
