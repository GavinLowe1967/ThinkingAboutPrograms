package thinkingAboutPrograms.spellCheck

import thinkingAboutPrograms.dictionary.Dictionary

/** A simple application to check the spelling of words */
object SpellCheck{
  def main(args: Array[String]) = {
    if(args.length != 1) println("Please provide one argument")
    else{
      val w = args(0)
      val dict = new Dictionary("knuth_words")
      if(dict.isWord(w)) println(w+" is a valid word")
      else println("word "+w+" not found")
    }
  }
}
