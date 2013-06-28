How to retrieve the key for many time pad ciphertexts
-----------------------------------------------------
1) XOR all ciphers each other with the function
'''scala
def traverseList(list: List[HEX]): List[HEX] =  {
    def traverseInternal(acc: List[HEX], curlist: List[HEX]): List[HEX] = curlist match {
      case Nil => List()
      case h :: Nil => acc ++ (list diff List(h)).map{h xor _}
      case h :: t => traverseInternal(acc ++ (list diff List(h)).map{h xor _} ,t)
    }
    traverseInternal(List[HEX](), list)
  }
'''
2) search for readable text by sliding a 'guess' through the xored ciphers
'''scala
def readable(s: String) = s.forall{c =>
  ((c >= 'a' && c <= 'z') || (c >= 'a' && c <= 'Z') || c.isDigit || c == ' ' || c == ',' || c == '.')}

def searchForReadableText(ciphertext: HEX, guess: HEX) =
  ciphertext.hex.sliding(guess.hex.length, 1).map{ s => (s xor guess).toASCII }.filter {readable(_)}

val GUESS = ...
traverseList(ciphers).foreach{searchForReadableText(_, stringToHex(GUESS) ).foreach(println(_))}
'''
3) retry with other guesses from readable test from above and spread the guess trying to complete the statements
4) once you have an entire statement xor it with the ciphertext to obtain the key
5) once you have the key
'''scala
ciphers.foreach((h: HEX) => println((k xor h).toASCII))
'''