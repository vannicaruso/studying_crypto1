import it.vannicaruso.crypto.HEX
import it.vannicaruso.crypto.HEX._
import it.vannicaruso.crypto.crypto._

import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
//val all = traverseList(ciphers).length
"abcd" xor "abcd"
//val the = stringToHex(" the ")
//val cipher_1_2 = cipher1 xor cipher2
val asciiEncoder =
  Charset.forName("ISO-8859-1").newEncoder()

def scan(ciphertext: HEX, guess: HEX) =
  ciphertext.hex.sliding(guess.hex.length, 1).foreach {
    window =>
      val xored = window xor guess
      val ascii_string = xored.toASCII
      if (asciiEncoder.canEncode(ascii_string))
        print("|" + ascii_string + "|")
  }



