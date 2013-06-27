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



//'r the nu' -> probably (1,2)
//'r the nu' -> ng about (1,3)
//'r the nu' -> xt produ (1,4)
//'that ' -> ' with'
//scan(cipher_1_2, stringToHex("nor the nums"))
//scan(cipher3 xor cipher4, stringToHex("ring about"))
//("2e584fa980fbbea5f6155946b443f27f5a8fa70c845fb940c10b4bbeec5ae56d1cde1dba50d8cc257ff04d1ce6eba546a6439295734326a8bcc863f039dce9145b7dcf8fcca2f92f5645" xor cipher1).toASCII

val ciph =  "6c73d5240a948c86981bc294814d"
val msg = "attack at dawn"
val key = (stringToHex(msg) xor ciph)













































//scan (cipher1 xor cipher3, stringToHex("nor the nums"))


//scan (cipher1 xor cipher4, stringToHex("next product "))






























































































































































































































































































































