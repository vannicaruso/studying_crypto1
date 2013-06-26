package it.vannicaruso.crypto


/**
 * La classe che rappresenta una stringa esadecimale
 * @param hex
 */
class HEX(val hex: String){
  import HEX._
  /*requirement is that it ha an hex string inside*/
  require(check(hex),"Not Hex String!!! ->" + hex)

  /*making xor*/
  def xor(that: HEX) = new HEX(valueOf(xor_byte_array(hex_to_byte_array(hex), hex_to_byte_array(that.hex))))

  def toASCII = byte_array_to_hex(hex_to_byte_array(hex))

  override def toString = hex.toLowerCase
}



object HEX {
  implicit def string2HEX(s: String) = new HEX(s)
  val hexDigits = Set[Char]() ++ "0123456789abcdefABCDEF".toArray
  /**
   * XOR tra 2 byte
   * @param b1 byte 1
   * @param b2 byte 2
   * @return il byte che rappresenta lo XOR tra b1 e b2
   */
  private def xor_byte(b1: Byte, b2: Byte) = (b1 ^ b2).toByte

  /**
   * Esegue lo XOR tra due array di byte e restituisce l'array di byte corrispondenti
   * @param b1  il primo array di byte
   * @param b2  il secondo array di byte
   * @return l'array che costituisce lo XOR
   */
  private def xor_byte_array(b1: Array[Byte], b2: Array[Byte]) =
    (b1 zip b2).foldLeft(Array[Byte]()){(acc,actual) => acc :+ xor_byte(actual._1, actual._2)}

  /**
   * Trasforma una stringa esadecimale nel corrispondente array di byte
   * @param s la stringa esadecimale
   * @return  l'array di byte corrispondente
   */
  private def hex_to_byte_array(s: String) = s.sliding(2,2).map{Integer.parseInt(_,16).toByte}.toArray

  /**
   *
   * @param a
   * @return
   */
  private def byte_array_to_hex (a: Array[Byte]) = new String(a)

  /**
   *
   * @param s
   * @return
   */
  def stringToHex(s :String) =
    s.getBytes.map{ b => String.format("%02X", java.lang.Byte.valueOf(b)) }.mkString.toLowerCase

  private def check(s: String) =
    if (!s.forall(char => hexDigits.contains(char))) false else true

  def valueOf(buf: Array[Byte]): String = buf.map("%02X" format _).mkString


}
