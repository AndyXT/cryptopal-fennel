(local base64 (require :base64))
(local string (require :string))
(local table (require :table))
(local bit32 (require :bit32))

; (fn unpack []
;   (if (unpack)
;     unpack
;     table.unpack))

(fn char-to-byte [char]
  (let [byte-val (string.byte char)]
    (if (and (>= byte-val (string.byte "0")) (<= byte-val (string.byte "9")))
      (- byte-val (string.byte "0"))

      (and (>= byte-val (string.byte "a")) (<= byte-val (string.byte "f")))
      (+ (- byte-val (string.byte "a")) 10)

      (and (>= byte-val (string.byte "A")) (<= byte-val (string.byte "F")))
      (+ (- byte-val (string.byte "A")) 10)

      ((print "error: CharToByte") 0))))

(fn hex-string-to-bytes [hex-str]
  (let [byte-array {}]
    (for [i 1 (- (length hex-str) 1) 2]
      (let [char1 (string.sub hex-str i i)
            char2 (string.sub hex-str (+ i 1) (+ i 1))
            byte-val (+ (* (char-to-byte char1) 16) (char-to-byte char2))]
        (table.insert byte-array byte-val)))
    byte-array))

(fn bytes-array-to-string [bytes-array]
  (local str (string.char (table.unpack bytes-array)))
  str)

(fn challenge1-1 []
  (local hex-string "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
  (local b64-ans "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
  (local hex-str (bytes-array-to-string (hex-string-to-bytes hex-string)))
  (local b64-str (base64.encode hex-str))
  (print b64-str)
  (print b64-ans)
  (if (= b64-str b64-ans)
    (print "Challenge 1-1 passed")
    (print "Challenge 1-1 failed")))

(fn xor-byte-array [bytes1 bytes2]
  (let [result []]
    (for [i 1 (math.max (length bytes1) (length bytes2)) 1]
      (table.insert result (bit32.bxor (. bytes1 i) (. bytes2 i))))
    result))

(fn byte-to-hex-str [byte]
  (local hex-alphabet ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "a" "b" "c" "d" "e" "f"])
  (let [zero-place (% byte 16)
        sixteen-place (/ (- byte zero-place) 16)]
    (.. (. hex-alphabet (+ sixteen-place 1)) ; concatenate here
         (. hex-alphabet (+ zero-place 1)))))

(fn bytes-to-hex-string [bytes]
  (var hex-str "")
  (each [_ byte (ipairs bytes)]
    (let [hex-byte (byte-to-hex-str byte)]
      (set hex-str (.. hex-str hex-byte))))
  hex-str)

(fn challenge1-2 []
  (local hex-string1 "1c0111001f010100061a024b53535009181c")
  (local hex-string2 "686974207468652062756c6c277320657965")
  (local xor-ans "746865206b696420646f6e277420706c6179")
  (local bytes1 (hex-string-to-bytes hex-string1))
  (local bytes2 (hex-string-to-bytes hex-string2))
  (local xor-str (bytes-to-hex-string (xor-byte-array bytes1 bytes2)))
  (print xor-str)
  (print xor-ans)
  (if (= xor-str xor-ans)
    (print "Challenge 1-2 passed")
    (print "Challenge 1-2 failed")))

(fn main []
 (challenge1-1)
 (challenge1-2))

(main)
