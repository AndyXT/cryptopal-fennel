(local base64 (require :base64))
(local string (require :string))
(local table (require :table))

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
  (local str (string.char (unpack bytes-array)))
  str)

(fn challenge11 []
  (local hex-string "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
  (local b64-ans "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
  (local hex-str (bytes-array-to-string (hex-string-to-bytes hex-string)))
  (local b64-str (base64.encode hex-str))
  (print b64-str)
  (print b64-ans)
  (if (= b64-str b64-ans)
    (print "Challenge 11 passed")
    (print "Challenge 11 failed")))

(fn main []
 (challenge11))

(main)
