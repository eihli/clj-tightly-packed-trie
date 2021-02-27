(ns com.owoga.tightly-packed-trie.bit-manip
  (:require [clojure.string :as string]))

(defn bitstring->int
  "Turns a binary string representation of an integer into an integer.
  Throws at Integer/MAX_VALUE."
  [binary-string]
  (Integer/parseInt binary-string 2))

(comment
  (map
   bitstring->int
   ["0" "1" "10" "11" "100" (Integer/toBinaryString (bit-shift-right 0xFFFFFFFF 1))])
  ;; => (0 1 2 3 4 2147483647)
  (bitstring->int (Integer/toBinaryString (inc (bit-shift-right 0xFFFFFFFF 1))))
  ;; Value out of range for int: 2147483648
  )

(defn to-binary-string
  "For consistency when viewing bytes, 0-pads Integer/toBinaryString as 8 characters.

  Assumes b is an unsigned byte!
  Truncates all but 8 least significant bits.

  (Integer/toBinaryString 3) -> 11
  (to-binary-string 3)       -> 00000011
  (to-binary-string 128      -> 00000000)
  "
  [b]
  (let [s (string/replace
           (format "%8s" (Integer/toBinaryString b))
           #" "
           "0")]
    (subs s (- (count s) 8))))

(defn ubyte
  "Clojures `byte` is signed, making the max value 127.
  This gives us a 'byte'-like thing (It's actually a java.lang.Long),
  but it has the same bits as a byte. It just takes up more space in memory."
  [b]
  (if (> b 255)
    (throw (java.lang.IllegalArgumentException.
            (format "Value ouf of range for ubyte: %d" b)))
    (bit-and 0xff b)))

(defn sbyte
  "The reverse of ubyte. Turns a long representation of a byte into
  an actual signed byte. If the long has significant bits beyond 8,
  they are left as-is. If you're giving this a long that has significant
  bits beyond 8, take note: that might not be what you want?"
  [b]
  (byte (bit-or -0x100 b)))


(defn nth-bit
  "Returns as a byte the nth bit of b indexed from the least-significant-bit."
  [b n]
  (if (bit-test b n)
    (byte 1)
    (byte 0)))

(comment
  (let [n (bitstring->int "0101")]
    (->> (range 4)
         (map (partial nth-bit n))))
  ;; => (1 0 1 0)
  )

(defn ones-mask
  "Returns a long that is reprsented by the binary string of n 1s."
  [n]
  (reduce (fn [a _] (bit-or 1 (bit-shift-left a 1))) 0 (range n)))

;; Decoding variable-length-encoded numbers.
;;
;; bit-slice and combine-significant-bits
;; are a useful combination when you need to decode
;; a number that is encoded as bytes with flag bits set.
;;
;; Slice the flag bits off of each byte, then
;; combine the significant digits of each byte.
(defn bit-slice
  "Start is least-significant bit.
  (bit-slice 2 6 10101010)
  ->              ,1010,
  "
  [start end b]
  (let [mask (bit-shift-left (ones-mask (- end start)) start)]
    (bit-shift-right (bit-and b mask) start)))

(defn combine-significant-bits
  "Chops off all but the significant bits of each byte
  and then 'concats' the bits together into a long.
  (combine-significant-bits 7 [10001010 11001011])
  ->                            0001010  1001011
  ->                              00010101001011
  ->                                        1355 (As a long...)"
  [num-significant-bits & bytes]
  (reduce
   (fn [a b]
     (bit-or b (bit-shift-left a num-significant-bits)))
   0
   bytes))

(comment
  (let [b1 (bitstring->int "0110110")
        b2 (bitstring->int "1001001")
        ;; remove 2 flag bits
        slice (partial bit-slice 0 6)
        b1' (slice b1)
        b2' (slice b2)]
    (map
     to-binary-string
     [b1
      b1'
      b2
      b2'
      (combine-significant-bits 6 b1' b2' )]))
  ;; => ("00110110"
  ;;     "00110110"
  ;;     "01001001"
  ;;     "00001001"
  ;; "110110001001")
  )

