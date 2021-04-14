(ns com.owoga.tightly-packed-trie.encoding
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [com.owoga.tightly-packed-trie.bit-manip :as bm])
  (:import (java.nio ByteBuffer)))

(defn encode
  "Encodes number as a variable-length byte-array.
  The first bit of each byte in the array is a flag bit
  that specifies whether to read the next byte.

  To encode: Take the first 7 bits and put it in a byte
  that has the most significant bit set to 1. This is the flag
  bit that tells us that this byte is the last byte in the encoded number.
  If the number didn't fit into those first 7 bits, bit-shift-right 7 bits
  and grow the list of bytes by taking 7 bits at a time from the number
  while leaving the flag bit on every byte other than the first as 0.

  (->> [0 1 2 127 128 129]
       (map encode)
       (map #(map bm/to-binary-string %)))
  ;; => ((\"10000000\")
  ;;     (\"10000001\")
  ;;     (\"10000010\")
  ;;     (\"11111111\")
  ;;     (\"00000001\" \"10000000\")
  ;;     (\"00000001\" \"10000001\"))


  To decode: if the flag bit is not set, read the next byte and
  concat the last 7 bits of the current byte to
  the last 7 bits of the next byte."
  (#^bytes [n]
   (loop [b (list (bit-set (mod n 0x80) 7))
          n (quot n 0x80)]
     (if (zero? n)
       (byte-array b)
       (recur (cons (mod n 0x80) b) (quot n 0x80))))))

(comment
  (->> [0 1 2 127 128 129]
       (map encode)
       (map #(map bm/to-binary-string %)))
  ;; => (("10000000")
  ;;     ("10000001")
  ;;     ("10000010")
  ;;     ("11111111")
  ;;     ("00000001" "10000000")
  ;;     ("00000001" "10000001"))
  )

(defn decode
  "Decode one variable-length-encoded number from a ByteBuffer,
  advancing the buffer's position to the byte following the encoded number."
  ^Integer [^java.nio.ByteBuffer byte-buffer]
  (loop [bytes (list ^Byte (.get byte-buffer))]
    (if (bit-test (first bytes) 7)
      (->> (cons (bit-clear (first bytes) 7) (rest bytes))
           reverse
           (map bm/ubyte)
           (apply (partial bm/combine-significant-bits 7)))
      (recur (cons (.get byte-buffer) bytes)))))

(comment
  (->> [0 1 2 127 128 129 9876543210]
       (map encode)
       (map #(java.nio.ByteBuffer/wrap %))
       (map decode))
  ;; => (0 1 2 127 128 129 9876543210)
  )

(defn key-byte? [b]
  (bit-test b 7))

(def offset-byte? (complement key-byte?))

(defn encode-key-to-tightly-packed-trie-index
  #^bytes [n]
  (->> n encode (map #(bit-set % 7)) byte-array))

(defn encode-offset-to-tightly-packed-trie-index
  #^bytes [n]
  (->> n encode (map #(bit-clear % 7)) byte-array))

(defn decode-number-from-tightly-packed-trie-index
  [^java.nio.ByteBuffer byte-buffer]
  (let [first-byte (.get byte-buffer)
        continue? (fn []
                    (and (.hasRemaining byte-buffer)
                         (= (key-byte? (.get byte-buffer (.position byte-buffer)))
                            (key-byte? first-byte))))]
    (loop [bytes [first-byte]]
      (if (continue?)
        (recur (conj bytes (.get byte-buffer)))
        (->> bytes
             (map (partial bit-and 0xFF))
             (map #(bit-clear % 7))
             (apply (partial bm/combine-significant-bits 7)))))))

(comment
  (let [byte-buffer (java.nio.ByteBuffer/allocate 64)]
    (.put byte-buffer (encode-key-to-tightly-packed-trie-index 0))
    (.put byte-buffer (encode-offset-to-tightly-packed-trie-index 1))
    (.put byte-buffer (encode-key-to-tightly-packed-trie-index 9876543210))
    (.put byte-buffer (encode-offset-to-tightly-packed-trie-index 1234567890))
    (.limit byte-buffer (.position byte-buffer))
    (.rewind byte-buffer)
    [(decode-number-from-tightly-packed-trie-index byte-buffer)
     (decode-number-from-tightly-packed-trie-index byte-buffer)
     (decode-number-from-tightly-packed-trie-index byte-buffer)
     (decode-number-from-tightly-packed-trie-index byte-buffer)])
  ;; => [0 1 9876543210 1234567890]
  )

(defn slurp-bytes [x]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (io/copy (io/input-stream x) out)
    (.toByteArray out)))

