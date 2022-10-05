
(defn pr_str [data]
  (cond
    (or (string? data)
        (symbol? data)) (string/format "%s" data)
    (or (boolean? data)
        (nil?     data)) (string/format "%v" data)
    (number? data) (if (int? data)
                     (string/format "%d" data)
                     (string/format "%f" data))
    (array?  data) (let [parts (map pr_str data)]
                     (string/format "(%s)"
                                    (string/join parts " ")))))

(pr_str 123)
(pr_str 1.2)
(pr_str "test")
(pr_str 'blarg)
(pr_str @[1 2 3])
(pr_str @["one" 2 'three])
(pr_str true)
(pr_str false)
(pr_str nil)
