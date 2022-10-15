(ns com.phronemophobic.membrane.svg-path-parse
  (:require [instaparse.core :as insta]
            [clojure.zip :as z]
            [zippo.core :as zippo]
            clojure.walk

            [meander.strategy.epsilon :as r]
            [meander.epsilon :as m]
            [clojure.java.io :as io]))

(defn svg-grammar []
  (slurp (io/resource "svg-grammar.txt")))


(def svg-path-parser
  (insta/parser
   (svg-grammar)
   ))

(def example-path
  "M0 8C0 3.58172 3.58172 0 8 0L152 0C156.418 0 160 3.58172 160 8L160 72C160 76.4183 156.418 80 152 80L8 80C3.58172 80 0 76.4183 0 72L0 8Z")

(defn parse-zip [p]
  (z/zipper vector?
            seq
            #(vec %2)
            p))

(defn is-whitespace? [p]
  (and (vector? p)
       (#{:wsp :comma_wsp} (first p))))

(defn search-zip-walk [zip pred]
  (z/root
   (zippo/loc-update-all zip
                         #(z/edit % pred))))


(defn delete-whitespace [p]
  (search-zip-walk (parse-zip p)
                   (fn [p]
                     (if (vector? p)
                       (vec (remove is-whitespace? p))
                       p))))

(defn simplify-numbers [p]
  (search-zip-walk (parse-zip p)
                   (fn [p]
                     (if-let [match (m/find p

                                      [:number
                                       [:fractional-constant [:digits ?whole] "." [:digits ?fraction]]
                                       [:exponent "e" [:sign ?sign] [:digits ?digits]]]
                                      (read-string (str ?whole "." ?fraction "e" ?sign ?digits))

                                      [:number [:fractional-constant [:digits ?whole]]]
                                      (read-string ?whole)

                                      [:number
                                       [:fractional-constant [:digits ?whole] "." [:digits ?fraction]]]
                                      (read-string (str ?whole "." ?fraction)))]
                       match
                       p))))



(defn simplify-coords [p]
  (clojure.walk/postwalk
   (fn [p]
     (if-let [match (m/find p

                      [:coordinate_pair_sequence !coords]
                      !coords

                      [:coordinate_pair ?one ?two]
                      [?one ?two]

                      [:curveto_coordinate_sequence ?x]
                      ?x

                      [:coordinate_pair_triplet ?one ?two ?three]
                      [?one ?two ?three]

                      [:lineto "L" [?coord]]
                      [:line-to-absolute ?coord]

                      [:lineto "l" [?coord]]
                      [:line-to-relative ?coord]

                      [:moveto "M" [?coord]]
                      [:move-to-absolute ?coord]

                      [:moveto "m" [?coord]]
                      [:move-to-relative ?coord]

                      [:curveto "C" ?coords]
                      [:curve-to-absolute ?coords]

                      [:curveto "c" ?coords]
                      [:curve-to-relative ?coords]

                      [:closepath (m/or "Z" "z")]
                      [:close-path]

                      [:drawto_command ?command]
                      ?command

                      [:coordinate ?n]
                      ?n

                      [:coordinate [:sign ?sign] ?n]
                      (case ?sign
                        "-" (- ?n)
                        "+" (- ?n))
                      
                      :svg_path
                      :svg-path)]
       match
       p))
   p))

(defn parse-svg-path [s]
  (-> (svg-path-parser s)
      (delete-whitespace)
      simplify-numbers
      simplify-coords
      rest
      vec))


