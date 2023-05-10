(define-resource verkeersbordcombinatie ()
  :class (s-prefix "lblodmow:Verkeersbordcombinatie")
  :properties `((:identifier :string ,(s-prefix "dct:identifier"))) ; identifier from irg
  :has-many `((maatregelconcept :via ,(s-prefix "dct:hasPart")
                                 :as "maatregelconcepten"))
  :resource-base (s-url "http://data.lblod.info/verkeersbordcombinaties/")
  :on-path "verkeersbordcombinaties")