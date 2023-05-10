(define-resource functionaris-status-code ()
  :class (s-prefix "lblodlg:FunctionarisStatusCode")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:scope-note :string ,(s-prefix "skos:scopeNote")))
  :resource-base (s-url "http://data.vlaanderen.be/id/concept/functionarisStatusCode/")
  :features '(include-uri)
  :on-path "functionaris-status-codes")

;; Old (overwritten) version of functionaris-status-code
;;
;; (define-resource functionaris-status-code ()
;;   ;;TODO: publish this somewhere
;;   :class (s-prefix "lblod:FunctionarisStatusCode")
;;   :properties `((:label :string ,(s-prefix "skos:prefLabel")))
;;   :resource-base (s-url "http://data.vlaanderen.be/id/concept/functionarisStatusCode/")
;;   :features '(include-uri)
;;   :on-path "functionaris-status-codes")