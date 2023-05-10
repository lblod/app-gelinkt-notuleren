(define-resource functionaris ()
  :class (s-prefix "lblodlg:Functionaris")
  :properties `((:start :datetime ,(s-prefix "mandaat:start"))
                (:einde :datetime ,(s-prefix "mandaat:einde")))
  :has-one `((bestuursfunctie :via ,(s-prefix "org:holds")
                              :as "bekleedt")
             (functionaris-status-code :via ,(s-prefix "mandaat:status")
                                       :as "status")
             (persoon :via ,(s-prefix "mandaat:isBestuurlijkeAliasVan")
                      :as "is-bestuurlijke-alias-van"))
  :resource-base (s-url "http://data.lblod.info/id/functionarissen/")
  :features '(include-uri)
  :on-path "functionarissen")

;; Old (overwritten) version of functionaris
;;
;; (define-resource functionaris ()
  ;;Functionaris is considered subclass of org:Membership as of now. TODO: publish this
;;   :class (s-prefix "lblod:Functionaris")
;;   :has-one `((tijdsinterval :via ,(s-prefix "org:memberDuring")
;;                             :as "lid-van-tot")
;;              (persoon :via ,(s-prefix "org:member")
;;                       :as "lid")
;;              (rol :via ,(s-prefix "org:role")
;;                   :as "rol")
;;              (bestuurseenheid :via ,(s-prefix "org:organization")
;;                               :as "is-lidmaatschap-bij")
;;              (functionaris-status-code :via ,(s-prefix "lblod:functionarisStatusCode")
;;                                        :as "status"))

;;   :resource-base (s-url "http://data.lblod.info/id/functionaris/")
;;   :features '(include-uri)
;;   :on-path "functionarissen"
;;   )