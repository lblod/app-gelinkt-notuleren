(define-resource attachment ()
  :class (s-prefix "ext:Attachment")
  :properties `((:decision :uri ,(s-prefix "dct:isPartOf")))
  :has-one `((document-container :via ,(s-prefix "ext:hasAttachments")
                            :inverse t
                            :as "document-container")
             (file :via ,(s-prefix "ext:hasFile")
                            :as "file")
                   (concept :via ,(s-prefix "ext:attachmentType")
                            :as "type"))
  :resource-base (s-url "http://lblod.data.gift/attachment/")
  :features `(include-uri)
  :on-path "attachments"
)