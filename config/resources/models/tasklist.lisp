(define-resource tasklist ()
  :class (s-prefix "ext:Tasklist")

  :properties `((:name :string ,(s-prefix "ext:tasklistName")))

  :has-many `((task :via ,(s-prefix "ext:tasklistTask")
                   :as "tasks"))

  :resource-base (s-url "http://data.lblod.info/id/tasklists/")
  :features '(include-uri)
  :on-path "tasklists")