(define-resource tasklist-solution ()
  :class (s-prefix "ext:TasklistSolution")

  :properties `((:name :string ,(s-prefix "ext:tasklistSolutionName"))
                (:index :number ,(s-prefix "ext:tasklistSolutionIndex")))

  :has-one `((tasklist :via ,(s-prefix "ext:tasklistSolutionTasklist")
                        :as "tasklist"))

  :has-many `((task-solution :via ,(s-prefix "ext:tasklistSolutionTaskSolution")
                   :as "task-solutions"))

  :resource-base (s-url "http://data.lblod.info/id/tasklist-solutions/")
  :features '(include-uri)
  :on-path "tasklist-solutions")