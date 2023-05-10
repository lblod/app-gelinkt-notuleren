(define-resource task-solution ()
  :class (s-prefix "ext:TaskSolution")
  :properties `((:status :bool ,(s-prefix "ext:taskSolutionStatus")))

  :has-one `((task :via ,(s-prefix "ext:taskSolutionTask")
                   :as "task"))

  :resource-base (s-url "http://data.lblod.info/id/task-solutions/")
  :features '(include-uri)
  :on-path "task-solutions")