(define-resource tasklist ()
  :class (s-prefix "ext:Tasklist")

  :properties `((:name :string ,(s-prefix "ext:tasklistName")))

  :has-many `((task :via ,(s-prefix "ext:tasklistTask")
                   :as "tasks"))

  :resource-base (s-url "http://data.lblod.info/id/tasklists/")
  :features '(include-uri)
  :on-path "tasklists")

(define-resource task ()
  :class (s-prefix "ext:Task")
  :properties `((:title :string ,(s-prefix "ext:taskTitle"))
                (:description :string ,(s-prefix "ext:taskDescription"))
                (:priority :number ,(s-prefix "ext:taskPriority")))

  :has-one `((task :via ,(s-prefix "ext:taskChild")
                  :inverse t
                  :as "task-parent"))

  :has-many `((task :via ,(s-prefix "ext:taskChild")
                    :as "task-childs"))

  :resource-base (s-url "http://data.lblod.info/id/tasks/")
  :features '(include-uri)
  :on-path "tasks")

(define-resource tasklist-solution ()
  :class (s-prefix "ext:TasklistSolution")

  :properties `((:name :string ,(s-prefix "ext:tasklistSolutionName")))

  :has-one `((tasklist :via ,(s-prefix "ext:tasklistSolutionTasklist")
                        :as "tasklist"))

  :has-many `((task-solution :via ,(s-prefix "ext:tasklistSolutionTaskSolution")
                   :as "task-solutions"))

  :resource-base (s-url "http://data.lblod.info/id/tasklist-solutions/")
  :features '(include-uri)
  :on-path "tasklist-solutions")

(define-resource task-solution ()
  :class (s-prefix "ext:TaskSolution")
  :properties `((:status :bool ,(s-prefix "ext:taskSolutionStatus")))

  :has-one `((task :via ,(s-prefix "ext:taskSolutionTask")
                   :as "task")
             (task-solution :via ,(s-prefix "ext:taskSolutionChild")
                  :inverse t
                  :as "task-solution-parent"))

  :has-many `((task-solution :via ,(s-prefix "ext:taskSolutionChild")
                    :as "task-solution-childs"))

  :resource-base (s-url "http://data.lblod.info/id/task-solutions/")
  :features '(include-uri)
  :on-path "task-solutions")
