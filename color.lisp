(defvar *color-table* '(:fg-black "30"
                        :bg-black "40"
                        :fg-red "31"
                        :bg-red "41"
                        :fg-green "32"
                        :bg-green "42"
                        :fg-yellow "33"
                        :bg-yellow "43"
                        :fg-blue "34"
                        :bg-blue "44"
                        :fg-magenta "35"
                        :bg-magenta "45"
                        :fg-cyan "36"
                        :bg-cyan "46"
                        :fg-white "37"
                        :bg-white "47"))


(defun colorize (color st)
  (let ((color-code (getf *color-table* color)))
    (format t "~c[~Am~A~c[0m" #\ESC color-code st #\ESC)))
