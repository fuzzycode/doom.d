
(map! (:leader
        (:prefix ("x" . "text")
          (:prefix ("t" . "transpose")
            :desc "Chars" :g "c" 'transpose-chars
            :desc "Lines" :g "l" 'transpose-lines
            :desc "Words" :g "w" 'transpose-words))))
