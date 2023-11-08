(in-package :cl-braces.vm.bytecode)

(defisa
    :version (1 0)
    :instructions
    ((#x00 halt  () "Halts the execution")
     (#x01 noop  () "Does nothing")
     (#x02 neg ((reg dst)) "Negatate value in register $dst")
     (#x03 sub ((reg dst) (reg lhs) (reg rhs)) "Subtract")
     (#x04 add ((reg dst) (reg lhs) (reg rhs)) "Add")
     (#x05 mul ((reg dst) (reg lhs) (reg rhs)) "Mul")
     (#x06 div ((reg dst) (reg lhs) (reg rhs)) "Div")
     (#x10 loada ((reg dst) (addr src)) "Load a constant from the provided address into the register")))

(setf *current-isa* *isa-1.0*)
