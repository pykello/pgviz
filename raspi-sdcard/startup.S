.global _Reset
Reset:
LDR sp, =stack_top
BL c_entry
B .
