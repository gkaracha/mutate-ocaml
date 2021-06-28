type mutation_rule = (Parsetree.expression -> Parsetree.expression option)

val rule_true_false : mutation_rule
val rule_false_true : mutation_rule

val rule_and_or : mutation_rule
val rule_or_and : mutation_rule

val rule_append_swap_args : mutation_rule
val rule_append_keep_first : mutation_rule
val rule_append_keep_second : mutation_rule

val rule_list_head : mutation_rule
val rule_list_tail : mutation_rule
