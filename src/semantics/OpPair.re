[@deriving sexp]
type t('operand, 'operator) = ('operator, 'operand);

let operator: t('operand, 'operator) => 'operator = fst;
let operand: t('operand, 'operator) => 'operand = snd;

let update_operator = (operator, (_, operand)) => (operator, operand);
let update_operand = (operand, (operator, _)) => (operator, operand);
