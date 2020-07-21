type t('t1, 't2) = (array('t1), array('t2))
let fst((fst, snd)) = fst[0]
let snd((fst, snd)) = snd[0]