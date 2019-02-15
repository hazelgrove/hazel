module type MONAD = {
  type t('a);
  let return : 'a => t('a);
  let bind : (t('a), ('a => t('b))) => t('b);
};

