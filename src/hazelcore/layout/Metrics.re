type metrics = {
  height: int,
  last_width: int,
  last_width_is_relative: bool,
};

let rec metrics'': 'annot. Pretty.Layout.t('annot) => metrics =
  // TODO: rename
  layout => {
    Obj.magic(snd(Lazy.force(metrics_memo_table), Obj.magic(layout)));
  }

and metrics_memo_table:
  Lazy.t(
    (Memoize.WeakPoly.Table.t(metrics), Pretty.Layout.t(unit) => metrics),
  ) =
  lazy(Memoize.WeakPoly.make(metrics'))

and metrics' = (layout: Pretty.Layout.t(unit)): metrics =>
  switch (layout) {
  | Text(string) => {
      height: 1,
      last_width: StringUtil.utf8_length(string),
      last_width_is_relative: true,
    }
  | Linebreak => {height: 2, last_width: 0, last_width_is_relative: false}
  | Annot(_, l) => metrics''(l)
  | Align(l) => {...metrics''(l), last_width_is_relative: false}
  | Cat(l1, l2) =>
    let metrics1 = metrics''(l1);
    let metrics2 = metrics''(l2);
    let height = metrics1.height + metrics2.height - 1;
    let metrics =
      if (!metrics2.last_width_is_relative) {
        metrics2;
      } else {
        {...metrics1, last_width: metrics1.last_width + metrics2.last_width};
      };
    {...metrics, height};
  };

let metrics: 'annot. Pretty.Layout.t('annot) => metrics =
  layout => {
    let l = metrics''(layout);
    Memoize.WeakPoly.Table.clear(fst(Lazy.force(metrics_memo_table)));
    l;
  };
