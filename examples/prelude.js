var pure = 
  { "succ": function(x) { return x + 1; }
  , "zero": 0
  , "tru":  true
  , "fls":  false
  , "pair": function(a) { return function(b) { return {fst: a, snd: b}; }; }
  , "null": []
  , "cons": function(head) { return function(tail) { return [head].concat(tail); }; }
  , "toJS":
      { "bool": function(b)  { return b(pure.tru)(pure.fls); }
      , "pair": function(p)  { return p(pure.pair); }
      , "list": function(xs) { return xs(pure.cons)(pure['null']); }
      , "nat":  function(n)  { return n(pure.succ)(pure.zero); }
      }
  };
