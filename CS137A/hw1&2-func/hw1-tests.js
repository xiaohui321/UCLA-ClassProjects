tests(
  F,
  //primValue
  {
    name:'null',
    code:'null',
    expected: null
  },
  {
    name: 'number',
    code: '1234',
    expected: 1234
  },
  {
    name: 'true',
    code: 'true',
    expected: true
  },
  {
    name: 'false',
    code: 'false',
    expected: false
  },
  //op
  {
    name: 'plus',
    code: '6 + 7',
    expected: 13
  },
  {
    name: 'minus',
    code: '6 - 7',
    expected: -1
  },
  {
    name: 'times',
    code: '6 * 7',
    expected: 42
  },
  {
    name: 'divide',
    code: '12 / 2',
    expected: 6
  },
  {
    name: 'reminder',
    code: '6 % 5',
    expected: 1 
  },
  {
    name: 'more than',
    code: '7 > 6',
    expected: true
  },
    {
    name: 'more than',
    code: '7 < -6',
    expected: false
  },
  {
    name: 'less than',
    code: '6 < 7',
    expected: true
  },
    {
    name: 'less than',
    code: '6 < -7',
    expected: false
  },
  {
    name: 'all together now',
    code: '1 + 8 - 4 * 5 / 10',
    expected: 7
  },
  {
    name: 'or',
    code: 'true || false',
    expected: true
  },
  {
    name: 'and',
    code: 'false && true',
    expected: false
  },
  {
    name: 'boolean with number',
    code: 'true || 0',
    shouldThrow: true
  },
  {
    name: 'boolean with number',
    code: 'false && 1',
    shouldThrow: true
  },
  {
    name: '= operator',
    code:'1 = 1',
    expected: true
  },
  {
    name: '= operator',
    code:'1 = 2',
    expected: false
  },
  {
    name: '= operator',
    code:' 2 = (fun x -> x)',
    expected: false
  },
  {
    name: '!= operator',
    code:'1 != 1',
    expected: false
  },
  {
    name: '!= operator',
    code:'1 != 2',
    expected: true
  },
  {
    name: '!= operator',
    code:' 2 != (fun x -> x)',
    expected: true
  },
  {
    name: 'arithmetic and relational operators should require args to be numbers',
    code: '(fun x -> x) + 1',
    shouldThrow: true
  },
  {
    name: 'boolean operators should require args to be booleans',
    code: '1 || true',
    shouldThrow: true
  },
  {
    name: 'boolean operators should require args to be booleans',
    code: '(fun x -> x) || true',
    shouldThrow: true
  },
  {
    name: 'conditional',
    code: 'if 2 > 3 then 4321 else 1234',
    expected: 1234
  },
  {
    name: 'let',
    code: 'let x = 3 + 4 in\n' +
          '  x - 2',
    expected: 5
  },
  {
    name: 'unbound identifier',
    code: 'x + 1',
    shouldThrow: true
  },
  {
    name: 'fun and call',
    code: '(fun x -> x + 1) 3',
    expected: 4
  },
  {
    name: 'passing too many args is not OK',
    code: '(fun x -> x + 1) 3 4',
    shouldThrow: true
  },
  {
    name: 'nested funs',
    code: 'let add = fun x -> fun y -> x + y in\n' +
          '  let inc = add 1 in\n' +
          '    inc 10',
    expected: 11
  },
  {
    name: 'equality of functions (shallow)',
    code: 'let f = fun x -> 0 in ' +
          'let f2 = f in ' +
          'f = f2',
    expected: true
  },
  {
    name: 'inequality of functions (shallow)',
    code: 'let f  = fun x -> 0 in ' +
          'let f2 = fun x -> 0 in ' +
           'f != f2',
    expected: true
  },
  {
    name: 'arithmetic and relational operators should require args to be numbers',
    code: '(fun x -> x) + 1',
    shouldThrow: true
  },
  {
    name: 'or',
    code: 'true || false',
    expected: true
  },
  {
    name: 'if requires e1 to be boolean',
    code: 'if 4 then 2 else false',
    shouldThrow: true
  },
  {
    name: 'boolean operators should require args to be booleans',
    code: '(fun x -> x) || true',
    shouldThrow: true
  },
  {
    name: 'conditional',
    code: 'if 2 > 3 then 4321 else 1234',
    expected: 1234
  },
  {
    name: 'let',
    code: 'let x = 3 + 4 in\n' +
          '  x - 2',
    expected: 5
  },
  {
    name: 'unbound identifier',
    code: 'x + 1',
    shouldThrow: true
  },
  {
    name: 'fun and call',
    code: '(fun x -> x + 1) 3',
    expected: 4
  },
  {
    name: 'passing too many args is not OK',
    code: '(fun x -> x + 1) 3 4',
    shouldThrow: true
  },
  {
    name: 'function with multiple parameters',
    code: '(fun x y -> x + y) 1 2',
    expected: 3
  },
  {
    name: 'equality of different types',
    code: '(fun x -> x) = 1 || 1 = true || null = 2 || null = (fun y -> 1)',
    expected: false
  },
  {
    name: 'nested funs',
    code: 'let add = fun x -> fun y -> x + y in\n' +
          '  let inc = add 1 in\n' +
          '    inc 10',
    expected: 11
  },
  {
    name: 'shadowing',
    code: 'let x = 1 in' +
          '(fun x -> x) 2 + x',
    expected: 3
  },
  {
    name: 'closure',
    code: 'let y = 1 in' +
          '(fun x -> x + y) 2',
    expected: 3
  },
  {
    name: 'local variables',
    code: 'let y = 1 in' +
          '(fun x -> x + y) 2 + x',
    shouldThrow: true
  },
  {
  name: 'if condtion must be boolean',
  code: 'if 0 then 1 else 2',
  shouldThrow: true
  },
  {
    name: 'plus',
    code: '6 * 7',
    expected: 42
  },
  {
    name: 'less than',
    code: '6 < 7',
    expected: true
  },
  {
    name: 'arithmetic and relational operators should require args to be numbers',
    code: '(fun x -> x) + 1',
    shouldThrow: true
  },
  {
    name: 'or',
    code: 'true || false',
    expected: true
  },
  {
    name: 'boolean operators should require args to be booleans',
    code: '(fun x -> x) || true',
    shouldThrow: true
  },
  {
    name: 'conditional',
    code: 'if 2 > 3 then 4321 else 1234',
    expected: 1234
  },
  {
    name: 'let',
    code: 'let x = 3 + 4 in\n' +
          '  x - 2',
    expected: 5
  },
  {
    name: 'unbound identifier',
    code: 'x + 1',
    shouldThrow: true
  },
  {
    name: 'fun and call',
    code: '(fun x -> x + 1) 3',
    expected: 4
  },
  {
    name: 'passing too many args is not OK',
    code: '(fun x -> x + 1) 3 4',
    shouldThrow: true
  },
  {
    name: 'nested funs',
    code: 'let add = fun x -> fun y -> x + y in\n' +
          '  let inc = add 1 in\n' +
          '    inc 10',
    expected: 11
  },
 
/* start Runhang's test */
  {
    name: 'rh-t1',
    code: 'let f = fun x -> ((x + 2) * x / 1) % 2 in\n' + 
          '(let x = 5 in let x = 3 in x) * (let x = 3 in f (x + 2))',
    expected: 3
  },
 
  {
    name: 'rh-t2', 
    code: 'let f = fun x -> let x = 1 in x = 1 in f 2',
    expected: true
  },
 
/* start Zhixuan's test */ 
  {
    name: 'primValueNumber1',
    code: '1',
    expected: 1
  },
  {
    name: 'primValueNumber2',
    code: '-10',
    expected: -10
  },
  {
    name: 'primValueNumber3',
    code: '0',
    expected: 0
  },
  {
    name: 'primValueBoolean1',
    code: 'true',
    expected: true
  },
  {
    name: 'primValueBoolean2',
    code: 'false',
    expected: false
  },
  {
    name: 'primValueNull1',
    code: 'null',
    expected: null
  },
  {
    name: 'primValueNull2',
    code: 'undefined',
    shouldThrow: true
  },
  // ops
  {
    name: 'arithmetic and relational operators type checking',
    code: 'true > false',
    shouldThrow: true
  },
  {
    name: 'arithmetic and relational operators type checking',
    code: 'true < 3',
    shouldThrow: true
  },
  {
    name: 'arithmetic and relational operators type checking',
    code: 'true / 3',
    shouldThrow: true
  },
  {
    name: 'arithmetic and relational operators type checking',
    code: '3 - false',
    shouldThrow: true
  },
  {
    name: 'arithmetic and relational operators type checking',
    code: 'true % false',
    shouldThrow: true
  },
  // TODO: confirm this
  {
    name: 'logical operators type checking',
    code: '1 || 1',
    shouldThrow: true
  },
  {
    name: 'logical operators type checking',
    code: '4 && 1',
    shouldThrow: true
  },
  {
    name: 'logical operators type checking',
    code: 'true || 1',
    shouldThrow: true
  },
  {
    name: 'logical operators type checking',
    code: 'false && 1',
    shouldThrow: true
  },
  // equality
  {
    name: 'equalty',
    code: '1 = 1',
    expected: true
  },
  {
    name: 'equalty',
    code: 'true = 1',
    expected: false
  },
  {
    name: 'equalty',
    code: '1 = null',
    expected: false
  },
  {
    name: 'equalty',
    code: 'null = null',
    expected: true
  },
  // id
  {
    name: 'variable shadowing',
    code: 'let x = 3 in let x = 4 in x',
    expected: 4
  },
  {
    name: 'unbound value',
    code: 'let f = fun x y -> x*y in f y',
    shouldThrow: true
  },
  // if
  // TODO: confirm this
  {
    name: 'if requires e1 to be boolean',
    code: 'if 4 then 2 else false',
    shouldThrow: true
  },
  {
    name: 'conditional',
    code: 'if 2 < 3 then 4321 else 1234',
    expected: 4321
  },
  // call
  {
    name: 'call',
    code: 'let x = fun x -> x in x 1',
    expected: 1
  },
  {
    name: 'calling with multiple args',
    code: 'let f = fun x y -> x*y in f 10 3',
    expected: 30
  },
  {
    name: 'calling with too few args',
    code: 'let f = fun x y -> x in f 10 ',
    shouldThrow: true
  },
  {
    name: 'calling with too many args',
    code: 'let f = fun x y -> x in f 10 3 true',
    shouldThrow: true
  },
  
  // end
  {
    name: 'end of args placeholder',
    code: 'if 4 then 2 else false',
    shouldThrow: true
  }
);

