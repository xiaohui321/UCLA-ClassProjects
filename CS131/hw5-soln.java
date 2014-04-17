
import java.util.*;

enum Op {
    PLUS { public double calculate(double a1, double a2) { return a1 + a2; } },
    MINUS { public double calculate(double a1, double a2) { return a1 - a2; } },
    TIMES { public double calculate(double a1, double a2) { return a1 * a2; } },
    DIVIDE { public double calculate(double a1, double a2) { return a1 / a2; } };

    abstract double calculate(double a1, double a2);
}


interface AExp {
    double eval();
    List<Sopn> toRPN();
}

class Num implements AExp {
    protected double val;

    public Num(double val) { this.val = val; }

    public String toString() { return "" + val; }
    
    public double eval() { return val; }

    public List<Sopn> toRPN() {
	List<Sopn> res = new LinkedList<Sopn>();
	res.add(new Push(this.val));
	return res;
    }

}

class BinOp implements AExp {
    protected AExp left, right;
    protected Op op;

    public BinOp(AExp left, Op op, AExp right) {
	this.left = left;
	this.op = op;
	this.right = right;
    }

    public String toString() {
	return "BinOp(" + left + ", " + op + ", " + right + ")";
    }
    
    public double eval() {
	return op.calculate(left.eval(), right.eval());
    }

    public List<Sopn> toRPN() {
	List<Sopn> res = left.toRPN();
	res.addAll(right.toRPN());
	res.add(new Calculate(this.op));
	return res;
    }

}

interface Sopn {
    void eval(Stack<Double> stack);
}

class Push implements Sopn {
    protected double val;

    public Push(double val) { this.val = val; }

    public String toString() {
	return "Push " + val;
    }

    public void eval(Stack<Double> stack) {
	stack.push(val);
    }

}

class Swap implements Sopn {

    public Swap() {}

    public String toString() {
	return "Swap";
    }
    
    public void eval(Stack<Double> stack) {
	Double f1 = stack.pop();
	Double f2 = stack.pop();
	stack.push(f1);
	stack.push(f2);
    }

}

class Calculate implements Sopn {
    protected Op op;

    public Calculate(Op op) { this.op = op; }

    public String toString() {
	return "Calculate " + op;
    }
    
    public void eval(Stack<Double> stack) {
	Double f2 = stack.pop();
	Double f1 = stack.pop();
	stack.push(op.calculate(f1, f2));
    }

}

class RPNExp {
    protected List<Sopn> instrs;

    public RPNExp(List<Sopn> instrs) { this.instrs = instrs; }

    public double eval() {
	Stack<Double> stack = new Stack<Double>();
	for (Sopn i : instrs) {
	    i.eval(stack);
	}
	return stack.peek();
    }

}


class NotFoundException extends Exception {}

interface Dict<K,V> {
    void put(K k, V v);
    V get(K k) throws NotFoundException;
}

class DictImpl2<K,V> implements Dict<K,V> {
    protected Node<K,V> root = new Empty<K,V>();

    public void put(K k, V v) { root = root.put(k,v); }

    public V get(K k) throws NotFoundException { return root.get(k); }

}

interface Node<K,V> {
    Node<K,V> put(K k, V v);
    V get(K k) throws NotFoundException;
}

class Empty<K,V> implements Node<K,V> {
    public Node<K,V> put(K k, V v) { return new Entry<K,V>(k, v, this); }
    
    public V get(K k) throws NotFoundException {
	throw new NotFoundException();
    }
}

class Entry<K,V> implements Node<K,V> {
    protected K k;
    protected V v;
    protected Node<K,V> next;

    Entry(K k, V v, Node<K,V> next) {
	this.k = k;
	this.v = v;
	this.next = next;
    }

    public Node<K,V> put(K k, V v) {
	if (k.equals(this.k))
	    this.v = v;
	else
	    this.next = this.next.put(k,v);
	return this;
    }
    
    public V get(K k) throws NotFoundException {
	if (k.equals(this.k))
	    return this.v;
	else
	    return next.get(k);
    }
}




interface DictFun<A,R> {
    R invoke(A a) throws NotFoundException;
}

class DictImpl3<K,V> implements Dict<K,V> {
    protected DictFun<K,V> fun = new DictFun<K,V>() {
	public V invoke(K k) throws NotFoundException {
	    throw new NotFoundException();
	}
    };

    public void put(final K k, final V v) {
	final DictFun<K,V> oldFun = fun;
	fun = new DictFun<K,V>() {
	    public V invoke(K kArg) throws NotFoundException {
		if (kArg.equals(k))
		    return v;
		else
		    return oldFun.invoke(kArg);
	    }
	};
    }

    public V get(K k) throws NotFoundException { return fun.invoke(k); }
}


class Pair<A,B> {
    protected A fst;
    protected B snd;

    Pair(A fst, B snd) { this.fst = fst; this.snd = snd; }

    A fst() { return fst; }
    B snd() { return snd; }
}

interface FancyDict<K,V> extends Dict<K,V> {
    void clear();
    boolean containsKey(K k);
    void putAll(List<Pair<K,V>> entries);
}

class FancyDictImpl2<K,V> extends DictImpl2<K,V> implements FancyDict<K,V> {

    public void clear() { this.root = new Empty<K,V>(); }

    public boolean containsKey(K k) {
	try {
	    V v = this.get(k);
	    return true;
	} catch(NotFoundException e) {
	    return false;
	}
    }

    public void putAll(List<Pair<K,V>> entries) {
	for (Pair<K,V> p : entries) {
	    this.put(p.fst(), p.snd());
	}
    }
}


class CalcTest {
    public static void main(String[] args) {
	    // a test for Problem 1a
	AExp aexp =
	    new BinOp(new BinOp(new Num(1.0), Op.PLUS, new Num(2.0)),
		      Op.TIMES,
		      new Num(3.0));
	System.out.println("aexp evaluates to " + aexp.eval()); // aexp evaluates to 9.0

	// a test for Problem 1b
	List<Sopn> instrs = new LinkedList<Sopn>();
	instrs.add(new Push(1.0));
	instrs.add(new Push(2.0));
	instrs.add(new Calculate(Op.PLUS));
	instrs.add(new Push(3.0));
	instrs.add(new Swap());
	instrs.add(new Calculate(Op.TIMES));
	RPNExp rpnexp = new RPNExp(instrs);
	System.out.println("rpnexp evaluates to " + rpnexp.eval());  // rpnexp evaluates to 9.0

	// a test for Problem 1c
	System.out.println("aexp converts to " + aexp.toRPN());

    }
}


class DictTest {
    public static void main(String[] args) {

	    // a test for Problem 2a
	Dict<String,Integer> dict1 = new DictImpl2<String,Integer>();
	dict1.put("hello", 23);
	dict1.put("bye", 45);
	try {
	    System.out.println("bye maps to " + dict1.get("bye")); // prints 45
	    System.out.println("hi maps to " + dict1.get("hi"));  // throws an exception
	} catch(NotFoundException e) {
	    System.out.println("not found!");  // prints "not found!"
	}

	// a test for Problem 2b
	Dict<String,Integer> dict2 = new DictImpl3<String,Integer>();
	dict2.put("hello", 23);
	dict2.put("bye", 45);
	try {
	    System.out.println("bye maps to " + dict2.get("bye"));  // prints 45
	    System.out.println("hi maps to " + dict2.get("hi"));   // throws an exception
	} catch(NotFoundException e) {
	    System.out.println("not found!");  // prints "not found!"
	}
		
	    // a test for Problem 2c
	FancyDict<String,Integer> dict3 = new FancyDictImpl2<String,Integer>();
	dict3.put("hello", 23);
	dict3.put("bye", 45);
	System.out.println(dict3.containsKey("bye")); // prints true
	dict3.clear();
	System.out.println(dict3.containsKey("bye")); // prints false

    }
}
