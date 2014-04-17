/* CS 131 Winter 14
   Homework 5
   Author: Xiaohui, Zhou
   ID:     104014248
*/


import java.util.*;

// a type for arithmetic expressions
interface AExp {
     double eval(); 	                       // Problem 1a
    // List<Sopn> toRPN(); 	               // Problem 1c
}

// a representation of four arithmetic operators
enum Op {
    PLUS { public double calculate(double a1, double a2) { return a1 + a2; } },
    MINUS { public double calculate(double a1, double a2) { return a1 - a2; } },
    TIMES { public double calculate(double a1, double a2) { return a1 * a2; } },
    DIVIDE { public double calculate(double a1, double a2) { return a1 / a2; } };

    abstract double calculate(double a1, double a2);
}

// a type for stack operations
interface Sopn {
}

/***************************************************************************************************
 *  Problem 1a
 *
 */
class Num{
    private double value;
    public NUm(double v){
    	value = v;
    }
    public double ReturnValue(){
    	
    }



}



class BinOp implements AExp{
    BinOp(){
	
    }


    double Eval(){
    }

}










// an RPN expression is essentially a wrapper around a list of stack operations
class RPNExp {
    protected List<Sopn> instrs;

    public RPNExp(List<Sopn> instrs) { this.instrs = instrs; }

    // public double eval() {}  // Problem 1b
}


/***************************************************************************************************
 *  Test class
 *
 */

class CalcTest {
    public static void main(String[] args) {
	 //a test for Problem 1a
	AExp aexp =
	    new BinOp(new BinOp(new Num(1.0), Op.PLUS, new Num(2.0)),
	 	      Op.TIMES,
	 	      new Num(3.0));
	System.out.println("aexp evaluates to " + aexp.eval()); // aexp evaluates to 9.0
	//a test for Problem 1b
	/*
	List<Sopn> instrs = new LinkedList<Sopn>();
	instrs.add(new Push(1.0));
	instrs.add(new Push(2.0));
	instrs.add(new Calculate(Op.PLUS));
	instrs.add(new Push(3.0));
	instrs.add(new Swap());
	instrs.add(new Calculate(Op.TIMES));
	RPNExp rpnexp = new RPNExp(instrs);
	System.out.println("rpnexp evaluates to " + rpnexp.eval());  // rpnexp evaluates to 9.0
	*/
	// a test for Problem 1c
	// System.out.println("aexp converts to " + aexp.toRPN());

    }
}


interface Dict<K,V> {
    void put(K k, V v);
    V get(K k) throws NotFoundException;
}

class NotFoundException extends Exception {}


// Problem 2a
class DictImpl2<K,V> implements Dict<K,V> {
    protected Node<K,V> root;

    DictImpl2() { throw new RuntimeException("not implemented"); }

    public void put(K k, V v) { throw new RuntimeException("not implemented"); }

    public V get(K k) throws NotFoundException { throw new RuntimeException("not implemented"); }
}

interface Node<K,V> {
}

class Empty<K,V> implements Node<K,V> {
    Empty() {}
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
}


interface DictFun<A,R> {
    R invoke(A a) throws NotFoundException;
}

// Problem 2b
class DictImpl3<K,V> implements Dict<K,V> {
    protected DictFun<K,V> dFun;

    DictImpl3() { throw new RuntimeException("not implemented"); }

    public void put(K k, V v) { throw new RuntimeException("not implemented"); }

    public V get(K k) throws NotFoundException { throw new RuntimeException("not implemented"); }
}


class Pair<A,B> {
    protected A fst;
    protected B snd;

    Pair(A fst, B snd) { this.fst = fst; this.snd = snd; }

    A fst() { return fst; }
    B snd() { return snd; }
}

// Problem 2c
interface FancyDict<K,V> extends Dict<K,V> {
    void clear();
    boolean containsKey(K k);
    void putAll(List<Pair<K,V>> entries);
}


class DictTest {
    public static void main(String[] args) {

	// a test for Problem 2a
	// Dict<String,Integer> dict1 = new DictImpl2<String,Integer>();
	// dict1.put("hello", 23);
	// dict1.put("bye", 45);
	// try {
	//     System.out.println("bye maps to " + dict1.get("bye")); // prints 45
	//     System.out.println("hi maps to " + dict1.get("hi"));  // throws an exception
	// } catch(NotFoundException e) {
	//     System.out.println("not found!");  // prints "not found!"
	// }

	// a test for Problem 2b
	// Dict<String,Integer> dict2 = new DictImpl3<String,Integer>();
	// dict2.put("hello", 23);
	// dict2.put("bye", 45);
	// try {
	//     System.out.println("bye maps to " + dict2.get("bye"));  // prints 45
	//     System.out.println("hi maps to " + dict2.get("hi"));   // throws an exception
	// } catch(NotFoundException e) {
	//     System.out.println("not found!");  // prints "not found!"
	// }

	// a test for Problem 2c
	// FancyDict<String,Integer> dict3 = new FancyDictImpl2<String,Integer>();
	// dict3.put("hello", 23);
	// dict3.put("bye", 45);
	// System.out.println(dict3.containsKey("bye")); // prints true
	// dict3.clear();
	// System.out.println(dict3.containsKey("bye")); // prints false

    }
}
