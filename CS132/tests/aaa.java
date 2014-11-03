

class Main {
	public static void main(String[] a){
		System.out.println(new A().run(1));
	}
}

class A {
	int x;
	int[] run;

	public boolean run(int y) {
		x = 1;
		if(! x){
			x =2;
		}
		else{
			y =2;
		}
		return !1;
	}

}
