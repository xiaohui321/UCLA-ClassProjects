class extend{
    public static void main(String[] a){
        System.out.println(new D().ha());
    }
}


class A{
	public int ha(){
		return 1;
	}
}

class B extends A{

}

class C extends B{

}
class D extends C{
	public int lala() {
		return this.ha(1); //TE
	}
}