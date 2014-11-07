class Main {
    public static void main(String[] a){
        System.out.println(1);
    }
}

class A {
    public int run() {
        int x;
        x = 1;
        return x;
    }
}

class B extends A {
    public boolean run() { // TE
        int x;
        x = 1;
        return x;
    }
}