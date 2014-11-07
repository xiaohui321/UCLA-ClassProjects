import syntaxtree.*;
import visitor.*;

public class Typecheck {
    public static void main(String [] args) {
       try {

             Node root = new MiniJavaParser(System.in).Goal();
                    
             FirstRoundVisitor visitor1= new FirstRoundVisitor();
             root.accept(visitor1);
                    
             GJDepthFirst<SymbolTable.Type,Object> visitor2 = new SecondRoundVisitor(visitor1.getsymbolTable());
             root.accept(visitor2,null);
                    
             //GJDepthFirst<String,Object> visitor3 = new ThirdRoundVisitor(visitor1.getsymbolTable());
             //root.accept(visitor3,null);
             System.out.println("Program type checked successfully");
        }
        catch (ParseException e) {
            System.out.println("Type error");
            System.exit(1);
        }
        catch (Error e1) {
           System.out.println("Type error");
           System.exit(1);
        }
   }
}